CLASS zcl_inb_core_pts_filtsort_ptwy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_core_pts_filt_sort .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS add_messages_bapiret
      CHANGING
        !ct_bapiret TYPE bapirettab .
    METHODS read_material
      IMPORTING
        !iv_lgnum           TYPE /scwm/lgnum
        !iv_matid           TYPE /scwm/de_matid
        !iv_lgtyp           TYPE /scwm/lgtyp
        !iv_entitled        TYPE /scwm/de_entitled
      RETURNING
        VALUE(rs_mat_lgtyp) TYPE /scwm/s_material_lgtyp .
    METHODS check_capacity
      IMPORTING
        !iv_matid          TYPE /scwm/de_matid
        !iv_entitled       TYPE /scwm/de_entitled
        !iv_lptyp          TYPE /scwm/lvs_lptyp
        !is_t331           TYPE /scwm/t331
      RETURNING
        VALUE(rt_bintypes) TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty .
    METHODS select_next_penalty_bins
      IMPORTING
        !is_penalty          TYPE zcl_crud_inb_sttype_ptwy=>ts_penalty_str
        !is_t331             TYPE /scwm/t331
      RETURNING
        VALUE(rt_next_lagpl) TYPE /scwm/tt_lagpl .
    METHODS act_area_evaluate
      IMPORTING
        !is_t331            TYPE /scwm/t331
        !is_t333            TYPE /scwm/t333
        !it_hlplagpl        TYPE /scwm/tt_lagpl
      RETURNING
        VALUE(rt_lagp_area) TYPE /scwm/tt_lagps_area .
ENDCLASS.



CLASS ZCL_INB_CORE_PTS_FILTSORT_PTWY IMPLEMENTATION.


  METHOD /scwm/if_ex_core_pts_filt_sort~filt_sort.
**********************************************************************
*& Key           : RM-230316
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      ls_lagp_ok    TYPE rsdsselopt,
      lt_lagp_ok    TYPE rseloption,
      lt_lagp_1_bin TYPE rseloption,
      lt_lagp_area  TYPE /scwm/tt_lagps_area,
      lo_inst_ptwy  TYPE REF TO zcl_crud_inb_sttype_ptwy.

    DATA:
      lt_hlplagpl_save      TYPE /scwm/tt_lagpl,
      lv_cnt_curr_bins      TYPE i,
      lv_cnt_next_bins      TYPE i,
      lv_capa_all_bins      TYPE /scwm/s_quan-quan,
      lv_fix_capa_curr_bint TYPE /scwm/s_quan-quan,
      lv_capa_next_bints    TYPE /scwm/s_quan-quan.

    IF zcl_switch=>get_switch_state( iv_lgnum = is_t331-lgnum
                                     iv_devid = zif_switch_const=>c_zinb_003 ) = abap_false.
      " Do not execute logic
      RETURN.
    ENDIF.

    lo_inst_ptwy = zcl_crud_inb_sttype_ptwy=>get_inst( ).

    IF ct_hlplagpl IS INITIAL AND lo_inst_ptwy->ms_st_type_cust-put_rule <> zif_wme_c=>gs_zptwy_rule-empt_bin.
      RETURN.
    ENDIF.

    CASE lo_inst_ptwy->ms_st_type_cust-put_rule.
      WHEN zif_wme_c=>gs_zptwy_rule-add_to_stock.
        " Delete list of determined empty bins
        DELETE ct_hlplagpl WHERE kzler = abap_true.

      WHEN zif_wme_c=>gs_zptwy_rule-empt_bin.
        " Select empty bins will be done in BAdI /SCWM/EX_CORE_PTS_EMPTY_BIN, method /SCWM/IF_EX_CORE_PTS_EMPTY_BIN~DETERMINE_EMPTY_BINS

      WHEN zif_wme_c=>gs_zptwy_rule-empt_bin_max.
        " Check capacity of Empty bin = X (MAX)
        IF lo_inst_ptwy->ms_st_type_cust-splitput = zif_wme_c=>gs_zsplit-yb.

          " Save original WT to be check in case of split
          IF lo_inst_ptwy->ms_orig_wt-rdocid <> is_ltap-rdocid AND
             lo_inst_ptwy->ms_orig_wt-ritmid <> is_ltap-ritmid.
            lo_inst_ptwy->ms_orig_wt = is_ltap.
          ENDIF.

          DATA(lv_max_bins) = read_material( iv_lgnum    = is_t331-lgnum
                                             iv_lgtyp    = is_t331-lgtyp
                                             iv_matid    = is_ltap-matid
                                             iv_entitled = is_ltap-entitled )-zz1_maxput_stt.

          IF lv_max_bins IS INITIAL.
            RETURN.
          ENDIF.

          DATA(lt_bintypes) = check_capacity( iv_matid = is_ltap-matid
                                              iv_entitled = is_ltap-entitled
                                              iv_lptyp = ct_hlplagpl[ 1 ]-lptyp
                                              is_t331  = is_t331 ).

          IF lt_bintypes IS INITIAL.
            RETURN.
          ENDIF.

          IF is_ltap-vsola > ( lv_max_bins * lt_bintypes[ 1 ]-max_qty ).
            " Capacity not enough

            READ TABLE lo_inst_ptwy->mt_penalty_list INTO DATA(ls_curr_penalty)
                                         WITH KEY lgtyp    = ct_hlplagpl[ 1 ]-lgtyp
                                                  storsect = ct_hlplagpl[ 1 ]-lgber
                                                  bintype  = ct_hlplagpl[ 1 ]-lptyp.
            IF sy-subrc <> 0.
              RETURN.
            ENDIF.

            " Mark the current penalty line as used
            ls_curr_penalty-executed = abap_true.
            MODIFY lo_inst_ptwy->mt_penalty_list FROM ls_curr_penalty INDEX sy-tabix.

            " Take next penalty list
            READ TABLE lo_inst_ptwy->mt_penalty_list INTO DATA(ls_nex_penalty) WITH KEY executed = abap_false.
            IF sy-subrc <> 0.
              RETURN.
            ENDIF.

            " If the next penalty tring is the first one from the next search sequence group
            " Do not proceed and continue the check with the next search sequence from the ZTINB_STTYPECUST table
            IF ls_nex_penalty-new_seq = abap_true.
              DELETE ct_hlplagpl WHERE lgtyp = is_t331-lgtyp.
              RETURN.
            ENDIF.

            DATA(lt_next_bintypes) = check_capacity( iv_matid = is_ltap-matid
                                                     iv_entitled = is_ltap-entitled
                                                     iv_lptyp = ls_nex_penalty-bintype
                                                     is_t331  = is_t331 ).

            IF lt_next_bintypes IS INITIAL.
              RETURN.
            ENDIF.

            " Find next penalty bins
            DATA(lt_next_lgpla) = select_next_penalty_bins( is_t331    = is_t331
                                                            is_penalty = ls_nex_penalty ).

            IF lt_next_lgpla IS INITIAL.
              " No Storage bins found
              DELETE ct_hlplagpl WHERE lgtyp = is_t331-lgtyp.
              RETURN.
            ENDIF.

            " Sort next penalty bins
            DATA(lt_next_area) = act_area_evaluate( is_t331     = is_t331
                                                    is_t333     = is_t333
                                                    it_hlplagpl = lt_next_lgpla ).
            " Sort current penalty bins
            DATA(lt_curr_area) = act_area_evaluate( is_t331     = is_t331
                                                    is_t333     = is_t333
                                                    it_hlplagpl = ct_hlplagpl ).

            lt_hlplagpl_save = ct_hlplagpl.
            DELETE ct_hlplagpl WHERE lgtyp = is_t331-lgtyp.

            DO lv_max_bins - 1 TIMES.
              " Check how much can be fitted in max_bins

              lv_cnt_curr_bins = lv_max_bins - sy-index .
              lv_cnt_next_bins = sy-index.

              lv_fix_capa_curr_bint = lv_cnt_curr_bins * lt_bintypes[ 1 ]-max_qty.
              lv_capa_next_bints = lt_next_bintypes[ 1 ]-max_qty * lv_cnt_next_bins.

              IF lv_fix_capa_curr_bint + lv_capa_next_bints > is_ltap-vsola.

                " Assign sorted bins to  CT_HLPLAGPL
                APPEND LINES OF lt_hlplagpl_save FROM 1 TO lv_cnt_curr_bins TO ct_hlplagpl.
                APPEND LINES OF lt_next_lgpla FROM 1 TO lv_cnt_next_bins TO ct_hlplagpl.

                EXIT.
              ENDIF.
            ENDDO.

          ELSE.
            " Capacity enough
            " Sort bins
            lt_lagp_area = act_area_evaluate( is_t331     = is_t331
                                              is_t333     = is_t333
                                              it_hlplagpl = ct_hlplagpl ).

            LOOP AT lt_lagp_area ASSIGNING FIELD-SYMBOL(<ls_lagp_area>).
              " Take the first n Max bins
              CLEAR ls_lagp_ok.
              IF sy-tabix > lv_max_bins.
                EXIT.
              ENDIF.

              ls_lagp_ok = VALUE #( sign   = wmegc_sign_inclusive
                                    option = wmegc_option_eq
                                    low    = <ls_lagp_area>-lgpla ).

              APPEND ls_lagp_ok TO lt_lagp_ok.
            ENDLOOP.

            DELETE ct_hlplagpl WHERE lgpla NOT IN lt_lagp_ok.
          ENDIF.
        ENDIF.

      WHEN zif_wme_c=>gs_zptwy_rule-empt_bin_1.
        " Only 1 bin should be used; delete the rest
        lt_lagp_1_bin = VALUE #( ( sign   = wmegc_sign_inclusive
                                   option = wmegc_option_eq
                                   low    = ct_hlplagpl[ 1 ]-lgpla ) ).

        DELETE ct_hlplagpl WHERE lgpla NOT IN lt_lagp_1_bin.
    ENDCASE.

  ENDMETHOD.


  METHOD act_area_evaluate.
**********************************************************************
*& Key           : RM-230316
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
          lt_lgpla_unsorted TYPE /scwm/tt_lgpla.

    lt_lgpla_unsorted = VALUE #( FOR <ls_hlplagpl> IN it_hlplagpl
                               ( <ls_hlplagpl>-lgpla ) ).

    CALL FUNCTION '/SCWM/ACT_AREA_EVALUATE'
      EXPORTING
        iv_lgnum     = is_t331-lgnum
        it_lgpla     = lt_lgpla_unsorted
        iv_act_type  = is_t333-act_type
      IMPORTING
        et_lagp_area = rt_lagp_area
      EXCEPTIONS
        not_found    = 1
        OTHERS       = 2.


  ENDMETHOD.


  METHOD add_messages_bapiret.
**********************************************************************
*& Key           : RM-230316
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      ls_bapiret TYPE bapiret2.

    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO DATA(lv_msg).

    ls_bapiret = VALUE #( id         = sy-msgid
                          type       = sy-msgty
                          number     = sy-msgno
                          message_v1 = sy-msgv1
                          message_v2 = sy-msgv2
                          message_v3 = sy-msgv3
                          message_v4 = sy-msgv4
                          message    = lv_msg ).

    APPEND ls_bapiret TO ct_bapiret.

  ENDMETHOD.


  METHOD check_capacity.
**********************************************************************
*& Key           : RM-230327
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    NEW zcl_algorithm_facade( )->determine_bintyp_capacity(
      EXPORTING
        iv_lgnum        = is_t331-lgnum
        it_entitled     = VALUE #( ( entitled = iv_entitled ) )
        is_select_crit  = VALUE #( lgtyp_r = VALUE #( ( sign   = wmegc_sign_inclusive
                                                        option = wmegc_option_eq
                                                        low    = is_t331-lgtyp ) )
                                   lptyp_r = VALUE #( ( sign   = wmegc_sign_inclusive
                                                        option = wmegc_option_eq
                                                        low    = iv_lptyp ) )
                                   matid_r = VALUE #( ( sign   = wmegc_sign_inclusive
                                                        option = wmegc_option_eq
                                                        low    = iv_matid ) ) )
      IMPORTING
        et_lptyp_maxqty = rt_bintypes ).


  ENDMETHOD.


  METHOD read_material.
**********************************************************************
*& Key           : RM-230316
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
          lt_mat_lgtyp TYPE /scwm/tt_material_lgtyp.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid     = iv_matid
            iv_entitled  = iv_entitled
            iv_lgnum     = iv_lgnum
            iv_lgtyp     = iv_lgtyp
          IMPORTING
            et_mat_lgtyp = lt_mat_lgtyp.

      CATCH /scwm/cx_md_interface
            /scwm/cx_md_material_exist
            /scwm/cx_md_mat_lgnum_exist
            /scwm/cx_md_lgnum_locid
            /scwm/cx_md.

        RETURN.
    ENDTRY.

    READ TABLE lt_mat_lgtyp INTO DATA(ls_mat_lgtyp) WITH KEY lgtyp = iv_lgtyp.

    IF sy-subrc = 0.
      rs_mat_lgtyp = ls_mat_lgtyp.
    ENDIF.


  ENDMETHOD.


  METHOD select_next_penalty_bins.
**********************************************************************
*& Key           : RM-230316
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
      lv_order_by(100) TYPE c.

    " Select new bins based on the next penalty
    IF is_t331-prber IS INITIAL AND is_t331-prlet IS INITIAL.
      lv_order_by = 'mandt, lgnum, lgtyp, kzler, skzue, plauf DESCENDING, kzvol, fcapa, clsp_sort, lgber, lptyp, lgpla'.
    ELSEIF is_t331-prber IS INITIAL.
      lv_order_by = 'mandt, lgnum, lgtyp, kzler, skzue, lptyp, plauf DESCENDING, kzvol, fcapa, clsp_sort, lgber, lgpla'.
    ELSEIF is_t331-prlet IS INITIAL.
      lv_order_by = 'mandt, lgnum, lgtyp, kzler, skzue, lgber, plauf DESCENDING, kzvol, fcapa, clsp_sort, lptyp, lgpla'.
    ELSE.
      lv_order_by = 'mandt, lgnum, lgtyp, kzler, skzue, lgber, lptyp, plauf DESCENDING, kzvol, fcapa, clsp_sort, lgpla'.
    ENDIF.

    " SELECTION based ON /scwm/lput_bin_detf26 PERFORM bin_determination_2
    TRY.
        SELECT * FROM /scwm/lagpl INTO TABLE @rt_next_lagpl
                WHERE lgnum = @is_t331-lgnum
                AND lgtyp = @is_penalty-lgtyp
                AND kzler = @abap_true
                AND skzue = @space
                AND lgber = @is_penalty-storsect
                AND lptyp = @is_penalty-bintype
                AND plauf = @space
                ORDER BY (lv_order_by).
      CATCH cx_sy_dynamic_osql_semantics
            cx_sy_dynamic_osql_syntax.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
