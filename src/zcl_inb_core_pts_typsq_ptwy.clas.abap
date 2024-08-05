class ZCL_INB_CORE_PTS_TYPSQ_PTWY definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_PTS_TYPSQ .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  constants:
    BEGIN OF c_pick_stock,
      yes TYPE zde_pick_stock VALUE 'Y',
      no  TYPE zde_pick_stock VALUE 'N',
    END OF c_pick_stock .

  methods CHECK_PICKING_STOCK
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_MAT_ID type /SCWM/DE_MATID
      !IV_MIXED_PALLET type ABAP_BOOL
    exporting
      value(EV_PICK_STOCK) type ZDE_PICK_STOCK
      !ET_BAPIRET type BAPIRETTAB .
  methods ADD_MESSAGES_BAPIRET
    changing
      !CT_BAPIRET type BAPIRETTAB .
ENDCLASS.



CLASS ZCL_INB_CORE_PTS_TYPSQ_PTWY IMPLEMENTATION.


  METHOD /scwm/if_ex_core_pts_typsq~storage_type_seq.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      lv_pick_stock      TYPE abap_bool,
      lo_inst_ptwy       TYPE REF TO zcl_crud_inb_sttype_ptwy,
      ls_st_sseq_mapping TYPE ztinb_pcksseqmap,
      lv_mixed_pallet    TYPE abap_bool. " ToDo: to change with real flag

    IF zcl_switch=>get_switch_state( iv_lgnum = flt_val
                                     iv_devid = zif_switch_const=>c_zinb_003 ) = abap_false.
      " Do not execute logic
      RETURN.
    ENDIF.

    lv_mixed_pallet = zcl_bin_determination_ctrl=>sv_location_mixed.

    DATA(lt_st_sseq_mapping) = zcl_crud_ztinb_pcksseqmap=>select_multi_by_putsseq_monpal(
                                                          iv_lgnum             = flt_val
                                                          iv_dummy_sseq        = iv_put_sseq
                                                          iv_stss_mixed_pallet = lv_mixed_pallet ).

    IF lt_st_sseq_mapping IS INITIAL.
      " No Storage type sequence mapping found for sseq &1.
      MESSAGE w001(zmc_inb) WITH iv_put_sseq INTO DATA(lv_msg).

      add_messages_bapiret( CHANGING ct_bapiret = et_bapiret ).
      RETURN.
    ENDIF.

    DATA(lv_lines) = lines( lt_st_sseq_mapping ).

    IF lv_lines = 1 AND lt_st_sseq_mapping[ 1 ]-pick_stock IS INITIAL AND
                        lt_st_sseq_mapping[ 1 ]-stss_mixed_pallet IS INITIAL.
      " Mono Pallets
      DATA(lv_check_stock) = abap_false.
      ev_put_sseq = lt_st_sseq_mapping[ 1 ]-put_sseq.
      RETURN.
    ELSE.
      " Sort pick_stock in descending order to determine if there is an entry
      " with pick_stock <> empty. If yes - check stock, otherwise check stock is not neccessary
      SORT lt_st_sseq_mapping BY pick_stock DESCENDING.
      IF lt_st_sseq_mapping[ 1 ]-pick_stock IS NOT INITIAL.
        lv_check_stock = abap_true.
      ENDIF.
    ENDIF.

    IF lv_check_stock = abap_false.
      RETURN.
    ENDIF.

    check_picking_stock(
      EXPORTING
        iv_lgnum        = flt_val
        iv_mat_id       = is_ltap-matid
        iv_mixed_pallet = lv_mixed_pallet
      IMPORTING
        ev_pick_stock   = lv_pick_stock
        et_bapiret      = et_bapiret ).

    IF lv_pick_stock IS INITIAL.
      RETURN.
    ENDIF.

    ls_st_sseq_mapping = VALUE #( lt_st_sseq_mapping[ lgnum      = flt_val
                                                      dummy_sseq = iv_put_sseq
                                                      pick_stock = lv_pick_stock ] OPTIONAL ).

    IF ls_st_sseq_mapping IS INITIAL.
      " No Storage type sequence mapping found for sseq &1.
      MESSAGE w001(zmc_inb) WITH iv_put_sseq INTO lv_msg.

      add_messages_bapiret( CHANGING ct_bapiret = et_bapiret ).
      RETURN.
    ENDIF.

    lo_inst_ptwy = zcl_crud_inb_sttype_ptwy=>get_inst( ).

    IF lo_inst_ptwy->mt_st_type_cust_used IS INITIAL.
      lo_inst_ptwy->select_by_lgnum_put_sseq( iv_lgnum    = flt_val
                                              iv_put_sseq = ls_st_sseq_mapping-put_sseq ).
    ENDIF.

    IF lo_inst_ptwy->mt_st_type_cust_used IS INITIAL.
      RETURN.
    ENDIF.

    SORT lo_inst_ptwy->mt_st_type_cust_used BY seqno.

    " Clear the penalty table from the previous run
    IF lo_inst_ptwy->mt_penalty_list IS NOT INITIAL.
      DELETE lo_inst_ptwy->mt_penalty_list WHERE lgnum = flt_val.
    ENDIF.

    ev_put_sseq = ls_st_sseq_mapping-put_sseq.

  ENDMETHOD.


  METHOD add_messages_bapiret.
**********************************************************************
*& Key           : RM-230309
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


  METHOD check_picking_stock.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
      lt_productid     TYPE /scwm/tt_matid,
      lt_to            TYPE /scwm/tt_to_det_mon,
      lt_huhdr         TYPE /scwm/tt_huhdr,
      lt_pick_st_types TYPE ztt_picksttype,
      lo_pts_typsq_mdl TYPE REF TO zcl_core_pts_typsq_mdl.


    IF iv_mixed_pallet = abap_true.
      lt_pick_st_types = zcl_crud_ztinb_picksttype=>select_multi_by_lgnum_mixpal(
                                                    iv_lgnum      = iv_lgnum
                                                    iv_mix_pallet = abap_true ).
    ELSE.
      lt_pick_st_types = zcl_crud_ztinb_picksttype=>select_multi_by_lgnum_monopal(
                                                    iv_lgnum       = iv_lgnum
                                                    iv_mono_pallet = abap_true ).
    ENDIF.

    IF sy-subrc <> 0.
      " No entries in table & for &
      MESSAGE i582(rp) WITH 'ZTINB_PICKSTTYPE' iv_lgnum INTO DATA(lv_msg).

      add_messages_bapiret( CHANGING ct_bapiret = et_bapiret ).
      RETURN.
    ENDIF.

    APPEND iv_mat_id TO lt_productid.

    CALL FUNCTION '/SCWM/SELECT_STOCK'
      EXPORTING
        iv_lgnum = iv_lgnum
        ir_lgtyp = VALUE rseloption( FOR <ls_pick_st_types> IN lt_pick_st_types
                                        ( low    = <ls_pick_st_types>-lgtyp
                                          sign   = wmegc_sign_inclusive
                                          option = wmegc_option_eq ) )
        it_matid = lt_productid
      IMPORTING
        et_huhdr = lt_huhdr.

    IF lt_huhdr IS INITIAL.
      " No stock available
      ev_pick_stock = c_pick_stock-no.
    ELSE.
      " Available stock
      ev_pick_stock = c_pick_stock-yes.
    ENDIF.

    " Check if there are nay open WTs with destination storage type
    CALL FUNCTION '/SCWM/TO_GET_WIP'
      EXPORTING
        iv_lgnum   = iv_lgnum
        iv_open    = abap_true
        iv_dstdata = abap_true
        is_selcrit = VALUE /scwm/s_to_selcrit_mon(
                     r_lgtyp = VALUE rseloption( FOR <ls_pick_st_type> IN lt_pick_st_types
                                                   ( low    = <ls_pick_st_type>-lgtyp
                                                     sign   = wmegc_sign_inclusive
                                                     option = wmegc_option_eq ) ) )
      IMPORTING
        et_to      = lt_to.

    IF lt_to IS NOT INITIAL.
      " Available stock
      ev_pick_stock = c_pick_stock-yes.
    ENDIF.

    " Check for pre-created WTs
    lo_pts_typsq_mdl = zcl_core_pts_typsq_mdl=>get_instance( ).
    DATA(lt_pre_crt_wts) = lo_pts_typsq_mdl->get_pre_crt_wt(
                           it_lgtyp = VALUE /scwm/tt_lgtyp_r( FOR <ls_pick_st_types> IN lt_pick_st_types
                                                              ( sign   = wmegc_sign_inclusive
                                                                option = wmegc_option_eq
                                                                low    = <ls_pick_st_types>-lgtyp ) ) ).
    IF lt_pre_crt_wts IS NOT INITIAL.
      " Pre-created WTs exists
      ev_pick_stock = c_pick_stock-yes.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
