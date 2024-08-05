class ZCL_INB_CORE_PTS_EMPTY_BIN_PTW definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_PTS_EMPTY_BIN .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  methods SELECT_EMPTY_BINS
    importing
      !IS_T331 type /SCWM/T331
      !IS_PENALTY type ZCL_CRUD_INB_STTYPE_PTWY=>TS_PENALTY_STR
    returning
      value(RT_EMPTY_LGPLA) type /SCWM/TT_LAGPL .
ENDCLASS.



CLASS ZCL_INB_CORE_PTS_EMPTY_BIN_PTW IMPLEMENTATION.


  METHOD /scwm/if_ex_core_pts_empty_bin~delete_empty_bin_buffer.
**********************************************************************
*& Key           : RM-230425
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Empty implementation to avoid dumps
**********************************************************************

  ENDMETHOD.


  METHOD /scwm/if_ex_core_pts_empty_bin~determine_empty_bins.
**********************************************************************
*& Key           : RM-230425
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_CORE_PTS_EMPTY_BIN
*& determination of empty bins for putaway strategy
**********************************************************************
    DATA:
          lo_inst_ptwy  TYPE REF TO zcl_crud_inb_sttype_ptwy.

    IF zcl_switch=>get_switch_state( iv_lgnum = iv_lgnum
                                     iv_devid = zif_switch_const=>c_zinb_003 ) = abap_false.
      " Do not execute logic
      RETURN.
    ENDIF.

    IF ct_lagpl IS NOT INITIAL.
      RETURN.
    ENDIF.

    lo_inst_ptwy = zcl_crud_inb_sttype_ptwy=>get_inst( ).

    IF lo_inst_ptwy->ms_st_type_cust-put_rule = zif_wme_c=>gs_zptwy_rule-empt_bin AND
       lo_inst_ptwy->ms_st_type_cust-splitput = zif_wme_c=>gs_zsplit-no.

      ct_lagpl = select_empty_bins( is_t331    = is_t331
                                    is_penalty = lo_inst_ptwy->ms_penalty_str_curr ).

      IF ct_lagpl IS NOT INITIAL.
*         if bin was found with our own logic, then we do NOT want to
*         run the standard empty bin determination
        cv_do_not_run_std = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD select_empty_bins.
**********************************************************************
*& Key           : RM-230425
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Select new bins based on the next penalty
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
        SELECT * FROM /scwm/lagpl INTO TABLE @rt_empty_lgpla
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
