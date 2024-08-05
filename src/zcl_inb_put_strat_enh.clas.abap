class ZCL_INB_PUT_STRAT_ENH definition
  public
  final
  create public .

public section.

  methods UPDATE_PUT_RULE_AND_SPLIT
    importing
      !IS_T331 type /SCWM/T331
      !IS_PENALTY type /SCWM/S_PENALTY_SRT
      !IS_LTAP type /SCWM/LTAP
    changing
      !CV_SPLITPUT type XFELD
      !CV_PUT_RULE type /SCWM/DE_PUT_RULE .
protected section.
private section.

  methods UPDATE_GLOBAL_VARS
    importing
      !IS_ST_TYPE_CUST type ZCL_CRUD_INB_STTYPE_PTWY=>TS_ST_TYPE_CUST
    changing
      !CV_SPLITPUT type XFELD
      !CV_PUT_RULE type /SCWM/DE_PUT_RULE .
  methods GET_CONSTANT
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_PARAMETER type ZDE_PARAM_ID2
    returning
      value(RV_CONST) type ZDE_PARAM_LOW .
ENDCLASS.



CLASS ZCL_INB_PUT_STRAT_ENH IMPLEMENTATION.


  METHOD get_constant.
**********************************************************************
*& Key           : RM-230314
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get constant from parameter framework
**********************************************************************

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = iv_lgnum
        iv_process   = zif_param_const=>c_zinb_0001
        iv_parameter = iv_parameter
      IMPORTING
        ev_constant  = rv_const ).

  ENDMETHOD.


  METHOD update_global_vars.
**********************************************************************
*& Key           : RM-230502
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Update split and putaway rule
**********************************************************************

    " Split
    CASE is_st_type_cust-splitput.
      WHEN zif_wme_c=>gs_zsplit-no.
        cv_splitput = abap_false.

      WHEN zif_wme_c=>gs_zsplit-yb.
        cv_splitput = abap_true.

      WHEN zif_wme_c=>gs_zsplit-ys.
        cv_splitput = abap_true.
    ENDCASE.

    " Putaway rulems_st_type_cust-put_rule
    CASE is_st_type_cust-put_rule.
      WHEN zif_wme_c=>gs_zptwy_rule-add_to_stock.
        cv_put_rule = wmegc_prl_addempt.

      WHEN zif_wme_c=>gs_zptwy_rule-empt_bin.
        cv_put_rule = wmegc_prl_empt.

      WHEN zif_wme_c=>gs_zptwy_rule-empt_bin_max.
        cv_put_rule = wmegc_prl_empt.

      WHEN zif_wme_c=>gs_zptwy_rule-empt_bin_1.
        cv_put_rule = wmegc_prl_empt.
    ENDCASE.

  ENDMETHOD.


  METHOD update_put_rule_and_split.
**********************************************************************
*& Key           : RM-230314
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Update global split and putaway rule according to penalty sequence
*& determined in BAdI impl. /SCWM/EX_CORE_PTS_TYPSQ
**********************************************************************

    DATA:
          lo_inst_ptwy TYPE REF TO zcl_crud_inb_sttype_ptwy.

    IF zcl_switch=>get_switch_state( iv_lgnum = is_t331-lgnum
                                     iv_devid = zif_switch_const=>c_zinb_003 ) = abap_false.
      " Do not execute logic
      RETURN.
    ENDIF.

    lo_inst_ptwy = zcl_crud_inb_sttype_ptwy=>get_inst( ).

    IF lo_inst_ptwy->mt_penalty_list IS INITIAL.
      RETURN.
    ENDIF.

    " Check if the next sequence should be used
    READ TABLE lo_inst_ptwy->mt_penalty_list INTO DATA(ls_curr_pen) WITH KEY lgnum = is_penalty-lgnum
                                                                             lgtyp = is_penalty-lgtyp
                                                                             storsect = is_penalty-storsect
                                                                             bintype = is_penalty-bintype.
    " Mark the current penalty sequence to use for empty bin determination in filter badi
    lo_inst_ptwy->ms_penalty_str_curr = ls_curr_pen.

    IF zcl_bin_determination_ctrl=>sv_force_empty_bin = abap_true AND
        is_ltap-nltyp IS NOT INITIAL.
      " Suppress the current logic and use Set Empty bin and do not split
      DATA(lv_put_rule) = get_constant( iv_lgnum = is_t331-lgnum iv_parameter = zif_param_const=>c_zde_put_rule ).
      DATA(lv_split)    = get_constant( iv_lgnum = is_t331-lgnum iv_parameter = zif_param_const=>c_zde_splitput_lgtyp ).

      lo_inst_ptwy->ms_st_type_cust-lgnum = is_t331-lgnum.
      lo_inst_ptwy->ms_st_type_cust-put_rule = lv_put_rule.
      lo_inst_ptwy->ms_st_type_cust-splitput = lv_split.

      update_global_vars( EXPORTING is_st_type_cust = lo_inst_ptwy->ms_st_type_cust
                          CHANGING  cv_put_rule     = cv_put_rule
                                    cv_splitput     = cv_splitput ).

      RETURN.
    ENDIF.

    IF sy-subrc <> 0 OR ls_curr_pen-new_seq = abap_false.
      update_global_vars( EXPORTING is_st_type_cust = lo_inst_ptwy->ms_st_type_cust
                          CHANGING  cv_put_rule     = cv_put_rule
                                    cv_splitput     = cv_splitput ).

      RETURN.
    ENDIF.

    " In case of Empty Bin MAX and Split check the quantity of the task regarding the orginal one
    IF lo_inst_ptwy->ms_st_type_cust-put_rule = zif_wme_c=>gs_zptwy_rule-empt_bin_max AND
       lo_inst_ptwy->ms_orig_wt-rdocid = is_ltap-rdocid AND
       lo_inst_ptwy->ms_orig_wt-ritmid = is_ltap-ritmid AND
       lo_inst_ptwy->ms_orig_wt-vsola > is_ltap-vsola.
      " Do not change Storage Type Search Sequence

      update_global_vars( EXPORTING is_st_type_cust = lo_inst_ptwy->ms_st_type_cust
                          CHANGING  cv_put_rule     = cv_put_rule
                                    cv_splitput     = cv_splitput ).

      RETURN.
    ENDIF.

    READ TABLE lo_inst_ptwy->mt_st_type_cust_used WITH KEY lgtyp = is_t331-lgtyp
                                                           executed = abap_false
                                                       INTO DATA(ls_ptwy_cust).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ls_ptwy_cust-executed = abap_true.
    MODIFY lo_inst_ptwy->mt_st_type_cust_used FROM ls_ptwy_cust INDEX sy-tabix.

    lo_inst_ptwy->ms_st_type_cust = ls_ptwy_cust.

    update_global_vars( EXPORTING is_st_type_cust = lo_inst_ptwy->ms_st_type_cust
                        CHANGING  cv_put_rule     = cv_put_rule
                                  cv_splitput     = cv_splitput ).

  ENDMETHOD.
ENDCLASS.
