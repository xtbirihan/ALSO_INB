FUNCTION z_inb_to_prep_whr_ui_int .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_MODE) TYPE  /SCWM/DE_WHR_TO_PROC_MODE
*"     VALUE(IV_PROCESS) TYPE  /SCWM/DE_TO_WHR_PROC OPTIONAL
*"     VALUE(IV_SQUIT) TYPE  /SCWM/RL03TSQUIT DEFAULT SPACE
*"     VALUE(IV_FULL_PICK) TYPE  /SCWM/DE_FULL_PICK OPTIONAL
*"     VALUE(IV_MVE_HU) TYPE  /SCWM/DE_WHR_MVE_HU OPTIONAL
*"     VALUE(IV_MVE_MAT) TYPE  /SCWM/DE_WHR_MVE_MAT OPTIONAL
*"     VALUE(IV_MVE_HU_MULT) TYPE  /SCWM/DE_WHR_MVE_HU_MULT OPTIONAL
*"     VALUE(IV_BNAME) TYPE  /SCWM/LVS_BNAME DEFAULT SY-UNAME
*"     VALUE(IV_WTCODE) TYPE  /SCWM/DE_WTCODE DEFAULT ' '
*"     VALUE(IV_FILTER_HU) TYPE  XFELD DEFAULT ' '
*"     VALUE(IS_PARAM) TYPE  /SCWM/S_TO_CREATE_WHR_PARAM OPTIONAL
*"     VALUE(IV_ROUTE_CREA_PROD_LB) TYPE  XFELD OPTIONAL
*"     VALUE(IV_WAVE_RELEASE) TYPE  /SCWM/DE_WAVE_RELEASE_WHR OPTIONAL
*"     VALUE(IT_PREPARE_WHR_INT) TYPE  /SCWM/TT_TO_PREPARE_WHR_INT
*"       OPTIONAL
*"     VALUE(IT_PREPARE_HU_INT) TYPE  /SCWM/TT_TO_PREPARE_HU_INT
*"       OPTIONAL
*"     VALUE(IV_FORCE_EMPTY_BIN) TYPE  FLAG OPTIONAL
*"     VALUE(IV_NO_LOCK_RELEASE) TYPE  FLAG OPTIONAL
*"     VALUE(IV_LOCATION_MIXED) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(ET_LTAP_VB) TYPE  /SCWM/TT_LTAP_VB
*"     VALUE(ET_LTAP_DEL) TYPE  /SCWM/TT_LTAP_VB
*"     VALUE(ET_DOC) TYPE  /SCWM/TT_PDENIAL_ATTR
*"     VALUE(ET_OPEN_QTY) TYPE  /SCWM/TT_WHR_OPEN_QTY
*"     VALUE(ET_BAPIRET) TYPE  BAPIRETTAB
*"     VALUE(EV_SEVERITY) TYPE  BAPI_MTYPE
*"     VALUE(ET_WHR_ITEMS_CD) TYPE  /SCWM/DLV_ITEM_OUT_PRD_TAB
*"----------------------------------------------------------------------
  /scwm/cl_tm=>set_lgnum( iv_lgnum ).
  zcl_bin_determination_ctrl=>sv_force_empty_bin = iv_force_empty_bin.
  zcl_bin_determination_ctrl=>sv_location_mixed = iv_location_mixed.

  CALL FUNCTION '/SCWM/TO_PREP_WHR_UI_INT'
    EXPORTING
      iv_lgnum              = iv_lgnum
      iv_mode               = iv_mode
      iv_process            = iv_process
      iv_squit              = iv_squit
      iv_full_pick          = iv_full_pick
      iv_mve_hu             = iv_mve_hu
      iv_mve_mat            = iv_mve_mat
      iv_mve_hu_mult        = iv_mve_hu_mult
      iv_bname              = iv_bname
      iv_wtcode             = iv_wtcode
      iv_filter_hu          = iv_filter_hu
      is_param              = is_param
      iv_route_crea_prod_lb = iv_route_crea_prod_lb
      iv_wave_release       = iv_wave_release
      it_prepare_whr_int    = it_prepare_whr_int
      it_prepare_hu_int     = it_prepare_hu_int
    IMPORTING
      et_ltap_vb            = et_ltap_vb
      et_ltap_del           = et_ltap_del
      et_doc                = et_doc
      et_open_qty           = et_open_qty
      et_bapiret            = et_bapiret
      ev_severity           = ev_severity
      et_whr_items_cd       = et_whr_items_cd.
  IF iv_no_lock_release EQ abap_false.
    /scwm/cl_tm=>cleanup( ).
    ROLLBACK WORK.
  ENDIF.
  zcl_bin_determination_ctrl=>sv_force_empty_bin = abap_false.
  zcl_bin_determination_ctrl=>sv_location_mixed = abap_false.
ENDFUNCTION.
