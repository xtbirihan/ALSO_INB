interface ZIF_WS_UI_DEFAULTS
  public .


  methods SET_DEFAULTS
    importing
      !IS_DEFAULTS type DATA
    raising
      ZCX_WORKSTATION .
  methods GET_DEFAULTS
    exporting
      !ES_DEFAULTS type DATA .
  methods GET_DEFAULT_SCREEN_NO
    exporting
      value(EV_DYNNR) type SY-DYNNR
      value(EV_REPID) type SY-REPID .
  methods IS_DEFAULTS_SUFFICIENT
    returning
      value(RV_YES) type ABAP_BOOL .
  methods CALL_SCREEN .
endinterface.
