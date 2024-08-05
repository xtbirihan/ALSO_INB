interface ZIF_WS_DEFAULTS_SP
  public .


  methods GET_DEFAULTS
    exporting
      value(ES_DEFAULTS) type ZSTR_WS_DEFAULTS
      value(ES_LAGP) type /SCWM/LAGP .
  methods SET_DEFAULTS_DECO
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_STOR_BIN_LGPLA type /SCWM/DE_LGPLA
    exporting
      !ES_LAGP type /SCWM/LAGP
    raising
      ZCX_WORKSTATION .
  methods SET_DEFAULTS_WORKCENTER
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_WORKCENTER type /SCWM/DE_WORKSTATION
    raising
      ZCX_WORKSTATION .
endinterface.
