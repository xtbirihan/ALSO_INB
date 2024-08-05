interface ZIF_WS_DECO_UI
  public .


  interfaces ZIF_WS_SUBSCR_UI .
  interfaces ZIF_WS_UI_DEFAULTS .

  types:
    tt_hu_ident TYPE STANDARD TABLE OF /scwm/huident WITH DEFAULT KEY .
  types:
    BEGIN OF ty_hu_to_be_created,
      huident     TYPE /scwm/huident,
      pmatid      TYPE /scwm/de_pmatid,
      internal_nr TYPE abap_bool,
      hutype      TYPE /scwm/de_hutyp,
    END OF ty_hu_to_be_created .
  types:
    tt_hu_to_be_created TYPE STANDARD TABLE OF ty_hu_to_be_created WITH DEFAULT KEY .

  constants C_OKCODE_OK type SYUCOMM value 'OK' ##NO_TEXT.
  constants C_OKCODE_CANCEL type SYUCOMM value 'CANCEL' ##NO_TEXT.
  constants C_OKCODE_ENTER type SYUCOMM value 'ENTER' ##NO_TEXT.
  constants C_OKCODE_OK_W type SYUCOMM value 'OK_W' ##NO_TEXT.
  constants C_OKCODE_CANCEL_W type SYUCOMM value 'CANCEL_W' ##NO_TEXT.
  constants C_OKCODE_CHANGE type SYUCOMM value 'CHANGE' ##NO_TEXT.

  methods IS_WORKSTATION_MIXEDPALLET
    returning
      value(RV_MIXED) type ABAP_BOOL .
  methods CHECK_HU
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION .
  methods GET_PACK_MAT_DAT_FORF4
    importing
      !IV_PACK_MAT type MATNR
    exporting
      !EV_HUTYPE type /SCWM/DE_HUTYP
      !EV_HUTYPTEXT type /SCWM/DE_HUTYPT
      !EV_PACK_MAT_ID type /SCMB/MD_PRODID
      !ES_PMAT_SETTING type ZTINB_WSPACK
      !EV_PACK_MAT_TEXT type MAKTX
      !EV_NR_INTERNAL type FLAG .
  methods INIT_SCREEN
    importing
      !IO_CURR_STOCK_SIT type ref to CL_GUI_CUSTOM_CONTAINER optional .
  methods CHECK_TARGET_STYPE_FOR_CHPROP
    importing
      !IV_NLTYP type /SCWM/LTAP_NLTYP
    raising
      ZCX_WORKSTATION .
endinterface.
