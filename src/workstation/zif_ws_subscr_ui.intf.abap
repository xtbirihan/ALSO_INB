interface ZIF_WS_SUBSCR_UI
  public .


  methods GET_STATUS
    exporting
      !EV_STATUS type STRING
      !ET_EXCLUDES type STRING_TABLE .
  methods GET_TITLE
    exporting
      !EV_TITLE type SYTITLE
      !EV_PARAM type STRING .
  methods PROCESS_USER_COMMAND
    importing
      !IV_UCOMM type SYUCOMM
    exporting
      value(ES_BAPIRET) type BAPIRET2
      value(EV_LEAVE_SCREEN) type ABAP_BOOL
    changing
      !CS_TABSTRIP type CXTAB_TABSTRIP .
  methods INIT
    importing
      !IO_PREV_SUBSCREEN type ref to ZIF_WS_SUBSCR_UI optional
    exporting
      value(EV_DEFAULT_NEEDED) type ABAP_BOOL
      value(EV_SUBSCREEN_NO) type SYDYNNR
      value(EV_TAB) type SCXTAB_TABSTRIP-ACTIVETAB
      !EV_SUBSCREEN_PRG type SY-REPID .
  methods PBO_TAB
    exporting
      !ES_SCREEN_DATA type DATA .
  methods PAI_TAB
    importing
      !IS_SCREEN_DATA type DATA
      !IV_OK_CODE type SYUCOMM optional
    raising
      ZCX_WORKSTATION .
  methods LEAVE_SCREEN
    returning
      value(RV_LEAVE) type ABAP_BOOL .
endinterface.
