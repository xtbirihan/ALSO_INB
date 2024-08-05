INTERFACE zif_ws_ui_main
  PUBLIC .


  TYPES tt_tabs TYPE STANDARD TABLE OF scrfname WITH DEFAULT KEY.

  METHODS get_status
    EXPORTING
      !ev_status   TYPE string
      !et_excludes TYPE string_table .
  METHODS get_title
    EXPORTING
      !ev_title TYPE sytitle
      !ev_param TYPE string .
  METHODS process_user_command
    IMPORTING
      !iv_ucomm              TYPE syucomm
    EXPORTING
      VALUE(es_bapiret)      TYPE bapiret2
      VALUE(ev_leave_screen) TYPE abap_bool
    CHANGING
      !cs_tabstrip           TYPE cxtab_tabstrip .
  METHODS pai
    RAISING
      zcx_workstation .
  METHODS pbo
    EXPORTING
      VALUE(ev_tab_subscreen) TYPE sy-dynnr
      VALUE(ev_tab_repid)     TYPE syrepid
    CHANGING
      !cs_tabstrip            TYPE cxtab_tabstrip .
  METHODS init
    IMPORTING
      it_tabs                  TYPE tt_tabs
      !io_subscreen_ui         TYPE REF TO zif_ws_subscr_ui
    EXPORTING
      VALUE(ev_default_needed) TYPE abap_bool .
  METHODS get_main_screen_no
    RETURNING
      VALUE(rv_dynnr) TYPE sy-dynnr .
  METHODS get_subscreen_ctrl
    RETURNING
      VALUE(ro_subscr) TYPE REF TO zif_ws_subscr_ui .
ENDINTERFACE.
