FUNCTION z_inb_ws_defaults_popup .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_DEF_CTRL) TYPE REF TO  ZIF_WS_UI_DEFAULTS OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_DEFAULTS) TYPE  ZSTR_WS_DEFAULTS
*"----------------------------------------------------------------------
  IF io_def_ctrl IS BOUND.
    go_def_controller = io_def_ctrl.
  ELSEIF go_ws_deco_ui IS BOUND.
    go_def_controller = go_ws_deco_ui.
  ELSE.
    go_def_controller = NEW zcl_general_ui_functions( NEW zcl_ws_deco_sp( ) ).
  ENDIF.
  go_def_controller->get_defaults(
    IMPORTING
      es_defaults = zstr_ws_defaults
  ).
  go_def_controller->get_default_screen_no(
    IMPORTING
      ev_dynnr = DATA(lv_screen)
  ).
  CALL SCREEN lv_screen STARTING AT 10 10.

  go_def_controller->get_defaults(
    IMPORTING
      es_defaults = es_defaults
  ).
ENDFUNCTION.
