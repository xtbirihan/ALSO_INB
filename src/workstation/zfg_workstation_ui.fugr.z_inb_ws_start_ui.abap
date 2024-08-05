FUNCTION z_inb_ws_start_ui .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_CONTROLLER) TYPE REF TO  ZIF_WS_DECO_UI
*"----------------------------------------------------------------------

  go_ws_main_ui = NEW zcl_ws_main( ).
  go_ws_deco_ui = io_controller.

  go_ws_main_ui->init(
    EXPORTING
      io_subscreen_ui   = go_ws_deco_ui
      it_tabs           = VALUE #( ( zcl_ws_main=>c_tab_deco_pallets ) ( zcl_ws_main=>c_tab_tote_place ) )
    IMPORTING
      ev_default_needed = DATA(lv_default_needed)                 " Workstation defaults
  ).

  go_def_controller = go_ws_deco_ui.
  IF lv_default_needed EQ abap_true.
    go_def_controller->get_defaults(
      IMPORTING
        es_defaults = zstr_ws_defaults
    ).
    CALL FUNCTION 'Z_INB_WS_DEFAULTS_POPUP'.
    IF NOT go_def_controller->is_defaults_sufficient( ).
      RETURN.
    ENDIF.
  ENDIF.

  DATA(lv_main_screen) = go_ws_main_ui->get_main_screen_no( ).

  CALL SCREEN lv_main_screen.

ENDFUNCTION.
