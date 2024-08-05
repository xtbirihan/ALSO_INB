FUNCTION z_inb_start_tabbed_ui .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_CONTROLLER) TYPE REF TO  ZIF_WS_SUBSCR_UI
*"     REFERENCE(IT_TABS) TYPE  ZIF_WS_UI_MAIN=>TT_TABS
*"----------------------------------------------------------------------

  go_ws_main_ui = NEW zcl_ws_main( ).

  go_ws_main_ui->init(
    EXPORTING
      io_subscreen_ui   = io_controller
      it_tabs           = it_tabs
  ).

  go_def_controller ?= io_controller.

  DATA(lv_main_screen) = go_ws_main_ui->get_main_screen_no( ).

  CALL SCREEN lv_main_screen.

ENDFUNCTION.
