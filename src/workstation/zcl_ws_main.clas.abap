class ZCL_WS_MAIN definition
  public
  final
  create public .

public section.

  interfaces ZIF_WS_UI_MAIN .

  constants C_FUNC_DECO_PALLETS type SY-UCOMM value 'DECO_PALLETS' ##NO_TEXT.
  constants C_FUNC_REPL_UI type SY-UCOMM value 'REPL_UI' ##NO_TEXT.
  constants C_FUNC_TOTE_PLACE type SY-UCOMM value 'TOTE_PLACE' ##NO_TEXT.
  constants C_MAIN_SCREEN type SY-DYNNR value '0100' ##NO_TEXT.
  constants C_TAB_DECO_PALLETS type SCRFNAME value 'TAB_DECO_PALLETS' ##NO_TEXT.
  constants C_TAB_REPL_UI type SCRFNAME value 'TAB_REPLENISH' ##NO_TEXT.
  constants C_TAB_TOTE_PLACE type SCRFNAME value 'TAB_TOTE_PLACE' ##NO_TEXT.
  constants C_FUNC_DISPLAY_PRODCT type SY-UCOMM value 'DISPPRD' ##NO_TEXT.
protected section.
private section.

  data MO_SUBSCREEN type ref to ZIF_WS_SUBSCR_UI .
  data MT_ALLOWED_TABS type ZIF_WS_UI_MAIN=>TT_TABS .
  data MV_CURRENT_TAB type SCXTAB_TABSTRIP-ACTIVETAB .
  data MV_CURRENT_TAB_REPID type SY-REPID .
  data MV_CURRENT_TAB_SCREEN type SY-DYNNR .
  data MO_DECO_UI type ref to ZIF_WS_DECO_UI .
ENDCLASS.



CLASS ZCL_WS_MAIN IMPLEMENTATION.


  method ZIF_WS_UI_MAIN~GET_MAIN_SCREEN_NO.
    rv_dynnr = c_main_screen.
  endmethod.


  method ZIF_WS_UI_MAIN~GET_STATUS.
    mo_subscreen->get_status(
      IMPORTING
        ev_status   = ev_status
        et_excludes = et_excludes                 " Table of Strings
    ).
  endmethod.


  method ZIF_WS_UI_MAIN~GET_SUBSCREEN_CTRL.
    ro_subscr = mo_subscreen.
  endmethod.


  method ZIF_WS_UI_MAIN~GET_TITLE.
    mo_subscreen->get_title(
      IMPORTING
        ev_title = ev_title                 " Title Line
        ev_param = ev_param                 " Parameter
    ).
  endmethod.


  METHOD zif_ws_ui_main~init.
    IF io_subscreen_ui IS INSTANCE OF zif_ws_deco_ui.
      mo_deco_ui ?= io_subscreen_ui.
    ENDIF.
    mo_subscreen = io_subscreen_ui.
    mo_subscreen->init(
      IMPORTING
         ev_default_needed = ev_default_needed                 " Workstation defaults
         ev_subscreen_no   = mv_current_tab_screen                 " Dynpro Number
         ev_subscreen_prg  = mv_current_tab_repid
    ).
    mt_allowed_tabs = it_tabs.
  ENDMETHOD.


  method ZIF_WS_UI_MAIN~PAI ##needed.

  endmethod.


  METHOD zif_ws_ui_main~pbo.
    ev_tab_subscreen = mv_current_tab_screen.
    ev_tab_repid     = mv_current_tab_repid.
    cs_tabstrip-activetab = mv_current_tab.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'TAB'.
        READ TABLE mt_allowed_tabs TRANSPORTING NO FIELDS
             WITH KEY table_line = screen-name.
        IF sy-subrc NE 0.
          screen-active = '0'.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_ws_ui_main~process_user_command.
    DATA(lo_subscreen_prev) = mo_subscreen.

    CASE iv_ucomm.
      WHEN c_func_repl_ui.
        IF mo_subscreen IS INSTANCE OF zcl_replenishment_ui
           OR
           mo_subscreen IS BOUND AND mo_subscreen->leave_screen( ) EQ abap_false.
          RETURN.
        ENDIF.
        mo_subscreen = NEW zcl_replenishment_ui( ).

      WHEN c_func_deco_pallets.
        IF mo_subscreen IS INSTANCE OF zcl_ws_deco_ui
           OR
           mo_subscreen IS BOUND AND mo_subscreen->leave_screen( ) EQ abap_false.
          RETURN.
        ENDIF.
        IF mo_deco_ui IS NOT BOUND.
          mo_deco_ui   = NEW zcl_ws_deco_ui( ).
        ENDIF.
        mo_subscreen = mo_deco_ui.

      WHEN c_func_tote_place.
        IF mo_subscreen IS INSTANCE OF zcl_ws_tote_place_ui
           OR
           mo_subscreen IS BOUND AND mo_subscreen->leave_screen( ) EQ abap_false.
          RETURN.
        ENDIF.
        mo_subscreen = NEW zcl_ws_tote_place_ui( ).

      WHEN OTHERS.
        mo_subscreen->process_user_command(
          EXPORTING
            iv_ucomm        = iv_ucomm                 " Function Code
          IMPORTING
            es_bapiret      = es_bapiret                 " Return Parameter
            ev_leave_screen = ev_leave_screen
          CHANGING
            cs_tabstrip     = cs_tabstrip
        ).
        RETURN.
    ENDCASE.
    mo_subscreen->init(
      EXPORTING
        io_prev_subscreen = lo_subscreen_prev
      IMPORTING
        ev_default_needed = DATA(lv_default_needed)                 " Workstation defaults
        ev_subscreen_no   = DATA(lv_current_tab_screen)                 " Dynpro Number
        ev_subscreen_prg  = DATA(lv_current_tab_prg)
        ev_tab            = DATA(lv_tab)
    ).
    IF lv_default_needed EQ abap_false.
      mv_current_tab_screen = lv_current_tab_screen.
      mv_current_tab_repid  = lv_current_tab_prg.
      cs_tabstrip-activetab = lv_tab.
      mv_current_tab = lv_tab.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
