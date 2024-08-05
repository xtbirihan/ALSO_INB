CLASS zcl_ws_tote_place_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ws_ui_defaults .
    INTERFACES zif_ws_subscr_ui .
    INTERFACES zif_ws_tote_place_ui .

    CONSTANTS c_main_status TYPE string VALUE 'TOTE_PLACE' ##NO_TEXT.
    CONSTANTS c_main_tittle TYPE string VALUE 'MAIN_TITLE' ##NO_TEXT.
    CONSTANTS c_sub_tote_place TYPE sy-dynnr VALUE '3100' ##NO_TEXT.
    CONSTANTS c_tab_tote_place TYPE scxtab_tabstrip-activetab VALUE 'TOTE_PLACE' ##NO_TEXT.
    CONSTANTS c_subscreen_repid TYPE syrepid VALUE 'SAPLZFG_WORKSTATION_UI' ##NO_TEXT.
  PROTECTED SECTION.
private section.

  constants C_FUNC_CHANGE_CART type SYUCOMM value 'CHANGE_CAR' ##NO_TEXT.
  constants C_FUNC_CHANGE_TOTE type SYUCOMM value 'CHANGE_TOT' ##NO_TEXT.
  constants C_FUNC_DEFAULTS type SYUCOMM value 'DEFAULTS' ##NO_TEXT.
  constants C_FUNC_DISPLAY_LOG type SYUCOMM value 'DISPLAYLOG' ##NO_TEXT.
  constants C_FUNC_EXECUTE type SYUCOMM value 'EXECUTE' ##NO_TEXT.
  constants C_FUNC_LEAVE type SYUCOMM value 'LEAVE' ##NO_TEXT.
  data MO_FUNCTION_LOG type ref to /SCWM/CL_LOG .
  data MO_DEFAULTS_UI type ref to ZIF_WS_UI_DEFAULTS .
  data MO_SP type ref to ZIF_WS_PTWY_CART_SP .
  data MS_DEFAULTS type ZSTR_WS_DEFAULTS_GENERAL .
  data MS_TOTE_PLACE type ZSTR_WS_TOTE_PLAC .
  data MT_EXCLUDES type STRING_TABLE .
  data MV_BALLOGHNDL type BALLOGHNDL .

  methods CLEAR_DATA .
  methods DISPLAY_LOG .
  methods EXECUTE
    raising
      ZCX_WORKSTATION .
  methods PUTAWAY_CART_CHANGED
    importing
      !IV_CART_POS type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION .
  methods TOTE_ID_CHANGED
    importing
      !IV_TOTE_ID type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION .
ENDCLASS.



CLASS ZCL_WS_TOTE_PLACE_UI IMPLEMENTATION.


  METHOD clear_data.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Clear screen data
**********************************************************************
    CLEAR:
      ms_tote_place-tote_id,
      ms_tote_place-putaway_cart,
      ms_tote_place-position_on_cart.
  ENDMETHOD.


  METHOD display_log.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Display log
**********************************************************************
    DATA: ls_display_profile TYPE  bal_s_prof.

    CHECK mo_function_log IS BOUND.

    TRY.
        CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
          IMPORTING
            e_s_display_profile = ls_display_profile.                  " Display Profile
        mo_function_log->display_log( iv_loghandle = mv_balloghndl is_display_profile = ls_display_profile ).
      CATCH /scwm/cx_basics ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD execute.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Move tote int the cart
**********************************************************************
    TRY.
        mo_sp->move_tote_into_cart(
          iv_tote_id     = ms_tote_place-tote_id
          iv_cart_id     = ms_tote_place-putaway_cart
          iv_pos_on_cart = ms_tote_place-position_on_cart ).

        clear_data( ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        clear_data( ).
        RAISE EXCEPTION lx_ws.
      CATCH /scwm/cx_core INTO DATA(lx_core).
        clear_data( ).
        RAISE EXCEPTION TYPE zcx_workstation
           MESSAGE e069(zmc_workstation)
           EXPORTING
             previous = lx_core.
    ENDTRY.
  ENDMETHOD.


  METHOD putaway_cart_changed.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Check whether the cart position corresponds to the customizing
**********************************************************************
    mo_sp->interpret_huident(
      EXPORTING
        iv_huident       = iv_cart_pos                 " Handling Unit Identification
      IMPORTING
        es_cart_settings = DATA(ls_cart_settings)                " Cart Barcode
        ev_cart_id       = DATA(lv_cart_id)
        ev_pos_on_cart   = DATA(lv_pos_on_cart)
    ).
    IF ls_cart_settings-pckg_type NE zif_ws_ptwy_cart_sp=>c_pckg_type_pos_on_cart.
      RAISE EXCEPTION TYPE zcx_workstation
           MESSAGE e060(zmc_workstation).
    ENDIF.
    ms_tote_place-putaway_cart = |{ zif_ws_ptwy_cart_sp=>c_pckg_type_cart }{ lv_cart_id }|.
    ms_tote_place-position_on_cart = lv_pos_on_cart.

  ENDMETHOD.


  METHOD tote_id_changed.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Check whether Tote ID corresponds to the customizing
**********************************************************************
    mo_sp->interpret_huident(
      EXPORTING
        iv_huident       = iv_tote_id                 " Handling Unit Identification
      IMPORTING
        es_cart_settings =  DATA(ls_cart_settings)                " Cart Barcode
    ).
    IF ls_cart_settings-pckg_type NE zif_ws_ptwy_cart_sp=>c_pckg_type_tote.
      RAISE EXCEPTION TYPE zcx_workstation
           MESSAGE e061(zmc_workstation).
    ENDIF.
    ms_tote_place-tote_id = iv_tote_id.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~get_status.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Get screen status with excluded functions
**********************************************************************
    ev_status = c_main_status.
    et_excludes = mt_excludes.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~get_title.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Get the title with parameter
**********************************************************************
    ev_title = c_main_tittle.
    MESSAGE i062(zmc_workstation) WITH ms_defaults-lgnum ms_defaults-workst_loc INTO ev_param.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~init.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Initialize the UI
**********************************************************************
    IF io_prev_subscreen IS INSTANCE OF zcl_replenishment_ui.
      mo_defaults_ui ?= io_prev_subscreen.
    ELSE.
      mo_defaults_ui = NEW zcl_general_ui_functions( NEW zcl_ws_deco_sp(  ) ).
    ENDIF.

    mo_defaults_ui->get_defaults(
      IMPORTING
        es_defaults = ms_defaults
    ).
    ev_default_needed = xsdbool( NOT mo_defaults_ui->is_defaults_sufficient( ) ).
    IF ev_default_needed IS INITIAL.
      TRY.
          zif_ws_ui_defaults~set_defaults( is_defaults = ms_defaults ).
        CATCH zcx_workstation. " Workstation errors
          CLEAR ms_defaults.
          ev_default_needed = abap_true.
      ENDTRY.
    ENDIF.

    ev_subscreen_no = c_sub_tote_place.
    ev_tab = c_tab_tote_place.
    ev_subscreen_prg = c_subscreen_repid.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~leave_screen.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& check whether screen can be left
**********************************************************************
    rv_leave = abap_true.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~pai_tab.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& PAI of the tab screen. Trigger function if field has been changed
**********************************************************************
    DATA: ls_tote_place TYPE zstr_ws_tote_plac.

    ls_tote_place = is_screen_data.

    IF ls_tote_place-tote_id NE ms_tote_place-tote_id.
      tote_id_changed( ls_tote_place-tote_id ).
    ENDIF.


    IF ls_tote_place-putaway_cart NE ms_tote_place-putaway_cart.
      putaway_cart_changed( ls_tote_place-putaway_cart ).
    ENDIF.


  ENDMETHOD.


  METHOD zif_ws_subscr_ui~pbo_tab.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& PBO of the tab subscreen
**********************************************************************
    es_screen_data = ms_tote_place.
    CLEAR mt_excludes.
    LOOP AT SCREEN.
      IF ms_tote_place-tote_id IS INITIAL AND screen-group1+0(2) EQ 'CA' .
        screen-input = '0'.
        APPEND c_func_change_cart TO mt_excludes.
        APPEND c_func_execute TO mt_excludes.
      ENDIF.
      IF ms_tote_place-tote_id IS NOT INITIAL AND screen-group1 EQ 'TOT'.
        screen-input = '0'.
      ENDIF.
      IF ms_tote_place-putaway_cart IS INITIAL AND screen-group1 EQ 'CAC'.
        APPEND c_func_execute TO mt_excludes.
        screen-input = '0'.
      ENDIF.
      IF ms_tote_place-putaway_cart IS NOT INITIAL AND screen-group1 EQ 'CAR'.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
    IF ms_tote_place-tote_id IS INITIAL.
      APPEND c_func_change_cart TO mt_excludes.
    ENDIF.
    IF ms_tote_place-putaway_cart IS INITIAL.
      APPEND c_func_execute TO mt_excludes.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ws_subscr_ui~process_user_command.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Process user activities
**********************************************************************
    DATA lv_answer TYPE c LENGTH 1.

    CLEAR: es_bapiret, ev_leave_screen.

    TRY.
        CASE iv_ucomm.
          WHEN c_func_display_log.
            display_log( ).

          WHEN c_func_leave.
            LEAVE TO SCREEN 0.

          WHEN c_func_defaults.
            mo_defaults_ui->call_screen( ).

          WHEN c_func_change_cart.
            CLEAR: ms_tote_place-position_on_cart, ms_tote_place-putaway_cart.

          WHEN c_func_change_tote.
            CLEAR: ms_tote_place-tote_id, ms_tote_place-position_on_cart, ms_tote_place-putaway_cart.

          WHEN c_func_execute.
            execute( ).
        ENDCASE.
      CATCH zcx_workstation INTO DATA(lx_ws).
        MESSAGE lx_ws TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.


  method ZIF_WS_UI_DEFAULTS~CALL_SCREEN.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Call the popup screen for entering default settings
**********************************************************************

    DATA: ls_def_orig TYPE zstr_ws_defaults,
          ls_def_new  TYPE zstr_ws_defaults.
    mo_defaults_ui->get_defaults(
      IMPORTING
        es_defaults = ls_def_orig
    ).
    mo_defaults_ui->call_screen( ).
    mo_defaults_ui->get_defaults(
      IMPORTING
        es_defaults = ls_def_new
    ).
    IF ls_def_new NE ls_def_orig.
      TRY.
          zif_ws_ui_defaults~set_defaults( is_defaults = ls_def_new ).
        CATCH zcx_workstation INTO DATA(lo_cx). " Workstation errors
          MESSAGE lo_cx TYPE 'I' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.
  endmethod.


  METHOD zif_ws_ui_defaults~get_defaults.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Get the default values
**********************************************************************
    mo_defaults_ui->get_defaults(
      IMPORTING
        es_defaults = es_defaults
    ).
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~get_default_screen_no.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Get the screen number and program name for setting the default values
**********************************************************************
   mo_defaults_ui->get_default_screen_no(
     IMPORTING
       ev_dynnr = ev_dynnr
       ev_repid = ev_repid
   ).
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~is_defaults_sufficient.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Check whether the defaults are filled completely
**********************************************************************
    rv_yes = mo_defaults_ui->is_defaults_sufficient( ).
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~set_defaults.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Set default values. If any of the field is already filled, then
*& confirm data loss with a popup
**********************************************************************
    DATA: lv_answer TYPE c LENGTH 1.

    IF ms_tote_place-tote_id IS NOT INITIAL OR ms_tote_place-putaway_cart IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = TEXT-rfi "'Set defaults'
          text_question         = 'No data previously entered on main screen will be saved.'(nds)                 " Question text in dialog box
          text_button_1         = 'Continue'(con)         " Text on the first pushbutton
          text_button_2         = 'Cancel'(can)         " Text on the second pushbutton
          display_cancel_button = abap_false              " Button for displaying cancel pushbutton
        IMPORTING
          answer                = lv_answer                " Return values: '1', '2', 'A'
        EXCEPTIONS
          OTHERS                = 0.
      IF lv_answer EQ '1'.
        CLEAR ms_tote_place.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    mo_defaults_ui->set_defaults( is_defaults = is_defaults ).
    mo_defaults_ui->get_defaults(
      IMPORTING
        es_defaults = ms_defaults
    ).
    IF ms_defaults-workst_loc IS NOT INITIAL.
      mo_sp = zcl_ws_ptwy_cart_sp=>create_instance(
          iv_lgnum          =  ms_defaults-lgnum                " Warehouse Number/Warehouse Complex
          iv_stor_bin_lgpla =  ms_defaults-workst_loc                " Storage Bin
      ).
    ELSE.
      mo_sp = zcl_ws_ptwy_cart_sp=>create_instance_repl(
                iv_lgnum      = ms_defaults-lgnum
                iv_workcenter = ms_defaults-workcenter
              ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
