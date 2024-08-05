CLASS zcl_general_ui_functions DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ws_ui_defaults .

    CONSTANTS c_default_screen TYPE sy-dynnr VALUE '2000' ##NO_TEXT.
    CONSTANTS c_default_screen_prog TYPE sy-repid VALUE 'SAPLZFG_WORKSTATION_UI' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !io_defaults TYPE REF TO zif_ws_defaults_sp .
  PROTECTED SECTION.

    DATA mo_defaults_sp TYPE REF TO zif_ws_defaults_sp .
    DATA ms_defaults TYPE zstr_ws_defaults .
    DATA ms_workstation_location TYPE /scwm/lagp .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GENERAL_UI_FUNCTIONS IMPLEMENTATION.


  METHOD constructor.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Set the model
**********************************************************************
    mo_defaults_sp = io_defaults.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~call_screen.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Call defaults screen as a popup to get the values
**********************************************************************
    CALL FUNCTION 'Z_INB_WS_DEFAULTS_POPUP'
      EXPORTING
        io_def_ctrl = me.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~get_defaults.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the defualt values from the memory or from the DB, if not read yet
**********************************************************************
    IF ms_defaults IS INITIAL.
      mo_defaults_sp->get_defaults(
        IMPORTING
          es_defaults = ms_defaults                 " Workstation defaults
          es_lagp     = ms_workstation_location                " Storage Bins
      ).
    ENDIF.
    es_defaults = CORRESPONDING #( ms_defaults ).
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~get_default_screen_no.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get teh defaults screen number and program, if it is not the main
**********************************************************************
    ev_dynnr = c_default_screen.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~is_defaults_sufficient.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Check whether the defaults are filled completely
**********************************************************************
    rv_yes = abap_true.
    IF ms_defaults-lgnum IS INITIAL OR ms_defaults-workst_loc IS INITIAL.
      rv_yes = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~set_defaults.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Set default values for the transaction
**********************************************************************

    DATA ls_defaults TYPE zstr_ws_defaults.
    ls_defaults = CORRESPONDING #( is_defaults ).
    IF ls_defaults-lgnum IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e001(zmc_workstation).
    ENDIF.
    IF ls_defaults-workst_loc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e002(zmc_workstation).
    ENDIF.

    mo_defaults_sp->set_defaults_deco(
      EXPORTING
        iv_lgnum          = ls_defaults-lgnum                " Warehouse Number/Warehouse Complex
        iv_stor_bin_lgpla = ls_defaults-workst_loc                " Storage Bin
      IMPORTING
        es_lagp = ms_workstation_location
    ).
    ms_defaults = ls_defaults.

  ENDMETHOD.
ENDCLASS.
