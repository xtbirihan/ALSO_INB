FUNCTION Z_INB_SCANNED_HUS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CT_HUIDENT) TYPE  ZIF_WS_DECO_UI=>TT_HU_TO_BE_CREATED
*"----------------------------------------------------------------------

  go_scanned_hus_screen = NEW lcl_scanned_hus_screen_grid( ).
  go_scanned_hus_screen->set_data( ct_huident ).

  CALL SCREEN '2210' STARTING AT 10 10.

  ct_huident = go_scanned_hus_screen->get_selected_data( ).


ENDFUNCTION.
