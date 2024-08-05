FUNCTION Z_INB_DELETE_HUS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(ET_HUIDENT) TYPE  ZIF_WS_DECO_UI=>TT_HU_IDENT
*"----------------------------------------------------------------------
  clear et_huident.

  go_delete_hus_screen = NEW lcl_delete_hus_screen_grid( ).

  CALL SCREEN '2200' STARTING AT 10 10.

  et_huident = go_delete_hus_screen->get_selected_data( ).


ENDFUNCTION.
