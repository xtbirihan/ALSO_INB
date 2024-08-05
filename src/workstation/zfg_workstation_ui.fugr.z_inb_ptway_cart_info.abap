FUNCTION Z_INB_PTWAY_CART_INFO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_HUIDENT) TYPE  /SCWM/HUIDENT
*"     REFERENCE(IO_PTWY_CART_SP) TYPE REF TO  ZIF_WS_PTWY_CART_SP
*"  EXPORTING
*"     REFERENCE(EV_PTWAY_CART) TYPE  /SCWM/HUIDENT
*"     REFERENCE(EV_PTWAY_CART_POS) TYPE  /SCWM/HUIDENT
*"     REFERENCE(EV_CANCELLED) TYPE  FLAG
*"----------------------------------------------------------------------
  CLEAR: ev_cancelled, ev_ptway_cart_pos, ev_ptway_cart.

  go_ptwy_cart_info_screen = NEW lcl_ptwy_cart_info( ).

  go_ptwy_cart_info_screen->set_data( iv_huident = iv_huident
                                       io_ptwy_cart_sp = io_ptwy_cart_sp ).


  CALL SCREEN '2400' STARTING AT 10 10.

  DATA(ls_cart_info) = go_ptwy_cart_info_screen->get_data( ).

  ev_cancelled = go_ptwy_cart_info_screen->is_cancelled( ).

  IF ev_cancelled EQ abap_false.
    ev_ptway_cart = ls_cart_info-putaway_cart.
    ev_ptway_cart_pos = ls_cart_info-position_on_cart.
  ENDIF.

ENDFUNCTION.
