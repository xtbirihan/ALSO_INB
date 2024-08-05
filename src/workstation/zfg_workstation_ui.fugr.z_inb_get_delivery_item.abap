FUNCTION z_inb_get_delivery_item.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_CANCELLED) TYPE  FLAG
*"  CHANGING
*"     REFERENCE(CT_DELIVERY_ITEM) TYPE  /SCWM/DLV_ITEM_OUT_PRD_TAB
*"----------------------------------------------------------------------

  go_get_deliv_screen = NEW lcl_get_delivery_screen( ).

  go_get_deliv_screen->set_data( ct_delivery_item ).

  CALL SCREEN '2100' STARTING AT 10 10.

  ct_delivery_item = go_get_deliv_screen->get_selected_data( ).
ENDFUNCTION.
