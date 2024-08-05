FUNCTION z_inb_change_proposed_wt.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_PROPOSED_WT) TYPE  ZSTR_WS_PROPOSED_WT
*"  EXPORTING
*"     VALUE(EV_CANCELLED) TYPE  FLAG
*"     VALUE(EV_NOF_WTS) TYPE  ZDE_NUMBER_OF_WTS
*"     VALUE(EV_QTY_PER_WT) TYPE  ZDE_QTY_PER_WT
*"     VALUE(EV_NLTYP) TYPE  /SCWM/LTAP_NLTYP
*"     VALUE(EV_QTY_PER_WT_UOM) TYPE  MEINS
*"----------------------------------------------------------------------

  go_change_pr_wt_screen = NEW lcl_changed_proposed_wt( ).

  go_change_pr_wt_screen->set_data( is_proposed_wt ).


  CALL SCREEN '2300' STARTING AT 10 10.

  DATA(ls_prop_wt) = go_change_pr_wt_screen->get_data( ).

  ev_cancelled = go_change_pr_wt_screen->is_cancelled( ).

  IF ev_cancelled EQ abap_false.
    ev_nof_wts = ls_prop_wt-nof_wts.
    ev_qty_per_wt = ls_prop_wt-qty_per_wt.
    ev_nltyp = ls_prop_wt-nltyp.
    ev_qty_per_wt_uom = ls_prop_wt-qty_per_wt_uom.
  ENDIF.

ENDFUNCTION.
