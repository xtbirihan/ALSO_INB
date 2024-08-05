class ZCL_INB_CORE_PTS_EMPTY_BIN definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_PTS_EMPTY_BIN .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INB_CORE_PTS_EMPTY_BIN IMPLEMENTATION.


  METHOD /scwm/if_ex_core_pts_empty_bin~delete_empty_bin_buffer.
**********************************************************************
*& Key           : RM-230316
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& To date 25.04.2023 empty implementation to avoid dumps
**********************************************************************

    BREAK-POINT ID zcg_putaway_strat.
    BREAK-POINT ID zcg_badi.

  ENDMETHOD.


  METHOD /scwm/if_ex_core_pts_empty_bin~determine_empty_bins.
**********************************************************************
*& Key           : RM-230425
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_CORE_PTS_EMPTY_BIN
*& Determine empty bins
**********************************************************************
    DATA:
          lo_empty_bin_ptwy TYPE REF TO zcl_inb_core_pts_empty_bin_ptw.

    BREAK-POINT ID zcg_putaway_strat.
    BREAK-POINT ID zcg_badi.

    lo_empty_bin_ptwy = NEW zcl_inb_core_pts_empty_bin_ptw( ).

    lo_empty_bin_ptwy->/scwm/if_ex_core_pts_empty_bin~determine_empty_bins(
      EXPORTING
        iv_lgnum                = iv_lgnum         " Warehouse Number/Warehouse Complex
        iv_nltyp                = iv_nltyp         " Destination Storage Type
        iv_nlber                = iv_nlber         " Destination Storage Section
        iv_nptyp                = iv_nptyp         " Destination Bin Type
        is_ltap                 = is_ltap          " Warehouse Task Internal
        is_t333                 = is_t333          " Warehouse Process Type
        is_t331                 = is_t331          " Storage Type Control for Destination Storage Type
        it_lagp_excl            = it_lagp_excl     " Storage Bins to be excluded from Search
        it_wt_memory            = it_wt_memory     " Table Type: Warehouse Tasks Internal
        io_log                  = io_log           " Log
        iv_log_row              = iv_log_row       " Request Line
      CHANGING
        ct_lagpl                = ct_lagpl                 " Empty Bins
        ct_lagpl_add_attributes = ct_lagpl_add_attributes  " Additional Attributes for Storage Bins
        cv_do_not_run_std       = cv_do_not_run_std        " Do not run Standard Empty Bin Search
        cs_ordim_cust           = cs_ordim_cust            " Customer Data in Warehouse Task
    ).

  ENDMETHOD.
ENDCLASS.
