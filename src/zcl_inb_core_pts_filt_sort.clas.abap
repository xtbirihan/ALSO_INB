class ZCL_INB_CORE_PTS_FILT_SORT definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_PTS_FILT_SORT .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INB_CORE_PTS_FILT_SORT IMPLEMENTATION.


  METHOD /scwm/if_ex_core_pts_filt_sort~filt_sort.
**********************************************************************
*& Key           : RM-230316
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_CORE_PTS_FILT_SORT
*& Sort empty storage bins
**********************************************************************
    DATA:
          lo_fltsrt_ptwy TYPE REF TO zcl_inb_core_pts_filtsort_ptwy.

    BREAK-POINT ID zcg_putaway_strat.
    BREAK-POINT ID zcg_badi.

    lo_fltsrt_ptwy = NEW zcl_inb_core_pts_filtsort_ptwy( ).

    lo_fltsrt_ptwy->/scwm/if_ex_core_pts_filt_sort~filt_sort(
      EXPORTING
        is_t331        = is_t331          " Storage Type Control
        is_t333        = is_t333          " Warehouse Process Type
        is_ltap        = is_ltap          " Warehouse Task Internal
        is_mat_global  = is_mat_global    " Material: Global Data
        is_mat_lgnum   = is_mat_lgnum     " Material: Warehouse-Number-Specific Data
        is_mat_lgtyp   = is_mat_lgtyp     " Material: Storage-Type-Specific Data
        is_mat_hazard  = is_mat_hazard    " Material: Hazardous Material Data
        it_mat_uom     = it_mat_uom       " Material: Table Type for Units of Measure
      IMPORTING
        et_bapiret     = et_bapiret       " Table with BAPI Return Information
      CHANGING
        ct_lagp_reserv = ct_lagp_reserv   " Storage Bins
        ct_qmat        = ct_qmat          " Available Quantities: Internal Table
        ct_hlplagpl    = ct_hlplagpl      " Table Type for /SCWM/LAGPL
        cs_ordim_cust  = cs_ordim_cust    " Customer Data in Warehouse Task
    ).

  ENDMETHOD.
ENDCLASS.
