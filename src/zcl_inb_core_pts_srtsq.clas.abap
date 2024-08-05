class ZCL_INB_CORE_PTS_SRTSQ definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_PTS_SRTSQ .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INB_CORE_PTS_SRTSQ IMPLEMENTATION.


  METHOD /scwm/if_ex_core_pts_srtsq~sort_sequence.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
          lo_srtsq_ptwy TYPE REF TO zcl_inb_core_pts_srtsq_ptwy.

    BREAK-POINT ID zcg_putaway_strat.
    BREAK-POINT ID zcg_badi.

    lo_srtsq_ptwy = NEW zcl_inb_core_pts_srtsq_ptwy( ).

    lo_srtsq_ptwy->/scwm/if_ex_core_pts_srtsq~sort_sequence(
      EXPORTING
        it_sorttab    = it_sorttab     " Table Type for Structure /SCWM/S_PENALTY_SRT
        is_ltap       = is_ltap        " Warehouse Task Internal
        is_mat_global = is_mat_global  " Material: Global Data
        is_mat_lgnum  = is_mat_lgnum   " Material: Warehouse-Number-Specific Data
        is_mat_hazard = is_mat_hazard  " Material: Hazardous Material Data
        it_mat_uom    = it_mat_uom     " Material: Table Type for Units of Measure
        is_t333       = is_t333        " Warehouse Process Type
        flt_val       = flt_val        " Warehouse Number/Warehouse Complex
      IMPORTING
        et_sorttab    = et_sorttab     " Table Type for Structure /SCWM/S_PENALTY_SRT
        et_bapiret    = et_bapiret     " Table with BAPI Return Information
      CHANGING
        cs_ordim_cust = cs_ordim_cust  " Customer Data in Warehouse Task
    ).


  ENDMETHOD.
ENDCLASS.
