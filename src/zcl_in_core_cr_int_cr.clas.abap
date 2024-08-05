class ZCL_IN_CORE_CR_INT_CR definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_CR_INT_CR .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IN_CORE_CR_INT_CR IMPLEMENTATION.


  METHOD /scwm/if_ex_core_cr_int_cr~insert.
**********************************************************************
*& Key           : RM-230531
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    BREAK-POINT ID zcg_putaway_strat.
    BREAK-POINT ID zcg_badi.

    DATA:
          lo_cr_int_cr_ptwy TYPE REF TO zcl_in_core_cr_int_cr_ptwy.

    lo_cr_int_cr_ptwy = NEW zcl_in_core_cr_int_cr_ptwy( ).

    lo_cr_int_cr_ptwy->/scwm/if_ex_core_cr_int_cr~insert(
     EXPORTING
       is_ltap      = is_ltap
     IMPORTING
       et_bapiret   = et_bapiret
     CHANGING
       cs_ltap_cust_s4 = cs_ltap_cust_s4
       cs_ltap_cust = cs_ltap_cust ).


  ENDMETHOD.
ENDCLASS.
