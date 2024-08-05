class ZCL_IN_CORE_CR_INT_CR_PTWY definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_CR_INT_CR .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IN_CORE_CR_INT_CR_PTWY IMPLEMENTATION.


  METHOD /scwm/if_ex_core_cr_int_cr~insert.
**********************************************************************
*& Key           : RM-230531
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
          lo_pts_typsq_mdl TYPE REF TO zcl_core_pts_typsq_mdl.

    IF zcl_switch=>get_switch_state( iv_lgnum = is_ltap-lgnum
                                     iv_devid = zif_switch_const=>c_zinb_003 ) = abap_false.
      " Do not execute logic
      RETURN.
    ENDIF.

    " Save pre-created WTs
    lo_pts_typsq_mdl = zcl_core_pts_typsq_mdl=>get_instance( ).

    lo_pts_typsq_mdl->modify_pre_crt_wt( is_pre_crt_wt = VALUE #( vsolm = is_ltap-vsolm
                                                                  meins = is_ltap-meins
                                                                  nltyp = is_ltap-nltyp
                                                                  nlber = is_ltap-nlber
                                                                  nlpla = is_ltap-nlpla ) ).

  ENDMETHOD.
ENDCLASS.
