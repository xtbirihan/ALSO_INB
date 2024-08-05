class ZCL_CORE_PTS_TYPSQ_MDL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_s_pre_crt_wt,
        vsolm TYPE /scwm/ltap_vsolm,
        meins TYPE /scwm/de_base_uom,
        nltyp TYPE /scwm/ltap_nltyp,
        nlber TYPE /scwm/ltap_nlber,
        nlpla TYPE /scwm/ltap_nlpla,
      END OF ty_s_pre_crt_wt .
  types:
    ty_tt_pre_crt_wt TYPE STANDARD TABLE OF ty_s_pre_crt_wt WITH EMPTY KEY .

  class-methods GET_INSTANCE
    returning
      value(RO_INST) type ref to ZCL_CORE_PTS_TYPSQ_MDL .
  methods MODIFY_PRE_CRT_WT
    importing
      !IS_PRE_CRT_WT type TY_S_PRE_CRT_WT .
  methods GET_PRE_CRT_WT
    importing
      !IT_LGTYP type /SCWM/TT_LGTYP_R
    returning
      value(RT_PRE_CRT_WT) type TY_TT_PRE_CRT_WT .
protected section.
private section.

  class-data SO_INST type ref to ZCL_CORE_PTS_TYPSQ_MDL .
  data MT_PRE_CRT_WT type TY_TT_PRE_CRT_WT .
ENDCLASS.



CLASS ZCL_CORE_PTS_TYPSQ_MDL IMPLEMENTATION.


  METHOD get_instance.
**********************************************************************
*& Key           : RM-230530
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get instance of the class
**********************************************************************

    IF so_inst IS NOT BOUND.
      so_inst = NEW #( ).
    ENDIF.

    ro_inst = so_inst.

  ENDMETHOD.


  METHOD get_pre_crt_wt.
**********************************************************************
*& Key           : RM-230530
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get buffer table data
**********************************************************************

    rt_pre_crt_wt = VALUE #( FOR <ls_pre_crt_wt> IN mt_pre_crt_wt
                             WHERE ( nltyp IN it_lgtyp )
                             ( vsolm = <ls_pre_crt_wt>-vsolm
                               meins = <ls_pre_crt_wt>-meins
                               nltyp = <ls_pre_crt_wt>-nltyp
                               nlber = <ls_pre_crt_wt>-nlber
                               nlpla = <ls_pre_crt_wt>-nlpla ) ).

  ENDMETHOD.


  METHOD modify_pre_crt_wt.
**********************************************************************
*& Key           : RM-230530
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Modify buffer table
**********************************************************************

    APPEND is_pre_crt_wt TO mt_pre_crt_wt.

  ENDMETHOD.
ENDCLASS.
