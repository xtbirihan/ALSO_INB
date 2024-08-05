class ZCL_DECO_FLAG definition
  public
  final
  create public .

public section.

  class-methods SET_PARAM_ZDECO_NAV .
  class-methods GET_PARAM_ZDECO_NAV
    returning
      value(RV_CALLED_FROM_DECO_FLAG) type XFELD .
protected section.
private section.

  class-data MV_CALLED_FROM_DECO type XFELD .
ENDCLASS.



CLASS ZCL_DECO_FLAG IMPLEMENTATION.


  METHOD GET_PARAM_ZDECO_NAV.
**********************************************************************
*& Key           : <AAHMEDOV>-090224
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& get static flag
**********************************************************************

    GET PARAMETER ID 'ZDECO_NAV' FIELD rv_called_from_deco_flag.

  ENDMETHOD.


  METHOD SET_PARAM_ZDECO_NAV.
**********************************************************************
*& Key           : <AAHMEDOV>-090224
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& set static flag
**********************************************************************
    SET PARAMETER ID 'ZDECO_NAV' FIELD abap_true.

  ENDMETHOD.
ENDCLASS.
