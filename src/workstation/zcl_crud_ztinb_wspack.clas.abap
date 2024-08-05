class ZCL_CRUD_ZTINB_WSPACK definition
  public
  final
  create public .

public section.

  types:
    TT_ZTINB_WSPACK TYPE STANDARD TABLE OF ztinb_wspack WITH DEFAULT KEY .

  class-methods SELECT_MULTI_BY_LGNUM
    importing
      !IV_WAREHOUSE type /SCWM/LGNUM
    returning
      value(RT_RESULT) type TT_ZTINB_WSPACK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTINB_WSPACK IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Read customizing Workstation Packaging Settings
**********************************************************************

    SELECT * FROM ztinb_wspack
      INTO TABLE rt_result
      WHERE lgnum EQ iv_warehouse.
  ENDMETHOD.
ENDCLASS.
