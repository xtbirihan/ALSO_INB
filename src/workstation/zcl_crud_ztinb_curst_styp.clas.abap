class ZCL_CRUD_ZTINB_CURST_STYP definition
  public
  final
  create public .

public section.

  types:
    tt_ztinb_curst_styp TYPE STANDARD TABLE OF ztinb_curst_styp WITH DEFAULT KEY .

  class-methods SELECT_MULTI_BY_LGNUM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RT_RESULT) type TT_ZTINB_CURST_STYP .
  class-methods SELECT_SINGLE_BY_LGTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGTYP type /SCWM/LGTYP
    returning
      value(RS_RESULT) type ZTINB_CURST_STYP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTINB_CURST_STYP IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Read customizing Current Stock Situation
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT FROM ztinb_curst_styp
           FIELDS *
           WHERE lgnum EQ @iv_lgnum
           INTO TABLE @rt_result.

  ENDMETHOD.


  METHOD select_single_by_lgtyp.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Read customizing Current Stock Situation for warehouse/storage type
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE FROM ztinb_curst_styp
           FIELDS *
           WHERE lgnum EQ @iv_lgnum
             AND lgtyp EQ @iv_lgtyp
            INTO @rs_result.

  ENDMETHOD.
ENDCLASS.
