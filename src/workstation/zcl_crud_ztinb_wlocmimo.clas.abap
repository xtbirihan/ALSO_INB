class ZCL_CRUD_ZTINB_WLOCMIMO definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_STORAGE_LOC
    importing
      !IV_WAREHOUSE type /SCWM/LGNUM
      !IV_STORAGE_LOCATION type /SCWM/LGPLA
    returning
      value(RS_WLOCMIMO) type ZTINB_WLOCMIMO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTINB_WLOCMIMO IMPLEMENTATION.


  METHOD select_single_by_storage_loc.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Read customizing Workstation Location Mono/Mixed
**********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT FROM ztinb_wlocmimo
           FIELDS *
           WHERE lgnum EQ @iv_warehouse
             AND lgpla EQ @iv_storage_location
            INTO TABLE @data(lt_wlocmimo)
          UP TO 1 rows.
    if sy-subrc ne 0.
      return.
      endif.

   rs_wlocmimo = lt_wlocmimo[ 1 ].
  ENDMETHOD.
ENDCLASS.
