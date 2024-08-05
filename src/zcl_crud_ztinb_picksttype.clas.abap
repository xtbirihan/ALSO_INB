class ZCL_CRUD_ZTINB_PICKSTTYPE definition
  public
  final
  create public .

public section.

  class-methods SELECT_MULTI_BY_LGNUM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RT_PICK_ST_TYPES) type ZTT_PICKSTTYPE .
  class-methods SELECT_MULTI_BY_LGNUM_MONOPAL
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_MONO_PALLET type ABAP_BOOL
    returning
      value(RT_PICK_ST_TYPES) type ZTT_PICKSTTYPE .
  class-methods SELECT_MULTI_BY_LGNUM_MIXPAL
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_MIX_PALLET type ABAP_BOOL
    returning
      value(RT_PICK_ST_TYPES) type ZTT_PICKSTTYPE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTINB_PICKSTTYPE IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
**********************************************************************
*& Key           : RM-230519
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get multiple entries by warehourse number
**********************************************************************
    SELECT FROM ztinb_picksttype
         FIELDS *
          WHERE lgnum = @iv_lgnum
           INTO TABLE @rt_pick_st_types.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztinb_picksttype' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_multi_by_lgnum_mixpal.
**********************************************************************
*& Key           : RM-230519
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get multiple entries by warehourse number and mix pallet flag
**********************************************************************
    SELECT FROM ztinb_picksttype
         FIELDS *
          WHERE lgnum        = @iv_lgnum
            AND mixed_pallet = @iv_mix_pallet
           INTO TABLE @rt_pick_st_types.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztinb_picksttype' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_multi_by_lgnum_monopal.
**********************************************************************
*& Key           : RM-230519
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get multiple entries by warehourse number and mono pallet flag
**********************************************************************
    SELECT FROM ztinb_picksttype
         FIELDS *
          WHERE lgnum       = @iv_lgnum
            AND mono_pallet = @iv_mono_pallet
           INTO TABLE @rt_pick_st_types.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztinb_picksttype' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
