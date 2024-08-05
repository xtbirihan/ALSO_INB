class ZCL_CRUD_ZTINB_PCKSSEQMAP definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_PUT_DUMM_SSEQ
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_PUT_SSEQ type /SCWM/DE_PUT_TYP_SSEQ
      !IV_PICK_STOCK type ZDE_PICK_STOCK
    returning
      value(RS_ST_SSEQ_MAP) type ZTINB_PCKSSEQMAP .
  class-methods SELECT_MULTI_BY_PUTSSEQ_MONPAL
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DUMMY_SSEQ type /SCWM/DE_PUT_TYP_SSEQ
      !IV_STSS_MIXED_PALLET type ZDE_MONO_STSS
    returning
      value(RT_ST_SSEQ_MAP) type ZTT_PCKSSEQMAP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTINB_PCKSSEQMAP IMPLEMENTATION.


  METHOD select_multi_by_putsseq_monpal.
**********************************************************************
*& Key           : RM-230518
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get Storage Type Search Sequence Map. by STSS and Mixed Pallet
**********************************************************************

    SELECT * FROM ztinb_pcksseqmap
            WHERE lgnum             = @iv_lgnum
              AND dummy_sseq        = @iv_dummy_sseq
              AND stss_mixed_pallet = @iv_stss_mixed_pallet
             INTO TABLE @rt_st_sseq_map.

  ENDMETHOD.


  METHOD select_single_by_put_dumm_sseq.
**********************************************************************
*& Key           : RM-230518
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get Storage Type Search Sequence Mapping by warehouse number,
*& putaway storage sequence and pick stock
**********************************************************************

    SELECT SINGLE FROM ztinb_pcksseqmap
           FIELDS *
            WHERE lgnum      = @iv_lgnum
              AND dummy_sseq = @iv_put_sseq
              AND pick_stock = @iv_pick_stock
             INTO CORRESPONDING FIELDS OF @rs_st_sseq_map.

  ENDMETHOD.
ENDCLASS.
