class ZCL_CRUD_INB_STTYPE_PTWY definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_st_type_cust.
        INCLUDE TYPE ztinb_sttypecust.
    TYPES: executed TYPE abap_bool,
      END OF ts_st_type_cust .
  types:
    tt_st_type_cust TYPE TABLE OF ts_st_type_cust .
  types:
    BEGIN OF ts_penalty_str.
        INCLUDE TYPE /scwm/s_penalty_srt.
    TYPES: executed TYPE abap_bool,
        new_seq  TYPE abap_bool,
        curr_seq TYPE abap_bool,
      END OF ts_penalty_str .
  types:
    tt_penalty_str TYPE STANDARD TABLE OF ts_penalty_str .

  data MT_ST_TYPE_CUST_USED type TT_ST_TYPE_CUST .
  data MS_ST_TYPE_CUST type TS_ST_TYPE_CUST .
  data MT_PENALTY_LIST type TT_PENALTY_STR .
  data MS_PENALTY_STR_CURR type TS_PENALTY_STR .
  data MS_ORIG_WT type /SCWM/LTAP .

  class-methods GET_INST
    returning
      value(RO_INST) type ref to ZCL_CRUD_INB_STTYPE_PTWY .
  methods SELECT_BY_LGNUM_PUT_SSEQ
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_PUT_SSEQ type /SCWM/DE_PUT_TYP_SSEQ
    returning
      value(RT_STTYPECUST) type ZTT_STTYPECUST .
protected section.
private section.

  class-data SO_INST type ref to ZCL_CRUD_INB_STTYPE_PTWY .
ENDCLASS.



CLASS ZCL_CRUD_INB_STTYPE_PTWY IMPLEMENTATION.


  METHOD get_inst.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Get an instance of the class
**********************************************************************

    IF so_inst IS NOT BOUND.
      so_inst = NEW #( ).
    ENDIF.

    ro_inst = so_inst.

  ENDMETHOD.


  METHOD select_by_lgnum_put_sseq.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*& Read storage type custom settings by warehouse number and putaway sequence
**********************************************************************

    SELECT FROM ztinb_sttypecust
         FIELDS *
          WHERE lgnum    = @iv_lgnum
            AND put_sseq = @iv_put_sseq
           INTO CORRESPONDING FIELDS OF TABLE @rt_sttypecust.

    mt_st_type_cust_used = rt_sttypecust.

  ENDMETHOD.
ENDCLASS.
