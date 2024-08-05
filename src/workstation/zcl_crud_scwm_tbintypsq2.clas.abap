CLASS zcl_crud_scwm_tbintypsq2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_seq TYPE STANDARD TABLE OF /scwm/tbintypsq2 WITH DEFAULT KEY.

    CLASS-METHODS get_seq_by_lgtyp_obintyp
      IMPORTING
        iv_lgnum         TYPE /scwm/lgnum
        iv_lgtyp         TYPE /scwm/lgtyp
        iv_obintyp       TYPE /scwm/de_bintype
      RETURNING
        VALUE(rt_result) TYPE tt_seq.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA st_seq TYPE SORTED TABLE OF /scwm/tbintypsq2
                      WITH NON-UNIQUE KEY lgnum lgtyp obintyp.
ENDCLASS.



CLASS ZCL_CRUD_SCWM_TBINTYPSQ2 IMPLEMENTATION.


  METHOD get_seq_by_lgtyp_obintyp.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Read customizing Alternative Storage Bin Type Sequence
**********************************************************************
    IF NOT line_exists( st_seq[ lgnum = iv_lgnum lgtyp = iv_lgtyp obintyp = iv_obintyp ] ).
      SELECT FROM /scwm/tbintypsq2
             FIELDS *
             WHERE lgnum EQ @iv_lgnum
               AND lgtyp EQ @iv_lgtyp
               AND obintyp EQ @iv_obintyp
             INTO TABLE @rt_result.
      INSERT LINES OF  rt_result INTO TABLE st_seq.
      RETURN.
    ENDIF.
    rt_result = VALUE #( FOR seq IN st_seq
                         WHERE ( lgnum = iv_lgnum AND lgtyp = iv_lgtyp AND obintyp = iv_obintyp )
                         ( seq )
                       ).

  ENDMETHOD.
ENDCLASS.
