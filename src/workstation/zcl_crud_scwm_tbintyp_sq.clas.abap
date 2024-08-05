class ZCL_CRUD_SCWM_TBINTYP_SQ definition
  public
  final
  create public .

public section.

  types:
    tt_tbintyp_sq TYPE SORTED TABLE OF /scwm/tbintyp_sq
                         WITH NON-UNIQUE KEY lgnum lgtyp hutyp lptyp .

  class-methods GET_SEQ_FOR_LGTYP_HUTYP_LPTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGTYP type /SCWM/LGTYP
      !IV_HUTYP type /SCWM/DE_HUTYP
      !IV_LPTYP type /SCWM/LVS_LPTYP
    returning
      value(RV_SEQ) type /SCWM/DE_SEQNO .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: st_tbintyp_sq TYPE tt_tbintyp_sq.
ENDCLASS.



CLASS ZCL_CRUD_SCWM_TBINTYP_SQ IMPLEMENTATION.


  METHOD get_seq_for_lgtyp_hutyp_lptyp.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Read customizing Storage Bin Type Search
**********************************************************************
    READ TABLE st_tbintyp_sq INTO DATA(ls_seq)
         WITH TABLE KEY lgnum = iv_lgnum
                        lgtyp = iv_lgtyp
                        hutyp = iv_hutyp
                        lptyp = iv_lptyp.
    IF sy-subrc NE 0.
      SELECT FROM /scwm/tbintyp_sq
             FIELDS *
             WHERE lgnum EQ @iv_lgnum
               AND lgtyp EQ @iv_lgtyp
               AND hutyp EQ @iv_hutyp
               AND lptyp EQ @iv_lptyp
             INTO TABLE @DATA(lt_seq)
               UP TO 1 ROWS.
      IF sy-subrc EQ 0.
        ls_seq = lt_seq[ 1 ].
        INSERT ls_seq INTO TABLE st_tbintyp_sq.
      ENDIF.
    ENDIF.
    rv_seq = ls_seq-seqno.
  ENDMETHOD.
ENDCLASS.
