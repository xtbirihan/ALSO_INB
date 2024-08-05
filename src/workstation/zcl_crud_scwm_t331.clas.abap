class ZCL_CRUD_SCWM_T331 definition
  public
  final
  create public .

public section.

  types:
    TT_t331 TYPE SORTED TABLE OF /scwm/T331 WITH UNIQUE key lgnum lgtyp .

  class-methods GET_STTYPE_CTRL_BY_STTYPE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGTYP type /SCWM/LGTYP
    returning
      value(RS_RESULT) type /SCWM/T331 .
protected section.
private section.

  class-data ST_T331 type TT_T331 .
ENDCLASS.



CLASS ZCL_CRUD_SCWM_T331 IMPLEMENTATION.


  METHOD get_sttype_ctrl_by_sttype.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Read customizing Storage Type Control
**********************************************************************
    READ TABLE st_t331 INTO rs_result
         WITH TABLE KEY lgnum = iv_lgnum
                        lgtyp = iv_lgtyp.
    IF sy-subrc NE 0.
      SELECT SINGLE FROM /scwm/t331
             FIELDS *
             WHERE lgnum EQ @iv_lgnum
               AND lgtyp EQ @iv_lgtyp
        INTO @rs_result.
      IF sy-subrc EQ 0.
        INSERT rs_result INTO TABLE st_t331.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
