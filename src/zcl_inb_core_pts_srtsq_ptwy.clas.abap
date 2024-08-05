class ZCL_INB_CORE_PTS_SRTSQ_PTWY definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_PTS_SRTSQ .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  methods ADD_MESSAGES_BAPIRET
    changing
      !CT_BAPIRET type BAPIRETTAB .
  methods UPDATE_PENALTY_SSEQU_GROUP
    importing
      !IT_PENALTY type /SCWM/TT_PENALTY_SRT
      !IO_INST_PTWY type ref to ZCL_CRUD_INB_STTYPE_PTWY .
ENDCLASS.



CLASS ZCL_INB_CORE_PTS_SRTSQ_PTWY IMPLEMENTATION.


  METHOD /scwm/if_ex_core_pts_srtsq~sort_sequence.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
      lo_inst_ptwy    TYPE REF TO zcl_crud_inb_sttype_ptwy.

    IF zcl_switch=>get_switch_state( iv_lgnum = flt_val
                                     iv_devid = zif_switch_const=>c_zinb_003 ) = abap_false.
      " Do not execute logic
      RETURN.
    ENDIF.

    DATA(lt_penalty_str) = it_sorttab.

    lo_inst_ptwy = zcl_crud_inb_sttype_ptwy=>get_inst( ).

    " Check if there is already used field
    READ TABLE lo_inst_ptwy->mt_st_type_cust_used WITH KEY executed = abap_true TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.

      LOOP AT lt_penalty_str ASSIGNING FIELD-SYMBOL(<ls_penalty_str>).

        READ TABLE lo_inst_ptwy->mt_st_type_cust_used WITH KEY lgtyp = <ls_penalty_str>-lgtyp TRANSPORTING NO FIELDS.

        IF sy-subrc <> 0.
          DELETE lt_penalty_str USING KEY loop_key.
        ENDIF.
      ENDLOOP.

    ENDIF.

    " penalty update
    update_penalty_ssequ_group( it_penalty   = lt_penalty_str
                                io_inst_ptwy = lo_inst_ptwy ).

    et_sorttab = lt_penalty_str.

  ENDMETHOD.


  METHOD add_messages_bapiret.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      ls_bapiret TYPE bapiret2.

    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO DATA(lv_msg).

    ls_bapiret = VALUE #( id         = sy-msgid
                          type       = sy-msgty
                          number     = sy-msgno
                          message_v1 = sy-msgv1
                          message_v2 = sy-msgv2
                          message_v3 = sy-msgv3
                          message_v4 = sy-msgv4
                          message    = lv_msg ).

    APPEND ls_bapiret TO ct_bapiret.

  ENDMETHOD.


  METHOD update_penalty_ssequ_group.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************
*---------------------------------------------------------
    " Sort penalty - find first
    " Copy from standard - /SCWM/LPUT_BIN_DETF78 line 962-1000
*---------------------------------------------------------
    DATA:
      lv_first    TYPE xfeld,
      lt_sort_pen TYPE io_inst_ptwy->tt_penalty_str,
      lt_lgtyp    TYPE /scwm/tt_lgtyp.

    lt_sort_pen = it_penalty.
    CLEAR lt_lgtyp.

    lt_lgtyp = VALUE #( FOR <ls_typ> IN lt_sort_pen
                        ( <ls_typ>-lgtyp ) ).
    SORT lt_lgtyp.
    DELETE ADJACENT DUPLICATES FROM lt_lgtyp.

    LOOP AT lt_lgtyp ASSIGNING FIELD-SYMBOL(<ls_lgtyp>).
      lv_first = abap_true.
      LOOP AT lt_sort_pen ASSIGNING FIELD-SYMBOL(<ls_srt_pen>) WHERE lgtyp = <ls_lgtyp>.

        IF lv_first = abap_true.
          CLEAR lv_first.
          READ TABLE lt_sort_pen INTO DATA(ls_srt_penalty)
                WITH KEY lgnum    = <ls_srt_pen>-lgnum
                         lgtyp    = <ls_srt_pen>-lgtyp
                         storsect = <ls_srt_pen>-storsect
                         bintype  = <ls_srt_pen>-bintype.

          IF sy-subrc = 0.
            ls_srt_penalty-first_lgt = abap_true.
            MODIFY lt_sort_pen FROM ls_srt_penalty USING KEY loop_key.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " Find first
      READ TABLE lt_sort_pen INTO ls_srt_penalty WITH KEY lgtyp     = <ls_lgtyp>
                                                          first_lgt = abap_true.
      IF sy-subrc = 0.
        LOOP AT lt_sort_pen ASSIGNING FIELD-SYMBOL(<ls_pen_first>)
                                             WHERE lgnum    = ls_srt_penalty-lgnum
                                               AND lgtyp    = ls_srt_penalty-lgtyp
                                               AND storsect = ls_srt_penalty-storsect
                                               AND bintype  = ls_srt_penalty-bintype.
          <ls_pen_first>-new_seq = abap_true.
          MODIFY lt_sort_pen FROM <ls_pen_first> USING KEY loop_key.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    io_inst_ptwy->mt_penalty_list = lt_sort_pen.

  ENDMETHOD.
ENDCLASS.
