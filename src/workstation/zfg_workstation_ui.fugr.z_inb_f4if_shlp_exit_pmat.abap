FUNCTION z_inb_f4if_shlp_exit_pmat.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_STRUKNAME) TYPE  STRUKNAME OPTIONAL
*"     REFERENCE(IV_NO_RAW16) TYPE  XFELD OPTIONAL
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR_T
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
* Copied from /SCMB/MDL_F4IF_SHLP_EXIT_PROD

  FIELD-SYMBOLS: <lt_seltab>  TYPE STANDARD TABLE.
  DATA: lr_seltab TYPE REF TO data,
        lv_matnr TYPE matnr.

  DATA:
    ls_selopt TYPE ddshselopt,
    ls_shlp   LIKE shlp.

*handling masking characters
  LOOP AT shlp-selopt INTO ls_selopt.
    IF NOT ( callcontrol-step EQ 'SELONE' OR callcontrol-step EQ 'PRESEL1' ).
      IF ls_selopt-shlpfield EQ 'MATNR'.
        CALL FUNCTION 'CONVERSION_EXIT_MDLP1_INPUT'
          EXPORTING
            input         = ls_selopt-low
          IMPORTING
            output        = ls_selopt-low
          EXCEPTIONS
            OTHERS        = 0.
      ENDIF.
    ENDIF.
  ENDLOOP.
* if packmats = * interpret this that only entries with PMTYP <> SPACE are selected.
* This is required as in a search help it is not possible to pass <> SPACE.
* The changed behavior is due to S/4 HANY as here /SAPAPO/MATPACK has an entry for every product and not only for
* which are packaging products (tab for packaging data in /SAPAPO/MAT1 is maintained).
* append generic exclusioon of empty PMATs
  CLEAR ls_selopt.
  ls_selopt-shlpname  = shlp-shlpname.
  ls_selopt-shlpfield = 'PMTYP'.
  ls_selopt-sign      = 'E'.
  ls_selopt-option    = 'EQ'.
  ls_selopt-low       = ' '.
  APPEND ls_selopt TO ls_shlp-selopt.

* delete inclusion of * or inclusion of SPACE to avoid that SPACE is included as PMTYP
  LOOP AT ls_shlp-selopt TRANSPORTING NO FIELDS WHERE shlpfield = 'PMTYP' AND
    sign = 'I' AND ( low = '*' OR low = '' ).
    DELETE ls_shlp-selopt .
    CONTINUE.
  ENDLOOP.

  shlp-selopt[] = ls_shlp-selopt[].

  CALL FUNCTION '/SCMB/MDL_F4IF_SHLP_EXIT'
    EXPORTING
      iv_funcname  = '/SCMB/MDL_PRODUCT_RANGE_GET'
      iv_strukname = iv_strukname
      iv_no_raw16  = iv_no_raw16
    TABLES
      shlp_tab     = shlp_tab
      record_tab   = record_tab
    CHANGING
      shlp         = shlp
      callcontrol  = callcontrol.

  DATA: lt_result_tab TYPE STANDARD TABLE OF ddshretval.

  IF callcontrol-step EQ 'RETURN'.
    PERFORM transform_outval(saplsdsd)
           TABLES record_tab
                  lt_result_tab
           USING callcontrol shlp.

    PERFORM create_f4structure(/scmb/saplmdl_f4)
          CHANGING  shlp
                    lr_seltab.
    ASSIGN lr_seltab->* TO <lt_seltab>.
    APPEND INITIAL LINE TO <lt_seltab> ASSIGNING FIELD-SYMBOL(<ls_new_selline>).
    LOOP AT lt_result_tab INTO DATA(ls_res).
      ASSIGN COMPONENT ls_res-fieldname OF STRUCTURE <ls_new_selline> TO FIELD-SYMBOL(<lv_field>).
      <lv_field> = ls_res-fieldval.
    ENDLOOP.
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ls_new_selline> TO FIELD-SYMBOL(<lv_matnr>).
    IF zcl_ws_deco_ui=>so_active_ui IS NOT BOUND OR <lv_matnr> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    call FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = <lv_matnr>
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        others       = 2
      .
    IF SY-SUBRC <> 0.
      return.
    ENDIF.
    zcl_ws_deco_ui=>so_active_ui->get_pack_mat_dat_forf4(
      EXPORTING
        iv_pack_mat   = lv_matnr
      IMPORTING
        ev_hutype        = DATA(lv_hutype)
        ev_hutyptext     = DATA(lv_hutyptext)
    ).
    ASSIGN COMPONENT 'HUTYPE' OF STRUCTURE <ls_new_selline> TO FIELD-SYMBOL(<lv_hutype>).
    ASSIGN COMPONENT 'HUTYPTEXT' OF STRUCTURE <ls_new_selline> TO FIELD-SYMBOL(<lv_hutyptext>).
    IF <lv_hutype> IS ASSIGNED.
      <lv_hutype> = lv_hutype.
    ENDIF.
    IF <lv_hutyptext> IS ASSIGNED.
      <lv_hutyptext> = lv_hutyptext.
    ENDIF.

    IF NOT <lt_seltab>[] IS INITIAL.
      DATA: lv_lfieldname  LIKE dfies-lfieldname.
      LOOP AT shlp-fieldprop INTO data(ls_fieldprop)
          WHERE shlpoutput = abap_true.

        lv_lfieldname = ls_fieldprop-fieldname.

        CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
          EXPORTING
            parameter         = ls_fieldprop-fieldname
            fieldname         = lv_lfieldname
          TABLES
            shlp_tab          = shlp_tab
            record_tab        = record_tab[]
            source_tab        = <lt_seltab>
          CHANGING
            shlp              = shlp
            callcontrol       = callcontrol
          EXCEPTIONS
            parameter_unknown = 1
            OTHERS            = 2.                          "#EC *
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFUNCTION.
