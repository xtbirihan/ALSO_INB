CLASS zcl_ws_deco_sp DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ws_defaults_sp .
    INTERFACES zif_ws_deco_sp .

    METHODS add_sy_to_bapiret
      CHANGING
        !ct_bapiret TYPE bapirettab .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_packing TYPE REF TO /scwm/cl_wm_packing .
    DATA ms_workstation TYPE /scwm/tworkst .
    DATA mv_ws_is_mixed_pallets TYPE abap_bool .
    DATA:
      mt_c_curr_stock_situtation TYPE STANDARD TABLE OF ztinb_curst_styp WITH DEFAULT KEY .
    DATA:
      mt_c_packaging_settings TYPE STANDARD TABLE OF ztinb_wspack WITH DEFAULT KEY .
    DATA ms_workst_set TYPE ztinb_wlocmimo .
    DATA mv_lgnum TYPE /scwm/lgnum .
    DATA mv_buffer_lgpla TYPE /scwm/lgpla .

    METHODS cancel_wt
      IMPORTING
        !iv_tanum TYPE /scwm/tanum
      RAISING
        zcx_workstation .
    METHODS empty_hu
      IMPORTING
        !iv_docid   TYPE /scwm/de_docid
        !iv_doccat  TYPE /scwm/de_doccat
        !iv_guid_hu TYPE /scwm/guid_hu
      RAISING
        zcx_workstation .
ENDCLASS.



CLASS ZCL_WS_DECO_SP IMPLEMENTATION.


  METHOD add_sy_to_bapiret.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Add entry with SY fields to BAPI return table
**********************************************************************
    APPEND VALUE bapiret2(
             type   = sy-msgty
             number = sy-msgno
             id     = sy-msgid
             message_v1 = sy-msgv1
             message_v2 = sy-msgv2
             message_v3 = sy-msgv3
             message_v4 = sy-msgv4
         )
      TO ct_bapiret.
  ENDMETHOD.


  METHOD cancel_wt.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Cancel Warehouse task
**********************************************************************
    DATA: lt_bapiret  TYPE bapirettab,
          lv_severity TYPE bapi_mtype.


*   cancel wts
    CALL FUNCTION '/SCWM/TO_CANCEL'
      EXPORTING
        iv_lgnum       = mv_lgnum
        it_cancl       = VALUE /scwm/tt_cancl( ( tanum = iv_tanum ) )
        iv_commit_work = abap_false "need commit work and wait for UI refresh
      IMPORTING
        et_bapiret     = lt_bapiret
        ev_severity    = lv_severity.

    IF lv_severity CA 'EAX'.
      /scwm/cl_tm=>cleanup( ).
      RAISE EXCEPTION TYPE zcx_workstation
        EXPORTING
          messages = lt_bapiret.
    ELSE.
      COMMIT WORK AND WAIT.
      /scwm/cl_tm=>cleanup( ).
    ENDIF.
  ENDMETHOD.


  METHOD empty_hu.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Empty HU
**********************************************************************
    DATA: lt_bapiret  TYPE bapirettab,
          lv_severity TYPE bapi_mtype.
    DATA(lo_pack_ibdl) = NEW /scwm/cl_dlv_pack_ibdl( ).

    CALL METHOD lo_pack_ibdl->init
      EXPORTING
        iv_lgnum        = mv_lgnum
        it_docid        = VALUE #( ( docid = iv_docid ) )
        iv_doccat       = iv_doccat
        iv_no_refresh   = abap_false
        iv_lock_dlv     = abap_true
      IMPORTING
        ev_foreign_lock = DATA(lv_foreign_lock).

    lo_pack_ibdl->get_hu(
      EXPORTING
        iv_guid_hu  =  iv_guid_hu                " Unique Internal Identification of a Handling Unit
     EXCEPTIONS
        not_found  = 1
        OTHERS = 2
    ).
    IF sy-subrc <> 0.
      add_sy_to_bapiret( CHANGING ct_bapiret = lt_bapiret ).
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          messages = lt_bapiret.
    ENDIF.

    lo_pack_ibdl->lock_hu(
      EXPORTING
        iv_hu  =  iv_guid_hu                " Unique Internal Identification of a Handling Unit
     EXCEPTIONS
        error  = 1
        OTHERS = 2
    ).
    IF sy-subrc <> 0.
      add_sy_to_bapiret( CHANGING ct_bapiret = lt_bapiret ).
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          messages = lt_bapiret.
    ENDIF.
    lo_pack_ibdl->empty_hu(
      EXPORTING
        iv_hu  = iv_guid_hu                 " Unique Internal Identification of a Handling Unit                        " Quantity Structure
      EXCEPTIONS
        error  = 1                " See Log
        OTHERS = 2
    ).
    IF sy-subrc EQ 0.
      lo_pack_ibdl->save(
        EXPORTING
          iv_commit = 'X'
          iv_wait   = 'X'
        EXCEPTIONS
          error     = 1                " See Log
          OTHERS    = 2
      ).
      IF sy-subrc <> 0.
        add_sy_to_bapiret( CHANGING ct_bapiret = lt_bapiret ).
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          EXPORTING
            messages = lt_bapiret.
      ENDIF.
    ELSE.
      add_sy_to_bapiret( CHANGING ct_bapiret = lt_bapiret ).
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          messages = lt_bapiret.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~change_delivery.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Change expiration or best before date in delivery item
**********************************************************************
    DATA: lo_msg_box        TYPE REF TO /scdl/cl_sp_message_box ##needed,
          ls_itm_update     TYPE REF TO /scwm/dlv_item_out_prd_str,
          lt_asp_sapext     TYPE /scdl/t_sp_a_item_sapext_prdi,
          lt_asp_sapext_upd TYPE /scdl/t_sp_a_item_sapext_prdi.

    CHECK iv_bestbefore_date IS NOT INITIAL OR iv_bestbefore_date IS NOT INITIAL.

    DATA(lo_inbound_dlv_handler) = NEW /scdl/cl_sp_prd_inb( io_message_box = lo_msg_box ).


    lo_inbound_dlv_handler->lock(
      EXPORTING
        inkeys   = VALUE /scdl/t_sp_k_item( ( CORRESPONDING #( cs_delivery_item ) ) )
        aspect   = /scdl/if_sp_c=>sc_asp_item
        lockmode = /scdl/cl_sp_prd_inb=>/scdl/if_sp1_locking~sc_exclusive_lock ).

    lo_inbound_dlv_handler->select(
      EXPORTING
        inkeys       = VALUE /scdl/t_sp_k_item( ( CORRESPONDING #( cs_delivery_item ) ) )
        aspect       = /scdl/if_sp_c=>sc_asp_item_sapext_prdi
      IMPORTING
        outrecords   = lt_asp_sapext
        rejected     = DATA(lv_rejected)
        return_codes = DATA(lt_ret_codes)
    ).
    IF lv_rejected = abap_false.
      IF iv_expiration_date IS NOT INITIAL.
        CONVERT DATE iv_expiration_date TIME '000000' INTO TIME STAMP DATA(lv_ts) TIME ZONE sy-zonlo.
        lt_asp_sapext[ 1 ]-bestbefore_date-tzone = sy-zonlo.
        lt_asp_sapext[ 1 ]-bestbefore_date-tstfr = lv_ts.
        lt_asp_sapext[ 1 ]-bestbefore_date-tstto = lv_ts.
      ENDIF.
      IF iv_bestbefore_date IS NOT INITIAL.
        CONVERT DATE iv_bestbefore_date TIME '000000' INTO TIME STAMP lv_ts TIME ZONE sy-zonlo.
        lt_asp_sapext[ 1 ]-bestbefore_date-tzone = sy-zonlo.
        lt_asp_sapext[ 1 ]-bestbefore_date-tstfr = lv_ts.
        lt_asp_sapext[ 1 ]-bestbefore_date-tstto = lv_ts.
      ENDIF.

      lo_inbound_dlv_handler->update(
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_item_sapext_prdi
          inrecords    = lt_asp_sapext
        IMPORTING
          outrecords   = lt_asp_sapext_upd
          rejected     = lv_rejected
          return_codes = lt_ret_codes
      ).

      lo_inbound_dlv_handler->save( IMPORTING rejected = DATA(rejected) ).

      IF lv_rejected = abap_false.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
        RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE e016(zmc_workstation)
          EXPORTING messages = CORRESPONDING #( lo_msg_box->get_messages( ) MAPPING id         = msgid
                                                                                   number     = msgno
                                                                                   message_v1 = msgv1
                                                                                   message_v2 = msgv2
                                                                                   message_v3 = msgv3
                                                                                   message_v4 = msgv4 ).
      ENDIF.
    ENDIF.
    lo_inbound_dlv_handler->unlock(
      EXPORTING
        inkeys = lt_asp_sapext_upd
        aspect = /scdl/if_sp_c=>sc_asp_item_sapext_prdi ).

  ENDMETHOD.


  METHOD zif_ws_deco_sp~check_hu.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Check HU existence and Inboud Shipment number
**********************************************************************
    mo_packing->get_hu(
      EXPORTING
        iv_huident = iv_huident                " Handling Unit Identification
      IMPORTING
        et_huident = DATA(lt_huident)
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    READ TABLE lt_huident INTO DATA(ls_huid)
         WITH KEY idart = zcl_ws_deco_ui=>c_idart_inb_ship.
    IF sy-subrc NE 0 OR ls_huid-huident NE iv_inbship.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e030(zmc_workstation) WITH iv_huident iv_inbship.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~create_proposal.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Create the proposal in a separate internal modus. It must be do so,
*& to avoid the scambling of memory buffers
**********************************************************************
    DATA:
      ls_prepare_whr_int TYPE /scwm/s_to_prepare_whr_int,
      lt_prepare_whr_int TYPE /scwm/tt_to_prepare_whr_int,
      lt_ltap_vb         TYPE /scwm/tt_ltap_vb,
      lt_doc             TYPE /scwm/tt_pdenial_attr,
      lt_open_qty        TYPE /scwm/tt_whr_open_qty,
      lv_severity        TYPE bapi_mtype,
      lt_ltap            LIKE et_ltap.

    CLEAR: et_bapiret, et_ltap.

    ls_prepare_whr_int-rdoccat = wmegc_doccat_pdi.
    ls_prepare_whr_int-rdocid  = iv_rdocid.
    ls_prepare_whr_int-ritmid  = iv_ritmid.
    ls_prepare_whr_int-vltyp   = iv_vltyp.
    ls_prepare_whr_int-nltyp   = iv_nltyp.
    ls_prepare_whr_int-vlber   = iv_vlber.
    ls_prepare_whr_int-vlpla   = iv_vlpla.
    ls_prepare_whr_int-matid   = iv_matid.
    ls_prepare_whr_int-hutyp   = iv_hutyp.
    ls_prepare_whr_int-opunit  = iv_puom.

    IF it_preliminary_proposal IS INITIAL.
      ls_prepare_whr_int-anfme   = iv_anfme.
      ls_prepare_whr_int-altme   = iv_altme.
      APPEND ls_prepare_whr_int TO lt_prepare_whr_int.
    ELSE.
      LOOP AT it_preliminary_proposal INTO DATA(ls_prel).
        ls_prepare_whr_int-anfme   = ls_prel-qty.
        ls_prepare_whr_int-altme   = ls_prel-uom.
        IF ls_prel-nltyp IS NOT INITIAL.
          ls_prepare_whr_int-nltyp   = ls_prel-nltyp.
        ENDIF.
        IF ls_prel-vlpla IS NOT INITIAL.
          ls_prepare_whr_int-vlpla   =  ls_prel-vlpla.
        ENDIF.
        APPEND ls_prepare_whr_int TO lt_prepare_whr_int.
      ENDLOOP.
    ENDIF.
    IF iv_no_lock_release_after EQ abap_true.
      CALL FUNCTION 'RFC_CONNECTION_CLOSE'
        EXPORTING
          destination = 'NONE'                 " Name of the RFC destination
        EXCEPTIONS
          OTHERS      = 0.
      /scwm/cl_tm=>cleanup( ).
      ROLLBACK WORK.
    ENDIF.

    /scwm/cl_tm=>set_lgnum( mv_lgnum ).

    CALL FUNCTION 'Z_INB_TO_PREP_WHR_UI_INT'
      DESTINATION 'NONE'
      EXPORTING
        iv_lgnum              = mv_lgnum
        iv_mode               = wmegc_whr_mode_dia
        iv_process            = wmegc_whr_proc_pu
        iv_bname              = sy-uname
        it_prepare_whr_int    = lt_prepare_whr_int
        iv_force_empty_bin    = zcl_bin_determination_ctrl=>sv_force_empty_bin
        iv_no_lock_release    = iv_no_lock_release_after
        iv_location_mixed     = iv_location_mixed
      IMPORTING
        et_ltap_vb            = et_ltap
        et_doc                = lt_doc
        et_open_qty           = lt_open_qty
        et_bapiret            = et_bapiret
        ev_severity           = lv_severity
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.
    IF sy-subrc NE 0. "If call failed try to call it direct
      CALL FUNCTION 'Z_INB_TO_PREP_WHR_UI_INT'
        EXPORTING
          iv_lgnum           = mv_lgnum
          iv_mode            = wmegc_whr_mode_dia
          iv_process         = wmegc_whr_proc_pu
          iv_bname           = sy-uname
          it_prepare_whr_int = lt_prepare_whr_int
          iv_force_empty_bin = zcl_bin_determination_ctrl=>sv_force_empty_bin
          iv_location_mixed  = iv_location_mixed
        IMPORTING
          et_ltap_vb         = et_ltap
          et_doc             = lt_doc
          et_open_qty        = lt_open_qty
          et_bapiret         = et_bapiret
          ev_severity        = lv_severity.
    ENDIF.
    IF iv_no_lock_release_after EQ abap_false.
      CALL FUNCTION 'RFC_CONNECTION_CLOSE'
        EXPORTING
          destination = 'NONE'                 " Name of the RFC destination
        EXCEPTIONS
          OTHERS      = 0.
      /scwm/cl_tm=>cleanup( ).
      ROLLBACK WORK.
    ENDIF.
    IF lv_severity CA wmegc_severity_eax AND et_ltap IS INITIAL.

      RAISE EXCEPTION TYPE zcx_workstation
        EXPORTING
          messages = et_bapiret.
    ENDIF.



    CLEAR: mt_c_packaging_settings, mt_c_curr_stock_situtation.
    mt_c_curr_stock_situtation = zcl_crud_ztinb_curst_styp=>select_multi_by_lgnum( iv_lgnum = mv_lgnum ).

  ENDMETHOD.


  METHOD zif_ws_deco_sp~delete_hu.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Delete HU
**********************************************************************
    /scwm/cl_tm=>set_lgnum( mv_lgnum ).
    mo_packing->get_hu(
      EXPORTING
        iv_huident = iv_huident                " Handling Unit Identification
      IMPORTING
        es_huhdr   = DATA(ls_huhdr)                  " Internal Structure for Processing the HU Header
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    mo_packing->delete_hu(
      EXPORTING
        iv_hu  = ls_huhdr-guid_hu                 " Unique Internal Identification of a Handling Unit
      EXCEPTIONS
        error  = 1                " error
        OTHERS = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL METHOD mo_packing->save(
      EXPORTING
        iv_commit = abap_true
        iv_wait   = abap_false
      EXCEPTIONS
        error     = 1
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    /scwm/cl_tm=>cleanup( ).
  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_buffer_stbin.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get Buffer storage bin
**********************************************************************
    rv_buffer_stbin = mv_buffer_lgpla.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_current_stock_situation.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the current stock situtation for storage type level
**********************************************************************
    DATA(lt_stock) = zif_ws_deco_sp~get_stock_data( iv_matnr = iv_matnr ).

    IF mt_c_curr_stock_situtation IS INITIAL.
      mt_c_curr_stock_situtation = zcl_crud_ztinb_curst_styp=>select_multi_by_lgnum( iv_lgnum = mv_lgnum ).
    ENDIF.

    SORT mt_c_curr_stock_situtation BY curr_stock_seq.
    rt_situ = VALUE #( FOR cust IN mt_c_curr_stock_situtation
                       WHERE ( curr_stock_rel EQ abap_true ) ( lgtyp = cust-lgtyp ) ).

    LOOP AT lt_stock ASSIGNING FIELD-SYMBOL(<stock>) GROUP BY ( lgtyp = <stock>-lgtyp ) INTO DATA(ls_grp_lgtyp).
      READ TABLE rt_situ REFERENCE INTO DATA(lr_curr_stock)
           WITH KEY lgtyp = ls_grp_lgtyp-lgtyp.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      LOOP AT GROUP ls_grp_lgtyp INTO DATA(lgtyp) GROUP BY ( lgpla = lgtyp-lgpla ) INTO DATA(ls_grp_lgpla).
        ADD 1 TO lr_curr_stock->no_bins.
        LOOP AT GROUP ls_grp_lgpla INTO DATA(ls_stock).
          ADD ls_stock-quan TO lr_curr_stock->quan.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    DELETE rt_situ WHERE quan IS INITIAL.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_delivery_from_is.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the deliveries for the IS number
**********************************************************************
    SELECT docid FROM /scdl/db_proch_i
      INTO TABLE rt_docid
      WHERE zzinbship EQ iv_inbship.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_delivery_items.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get delivery items for product in deliveries
**********************************************************************
    DATA: lt_selection TYPE /scwm/dlv_selection_tab,
          ls_selection LIKE LINE OF lt_selection,
          ls_t300      TYPE /scwm/s_t300_md.

    /scwm/cl_tm=>cleanup( ).

    DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).

    "Read T300 entry for SCUGUID
    CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
      EXPORTING
        iv_lgnum   = mv_lgnum
      IMPORTING
        es_t300_md = ls_t300
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc = 0.
      CLEAR ls_selection.
      ls_selection-fieldname = /scdl/if_dl_logfname_c=>sc_locationid_wh_h.
      ls_selection-sign      = wmegc_sign_inclusive.
      ls_selection-option    = wmegc_option_eq.
      ls_selection-low       = ls_t300-scuguid.
      APPEND ls_selection TO lt_selection.
    ENDIF.

    IF iv_putaway_open EQ abap_true. "Select only open putaway
      CLEAR ls_selection.
      ls_selection-fieldname = /scdl/if_dl_logfname_c=>sc_status_value_dpt_i.
      ls_selection-sign      = wmegc_sign_inclusive.
      ls_selection-option    = wmegc_option_eq.
      ls_selection-low       = '1'.
      APPEND ls_selection TO lt_selection.
      ls_selection-low       = '2'.
      APPEND ls_selection TO lt_selection.
    ENDIF.

    lt_selection = VALUE #( BASE lt_selection FOR docid IN it_docid
                                 ( fieldname = /scdl/if_dl_logfname_c=>sc_docid_h
                                   sign =  wmegc_sign_inclusive option = wmegc_option_eq
                                   low = docid-docid )
                          ).
    TRY.
        lo_dlv->query(
          EXPORTING
            it_selection    = lt_selection
*            it_docid        = it_docid
            iv_whno         = mv_lgnum
            is_read_options = VALUE #(  )
            is_include_data = VALUE #( item_addmeas = abap_true item_product_ext = abap_true )
          IMPORTING
            et_items        = DATA(lt_dlv_item)
        ).
      CATCH /scdl/cx_delivery INTO DATA(lo_cx).
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e011(zmc_workstation) WITH iv_matnr.
    ENDTRY.


    DELETE lt_dlv_item WHERE product-productno NE iv_matnr.
    rt_del_item = lt_dlv_item.

  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_hu_by_ident.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the HU by ID
**********************************************************************
    CLEAR: es_huhdr, et_huident.

    mo_packing->get_hu(
      EXPORTING
        iv_huident = |{ iv_huident ALPHA = IN }|                 " Handling Unit Identification
      IMPORTING
        et_huident = et_huident
        es_huhdr   = es_huhdr                 " Internal Structure for Processing the HU Header
      EXCEPTIONS
        OTHERS     = 0
    ).
  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_packspec.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get packing specification
**********************************************************************
    DATA:
      lt_det_data         TYPE    /scwm/tt_ps_determination_data,
      lt_packspec         TYPE    /scwm/tt_ps_det_packspec,
      lt_packspec_content TYPE    /scwm/tt_packspec_nested,

      lv_guid_ps          TYPE    /scwm/de_guid_ps,

      ls_packspec_header  TYPE    /scwm/s_ps_header_int,
      ls_packspec_content TYPE    /scwm/s_packspec_nested,
      ls_partyloc         TYPE    /scdl/dl_partyloc_str,
      ls_det_data         TYPE    /scwm/s_ps_determination_data,
      ls_data             TYPE    /scwm/s_lgnumlocid.

    FIELD-SYMBOLS:
      <ls_packspec>         TYPE    /scwm/s_ps_det_packspec.

    CLEAR : et_content, et_elementgroup, es_header, ev_pc_per_fulpal.
* determine scu by lgnum
    CALL FUNCTION '/SCWM/LGNUM_LOCID_READ'
      EXPORTING
        iv_lgnum       = mv_lgnum
      IMPORTING
        es_data        = ls_data
      EXCEPTIONS
        data_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation.
    ENDIF.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zcross_0006                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_condtype_0ibd              " Parameter ID for process
      IMPORTING
        ev_constant  = DATA(lv_condtype_0ibd)                " Parameter-Framework Low
    ).

    ls_det_data-line_id             = 1.
    ls_det_data-fields-pak_locid    = ls_data-locid.
    ls_det_data-fields-pak_matid    = iv_matid.
    APPEND ls_det_data TO lt_det_data.

    DATA(lv_procedure) = CONV /sapcnd/ctlist_name( lv_condtype_0ibd ).

    CALL FUNCTION '/SCWM/PS_FIND_EVALUATE_MULTI'
      EXPORTING
        iv_procedure    = lv_procedure
        it_det_data     = lt_det_data
        iv_read_refmat  = space
      IMPORTING
        et_packspec     = lt_packspec
      EXCEPTIONS
        determine_error = 1
        read_error      = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt_packspec ASSIGNING <ls_packspec>
      INDEX 1.
    CHECK sy-subrc = 0.

    READ TABLE <ls_packspec>-packspec INTO lv_guid_ps INDEX 1.
    CHECK sy-subrc EQ 0.

*   Read Packspec
    CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
      EXPORTING
        iv_guid_ps             = lv_guid_ps
        iv_read_elements       = abap_true
        iv_read_dyn_attributes = abap_true
      IMPORTING
        es_packspec_header     = es_header
        et_packspec_content    = et_content
        et_elementgroup        = et_elementgroup
      EXCEPTIONS
        error                  = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
             MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
          EXPORTING
            iv_matid     = iv_matid                 " Material GUID16  mit Konvertierungsexit
            iv_quan      = CONV /scwm/de_quantity( 1 )                  " Mengenfeld
            iv_unit_from = zif_c_mdm_tool=>c_units-palet
            iv_unit_to   = zif_c_mdm_tool=>c_units-piece                 " Mengeneinheit
            iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
          IMPORTING
            ev_quan      = ev_pc_per_fulpal.                 " Mengenfeld
      CATCH /scwm/cx_md_interface  /scwm/cx_md_batch_required /scwm/cx_md_internal_error
            /scwm/cx_md_batch_not_required  /scwm/cx_md_material_exist ##no_handler.     " Material existiert nicht
    ENDTRY.

  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_pack_mat_data.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get packing material data
**********************************************************************
    DATA: ls_mat_pack   TYPE  /scwm/s_material_pack,
          ls_mat_global TYPE  /scwm/s_material_global.

    CLEAR: ev_pack_mat, es_pmat_setting, ev_pack_mat_text, ev_hutype, ev_hutyptext, ev_nr_internal.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = iv_pack_mat_id
            iv_lgnum      = mv_lgnum
            iv_lgtyp      = iv_nltyp
            iv_entitled   = iv_entitled
          IMPORTING
            es_mat_global = ls_mat_global
            es_mat_pack   = ls_mat_pack.
      CATCH /scwm/cx_md.
        RAISE EXCEPTION TYPE zcx_workstation.
    ENDTRY.

    IF ls_mat_pack-pmtyp IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e022(zmc_workstation) WITH ls_mat_global-matnr.
    ENDIF.

    ev_hutype = ls_mat_pack-hutyp.
    SELECT SINGLE FROM /scwm/thutypt
           FIELDS hutyptext
           WHERE spras EQ @sy-langu
             AND hutyp EQ @ev_hutype
           INTO @ev_hutyptext .

    ev_nr_internal = abap_false.
    ev_pack_mat = ls_mat_global-matnr.
    ev_pack_mat_text = ls_mat_global-maktx.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_prop_hu_data.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get HU data proposal
**********************************************************************
    DATA lt_pset LIKE mt_c_packaging_settings.
    DATA lt_pset_flt LIKE mt_c_packaging_settings.
    DATA ls_mat_pack TYPE  /scwm/s_material_pack.

    CLEAR:
      ev_change_st_type_mono, ev_pack_mat, ev_pack_mat_id, es_pmat_setting,
      ev_pack_mat_text, ev_hutype, ev_hutyptext, ev_nr_internal,
      ev_no_settings.

    IF mt_c_packaging_settings IS INITIAL.
      mt_c_packaging_settings = zcl_crud_ztinb_wspack=>select_multi_by_lgnum( mv_lgnum ).
    ENDIF.
    IF mt_c_curr_stock_situtation IS INITIAL.
      mt_c_curr_stock_situtation = zcl_crud_ztinb_curst_styp=>select_multi_by_lgnum( iv_lgnum = mv_lgnum ).
    ENDIF.

    TRY.
        ev_change_st_type_mono = mt_c_curr_stock_situtation[ lgtyp = iv_nltyp ]-change_stype_mono.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    lt_pset = VALUE #( FOR pset IN  mt_c_packaging_settings WHERE ( lgtyp = iv_nltyp ) ( pset ) ).

    IF lt_pset IS INITIAL.
      ev_no_settings = abap_true.
      RETURN.
*      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e019(zmc_workstation) WITH iv_nltyp.
    ENDIF.

    IF lt_pset[ 1 ]-pcormc EQ  zif_ws_deco_sp~c_pc_or_mc-not_relevant.
      IF lines( lt_pset ) NE 1.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e020(zmc_workstation) WITH iv_nltyp.
      ENDIF.
      DATA(ls_pset) = lt_pset[ 1 ].
    ELSE.
      IF iv_keepcar EQ abap_true.
        DATA(lv_pc_or_mc) = zif_ws_deco_sp~c_pc_or_mc-store_in_master_cartons.
      ELSE.
        lv_pc_or_mc = zif_ws_deco_sp~c_pc_or_mc-store_in_pieces.
      ENDIF.

      IF lv_pc_or_mc EQ zif_ws_deco_sp~c_pc_or_mc-store_in_pieces.
        LOOP AT lt_pset TRANSPORTING NO FIELDS
             WHERE lptyp IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          lt_pset_flt = VALUE #( FOR pset IN  lt_pset WHERE ( lptyp  = iv_nlptyp
                                                          AND pcormc = lv_pc_or_mc
                                                             ) ( pset ) ).
        ELSE.
          lt_pset_flt = lt_pset.
        ENDIF.
      ELSE.
        IF iv_dirrpl = abap_true.
          lt_pset_flt = VALUE #( FOR pset IN  lt_pset WHERE ( mc_noadd_pm  EQ abap_true
                                                          AND mc_stin_tote EQ abap_false
                                                          AND pcormc       EQ lv_pc_or_mc
                                                           ) ( pset ) ).
        ELSE.
          lt_pset_flt = VALUE #( FOR pset IN  lt_pset WHERE ( mc_noadd_pm  EQ abap_false
                                                          AND mc_stin_tote EQ abap_true
                                                          AND lptyp        EQ iv_nlptyp
                                                          AND pcormc       EQ lv_pc_or_mc
                                                           ) ( pset ) ).
        ENDIF.
      ENDIF.
      IF lt_pset_flt IS INITIAL.
        ev_no_settings = abap_true.
        RETURN.
      ENDIF.
      IF lines( lt_pset_flt ) NE 1.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e020(zmc_workstation) WITH iv_nltyp.
      ENDIF.
      ls_pset = lt_pset[ 1 ].
    ENDIF.

    es_pmat_setting = ls_pset.
*    IF ls_pset-selection_logic EQ zif_ws_deco_sp~c_selection_logic-default_pack_mat.
    ev_pack_mat = ls_pset-pmat.
    IF ev_pack_mat IS NOT INITIAL.
      TRY.
          DATA(ls_prod) = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod(  iv_prodno    = ev_pack_mat ).

        CATCH /scmb/cx_md_access. " Exception Class for Master Data Accesses
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e027(zmc_workstation) WITH iv_nltyp.
      ENDTRY.
      ev_pack_mat_id = ls_prod-prodid.
      ev_pack_mat_text = ls_prod-prodtext.
    ENDIF.

    IF ev_pack_mat_id IS NOT INITIAL.
      zif_ws_deco_sp~get_pack_mat_data(
        EXPORTING
          iv_pack_mat_id   = ev_pack_mat_id                 " Packaging Material
          iv_entitled      = iv_entitled                 " Party Entitled to Dispose
          iv_nltyp         = iv_nltyp                    " Destination Storage Type
        IMPORTING
          ev_pack_mat      = ev_pack_mat                      " Packaging Material
          es_pmat_setting  = es_pmat_setting                  " Workstation Packaging Settings
          ev_pack_mat_text = ev_pack_mat_text                 " Material Description
          ev_hutype        = ev_hutype                        " Handling Unit Type
          ev_hutyptext     = ev_hutyptext                     " Description of Handling Unit Type
          ev_nr_internal   = ev_nr_internal                   " General Flag
      ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~get_stock_data.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get physical stock
**********************************************************************
    DATA: lo_mon_stock TYPE REF TO /scwm/cl_mon_stock,
          lt_stock_mon TYPE /scwm/tt_stock_mon.


    lo_mon_stock = NEW #( iv_lgnum = mv_lgnum ).

    IF iv_only_ws_spec EQ abap_true.
      DATA(lt_lgtyp) = zcl_crud_ztinb_curst_styp=>select_multi_by_lgnum( iv_lgnum = mv_lgnum ).
      SORT lt_lgtyp BY curr_stock_seq.
    ENDIF.
    "Select stock with selection criteria
    lo_mon_stock->get_physical_stock(
      EXPORTING
        iv_skip_bin      = abap_false
        iv_skip_resource = abap_true
        iv_skip_tu       = abap_true
        it_matnr_r       = VALUE #( ( sign = 'I' option = 'EQ' low = iv_matnr ) )
        it_lgtyp_r       = VALUE #( FOR ltyp IN lt_lgtyp ( sign = 'I' option = 'EQ' low = ltyp-lgtyp ) )
        it_lgpla_r       = it_rng_lgpla
      IMPORTING
        et_stock_mon     = lt_stock_mon
        ev_error         = DATA(lv_error)
    ).

    "Error reading stock data
    IF lv_error = abap_true.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE e015(zmc_workstation).
    ENDIF.

    rt_stock = lt_stock_mon.


  ENDMETHOD.


  METHOD zif_ws_deco_sp~goods_received.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Set goods received
**********************************************************************
    CONSTANTS: lc_action  TYPE string VALUE '/SCWM/ACT_POST_GM'.
    CONSTANTS: lc_post_gr_hu  TYPE string VALUE '/SCWM/ACT_HU_POST_GM'.
    CONSTANTS: lc_cancel_gr_hu  TYPE string VALUE '/SCWM/ACT_HU_CANCEL_GM'.

    DATA: lt_a_head         TYPE /scdl/t_sp_a_head,
          ls_itm_update     TYPE REF TO /scwm/dlv_item_out_prd_str,
          lt_asp_sapext     TYPE /scdl/t_sp_a_item_sapext_prdi,
          lt_asp_sapext_upd TYPE /scdl/t_sp_a_item_sapext_prdi,
          lt_sp_a_hu        TYPE /scwm/t_sp_a_hu.

    DATA(lo_inbound_dlv_handler) = NEW /scwm/cl_sp_prd_inb(
                                                            iv_mode = /scdl/cl_sp=>sc_mode_classic
                                                            ).


    lo_inbound_dlv_handler->lock(
      EXPORTING
        inkeys   = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )
        aspect   = /scwm/if_sp_c=>sc_asp_head
        lockmode = /scdl/cl_sp_prd_inb=>/scdl/if_sp1_locking~sc_shared_lock
      IMPORTING
        rejected = DATA(lv_rejected) ).

    IF lv_rejected = abap_false.

      IF iv_guid_hu IS INITIAL.
        lo_inbound_dlv_handler->execute(
          EXPORTING
            aspect       = /scwm/if_sp_c=>sc_asp_head
            inkeys       = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )
            action       = lc_action
          IMPORTING
            outrecords   = lt_a_head
            rejected     = lv_rejected ).
      ELSE.
        lo_inbound_dlv_handler->select_by_relation(
          EXPORTING
            relation     = /scwm/if_sp_c=>sc_rel_head_to_hu                 " Relation Name
            inrecords    = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )                " Source Aspects
            aspect       = /scwm/if_sp_c=>sc_asp_head                 " Source Aspect for Relation
          IMPORTING
            rejected     = lv_rejected
            outrecords   = lt_sp_a_hu
        ).
        lo_inbound_dlv_handler->execute(
          EXPORTING
            aspect       = /scwm/if_sp_c=>sc_asp_hu
            inkeys       = VALUE /scdl/t_sp_k_hu( ( docid = iv_docid huid = iv_guid_hu ) )
            action       = COND #( WHEN iv_reverse_goods_receipt EQ abap_false THEN lc_post_gr_hu
                                                                               ELSE lc_cancel_gr_hu )
            relation_inkey = VALUE /scdl/s_sp_k_head( docid = iv_docid )
            relation     = /scwm/if_sp_c=>sc_rel_head_to_hu
          IMPORTING
            outrecords   = lt_a_head
            rejected     = lv_rejected ).
      ENDIF.

      IF lv_rejected = abap_false.
        lo_inbound_dlv_handler->save( IMPORTING rejected = lv_rejected ).
        COMMIT WORK AND WAIT.

      ELSE.
        /scwm/cl_tm=>cleanup( ).
        /scwm/cl_tm=>set_lgnum( mv_lgnum ).

        ROLLBACK WORK.
        RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE w049(zmc_workstation).
      ENDIF.
    ENDIF.
    lo_inbound_dlv_handler->unlock(
      EXPORTING
        inkeys = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )
        aspect = /scwm/if_sp_c=>sc_asp_head ).

    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( mv_lgnum ).
  ENDMETHOD.


  METHOD zif_ws_deco_sp~hu_exists.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Does HU exist?
**********************************************************************
    rv_exists = abap_true.
    mo_packing->get_hu(
      EXPORTING
        iv_huident = iv_huident                " Handling Unit Identification
      IMPORTING
        et_huident = DATA(lt_huident)
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~is_workstation_mixedpallet.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Is workstation mixed workstation?
**********************************************************************
    rv_mixed = mv_ws_is_mixed_pallets.
  ENDMETHOD.


  METHOD zif_ws_deco_sp~pack_and_print_hus.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Pack and print the HU.
*& First, create the new HU and repack the quantity. Then check, whether
*& cart handling is active and call it. The last step is the print.
**********************************************************************
    DATA: lt_bapiret  TYPE bapirettab,
          lv_severity TYPE bapi_mtype,
          lv_tanum    TYPE /scwm/tanum,
          lt_ltap_vb  TYPE /scwm/tt_ltap_vb ##needed.
    DATA: ls_create_wt TYPE /scwm/s_to_create_int,
          lt_create_wt TYPE /scwm/tt_to_create_int,
          lv_vsola     TYPE /scwm/ltap-vsola.
    DATA: lo_print_hu TYPE REF TO zif_inb_deco_print_hu.

    CLEAR: ev_guid_hu, ev_tanum, ev_tapos, ev_who.

    DATA(lo_pack_ibdl) = NEW /scwm/cl_dlv_pack_ibdl( ).

    CALL METHOD lo_pack_ibdl->init
      EXPORTING
        iv_lgnum        = mv_lgnum
        it_docid        = VALUE #( ( docid = iv_docid ) )
        iv_doccat       = iv_doccat
        iv_no_refresh   = abap_false
        iv_lock_dlv     = abap_true
      IMPORTING
        ev_foreign_lock = DATA(lv_foreign_lock).

    IF NOT lv_foreign_lock IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE  ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv3.
    ENDIF.

    "If only internal HU ID are to be assigned then multiply the lines
    DATA(ls_wts) = is_proposed_wt.
    IF iv_alt_uom IS NOT INITIAL AND iv_alt_uom NE is_proposed_wt-meins.
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = ls_wts-matid                 " Material GUID16  mit Konvertierungsexit
              iv_quan      = ls_wts-vsolm                 " Mengenfeld
              iv_unit_from = ls_wts-meins                 " Mengeneinheit
              iv_unit_to   = iv_alt_uom                 " Mengeneinheit
              iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
            IMPORTING
              ev_quan      = lv_vsola.                 " Mengenfeld
          ls_wts-vsola = lv_vsola.
          ls_wts-altme = iv_alt_uom.
        CATCH /scwm/cx_md_interface           " Import Parameter fehlerhaft
              /scwm/cx_md_batch_required     " Charge ist zur Umrechnung notwendig
              /scwm/cx_md_internal_error     " Interner Fehler
              /scwm/cx_md_batch_not_required " Material nicht Chargenpflichtig, Charge nicht benötigt
              /scwm/cx_md_material_exist.     " Material existiert nicht
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e066(zmc_workstation).
      ENDTRY.
    ENDIF.

    DATA(ls_hu) = is_hu.
    lo_pack_ibdl->create_hu(
      EXPORTING
        iv_pmat      = ls_hu-pmatid                " Material GUID16 with Conversion Exit
        iv_huident   = ls_hu-huident                " Handling Unit Identification
      RECEIVING
        es_huhdr     = DATA(ls_huhdr)
      EXCEPTIONS
        error        = 1
        OTHERS       = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          hu_creation_failed = abap_true.
    ENDIF.
    ev_guid_hu = ls_huhdr-guid_hu.

    ls_huhdr-zz_plan_totes = iv_nof_totes.

    lo_pack_ibdl->change_huhdr(
      EXPORTING
        is_huhdr   = ls_huhdr                 " Internal Structure for Processing the HU Header
      EXCEPTIONS
        error      = 1
        not_locked = 2
        OTHERS     = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          hu_creation_failed = abap_true.
    ENDIF.

    DATA(ls_pack_stock) = CORRESPONDING /scwm/s_pack_stock( ls_wts ).
    lo_pack_ibdl->pack_stock(
      EXPORTING
        iv_dest_hu      = ls_huhdr-guid_hu                 " Unique Internal Identification of a Handling Unit
        is_material     = ls_pack_stock                 " Data that describes the stocks
        is_quantity     = VALUE #( quan = ls_wts-vsolm unit = ls_wts-meins )                 " Quantity Structure
      IMPORTING
        es_quantity     = DATA(ls_qty_packed) ##needed                " Quantity Structure
      EXCEPTIONS
        error           = 1                " See Log
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          hu_creation_failed = abap_true.
    ENDIF.
    CLEAR ls_create_wt.
    ls_create_wt = CORRESPONDING #( ls_wts ).
    ls_create_wt-vlenr = ls_huhdr-huident.
    ls_create_wt-nlenr = ls_huhdr-huident.
    ls_create_wt-anfme = ls_wts-vsola.
    ls_create_wt-altme = ls_wts-altme.
    ls_create_wt-opunit = iv_alt_uom.
    ls_create_wt-sguid_hu = ls_huhdr-guid_hu.
    ls_create_wt-dguid_hu = ls_huhdr-guid_hu.
    APPEND ls_create_wt TO lt_create_wt.

    lo_pack_ibdl->save(
      EXPORTING
        iv_commit = 'X'
        iv_wait   = 'X'
      EXCEPTIONS
        error     = 1                " See Log
        OTHERS    = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          hu_creation_failed = abap_true.
    ENDIF.

    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( mv_lgnum ).


    IF lv_severity NE wmegc_severity_err.
      CALL FUNCTION '/SCWM/TO_CREATE'
        EXPORTING
          iv_lgnum       = mv_lgnum
          iv_commit_work = abap_false
          it_create      = lt_create_wt
        IMPORTING
          ev_tanum       = lv_tanum
          et_ltap_vb     = lt_ltap_vb
          et_bapiret     = lt_bapiret
          ev_severity    = lv_severity.
    ENDIF.
    IF lv_severity CA wmegc_severity_eax.
      ROLLBACK WORK.
      DATA(ls_error) = VALUE bapiret2( lt_bapiret[ type = lv_severity ] OPTIONAL ).

      empty_hu(
          iv_docid        = iv_docid
          iv_doccat       = iv_doccat
          iv_guid_hu      = ls_huhdr-guid_hu
      ).


      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID ls_error-id TYPE lv_severity NUMBER ls_error-number
            WITH ls_error-message_v1 ls_error-message_v2
                 ls_error-message_v3 ls_error-message_v4
            EXPORTING
              messages = lt_bapiret.
    ELSE.
      COMMIT WORK AND WAIT.
      /scwm/cl_tm=>cleanup( ).
      READ TABLE lt_ltap_vb INDEX 1 INTO DATA(ls_ltap).
      ev_tanum = ls_ltap-tanum.
      ev_tapos = ls_ltap-tapos.
      ev_who   = ls_ltap-who.

      zif_ws_deco_sp~goods_received( iv_docid = iv_docid iv_guid_hu = ls_huhdr-guid_hu ).

      IF iv_dest_hu_to_test IS NOT INITIAL AND io_cart_callback IS BOUND.
        TRY.
            io_cart_callback->pack_hu( iv_dest_hu   = iv_dest_hu_to_test
                                       iv_source_hu = ls_huhdr-guid_hu
                                       iv_logpos    = iv_logpos_to_test
                                       iv_test_only = abap_true ).
          CATCH zcx_workstation INTO DATA(lx_move_test).

            /scwm/cl_tm=>cleanup( ).
            /scwm/cl_tm=>set_lgnum( mv_lgnum ).

            TRY.
                cancel_wt( lv_tanum ).
                zif_ws_deco_sp~goods_received( iv_docid = iv_docid iv_guid_hu = ls_huhdr-guid_hu iv_reverse_goods_receipt = abap_true ).
                empty_hu(
                  iv_docid   = iv_docid                 " Doc. Identification for Document-Related Stocks
                  iv_doccat  = iv_doccat                 " Doc. Category for Doc. Reference and Doc.-Related Stock
                  iv_guid_hu = ls_huhdr-guid_hu                 " Unique Internal Identification of a Handling Unit
                ).
              CATCH zcx_workstation INTO DATA(lx_reverse).
            ENDTRY.
            lx_move_test->hu_creation_failed = abap_true.
            RAISE EXCEPTION lx_move_test.
        ENDTRY.
      ENDIF.


      IF iv_no_move_to_buffer EQ abap_false.
        "move final HU

        CLEAR: lt_bapiret, lt_ltap_vb, lv_severity.

        CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'                    "#EC ENHOK
          EXPORTING
            iv_lgnum     = ms_workst_set-lgnum
            it_create_hu = VALUE /scwm/tt_to_crea_hu( (
                                              huident = ls_huhdr-huident
                                              squit   = abap_true
                                              nlpla   = mv_buffer_lgpla
                                              procty  = zif_wme_c=>gs_procty-immediate
                                           ) )
            iv_wtcode    = wmegc_wtcode_adhoc_hu
          IMPORTING
            et_ltap_vb   = lt_ltap_vb
            et_bapiret   = lt_bapiret
            ev_severity  = lv_severity.
        IF lv_severity CA wmegc_severity_eax.
          ROLLBACK WORK.
          ls_error = VALUE bapiret2( lt_bapiret[ type = lv_severity ] OPTIONAL ).
          RAISE EXCEPTION TYPE zcx_workstation
                MESSAGE ID ls_error-id TYPE lv_severity NUMBER ls_error-number
                WITH ls_error-message_v1 ls_error-message_v2
                     ls_error-message_v3 ls_error-message_v4
                EXPORTING
                  messages = lt_bapiret
               .
        ENDIF.

      ENDIF.

      IF lo_print_hu IS BOUND.
        lo_print_hu->print_hu(
          iv_warehouse = ls_huhdr-lgnum                 " Warehouse Number/Warehouse Complex
          iv_guid_hu   = ls_huhdr-guid_hu                 " Unique Internal Identification of a Handling Unit
        ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_ws_defaults_sp~get_defaults.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the defaults
**********************************************************************
    DATA: lv_terminal TYPE /scwm/de_wc_terminal.
    CALL FUNCTION 'TH_USER_INFO'
      EXPORTING
        client   = sy-mandt
        user     = sy-uname
      IMPORTING
        terminal = lv_terminal.

    SELECT SINGLE * FROM ztinb_wlocmimo INTO @DATA(ls_wlocmimo)
        WHERE terminal EQ @lv_terminal.
    IF sy-subrc EQ 0.
      es_defaults-lgnum = ls_wlocmimo-lgnum.
      es_defaults-workst_loc = ls_wlocmimo-lgpla.
    ELSE.
      SELECT FROM /scmb/esdus
        FIELDS *
        WHERE uname EQ @sy-uname
          AND action EQ @zif_ws_deco_sp~c_action_workstation
        INTO TABLE @DATA(lt_param).

      READ TABLE lt_param INTO DATA(ls_param)
           WITH KEY element = zif_ws_deco_sp~c_element_warehouse.
      IF sy-subrc EQ 0.
        es_defaults-lgnum = ls_param-active.
      ELSE.
        GET PARAMETER ID '/SCWM/LGN' FIELD es_defaults-lgnum.
      ENDIF.

      READ TABLE lt_param INTO ls_param
           WITH KEY element = zif_ws_deco_sp~c_element_workst_loc.
      IF sy-subrc EQ 0.
        es_defaults-workst_loc = ls_param-active.
      ELSE.
        GET PARAMETER ID 'ZDECO_WORKSTATION' FIELD es_defaults-workst_loc.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM /scwm/lagp INTO @es_lagp
      WHERE lgnum EQ @es_defaults-lgnum
        AND lgpla EQ @es_defaults-workst_loc.
  ENDMETHOD.


  METHOD zif_ws_defaults_sp~set_defaults_deco.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Set the defualts and initialize the packing
**********************************************************************
    DATA: lt_def_db TYPE STANDARD TABLE OF /scmb/esdus.

    CLEAR es_lagp.

    SELECT SINGLE * FROM /scwm/t300 INTO @DATA(ls_wh)
           WHERE lgnum EQ @iv_lgnum.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e007(zmc_workstation).
    ENDIF.

    SELECT SINGLE * FROM /scwm/lagp INTO @es_lagp
           WHERE lgnum EQ @iv_lgnum
             AND lgpla EQ @iv_stor_bin_lgpla.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e009(zmc_workstation).
    ENDIF.

    /scwm/cl_wm_packing=>get_instance(
      IMPORTING
        eo_instance = mo_packing ).
    SELECT SINGLE * FROM /scwm/tworkst INTO @ms_workstation
           WHERE lgnum EQ @iv_lgnum
             AND workstation EQ 'ALL'.
    mv_lgnum = iv_lgnum.
    mo_packing->init_by_workstation(
      EXPORTING
        is_workstation = ms_workstation                " Work Station Profile
        ir_bin         = VALUE #( ( sign = 'I' option = 'EQ' low = iv_stor_bin_lgpla ) )
      EXCEPTIONS
        error          = 1                " Error, see log
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e008(zmc_workstation).
    ENDIF.

    lt_def_db = VALUE #(
                  ( uname = sy-uname action = zif_ws_deco_sp~c_action_workstation element = zif_ws_deco_sp~c_element_warehouse active = iv_lgnum )
                  ( uname = sy-uname action = zif_ws_deco_sp~c_action_workstation element = zif_ws_deco_sp~c_element_workst_loc active = iv_stor_bin_lgpla )
                ).
    MODIFY /scmb/esdus FROM TABLE lt_def_db.
    COMMIT WORK.


    ms_workst_set = zcl_crud_ztinb_wlocmimo=>select_single_by_storage_loc(
                              iv_warehouse        = iv_lgnum
                              iv_storage_location = iv_stor_bin_lgpla
                           ).

    mv_ws_is_mixed_pallets = ms_workst_set-mixed.
    mv_buffer_lgpla        = ms_workst_set-buffer_lgpla.
  ENDMETHOD.


  METHOD zif_ws_defaults_sp~set_defaults_workcenter.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Set the defaults for the work center
**********************************************************************
    mv_lgnum = iv_lgnum.
    /scwm/cl_wm_packing=>get_instance(
      IMPORTING
        eo_instance = mo_packing ).
    SELECT SINGLE * FROM /scwm/tworkst INTO @ms_workstation
           WHERE lgnum       EQ @iv_lgnum
             AND workstation EQ @iv_workcenter.
    mv_lgnum = iv_lgnum.
    mv_buffer_lgpla = ms_workstation-lgpla.
    mo_packing->init_by_workstation(
      EXPORTING
        is_workstation = ms_workstation                " Work Station Profile
        ir_bin         = VALUE #( ( sign = 'I' option = 'EQ' low = ms_workstation-lgpla ) )
      EXCEPTIONS
        error          = 1                " Error, see log
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e008(zmc_workstation).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
