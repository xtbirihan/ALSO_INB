class ZCL_WS_PTWY_CART_SP definition
  public
  final
  create private .

public section.

  interfaces ZIF_WS_PTWY_CART_SP .

  class-methods CREATE_INSTANCE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_STOR_BIN_LGPLA type /SCWM/DE_LGPLA
    returning
      value(RO_INST) type ref to ZIF_WS_PTWY_CART_SP
    raising
      ZCX_WORKSTATION .
  class-methods CREATE_INSTANCE_REPL
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_WORKCENTER type /SCWM/DE_WORKSTATION
    returning
      value(RO_INST) type ref to ZIF_WS_PTWY_CART_SP
    raising
      ZCX_WORKSTATION .
protected section.
private section.

  types:
    BEGIN OF ty_deco_params,
      is_proposed_wt TYPE  /scwm/ltap,
      iv_docid       TYPE  /scwm/de_docid,
      iv_doccat      TYPE  /scwm/de_doccat,
      iv_alt_uom     TYPE  /scwm/sp_uom,
      iv_nof_totes   TYPE  zde_plan_totes,
    END OF ty_deco_params .
  types:
    BEGIN OF ty_repl_params,
      iv_lgnum    TYPE /scwm/lgnum,
      iv_tanum    TYPE /scwm/tanum,
      is_confirm  TYPE /scwm/to_conf,
      is_conf_exc	TYPE /scwm/s_conf_exc,
      iv_huident  TYPE /scwm/de_huident,
      iv_anfme    TYPE /scwm/de_ui_vsolm,
      iv_altme    TYPE /scwm/lrmei,
      is_wt_cur	  TYPE /scwm/s_to_det_mon,
    END OF ty_repl_params .

  data MV_LGNUM type /SCWM/LGNUM .
  data MO_SP_DECO type ref to ZIF_WS_DECO_SP .
  data:
    mt_cart_settings TYPE STANDARD TABLE OF ztinb_cart_bcode WITH DEFAULT KEY .
  data MO_PACK type ref to /SCWM/CL_WM_PACKING .
  data MS_DECO_PARAMS type TY_DECO_PARAMS .
  data MV_NLTYP type /SCWM/LTAP_NLTYP .
  data MO_REPL_SRV type ref to ZIF_WC_REPLENISHMENT .
  data MV_BUFFER_STBIN type /SCWM/LGPLA .
  data MS_REPL_PARAMS type TY_REPL_PARAMS .

  methods DELETE_HU
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION .
  methods PACK_AND_PRINT_CART
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
      !IV_PMATID type /SCWM/DE_PMATID
      !IV_CART_ID type /SCWM/HUIDENT
      !IV_POS_ON_CART type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP
      /SCWM/CX_CORE .
  methods PACK_AND_PRINT_LOWEST
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
      !IV_PMATID type /SCWM/DE_PMATID
      !IV_DEST_HU_TO_TEST type /SCWM/GUID_HU optional
      !IV_LOGPOS_TO_TEST type /SCWM/DE_LOGPOS optional
    exporting
      !EV_GUID_HU type /SCWM/GUID_HU
      !EV_TANUM type /SCWM/TANUM
      !EV_TAPOS type /SCWM/TAPOS
      !EV_WHO type /SCWM/DE_WHO
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP .
ENDCLASS.



CLASS ZCL_WS_PTWY_CART_SP IMPLEMENTATION.


  METHOD CREATE_INSTANCE.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Create instance for deconsolidation UI
**********************************************************************
    DATA(lo_inst) = NEW zcl_ws_ptwy_cart_sp( ).
    ro_inst = lo_inst.
    lo_inst->mv_lgnum = iv_lgnum.
    lo_inst->mo_sp_deco = new zcl_ws_deco_sp(  ).
    lo_inst->mo_sp_deco->zif_ws_defaults_sp~set_defaults_deco(
      EXPORTING
        iv_lgnum          = iv_lgnum                  " Warehouse Number/Warehouse Complex
        iv_stor_bin_lgpla = iv_stor_bin_lgpla                 " Storage Bin
    ).
    lo_inst->mv_buffer_stbin = lo_inst->mo_sp_deco->get_buffer_stbin( ).
    SELECT * FROM ztinb_cart_bcode
      INTO TABLE lo_inst->mt_cart_settings
     WHERE lgnum EQ iv_lgnum.
  ENDMETHOD.


  METHOD create_instance_repl.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Create instance for replenishment UI
**********************************************************************
    DATA(lo_inst) = NEW zcl_ws_ptwy_cart_sp( ).
    ro_inst = lo_inst.
    lo_inst->mv_lgnum = iv_lgnum.
    lo_inst->mo_sp_deco = NEW zcl_ws_deco_sp(  ).
    lo_inst->mo_sp_deco->zif_ws_defaults_sp~set_defaults_workcenter(
      EXPORTING
        iv_lgnum          = iv_lgnum                  " Warehouse Number/Warehouse Complex
        iv_workcenter     = iv_workcenter                 " Storage Bin
    ).
    lo_inst->mv_buffer_stbin = lo_inst->mo_sp_deco->get_buffer_stbin( ).
    CLEAR lo_inst->mo_sp_deco.

    SELECT * FROM ztinb_cart_bcode
      INTO TABLE lo_inst->mt_cart_settings
     WHERE lgnum EQ iv_lgnum.
    lo_inst->mo_repl_srv = NEW zcl_wc_replenishment( ).
  ENDMETHOD.


  METHOD delete_hu.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Delete HU
**********************************************************************
    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = mo_pack ).
    mo_pack->init_pack(
      EXPORTING
        iv_badi_appl  = 'WME'                  " Handling Unit Application
        iv_lgnum      = mv_lgnum
       EXCEPTIONS
         error         = 1                " Error, see log
         OTHERS        = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_pack->get_hu(
      EXPORTING
        iv_huident  = iv_huident                 " Unique Internal Identification of a Handling Unit
      IMPORTING
        et_huhdr    = DATA(lt_huhdr)
      EXCEPTIONS
        not_found    = 1                " Error, see log
        OTHERS       = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE lt_huhdr INTO DATA(ls_huhdr)
         WITH KEY huident = iv_huident.

    mo_pack->delete_hu(
      EXPORTING
        iv_hu        = ls_huhdr-guid_hu                 " Unique Internal Identification of a Handling Unit
      EXCEPTIONS
        error        = 1                " Error, see log
        OTHERS       = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF sy-subrc EQ 0.
      mo_pack->save(
        EXPORTING
          iv_commit = 'X'
          iv_wait   = 'X'
        EXCEPTIONS
          error     = 1                " See Log
          OTHERS    = 2
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_workstation
           MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      /scwm/cl_tm=>cleanup( ).
      /scwm/cl_tm=>set_lgnum( mv_lgnum ).
    ELSE.
      ROLLBACK WORK.
      /scwm/cl_tm=>cleanup( ).
      /scwm/cl_tm=>set_lgnum( mv_lgnum ).
    ENDIF.
  ENDMETHOD.


  METHOD pack_and_print_cart.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Pack and print with putaway cart handling. Check/create cart/tote
*& then create the deco/repl HU and move it's WT into the WO of the cart/tote
**********************************************************************
    DATA lv_huident_cart TYPE /scwm/huident.
    DATA lv_huident_tote TYPE /scwm/huident.
    DATA lv_huident TYPE /scwm/huident.


    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = mo_pack ).

    mo_pack->init(
      EXPORTING
        iv_lgnum = mv_lgnum
      EXCEPTIONS
        error  = 1
        OTHERS = 2 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    zif_ws_ptwy_cart_sp~interpret_huident(
      EXPORTING
        iv_huident       = iv_huident                 " Handling Unit Identification
      IMPORTING
        es_cart_settings = DATA(ls_cart_settings)                 " Cart Barcode
        ev_no_cart       = DATA(lv_no_cart)
        ev_cart_id       = DATA(lv_cart_id)                       " Handling Unit Identification
        ev_pos_on_cart   = DATA(lv_pos_on_cart)                   " Handling Unit Identification
        ev_tote_id       = DATA(lv_tote_id)                       " Handling Unit Identification
        ev_comp_on_tote  = DATA(lv_comp_on_tote)                  " Handling Unit Identification
    ).

    IF lv_no_cart EQ abap_true
       OR ( lv_pos_on_cart IS INITIAL
            AND lv_tote_id IS INITIAL
            AND lv_comp_on_tote IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e056(zmc_workstation) WITH iv_huident.
    ENDIF.


    ASSERT lv_cart_id IS NOT INITIAL AND lv_pos_on_cart IS NOT INITIAL
        OR lv_tote_id IS NOT INITIAL AND lv_comp_on_tote IS NOT INITIAL
        OR lv_tote_id IS NOT INITIAL AND lv_comp_on_tote IS INITIAL
           AND iv_cart_id IS NOT INITIAL AND iv_pos_on_cart IS NOT INITIAL.

    "Cart is to be created
    IF iv_cart_id IS NOT INITIAL.
      lv_cart_id = iv_cart_id.
    ENDIF.

    IF lv_cart_id IS NOT INITIAL.
      IF iv_cart_id IS NOT INITIAL.
        lv_huident = iv_cart_id.
      ELSE.
        lv_huident = |{ zif_ws_ptwy_cart_sp=>c_pckg_type_cart }{ lv_cart_id }|.
      ENDIF.

      zif_ws_ptwy_cart_sp~check_and_create_cart(
        EXPORTING
          iv_huident = lv_huident                 " Handling Unit Identification
          iv_pckg_type = zif_ws_ptwy_cart_sp=>c_pckg_type_cart
        IMPORTING
          ev_stype   = DATA(lv_stype)                 " Storage Type
          ev_guid_hu = DATA(lv_guid_cart_hu)
          ev_who     = DATA(lv_who_cart)
          ev_already_exist = DATA(lv_cart_existed_before)
      ).

      IF lv_stype IS NOT INITIAL  AND lv_stype NE mv_nltyp.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e058(zmc_workstation) WITH lv_stype mv_nltyp.
      ENDIF.
      lv_huident_cart = lv_huident.
    ENDIF.

    IF lv_pos_on_cart IS NOT INITIAL AND lv_tote_id IS INITIAL
       OR iv_pos_on_cart IS NOT INITIAL AND lv_tote_id IS NOT INITIAL.
      IF iv_pos_on_cart IS NOT INITIAL.
        lv_pos_on_cart = iv_pos_on_cart.
      ENDIF.

      DATA(lv_logpos) = CONV /scwm/de_logpos( |{ lv_pos_on_cart+1(1) }{ lv_pos_on_cart+3(1) }| ).
      TRY.
          pack_and_print_lowest(
            EXPORTING
              iv_huident = iv_huident                 " Handling Unit Identification
              iv_pmatid  = iv_pmatid                  " Packaging Material
              iv_dest_hu_to_test = lv_guid_cart_hu
              iv_logpos_to_test  = lv_logpos
            IMPORTING
              ev_guid_hu = DATA(lv_guid_cart_pos_hu)
              ev_tanum   = DATA(lv_tanum_cart_pos)
              ev_tapos   = DATA(lv_tapos_cart_pos)
              ev_who     = DATA(lv_who_cart_pos)
          ).
        CATCH zcx_workstation INTO DATA(lx_pnp_low).
          IF lx_pnp_low->hu_creation_failed EQ abap_true AND lv_huident_cart IS NOT INITIAL.
            delete_hu( |{ lv_huident_cart ALPHA = IN }| ).
          ENDIF.
          RAISE EXCEPTION lx_pnp_low.
      ENDTRY.

      zif_ws_ptwy_cart_sp~pack_hu( iv_dest_hu = lv_guid_cart_hu iv_source_hu = lv_guid_cart_pos_hu iv_logpos = lv_logpos ).
      IF lv_who_cart IS NOT INITIAL.
        zif_ws_ptwy_cart_sp~move_task_to_order(
          EXPORTING
            iv_tanum    = lv_tanum_cart_pos                 " Warehouse Task
            iv_from_who = lv_who_cart_pos                 " Warehouse Order Number
            iv_to_who   = lv_who_cart                 " Warehouse Order Number
        ).
      ENDIF.
    ENDIF.

    "Tote with compartment is to be created
    IF iv_pos_on_cart IS INITIAL AND lv_tote_id IS NOT INITIAL.
      lv_huident = |{ zif_ws_ptwy_cart_sp=>c_pckg_type_tote }{ lv_tote_id }|.
      zif_ws_ptwy_cart_sp~check_and_create_cart(
        EXPORTING
          iv_huident = lv_huident                 " Handling Unit Identification
          iv_pckg_type = zif_ws_ptwy_cart_sp=>c_pckg_type_tote
        IMPORTING
          ev_stype   = lv_stype                 " Storage Type
          ev_guid_hu = DATA(lv_guid_tote_hu)
          ev_who     = DATA(lv_who_tote)
      ).

      IF lv_stype IS NOT INITIAL  AND lv_stype NE mv_nltyp.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e058(zmc_workstation) WITH lv_stype mv_nltyp.
      ENDIF.
      lv_huident_tote = lv_huident.
    ENDIF.

    IF lv_comp_on_tote IS NOT INITIAL.
      pack_and_print_lowest(
        EXPORTING
          iv_huident = iv_huident                 " Handling Unit Identification
          iv_pmatid  = iv_pmatid                  " Packaging Material
        IMPORTING
          ev_guid_hu = DATA(lv_guid_compartment_hu)
          ev_tanum   = DATA(lv_tanum_compartment)
          ev_tapos   = DATA(lv_tapos_compartment)
          ev_who     = DATA(lv_who_compartment)
      ).
      zif_ws_ptwy_cart_sp~pack_hu( iv_dest_hu = lv_guid_tote_hu iv_source_hu = lv_guid_compartment_hu iv_logpos = CONV #( lv_comp_on_tote ) ).
      IF lv_who_tote IS NOT INITIAL.
        zif_ws_ptwy_cart_sp~move_task_to_order(
          EXPORTING
            iv_tanum    = lv_tanum_compartment                 " Warehouse Task
            iv_from_who = lv_who_compartment                 " Warehouse Order Number
            iv_to_who   = lv_who_tote                 " Warehouse Order Number
        ).
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD pack_and_print_lowest.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Pack and print deconsolidation ui/replenishment UI
**********************************************************************
    CLEAR: ev_guid_hu, ev_tanum, ev_tapos, ev_who.

    IF mo_sp_deco IS BOUND.
      mo_sp_deco->pack_and_print_hus(
        EXPORTING
          is_hu = VALUE #( huident = iv_huident pmatid = iv_pmatid )
          is_proposed_wt = ms_deco_params-is_proposed_wt
          iv_docid       = ms_deco_params-iv_docid                 " Structure with Doc. ID
          iv_doccat      = ms_deco_params-iv_doccat
          iv_alt_uom     = ms_deco_params-iv_alt_uom
          iv_nof_totes   = ms_deco_params-iv_nof_totes
          iv_dest_hu_to_test = iv_dest_hu_to_test
          iv_logpos_to_test  = iv_logpos_to_test
          io_cart_callback   = me
        IMPORTING
          ev_guid_hu = ev_guid_hu
          ev_tanum   = ev_tanum
          ev_tapos   = ev_tapos
          ev_who     = ev_who
          ).           " Unit of Measure
    ELSEIF mo_repl_srv IS BOUND.
      zif_ws_ptwy_cart_sp~check_and_create_cart(
        EXPORTING
          iv_huident   = iv_huident                 " Handling Unit Identification
          iv_pmatid    = iv_pmatid                 " Packaging Type
        IMPORTING
          ev_stype     = DATA(lv_stype)                 " Storage Type
          ev_guid_hu   = ev_guid_hu                 " Unique Internal Identification of a Handling Unit
          ev_who       = DATA(lv_who)                 " Warehouse Order Number
          et_to        = DATA(lt_to)                 " Transfer Order Details for the WM Monitor
          ev_already_exist = DATA(lv_already_exist)
      ).

      DATA(ls_confirm) = ms_repl_params-is_confirm.
      DATA(ls_wt_curr) = ms_repl_params-is_wt_cur.
      ls_confirm-nlpla = mv_buffer_stbin.
      ls_wt_curr-vlpla = mv_buffer_stbin.
      ls_wt_curr-sguid_hu = ev_guid_hu.
      ls_wt_curr-dguid_hu = ev_guid_hu.
      mo_repl_srv->pack_n_print(
        EXPORTING
          iv_lgnum    = mv_lgnum                    " Warehouse Number/Warehouse Complex
          iv_tanum    = ms_repl_params-iv_tanum                    " Warehouse Task
          is_confirm  = ms_repl_params-is_confirm                  " Item Fields for Warehouse Task for Confirmation
          is_conf_exc = ms_repl_params-is_conf_exc                 " Exception Code for WT Confirmation Item
          iv_huident  = ms_repl_params-iv_huident                  " Handling Unit Identification
          iv_anfme    = ms_repl_params-iv_anfme                    " Source Target Quantity in Base Unit of Measure
          iv_altme    = ms_repl_params-iv_altme                    " Alternative Unit of Measure for Stockkeeping Unit
          is_wt_cur   = ls_wt_curr                   " Transfer Order Details for the WM Monitor
        IMPORTING
          et_bapiret  = DATA(lt_bapiret)                 " Table with BAPI Return Information
          et_ltap_vb  = DATA(lt_ltap_vb)                 " Table Type: Warehouse Tasks Internal
      ).
      READ TABLE lt_ltap_vb INTO DATA(ls_ltap) INDEX 1.
      ev_tanum = ls_ltap-tanum.
      ev_tapos = ls_ltap-tapos.
      ev_who   = ls_ltap-who.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ws_ptwy_cart_sp~check_and_create_cart.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Check whether cart is already created, if not then create. Return the
*& warehous order assigne to the cart
**********************************************************************
    DATA lv_huident TYPE /scwm/huident.
    DATA lv_huident_search TYPE /scwm/huident.
    DATA: ls_selcrit TYPE /scwm/s_to_selcrit_mon,
          lt_vlenr_r TYPE rseloption.

    CLEAR: ev_stype, ev_guid_hu, ev_who, et_to, ev_already_exist.

    lv_huident = |{ iv_huident ALPHA = IN }|.
    SELECT * FROM /scwm/huhdr
      INTO TABLE @DATA(lt_hus)
      WHERE lgnum EQ @mv_lgnum
        AND huident EQ @lv_huident.

    IF lt_hus IS INITIAL. "Cart is not created
      IF iv_pmatid IS NOT INITIAL.
        DATA(lv_pmatid) = iv_pmatid.
      ELSEIF iv_pckg_type IS NOT INITIAL.
        READ TABLE mt_cart_settings INTO DATA(ls_cart_set)
             WITH KEY pckg_type = iv_pckg_type.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_workstation
                MESSAGE e050(zmc_workstation) WITH zif_ws_ptwy_cart_sp=>c_pckg_type_cart.
        ENDIF.

        TRY.
            DATA(ls_pmat) = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod(  iv_prodno    = ls_cart_set-pmat ).

          CATCH /scmb/cx_md_access. " Exception Class for Master Data Accesses
            RAISE EXCEPTION TYPE zcx_workstation MESSAGE e057(zmc_workstation) WITH ls_cart_set-pmat.
        ENDTRY.
        lv_pmatid = ls_pmat-prodid.
      ENDIF.

      " RM 07.09.2023
      IF mo_pack->gv_lgnum IS INITIAL.
        mo_pack->gv_lgnum = mv_lgnum.

        CALL FUNCTION '/SCWM/TO_INIT_NEW'
          EXPORTING
            iv_lgnum = mv_lgnum.
      ENDIF.

      mo_pack->create_hu(
        EXPORTING
          iv_pmat      = lv_pmatid                " Material GUID16 with Conversion Exit
          iv_huident   = lv_huident                " Handling Unit Identification
          i_location   = mv_buffer_stbin
        RECEIVING
          es_huhdr     = DATA(ls_huhdr)
        EXCEPTIONS
          error        = 1
          OTHERS       = 2
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      mo_pack->save(
        EXPORTING
          iv_commit = 'X'
          iv_wait   = 'X'
        EXCEPTIONS
          error     = 1                " See Log
          OTHERS    = 2
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      ev_guid_hu = ls_huhdr-guid_hu.
    ELSE.
      ev_already_exist = abap_true.
      mo_pack->get_hu(
        EXPORTING
          iv_huident = iv_huident                 " Handling Unit Identification
        IMPORTING
*          et_huident =
*          et_huitm   =                  " Material Items in the HU
          es_huhdr   =  ls_huhdr                " Internal Structure for Processing the HU Header
          et_huhdr   =  DATA(lt_huhdr)                " Table Type for HU Headers in the Internal Structure
          et_hutree  =  DATA(lt_hutree)                " Table with HU Hierarchy Entries
*          et_huref   =                  " Table with HU References
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2
      ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      ev_guid_hu = ls_huhdr-guid_hu.
      "Add open WT selection criteria
      ls_selcrit-r_tostat = VALUE #( ( sign = wmegc_sign_inclusive
                                       option = wmegc_option_eq
                                       low = wmegc_to_open ) ).

      "Add HU Number selection criteria
      ls_selcrit-r_lenr = VALUE #( FOR pos_hu IN lt_huhdr ( sign = wmegc_sign_inclusive
                                                              option = wmegc_option_eq
                                                              low = pos_hu-huident ) ).

      "Select Open HU WTs
      CALL FUNCTION '/SCWM/TO_GET_WIP'
        EXPORTING
          iv_lgnum   = mv_lgnum
          iv_open    = abap_true
          iv_srcdata = abap_true
          iv_dstdata = abap_true
          is_selcrit = ls_selcrit
        IMPORTING
          et_to      = et_to.

      IF et_to IS INITIAL.
        RETURN.
      ENDIF.

      ev_stype = et_to[ 1 ]-nltyp.
      ev_who = et_to[ 1 ]-who.

    ENDIF.

  ENDMETHOD.


  METHOD zif_ws_ptwy_cart_sp~interpret_huident.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Read the cart barcode customizing and interpret the barcode.
*& Determine the cart id, position in the cart, tote is and compartment id
**********************************************************************
    DATA: lv_cart_id      TYPE /scwm/huident,
          lv_pos_on_cart  TYPE /scwm/huident,
          lv_tote_id      TYPE /scwm/huident,
          lv_comp_on_tote TYPE /scwm/huident,
          lv_pos          TYPE i,
          lv_color_pos    TYPE i,
          lv_rest         TYPE /scwm/huident.

    CLEAR: es_cart_settings, ev_no_cart, ev_cart_id, ev_pos_on_cart,
           ev_tote_id.

    DATA(lv_huident) = iv_huident.
    SHIFT lv_huident LEFT DELETING LEADING '0'.

    SELECT SINGLE * FROM ztinb_cart_bcode
      INTO es_cart_settings
     WHERE lgnum EQ mv_lgnum
       AND pckg_type EQ lv_huident+lv_pos(2)
       AND color_code EQ zif_ws_ptwy_cart_sp=>c_no_color.
    IF sy-subrc NE 0.
      "Color relevant: search for the entry
      lv_color_pos = lv_pos + 2.
      SELECT SINGLE * FROM ztinb_cart_bcode
        INTO es_cart_settings
       WHERE lgnum EQ mv_lgnum
         AND pckg_type EQ lv_huident+lv_pos(2)
         AND color_code EQ lv_huident+lv_color_pos(2).
      IF sy-subrc NE 0.
        ev_no_cart = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    "Get cart id
    ADD 2 TO lv_pos.
    IF es_cart_settings-cart_id EQ abap_true.
      IF es_cart_settings-cart_id_len IS INITIAL.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e050(zmc_workstation) WITH es_cart_settings-pckg_type.
      ENDIF.

      lv_cart_id = lv_huident+lv_pos(es_cart_settings-cart_id_len).
      IF strlen( lv_cart_id ) LT es_cart_settings-cart_id_len.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e051(zmc_workstation) WITH iv_huident es_cart_settings-cart_id_len.
      ENDIF.
      ADD es_cart_settings-cart_id_len TO lv_pos.
      ev_cart_id = lv_cart_id.
    ENDIF.

    "Get position on cart
    IF es_cart_settings-pos_on_cart EQ abap_true.
      IF es_cart_settings-pos_on_cart_len IS INITIAL.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e050(zmc_workstation) WITH es_cart_settings-pckg_type.
      ENDIF.

      lv_pos_on_cart = lv_huident+lv_pos(es_cart_settings-pos_on_cart_len).
      IF strlen( lv_pos_on_cart ) LT es_cart_settings-pos_on_cart_len.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e052(zmc_workstation) WITH iv_huident es_cart_settings-pos_on_cart_len.
      ENDIF.
      ADD es_cart_settings-pos_on_cart_len TO lv_pos.
      ev_pos_on_cart = lv_pos_on_cart.
    ENDIF.

    "Get tote ID
    IF es_cart_settings-tote_id EQ abap_true.
      IF es_cart_settings-tote_id_len IS INITIAL.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e050(zmc_workstation) WITH es_cart_settings-pckg_type.
      ENDIF.

      lv_tote_id = lv_huident+lv_pos(es_cart_settings-tote_id_len).
      IF strlen( lv_tote_id ) LT es_cart_settings-tote_id_len.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e054(zmc_workstation) WITH iv_huident es_cart_settings-tote_id_len.
      ENDIF.
      ADD es_cart_settings-tote_id_len TO lv_pos.
      ev_tote_id = lv_tote_id.
    ENDIF.

    "Component on tote
    IF es_cart_settings-comp_on_tote EQ abap_true.
      IF es_cart_settings-comp_on_tote_l IS INITIAL.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e050(zmc_workstation) WITH es_cart_settings-pckg_type.
      ENDIF.

      lv_comp_on_tote = lv_huident+lv_pos(es_cart_settings-comp_on_tote_l).
      IF strlen( lv_comp_on_tote ) LT es_cart_settings-comp_on_tote_l.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e054(zmc_workstation) WITH iv_huident es_cart_settings-comp_on_tote_l.
      ENDIF.
      ADD es_cart_settings-comp_on_tote_l TO lv_pos.
      ev_comp_on_tote = lv_comp_on_tote.
    ENDIF.

    "Is there any remaining part?
    lv_rest = lv_huident+lv_pos.
    IF lv_rest IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE e053(zmc_workstation) WITH iv_huident lv_rest.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_WS_PTWY_CART_SP~MOVE_TASK_TO_ORDER.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Move task to a warehouse order
**********************************************************************
    DATA: lv_severity TYPE  bapi_mtype,
          lt_to       TYPE  /scwm/tt_tanum,
          lt_bapiret  TYPE  bapirettab.

    /scwm/cl_tm=>cleanup( ).

    CALL FUNCTION '/SCWM/WHO_TO_UNASSIGN'
      EXPORTING
        iv_lgnum    = mv_lgnum                  " Lagernummer/Lagerkomplex
        iv_who      = iv_from_who                 " Lagerauftrag: interne Identifikation
        it_to       = VALUE /scwm/tt_tanum( ( iv_tanum ) )               " TAs, für die die Zuordnung aufgehoben werden soll
        iv_commit   = abap_false
      IMPORTING
        ev_severity = lv_severity
        et_to       = lt_to                       " TAs, für die die Zuordnung aufgehoben wurde
        et_bapiret  = lt_bapiret.                  " Tabelle mit BAPI Return Informationen

    IF lv_severity CA wmegc_severity_eax.
      ROLLBACK WORK.
      DATA(ls_error) = VALUE bapiret2( lt_bapiret[ type = lv_severity ] OPTIONAL ).
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID ls_error-id TYPE lv_severity NUMBER ls_error-number
            WITH ls_error-message_v1 ls_error-message_v2
                 ls_error-message_v3 ls_error-message_v4
            EXPORTING
              messages = lt_bapiret
           .
    ENDIF.
    COMMIT WORK AND WAIT.
    /scwm/cl_tm=>cleanup( ).

    CALL FUNCTION '/SCWM/WHO_TO_ASSIGN'
      EXPORTING
        iv_lgnum    = mv_lgnum                                   " Lagernummer/Lagerkomplex
        iv_who      = iv_to_who                                " Lagerauftragsnummer
        it_to       = VALUE /scwm/tt_who_to_assign( ( tanum = iv_tanum use_wcr_sort = abap_true ) )                 " Lagerauftrag: TAs, die einem WHO zugeordnet werden sollen
        iv_commit   = abap_true
      IMPORTING
        ev_severity = lv_severity
        et_to       = lt_to                       " TAs, für die die Zuordnung aufgehoben wurde
        et_bapiret  = lt_bapiret.                  " Tabelle mit BAPI Return Informationen
    .
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
    COMMIT WORK AND WAIT.
    /scwm/cl_tm=>cleanup( ).
  ENDMETHOD.


  METHOD zif_ws_ptwy_cart_sp~move_tote_into_cart.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Move tote into the cart and set the logical position
**********************************************************************
    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = mo_pack ).
    mo_pack->init_pack(
      EXPORTING
        iv_badi_appl  = wmegc_huappl_wme                  " Handling Unit Application
        iv_lgnum      = mv_lgnum
       EXCEPTIONS
         error         = 1                " Error, see log
         OTHERS        = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_pack->get_hu(
      EXPORTING
        iv_huident = iv_tote_id                 " Handling Unit Identification
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2
    ).
    CASE sy-subrc.
      WHEN 1.
        RAISE EXCEPTION  TYPE zcx_workstation
          MESSAGE e063(zmc_workstation) WITH iv_tote_id.
      WHEN 2.
        RAISE EXCEPTION  TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.

    zif_ws_ptwy_cart_sp~check_and_create_cart(
      EXPORTING
        iv_huident = iv_tote_id                 " Handling Unit Identification
        iv_pckg_type = zif_ws_ptwy_cart_sp=>c_pckg_type_tote
      IMPORTING
        ev_stype   = DATA(lv_stype_tote)                 " Storage Type
        ev_guid_hu = DATA(lv_guid_tote_hu)
        ev_who     = DATA(lv_who_tote)
        et_to      = DATA(lt_to_tote)
    ).

    zif_ws_ptwy_cart_sp~check_and_create_cart(
      EXPORTING
        iv_huident = iv_cart_id                 " Handling Unit Identification
        iv_pckg_type = zif_ws_ptwy_cart_sp=>c_pckg_type_cart
      IMPORTING
        ev_stype   = DATA(lv_stype_cart)                 " Storage Type
        ev_guid_hu = DATA(lv_guid_cart_hu)
        ev_who     = DATA(lv_who_cart)
    ).

    IF lv_stype_cart IS NOT INITIAL AND lv_stype_cart NE lv_stype_tote.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e058(zmc_workstation) WITH lv_stype_cart lv_stype_tote.
    ENDIF.

    zif_ws_ptwy_cart_sp~pack_hu(
      EXPORTING
        iv_source_hu = lv_guid_tote_hu                 " Unique Internal Identification of a Handling Unit
        iv_dest_hu   = lv_guid_cart_hu                 " Unique Internal Identification of a Handling Unit
        iv_logpos    = |{ iv_pos_on_cart+1(1) }{ iv_pos_on_cart+3(1) }|                 " Logical Position of Handling Unit
    ).

    IF lv_who_cart IS NOT INITIAL.
      LOOP AT lt_to_tote INTO DATA(ls_to).
        zif_ws_ptwy_cart_sp~move_task_to_order(
          EXPORTING
            iv_tanum    = ls_to-tanum                 " Warehouse Task
            iv_from_who = lv_who_tote                 " Warehouse Order Number
            iv_to_who   = lv_who_cart                 " Warehouse Order Number
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_ptwy_cart_sp~pack_and_print_deco.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Complex HU creation with putaway cart handling. The deconsolidation
*& parameters are buffered in ms_deco_params and the HU creation will
*& be triggered at the proper phase of cart/Tote creation process
**********************************************************************
    CLEAR ms_deco_params.

    mv_nltyp = is_proposed_wt-nltyp.
    ms_deco_params-is_proposed_wt = is_proposed_wt.
    ms_deco_params-iv_docid       = iv_docid      .
    ms_deco_params-iv_doccat      = iv_doccat     .
    ms_deco_params-iv_alt_uom     = iv_alt_uom    .
    ms_deco_params-iv_nof_totes   = iv_nof_totes    .

    pack_and_print_cart(
      EXPORTING
        iv_huident     = iv_huident
        iv_pmatid      = iv_pmatid
        iv_cart_id     = iv_cart_id
        iv_pos_on_cart = iv_pos_on_cart
    ).

  ENDMETHOD.


  METHOD zif_ws_ptwy_cart_sp~pack_and_print_hu.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Pack and print of deco UI with cart. First determine the cart/tote
*& and its sub objects. Then create if it does no exist. Create the
*& HU into the created putaway object and move the WT to the warehouse
*& order of the cart/tote
**********************************************************************
    DATA lv_huident_cart TYPE /scwm/huident.
    DATA lv_huident_tote TYPE /scwm/huident.
    DATA lv_huident TYPE /scwm/huident.


    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = mo_pack ).
    mo_pack->init_pack(
      EXPORTING
        iv_badi_appl  = wmegc_huappl_wme                  " Handling Unit Application
        iv_lgnum      = mv_lgnum
       EXCEPTIONS
         error         = 1                " Error, see log
         OTHERS        = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    zif_ws_ptwy_cart_sp~interpret_huident(
      EXPORTING
        iv_huident       = iv_huident                 " Handling Unit Identification
      IMPORTING
        es_cart_settings = DATA(ls_cart_settings)                 " Cart Barcode
        ev_no_cart       = DATA(lv_no_cart)
        ev_cart_id       = DATA(lv_cart_id)                       " Handling Unit Identification
        ev_pos_on_cart   = DATA(lv_pos_on_cart)                   " Handling Unit Identification
        ev_tote_id       = DATA(lv_tote_id)                       " Handling Unit Identification
        ev_comp_on_tote  = DATA(lv_comp_on_tote)                  " Handling Unit Identification
    ).

    IF lv_no_cart EQ abap_true
       OR ( lv_pos_on_cart IS INITIAL
            AND lv_tote_id IS INITIAL
            AND lv_comp_on_tote IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e056(zmc_workstation) WITH iv_huident.
    ENDIF.


    ASSERT lv_cart_id IS NOT INITIAL AND lv_pos_on_cart IS NOT INITIAL
        OR lv_tote_id IS NOT INITIAL AND lv_comp_on_tote IS NOT INITIAL
        OR lv_tote_id IS NOT INITIAL AND lv_comp_on_tote IS INITIAL
           AND iv_cart_id IS NOT INITIAL AND iv_pos_on_cart IS NOT INITIAL.

    "Cart is to be created
    IF iv_cart_id IS NOT INITIAL.
      lv_cart_id = iv_cart_id.
    ENDIF.

    IF lv_cart_id IS NOT INITIAL.
      IF iv_cart_id IS NOT INITIAL.
        lv_huident = iv_cart_id.
      ELSE.
        lv_huident = |{ zif_ws_ptwy_cart_sp=>c_pckg_type_cart }{ lv_cart_id }|.
      ENDIF.

      zif_ws_ptwy_cart_sp~check_and_create_cart(
        EXPORTING
          iv_huident = lv_huident                 " Handling Unit Identification
          iv_pckg_type = zif_ws_ptwy_cart_sp=>c_pckg_type_cart
        IMPORTING
          ev_stype   = DATA(lv_stype)                 " Storage Type
          ev_guid_hu = DATA(lv_guid_cart_hu)
          ev_who     = DATA(lv_who_cart)
      ).

      IF lv_stype IS NOT INITIAL  AND lv_stype NE is_proposed_wt-nltyp.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e058(zmc_workstation) WITH lv_stype is_proposed_wt-nltyp.
      ENDIF.
      lv_huident_cart = lv_huident.
    ENDIF.

    IF lv_pos_on_cart IS NOT INITIAL AND lv_tote_id IS INITIAL
       OR iv_pos_on_cart IS NOT INITIAL AND lv_tote_id IS NOT INITIAL.
      IF iv_pos_on_cart IS NOT INITIAL.
        lv_pos_on_cart = iv_pos_on_cart.
      ENDIF.
      mo_sp_deco->pack_and_print_hus(
        EXPORTING
          is_hu = VALUE #( huident = iv_huident pmatid = iv_pmatid )
          is_proposed_wt =  is_proposed_wt
          iv_docid   = iv_docid                 " Structure with Doc. ID
          iv_doccat  = iv_doccat
          iv_alt_uom = iv_alt_uom                 " Unit of Measure
        IMPORTING
          ev_guid_hu = DATA(lv_guid_cart_pos_hu)
          ev_tanum   = DATA(lv_tanum_cart_pos)
          ev_tapos   = DATA(lv_tapos_cart_pos)
          ev_who     = DATA(lv_who_cart_pos)
      ).

      zif_ws_ptwy_cart_sp~pack_hu( iv_dest_hu = lv_guid_cart_hu iv_source_hu = lv_guid_cart_pos_hu iv_logpos = |{ lv_pos_on_cart+1(1) }{ lv_pos_on_cart+3(1) }| ).
      IF lv_who_cart IS NOT INITIAL.
        zif_ws_ptwy_cart_sp~move_task_to_order(
          EXPORTING
            iv_tanum    = lv_tanum_cart_pos                 " Warehouse Task
            iv_from_who = lv_who_cart_pos                 " Warehouse Order Number
            iv_to_who   = lv_who_cart                 " Warehouse Order Number
        ).
      ENDIF.
    ENDIF.


    "Tote with compartment is to be created
    IF iv_pos_on_cart IS INITIAL AND lv_tote_id IS NOT INITIAL.
      lv_huident = |{ zif_ws_ptwy_cart_sp=>c_pckg_type_tote }{ lv_tote_id }|.
      zif_ws_ptwy_cart_sp~check_and_create_cart(
        EXPORTING
          iv_huident = lv_huident                 " Handling Unit Identification
          iv_pckg_type = zif_ws_ptwy_cart_sp=>c_pckg_type_tote
        IMPORTING
          ev_stype   = lv_stype                 " Storage Type
          ev_guid_hu = DATA(lv_guid_tote_hu)
          ev_who     = DATA(lv_who_tote)
      ).

      IF lv_stype IS NOT INITIAL  AND lv_stype NE is_proposed_wt-nltyp.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e058(zmc_workstation) WITH lv_stype is_proposed_wt-nltyp.
      ENDIF.
      lv_huident_tote = lv_huident.
    ENDIF.

    IF lv_comp_on_tote IS NOT INITIAL.
      mo_sp_deco->pack_and_print_hus(
        EXPORTING
          is_hu = VALUE #( huident = iv_huident pmatid = iv_pmatid )
          is_proposed_wt =  is_proposed_wt
          iv_docid   = iv_docid                 " Structure with Doc. ID
          iv_doccat  = iv_doccat
          iv_alt_uom = iv_alt_uom                 " Unit of Measure
        IMPORTING
          ev_guid_hu = DATA(lv_guid_compartment_hu)
          ev_tanum   = DATA(lv_tanum_compartment)
          ev_tapos   = DATA(lv_tapos_compartment)
          ev_who     = DATA(lv_who_compartment)
      ).
      zif_ws_ptwy_cart_sp~pack_hu( iv_dest_hu = lv_guid_tote_hu iv_source_hu = lv_guid_compartment_hu iv_logpos = CONV #( lv_comp_on_tote ) ).
      IF lv_who_tote IS NOT INITIAL.
        zif_ws_ptwy_cart_sp~move_task_to_order(
          EXPORTING
            iv_tanum    = lv_tanum_compartment                 " Warehouse Task
            iv_from_who = lv_who_compartment                 " Warehouse Order Number
            iv_to_who   = lv_who_tote                 " Warehouse Order Number
        ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ws_ptwy_cart_sp~pack_and_print_repl.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& Complex HU creation with putaway cart handling. The replenishment
*& parameters are buffered in ms_repl_params and the HU creation will
*& be triggered at the proper phase of cart/Tote creation process
**********************************************************************
    mv_nltyp = is_wt_cur-nltyp. "RM 11.08.2023
    ms_repl_params-iv_tanum       = iv_tanum      .
    ms_repl_params-is_confirm     = is_confirm    .
    ms_repl_params-is_conf_exc    = is_conf_exc   .
    ms_repl_params-iv_huident     = iv_huident    .
    ms_repl_params-iv_anfme       = iv_anfme      .
    ms_repl_params-iv_altme       = iv_altme      .
    ms_repl_params-is_wt_cur      = is_wt_cur     .

    pack_and_print_cart(
      EXPORTING
        iv_huident     = iv_huident
        iv_pmatid      = iv_pmatid
        iv_cart_id     = iv_cart_id
        iv_pos_on_cart = iv_pos_on_cart
    ).

  ENDMETHOD.


  METHOD zif_ws_ptwy_cart_sp~pack_hu.
**********************************************************************
*& Key           : LH-161023
*& Request No.   : GAP-19 – “Build a Put-Away Cart”
**********************************************************************
*& Description (short)
*& PAck source HU into destiantion HU and set the logical position in the header
**********************************************************************
    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = mo_pack ).
    mo_pack->init_pack(
      EXPORTING
        iv_badi_appl  = 'WME'                  " Handling Unit Application
        iv_lgnum      = mv_lgnum
        it_guid_hu    = VALUE #( ( guid_hu = iv_source_hu ) ( guid_hu = iv_dest_hu ) )
       EXCEPTIONS
         error         = 1                " Error, see log
         OTHERS        = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    mo_pack->pack_hu(
      EXPORTING
        iv_source_hu = iv_source_hu                 " Unique Internal Identification of a Handling Unit
        iv_dest_hu   = iv_dest_hu                 " Unique Internal Identification of a Handling Unit
      EXCEPTIONS
        error        = 1                " Error, see log
        OTHERS       = 2
    ).
    IF sy-subrc <> 0.
      IF iv_test_only EQ abap_false.
        ROLLBACK WORK.
        /scwm/cl_tm=>cleanup( ).
        /scwm/cl_tm=>set_lgnum( mv_lgnum ).
      ENDIF.
      RAISE EXCEPTION TYPE zcx_workstation
         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF iv_test_only EQ abap_false.
      mo_pack->save(
        EXPORTING
          iv_commit = 'X'
          iv_wait   = 'X'
        EXCEPTIONS
          error     = 1                " See Log
          OTHERS    = 2
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_workstation
           MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      /scwm/cl_tm=>cleanup( ).
      /scwm/cl_tm=>set_lgnum( mv_lgnum ).
      RETURN.
    ENDIF.
    IF iv_logpos IS NOT INITIAL.
      mo_pack->/scwm/if_pack_bas~get_hu(
        EXPORTING
          iv_guid_hu = iv_source_hu                  " Unique Internal Identification of a Handling Unit
        IMPORTING
          es_huhdr   = DATA(ls_source_hdr)                 " Internal Structure for Processing the HU Header
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_workstation
           MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      ls_source_hdr-logpos = iv_logpos.
      mo_pack->change_huhdr(
        EXPORTING
          is_huhdr   = ls_source_hdr                 " Internal Structure for Processing the HU Header
        IMPORTING
          es_huhdr   = DATA(ls_source_chg)                 " Internal Structure for Processing the HU Header
        EXCEPTIONS
          error      = 1
          not_locked = 2
          OTHERS     = 3
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_workstation
         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      mo_pack->save(
        EXPORTING
          iv_commit = 'X'
          iv_wait   = 'X'
        EXCEPTIONS
          error     = 1                " See Log
          OTHERS    = 2
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_workstation
           MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
