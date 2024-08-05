CLASS zcl_ws_deco_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_matnr_entitled,
             matnr    TYPE matnr,
             entitled TYPE /scwm/de_entitled,
           END OF ty_matnr_entitled,
           tt_matnr_entitled TYPE STANDARD TABLE OF ty_matnr_entitled WITH EMPTY KEY.

    INTERFACES zif_ws_ui_defaults .
    INTERFACES zif_ws_deco_ui .
    INTERFACES zif_ws_subscr_ui .

    CONSTANTS c_idart_inb_ship TYPE /scwm/de_huidart VALUE 'I' ##NO_TEXT.
    CONSTANTS c_lock_mode_exclusive TYPE enqmode VALUE 'E' ##NO_TEXT.
    CONSTANTS c_lock_mode_shared TYPE enqmode VALUE 'S' ##NO_TEXT.
    CONSTANTS c_lock_owner_dialog TYPE char01 VALUE '1' ##NO_TEXT.
    CONSTANTS c_main_status TYPE string VALUE 'MAIN_SCREEN' ##NO_TEXT.
    CONSTANTS c_main_tittle TYPE string VALUE 'MAIN_TITLE' ##NO_TEXT.
    CONSTANTS c_subscreen_repid TYPE syrepid VALUE 'SAPLZFG_WORKSTATION_UI' ##NO_TEXT.
    CONSTANTS c_sub_deco_pallets TYPE sy-dynnr VALUE '3000' ##NO_TEXT.
    CONSTANTS c_tab_deco_pallets TYPE scxtab_tabstrip-activetab VALUE 'DECO_PALLETS' ##NO_TEXT.
    CLASS-DATA so_active_ui TYPE REF TO zif_ws_deco_ui READ-ONLY .

    CLASS-METHODS start .
    METHODS constructor .
protected section.

  data MO_SP type ref to ZIF_WS_DECO_SP .
  data MS_DEFAULTS type ZSTR_WS_DEFAULTS .
private section.

  types:
    BEGIN OF ty_curr_stock_situ,
        lgtyp   TYPE /scwm/lgtyp,
        no_bins TYPE /scwm/de_no_bins,
        quan    TYPE /scwm/de_ui_quan,
      END OF ty_curr_stock_situ .
  types:
    BEGIN OF ty_proposed_wt.
        INCLUDE TYPE zstr_ws_proposed_wt AS wt_group_data.
    TYPES:
        wts                  TYPE STANDARD TABLE OF /scwm/ltap WITH DEFAULT KEY,
        hus                  TYPE STANDARD TABLE OF zif_ws_deco_ui~ty_hu_to_be_created WITH DEFAULT KEY,
        lock_stbin_x         TYPE /scwm/tt_aquay_int,
        lock_stbin_y         TYPE STANDARD TABLE OF /scwm/aquay WITH DEFAULT KEY,
        newly_inserted       TYPE abap_bool,
        modified_in_proposal TYPE abap_bool,
        putaway_cart_rel     TYPE abap_bool,
        qty_per_wt_b         TYPE zde_qty_per_wt,
        qty_per_wt_uom_b     TYPE /scdl/dl_uom,
      END OF ty_proposed_wt .

  constants C_FUNC_CHANGE_HUS type SYUCOMM value 'CHANGE_HU' ##NO_TEXT.
  constants C_FUNC_CHANGE_PROPOSAL type SYUCOMM value 'CHANGE_PRP' ##NO_TEXT.
  constants C_FUNC_CREATE_PROPOSAL type SYUCOMM value 'CREATE_PRO' ##NO_TEXT.
  constants C_FUNC_DEFAULTS type SYUCOMM value 'DEFAULTS' ##NO_TEXT.
  constants C_FUNC_DISPLAY_HUS type SYUCOMM value 'DISPLAY_HU' ##NO_TEXT.
  constants C_FUNC_DISPLAY_LOG type SYUCOMM value 'DISPLAYLOG' ##NO_TEXT.
  constants C_FUNC_FX_CHANGE_DELIV type SYUCOMM value 'CHANGE_DEL' ##NO_TEXT.
  constants C_FUNC_FX_CHANGE_PROD type SYUCOMM value 'CHANGE_PRO' ##NO_TEXT.
  constants C_FUNC_FX_DELETE_HU type SYUCOMM value 'DELETE_HU' ##NO_TEXT.
  constants C_FUNC_LEAVE type SYUCOMM value 'LEAVE' ##NO_TEXT.
  constants C_FUNC_NEXT_WT type SYUCOMM value 'NEXT_WT' ##NO_TEXT.
  constants C_FUNC_PACK_PRINT_HU type SYUCOMM value 'PCK_PRNTHU' ##NO_TEXT.
  constants C_FUNC_PREVIOUS_WT type SYUCOMM value 'PREVIOUS_W' ##NO_TEXT.
  constants C_FUNC_RESET_PROPOSAL type SYUCOMM value 'RESET_PROP' ##NO_TEXT.
  constants C_FUNC_SAVE_EXPIRE_DATE type SYUCOMM value 'SAVE_EXP' ##NO_TEXT.
  constants C_FUNC_WT_LIST type SYUCOMM value 'WT_LIST' ##NO_TEXT.
  constants C_PLANNED_PTWY_QTY_CAT type /SCDL/DL_ADDMEAS_STR-QTY_CATEGORY value 'OPEN' ##NO_TEXT.
  constants C_PLANNED_PTWY_QTY_ROLE type /SCDL/DL_ADDMEAS_STR-QTY_ROLE value 'PA' ##NO_TEXT.
  class-data:
    st_st_bin_type_txt TYPE SORTED TABLE OF /scwm/t303t WITH UNIQUE KEY lptyp .
  class-data:
    st_st_type_txt TYPE SORTED TABLE OF /scwm/t301t WITH UNIQUE KEY lgtyp .
  data MO_ALV_CURR_STOCK_SIT type ref to CL_SALV_TABLE .
  data MO_CTRL_CURR_STOCK_SIT type ref to CL_GUI_CUSTOM_CONTAINER .
  data MO_FUNCTION_LOG type ref to /SCWM/CL_LOG .
  data MO_GENERAL_FUNC type ref to ZCL_GENERAL_UI_FUNCTIONS .
  data MO_PTWY_CART_SP type ref to ZIF_WS_PTWY_CART_SP .
  data MS_DECO_PALLETS type ZSTR_WS_DECON_PALLETS .
  data MS_DECO_PALLETS_PREV type ZSTR_WS_DECON_PALLETS  ##NEEDED.
  data MS_DELIVERY_ITEM type /SCWM/DLV_ITEM_OUT_PRD_STR .
  data MS_MAT_GLOBAL type /SCWM/S_MATERIAL_GLOBAL .
  data MS_MAT_LGNUM type /SCWM/S_MATERIAL_LGNUM .
  data MS_MAT_LGTYP type /SCWM/S_MATERIAL_LGTYP .
  data:
    mt_curr_stock_sit TYPE STANDARD TABLE OF ty_curr_stock_situ .
  data MT_DELIVERY_ITEM type /SCWM/DLV_ITEM_OUT_PRD_TAB .
  data MT_EXCLUDES type STRING_TABLE .
  data MT_INB_DELIVERY type /SCDL/DL_ID_TAB .
  data:
    mt_proposed_wt TYPE STANDARD TABLE OF ty_proposed_wt .
  data MV_BALLOGHNDL type BALLOGHNDL .
  data MV_CURRENT_TAB_SCREEN type SY-DYNNR  ##NEEDED.
  data MV_HU_UNKNOWN type FLAG .
  data MV_INTERNAL_HU_NR type ABAP_BOOL .
  data MV_LAST_MSG type STRING  ##NEEDED.
  data MV_LOCKED_DOCID type /SCDL/DL_DOCID .
  data MV_LOCKED_ITEMID type /SCDL/DL_ITEMID .
  data MV_MD_OK type ABAP_BOOL .
  data MV_MONO_PALLET type ABAP_BOOL .
  data MV_PMAT_CAN_BE_CHANGED type ABAP_BOOL .
  data MV_WT_GRP_INDEX type I .
  data MV_DISPL_PC_PER_TOT type ABAP_BOOL .
  data MV_ENTER_PRESSED type ABAP_BOOL .
  data MO_TEXT_HANDLING type ref to ZIF_TEXT_HANDLING .
  data MS_CURRENT_DLV_HEADER type /SCWM/DLV_HEADER_OUT_PRD_STR .

  methods ADD_EXCEPTION_TO_FL
    importing
      !IO_EXC type ref to CX_ROOT .
  methods CHANGE_DELIVERY_DATA
    raising
      ZCX_WORKSTATION .
  methods CHANGE_DELIVERY_ITEM
    raising
      ZCX_WORKSTATION .
  methods CHANGE_HUS .
  methods CHANGE_PROPOSAL
    raising
      ZCX_WORKSTATION .
  methods CHECK_PRODUCT .
  methods CLEAR_DATA_WHEN_HU_EMPTY .
  methods CLEAR_DATA_WHEN_PROD_EMPTY .
  methods CLEAR_PMAT_DEP_DATA .
  methods CREATE_PROPOSAL
    raising
      ZCX_WORKSTATION .
  methods DELETE_HU
    raising
      ZCX_WORKSTATION .
  methods DELIVERY_ITEM_CHANGED
    raising
      ZCX_WORKSTATION .
  methods DEST_HU_CHANGED
    importing
      !IV_DEST_HU type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION .
  methods DISPLAY_HUS .
  methods DISPLAY_LOG .
  methods DISPLAY_PRODUCT
    raising
      ZCX_WORKSTATION .
  methods DISPLAY_WT_GRP
    raising
      ZCX_WORKSTATION .
  methods DO_CREATE_PROPOSAL
    importing
      !IV_NLTYP type /SCWM/LTAP_NLTYP optional
      !IV_PUTAWAY_PER_WTS_QTY type ZDE_QTY_PUTAWAY optional
      !IV_PUTAWAY_PER_WTS_UOM type /SCDL/DL_UOM optional
      !IV_NOF_WTS type I optional
      !IV_CHANGE_PROPOSAL type ABAP_BOOL optional
      !IT_LTAP type /SCWM/TT_LTAP_VB optional
    raising
      ZCX_WORKSTATION .
  methods DO_RESET_PROPOSALS
    importing
      !IV_KEEP_PUTAWAY_QUANT type ABAP_BOOL optional .
  methods FILL_PRODUCT_INFORMATION
    raising
      ZCX_WORKSTATION .
  methods FILL_PUOM_WH_REL_DATA
    raising
      ZCX_WORKSTATION .
  methods FINISH_FUNCTION_LOG
    importing
      !IV_EXTERNAL_ID type STRING
      !IV_SAVE_ONLY_IF_ERROR type ABAP_BOOL
      !IV_DISPLAY type ABAP_BOOL
    exporting
      value(EV_DISPLAYED) type ABAP_BOOL .
  methods GET_DELIVERY_ITEMS
    importing
      !IV_MATNR type MATNR
    raising
      ZCX_WORKSTATION .
  methods GET_OPEN_PUTAWAY .
  methods GET_PACKING_INSTR
    importing
      !IT_MATNR_ENTITLED type TT_MATNR_ENTITLED optional
    returning
      value(RV_PACK_INSTR) type STRING .
  methods GR_DUMMY_HU_CHANGED
    importing
      !IV_GR_DUMMY_HU type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION .
  methods LOCK_DELIVERY_ITEM
    raising
      ZCX_WORKSTATION .
  methods LOCK_STOCK
    importing
      !IV_WT_GROUP type I optional .
  methods NEXT_WT
    raising
      ZCX_WORKSTATION .
  methods PACKAGING_MAT_CHANGED
    importing
      !IV_PMAT type /SCWM/DE_PMAT
    raising
      ZCX_WORKSTATION .
  methods PACK_PRINT_HU
    raising
      ZCX_WORKSTATION .
  methods PREV_WT
    raising
      ZCX_WORKSTATION .
  methods PRODUCT_CHANGED
    importing
      !IV_PRODUCT_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    raising
      ZCX_WORKSTATION .
  methods PUOM_WH_CHANGED
    importing
      !IV_UOM type /SCWM/DE_BASE_UOM
    raising
      ZCX_WORKSTATION .
  methods REFRESH_CURRENT_DLV_DATA
    raising
      ZCX_WORKSTATION .
  methods RESET_PROPOSALS .
  methods START_FUNCTION_LOG .
  methods UNLOCK_DELIVERY_ITEM .
  methods UNLOCK_STOCK
    importing
      !IV_WT_GROUP type I optional .
  methods WAREHOUSE_CHANGED .
  methods WHICH_AREA_IS_FILLED
    exporting
      !EV_DUMMY_HU_PROD_DELIVERY type ABAP_BOOL
      !EV_PUTAWAY_INPUT type ABAP_BOOL
      !EV_PROPOSAL type ABAP_BOOL
      !EV_HU type ABAP_BOOL
      !EV_ANY_OF_THEM type ABAP_BOOL .
  methods WT_LIST .
ENDCLASS.



CLASS ZCL_WS_DECO_UI IMPLEMENTATION.


  METHOD add_exception_to_fl.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Add exception to function log
**********************************************************************
    CHECK mo_function_log IS BOUND.
    IF io_exc IS INSTANCE OF if_t100_message.
      TRY.
          cl_message_helper=>set_msg_vars_for_if_t100_msg(
            EXPORTING
              text = CAST #( io_exc ) ).
        CATCH cx_sy_message_illegal_text.
          DATA(lv_text_100) = io_exc->get_text( ).
          mo_function_log->add_message(
            EXPORTING
              ip_msgty = /scwm/cl_log=>msgty_error
              ip_msg   = CONV #( lv_text_100 )
          ).
          RETURN.
      ENDTRY.
      mo_function_log->add_message( ).
    ELSE.
      DATA(lv_text) = io_exc->get_text( ).
      mo_function_log->add_message(
        EXPORTING
          ip_msgty = /scwm/cl_log=>msgty_error
          ip_msg   = CONV #( lv_text )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD change_delivery_data.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Change Delivery item best before/expiration date
**********************************************************************
    start_function_log( ).
    TRY.
        mo_sp->change_delivery(
          EXPORTING
            iv_bestbefore_date = ms_deco_pallets-expiration_date
          CHANGING
            cs_delivery_item   = ms_delivery_item
        ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        DATA(lv_error) = abap_true.
        mo_function_log->add_log( it_prot = lx_ws->messages ).
      CATCH /scwm/cx_sp INTO DATA(lx_sp).
        lv_error = abap_true.
        add_exception_to_fl( lx_sp ).
    ENDTRY.
    IF lv_error EQ abap_true.
      finish_function_log(
        EXPORTING
          iv_external_id        = 'Change Delivery Data'  ##no_text
          iv_save_only_if_error = abap_true
          iv_display            = abap_true
        IMPORTING
          ev_displayed          = DATA(lv_displayed)
      ).

      IF lv_displayed EQ abap_false.
        RAISE EXCEPTION lx_ws.
      ENDIF.
    ELSE.
      MESSAGE i017(zmc_workstation) WITH ms_delivery_item-docno ms_delivery_item-itemno.
    ENDIF.
  ENDMETHOD.


  METHOD change_delivery_item.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Change the active delivery item and dependent data
**********************************************************************
    get_delivery_items( ms_deco_pallets-matnr ).
    DATA(lt_dlv_item) = mt_delivery_item.
    CALL FUNCTION 'Z_INB_GET_DELIVERY_ITEM'
      CHANGING
        ct_delivery_item = lt_dlv_item.
    IF lt_dlv_item IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_delivery_item) = lt_dlv_item[ 1 ].
    IF ms_delivery_item-docid EQ ls_delivery_item-docid
       AND ms_delivery_item-itemid EQ ls_delivery_item-itemid.
      RETURN.
    ENDIF.
    unlock_delivery_item( ).
    ms_delivery_item = ls_delivery_item.

    delivery_item_changed( ).

  ENDMETHOD.


  METHOD change_hus.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& If change HU is pressed then clear the dependent data
**********************************************************************
    CLEAR ms_deco_pallets-dest_hu.
    IF mt_proposed_wt[ mv_wt_grp_index ]-putaway_cart_rel EQ abap_true.
      clear_pmat_dep_data( ).
    ENDIF.
  ENDMETHOD.


  METHOD change_proposal.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Change proposal. Call the popup for new proposal request,
*& then refresh the proposal dependent data
**********************************************************************
    DATA:
      lv_cancelled      TYPE  flag,
      lv_nof_wts        TYPE  zde_number_of_wts,
      lv_qty_per_wt     TYPE  zde_qty_per_wt,
      lv_nltyp          TYPE  /scwm/ltap_nltyp,
      lv_qty_per_wt_uom TYPE meins.

    ms_deco_pallets-proposed_wt-change_stype_mixed = xsdbool( mv_mono_pallet EQ abap_false ).
    ms_deco_pallets-proposed_wt-change_stype_mono = mv_mono_pallet.
    DO.
      CALL FUNCTION 'Z_INB_CHANGE_PROPOSED_WT'
        EXPORTING
          is_proposed_wt    = ms_deco_pallets-proposed_wt
        IMPORTING
          ev_cancelled      = lv_cancelled
          ev_nof_wts        = lv_nof_wts
          ev_qty_per_wt     = lv_qty_per_wt
          ev_nltyp          = lv_nltyp
          ev_qty_per_wt_uom = lv_qty_per_wt_uom.
      IF lv_cancelled EQ abap_true.
        RETURN.
      ENDIF.


      IF lv_nof_wts EQ ms_deco_pallets-nof_wts
         AND lv_qty_per_wt EQ ms_deco_pallets-qty_per_wt
         AND lv_nltyp EQ ms_deco_pallets-nltyp
        OR
         lv_nof_wts IS INITIAL
         AND lv_qty_per_wt IS INITIAL
         AND lv_nltyp IS INITIAL.

        MESSAGE i026(zmc_workstation).
        RETURN.
      ENDIF.

      "-----------------------------------------
      "Only target storage type has been changed

      start_function_log( ).
      "Remove and buffer the original proposal
      DATA(ls_prop) = mt_proposed_wt[ mv_wt_grp_index ].
      DATA(lv_grp_idx) = mv_wt_grp_index.
      unlock_stock( lv_grp_idx ).
      DELETE mt_proposed_wt INDEX mv_wt_grp_index.

      TRY.
          zcl_bin_determination_ctrl=>sv_force_empty_bin = abap_true.
          unlock_delivery_item( ).
          do_create_proposal(
              iv_nltyp               = lv_nltyp                 " Destination Storage Type
              iv_putaway_per_wts_qty = lv_qty_per_wt                 " Quantity for Put-away
              iv_putaway_per_wts_uom = lv_qty_per_wt_uom                 " Base Unit of Measure
              iv_nof_wts             = lv_nof_wts
              iv_change_proposal     = abap_true
              it_ltap                = ls_prop-wts                 " Table Type: Warehouse Tasks Internal
          ).
          zcl_bin_determination_ctrl=>sv_force_empty_bin = abap_false.
          "Remove locks and create new locks
          unlock_stock( ).
          lock_stock( ).
          "Find the first modified group
          LOOP AT mt_proposed_wt TRANSPORTING NO FIELDS
               WHERE newly_inserted EQ abap_true
                  OR modified_in_proposal EQ abap_true.
            mv_wt_grp_index = sy-tabix.
            EXIT.
          ENDLOOP.

        CATCH zcx_workstation INTO DATA(lx_ws).
          "Proposal was not succesful, put back the original one
          INSERT ls_prop INTO mt_proposed_wt INDEX lv_grp_idx.
          mv_wt_grp_index = lv_grp_idx.
          DATA(lv_error) = abap_true.
          mo_function_log->add_log( it_prot = lx_ws->messages ).
      ENDTRY.
      finish_function_log(
        EXPORTING
          iv_external_id        = 'Create Proposal' ##no_text
          iv_save_only_if_error = abap_false
          iv_display            = abap_true
        IMPORTING
          ev_displayed          = DATA(lv_displayed)
      ).
      IF lv_displayed EQ abap_false AND lx_ws IS BOUND.
        MESSAGE lx_ws TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.

      IF lv_error EQ abap_true.
        RETURN.
      ELSE.
        MESSAGE i018(zmc_workstation).
        EXIT.
      ENDIF.
    ENDDO.

    display_wt_grp( ).
  ENDMETHOD.


  METHOD check_product.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Check product master data
**********************************************************************
    NEW zcl_check_master_data( )->check_product(
      EXPORTING
        iv_matnr     = ms_deco_pallets-matnr                  " Material Number
        iv_warehouse = ms_deco_pallets-lgnum                 " Warehouse Number/Warehouse Complex
        iv_entitled  = ms_delivery_item-sapext-entitled
      RECEIVING
        rv_ok        = mv_md_ok
    ).

  ENDMETHOD.


  METHOD clear_data_when_hu_empty.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& clear HU dependent data
**********************************************************************
    CLEAR: ms_deco_pallets, mv_hu_unknown.
    clear_data_when_prod_empty( ).
  ENDMETHOD.


  METHOD clear_data_when_prod_empty.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Clear product dependent data
**********************************************************************
    DATA(ls_deco) = ms_deco_pallets.
    CLEAR: ms_deco_pallets, ms_delivery_item, ms_mat_global.
    ms_deco_pallets-gr_dummy_hu = ls_deco-gr_dummy_hu.
    ms_deco_pallets-inbound_shipment = ls_deco-inbound_shipment.
    CLEAR: mt_curr_stock_sit, mt_delivery_item, mt_inb_delivery.
    do_reset_proposals( ).
    IF mo_alv_curr_stock_sit IS BOUND.
      mo_alv_curr_stock_sit->refresh( ).
    ENDIF.
  ENDMETHOD.


  METHOD clear_pmat_dep_data.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Clear packing material dependent screen and control  data
**********************************************************************
    CLEAR:
      ms_deco_pallets-pmat,
      ms_deco_pallets-pmat_maktx,
      ms_deco_pallets-hu_type,
      ms_deco_pallets-hu_type_text,
      ms_deco_pallets-pmatid,
      ms_deco_pallets-nr_internal,
      ms_deco_pallets-cart_id,
      ms_deco_pallets-pos_on_cart,
      ms_deco_pallets-tote_id,
      ms_deco_pallets-comp_on_tote.
  ENDMETHOD.


  METHOD constructor.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Create instance
**********************************************************************
    super->constructor( ).
    mo_sp = NEW zcl_ws_deco_sp( ).
    so_active_ui = me.
    mo_general_func = NEW #( mo_sp ).

  ENDMETHOD.


  METHOD create_proposal.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Create proposal. IF there are already proposals, then confirm the change
**********************************************************************
    DATA: lv_answer TYPE flag.

    mo_ptwy_cart_sp = zcl_ws_ptwy_cart_sp=>create_instance( iv_lgnum = ms_defaults-lgnum iv_stor_bin_lgpla = ms_defaults-workst_loc ).

    IF ms_deco_pallets-putaway_qty EQ 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e036(zmc_workstation).
    ENDIF.
    IF ms_mat_lgnum-zz1_keepcar_whd EQ abap_true AND ms_deco_pallets-puom_wh IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e073(zmc_workstation).
    ENDIF.
    IF mt_proposed_wt IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Create Proposals'(crp)
          text_question         = 'Creation of new proposals and deletion of already existing ones?'(cde)                 " Question text in dialog box
          text_button_1         = 'Continue'(con)         " Text on the first pushbutton
          text_button_2         = 'Cancel'(can)         " Text on the second pushbutton
          display_cancel_button = abap_false              " Button for displaying cancel pushbutton
        IMPORTING
          answer                = lv_answer                " Return values: '1', '2', 'A'
        EXCEPTIONS
          OTHERS                = 0.
      IF lv_answer NE '1'.
        RETURN.
      ENDIF.
    ENDIF.

    start_function_log( ).

    do_reset_proposals( iv_keep_putaway_quant = abap_true ).
    TRY.
        zcl_bin_determination_ctrl=>sv_force_empty_bin = abap_false.
        do_create_proposal( ).

      CATCH zcx_workstation INTO DATA(lx_ws).
        DATA(lv_error) = abap_true.
        mo_function_log->add_log( it_prot = lx_ws->messages ).
    ENDTRY.
    IF mt_proposed_wt IS INITIAL.
      TRY.
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e075(zmc_workstation).
        CATCH zcx_workstation INTO lx_ws.
          lv_error = abap_true.
          mo_function_log->add_log( it_prot = lx_ws->messages ).
      ENDTRY.
    ENDIF.
    finish_function_log(
      EXPORTING
        iv_external_id        = 'Create Proposal' ##no_text
        iv_save_only_if_error = abap_false
        iv_display            = abap_true
      IMPORTING
        ev_displayed          = DATA(lv_displayed)
    ).
    IF lv_displayed EQ abap_false AND lx_ws IS BOUND.
      RAISE EXCEPTION lx_ws.
    ENDIF.
    IF lv_error EQ abap_true.
      RETURN.
    ELSE.
      MESSAGE i018(zmc_workstation).
    ENDIF.

    mv_wt_grp_index = 1.


    display_wt_grp( ).

  ENDMETHOD.


  METHOD delete_hu.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Delete HU
**********************************************************************
    DATA: lt_huident     TYPE zif_ws_deco_ui=>tt_hu_ident,
          lv_nof_hus     TYPE i,
          lv_answer      TYPE flag.

    start_function_log( ).


    IF ms_deco_pallets-product_ean_mpn IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Delete HUs'(dhu)
          text_question         = 'Not entire content of this HU has been processed.'(nen)                 " Question text in dialog box
          text_button_1         = 'Continue'(con)         " Text on the first pushbutton
          text_button_2         = 'Cancel'(can)         " Text on the second pushbutton
          display_cancel_button = abap_false              " Button for displaying cancel pushbutton
        IMPORTING
          answer                = lv_answer                " Return values: '1', '2', 'A'
        EXCEPTIONS
          OTHERS                = 0.
      IF lv_answer NE '1'.
        RETURN.
      ENDIF.
    ENDIF.

    IF mo_sp->is_workstation_mixedpallet( ).
      IF ms_deco_pallets-gr_dummy_hu IS INITIAL.
        MESSAGE w031(zmc_workstation).
        RETURN.
      ENDIF.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Delete HUs'(dhu)
          text_question         = 'The HU will be deleted'(deu)                 " Question text in dialog box
          text_button_1         = 'Continue'(con)         " Text on the first pushbutton
          text_button_2         = 'Cancel'(can)         " Text on the second pushbutton
          display_cancel_button = abap_false              " Button for displaying cancel pushbutton
        IMPORTING
          answer                = lv_answer                " Return values: '1', '2', 'A'
        EXCEPTIONS
          OTHERS                = 0.
      IF lv_answer NE '1'.
        RETURN.
      ENDIF.
      APPEND ms_deco_pallets-gr_dummy_hu TO lt_huident.
    ELSE.
      CALL FUNCTION 'Z_INB_DELETE_HUS'
        IMPORTING
          et_huident = lt_huident.
    ENDIF.

    LOOP AT lt_huident INTO DATA(lv_huident).
      TRY.
          mo_sp->delete_hu( |{ lv_huident ALPHA = IN }|  ).
          ADD 1 TO lv_nof_hus.
        CATCH zcx_workstation INTO DATA(lx_ws).
          add_exception_to_fl( lx_ws ).
          MESSAGE e014(zmc_workstation) WITH lv_huident INTO mv_last_msg.
          mo_function_log->add_message( ).

          DATA(lv_error) = abap_true.
          DELETE lt_huident.
      ENDTRY.
    ENDLOOP.
    IF lv_nof_hus NE 0.
      MESSAGE s013(zmc_workstation) WITH lv_nof_hus INTO mv_last_msg.
      mo_function_log->add_message( ).
      IF line_exists( lt_huident[ table_line = ms_deco_pallets-gr_dummy_hu ] ).
        CLEAR ms_deco_pallets-gr_dummy_hu.
        which_area_is_filled( IMPORTING ev_putaway_input = DATA(lv_putaway_input_filled) ).
        IF lv_putaway_input_filled EQ abap_true.
          mv_hu_unknown = abap_true.
          ms_deco_pallets-gr_dummy_hu = 'unknown'(unw).
        ELSE.
          clear_data_when_hu_empty( ).
        ENDIF.

      ENDIF.
    ENDIF.

    IF lv_error EQ abap_true.
      finish_function_log(
        EXPORTING
          iv_external_id        = 'DELETE_HU'
          iv_save_only_if_error = abap_true
          iv_display            = abap_true
        IMPORTING
          ev_displayed          = DATA(lv_displayed) ).

      IF lv_displayed EQ abap_false.
        RAISE EXCEPTION lx_ws.
      ENDIF.
    ELSE.
      MESSAGE s013(zmc_workstation) WITH lv_nof_hus.
    ENDIF.
  ENDMETHOD.


  METHOD delivery_item_changed.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Fill delivery item dependent data
**********************************************************************
    ms_deco_pallets-ewm_inb_del_item = condense( |{ ms_delivery_item-docno ALPHA = OUT }/{ ms_delivery_item-itemno ALPHA = OUT }| ).
    CONDENSE ms_deco_pallets-ewm_inb_del_item NO-GAPS.

    CLEAR ms_deco_pallets-avlgrp.
    SELECT FROM /scwm/tmapstloc
          INNER JOIN kna1
             ON /scwm/tmapstloc~plant EQ kna1~werks
          FIELDS /scwm/tmapstloc~avlgrp
          WHERE kna1~kunnr EQ @ms_delivery_item-sapext-entitled
          INTO TABLE @DATA(lt_avlgrp). "#EC CI_BUFFJOIN
    IF sy-subrc EQ 0.
      ms_deco_pallets-avlgrp = lt_avlgrp[ 1 ].
    ENDIF.

    do_reset_proposals( ).

    get_open_putaway( ).
    IF ms_delivery_item-sapext-bestbefore_date-tstfr IS NOT INITIAL.
      CONVERT TIME STAMP ms_delivery_item-sapext-bestbefore_date-tstfr
              TIME ZONE  ms_delivery_item-sapext-bestbefore_date-tzone
              INTO DATE ms_deco_pallets-expiration_date.
    ELSE.
      CLEAR ms_deco_pallets-expiration_date.
    ENDIF.

    DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).
    TRY.
        lo_dlv->query(
          EXPORTING
            it_docid        = VALUE #(  ( doccat = wmegc_doccat_pdi  docid = ms_delivery_item-docid ) )
            is_read_options = VALUE #(  )
            is_exclude_data = VALUE #( item_all = abap_true  )
            is_include_data = VALUE #( head_partyloc = abap_true  )
          IMPORTING
            et_headers        = DATA(lt_dlv_header)
        ).
        READ TABLE lt_dlv_header INTO ms_current_dlv_header INDEX 1.
      CATCH /scdl/cx_delivery INTO DATA(lo_cx) ##no_handler ##needed.
    ENDTRY.
  ENDMETHOD.


  METHOD dest_hu_changed.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Fill destination HU dependent data.
**********************************************************************
    READ TABLE mt_proposed_wt INDEX mv_wt_grp_index REFERENCE INTO DATA(lr_prop_wt).

    IF iv_dest_hu IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR lr_prop_wt->hus.
    "Check against putaway cart logic
    IF ms_deco_pallets-no_pack_settings EQ abap_true.
      mo_ptwy_cart_sp->interpret_huident(
        EXPORTING
          iv_huident       = iv_dest_hu                 " Handling Unit Identification
        IMPORTING
          es_cart_settings = DATA(ls_cart_settings)                 " Cart Barcode
          ev_no_cart       = DATA(lv_no_cart)
          ev_cart_id       = ms_deco_pallets-cart_id                      " Handling Unit Identification
          ev_pos_on_cart   = ms_deco_pallets-pos_on_cart                  " Handling Unit Identification
          ev_tote_id       = ms_deco_pallets-tote_id                      " Handling Unit Identification
          ev_comp_on_tote  = ms_deco_pallets-comp_on_tote                  " Handling Unit Identification
      ) ##needed.
      IF lr_prop_wt->putaway_cart_rel EQ abap_true AND ls_cart_settings-pmat IS NOT INITIAL.
        IF ms_deco_pallets-pos_on_cart IS INITIAL
           AND ms_deco_pallets-tote_id IS INITIAL
           AND ms_deco_pallets-comp_on_tote IS INITIAL.
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e055(zmc_workstation) WITH iv_dest_hu.
        ENDIF.
        IF mo_sp->hu_exists( iv_dest_hu ).
          clear_pmat_dep_data( ).
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e065(zmc_workstation) WITH iv_dest_hu.
        ENDIF.
        packaging_mat_changed( iv_pmat = ls_cart_settings-pmat ).
      ENDIF.
    ENDIF.

    ms_deco_pallets-dest_hu = iv_dest_hu.
    APPEND INITIAL LINE TO lr_prop_wt->hus REFERENCE INTO DATA(lr_new_hu).
    lr_new_hu->huident = iv_dest_hu.
    lr_new_hu->pmatid = ms_deco_pallets-pmatid.
    lr_new_hu->hutype = ms_deco_pallets-hu_type.
    ms_deco_pallets-nof_hus = lines( lr_prop_wt->hus ).

    TRY.
        pack_print_hu( ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        IF ms_deco_pallets-no_pack_settings EQ abap_true.
          clear_pmat_dep_data( ).
        ENDIF.
        RAISE EXCEPTION lx_ws.
    ENDTRY.
  ENDMETHOD.


  METHOD display_hus.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Display application log
**********************************************************************
    READ TABLE mt_proposed_wt INDEX mv_wt_grp_index REFERENCE INTO DATA(lr_prop_wt).
    CALL FUNCTION 'Z_INB_SCANNED_HUS'
      CHANGING
        ct_huident = lr_prop_wt->hus.
    ms_deco_pallets-nof_hus = lines( lr_prop_wt->hus ).
  ENDMETHOD.


  METHOD display_log.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Display application log
**********************************************************************
    DATA: ls_display_profile TYPE  bal_s_prof.

    CHECK mo_function_log IS BOUND.

    TRY.
        CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
          IMPORTING
            e_s_display_profile = ls_display_profile.                  " Display Profile
        mo_function_log->display_log( iv_loghandle = mv_balloghndl is_display_profile = ls_display_profile ).
      CATCH /scwm/cx_basics ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD display_product.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Display Product by calling the transaction ZMDM
**********************************************************************
    SET PARAMETER ID '/SCWM/LGN' FIELD ms_deco_pallets-lgnum.
    SET PARAMETER ID 'SCI' FIELD ms_deco_pallets-matnr.
    SET PARAMETER ID '/SCWM/ENTITLED' FIELD ms_deco_pallets-entitled.

    "AAHMEDOV-240209
    zcl_deco_flag=>set_param_zdeco_nav( ).
    "AAHMEDOV-240209

    CALL TRANSACTION 'ZMDM' AND SKIP FIRST SCREEN.
    fill_product_information( ).
    check_product( ).

  ENDMETHOD.


  METHOD display_wt_grp.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Display the current warehouse task group
**********************************************************************
    DATA lv_nof_tote TYPE f.
    CLEAR: mv_internal_hu_nr, mv_displ_pc_per_tot.
    CLEAR ms_deco_pallets-proposed_packing.


    CLEAR ms_deco_pallets-dest_hu.
    IF lines( mt_proposed_wt ) LT  mv_wt_grp_index.
      RETURN.
    ENDIF.
    ms_deco_pallets-proposed_wt = mt_proposed_wt[ mv_wt_grp_index ]-wt_group_data.

    IF ms_mat_lgtyp-lgtyp NE ms_deco_pallets-nltyp.
      TRY.
          CLEAR ms_mat_lgtyp.
          CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
            EXPORTING
              iv_matid     = ms_delivery_item-product-productid
              iv_entitled  = ms_delivery_item-sapext-entitled
              iv_lgnum     = ms_defaults-lgnum
              iv_lgtyp     = ms_deco_pallets-nltyp
            IMPORTING
              es_mat_lgtyp = ms_mat_lgtyp.
        CATCH /scwm/cx_md.
          RAISE EXCEPTION TYPE zcx_workstation.
      ENDTRY.
    ENDIF.

    mo_sp->get_prop_hu_data(
      EXPORTING
        iv_nltyp               = ms_deco_pallets-nltyp
        iv_nlptyp              = ms_deco_pallets-nlptyp
        iv_matid               = ms_delivery_item-product-productid
        iv_keepcar             = ms_mat_lgnum-zz1_keepcar_whd
        iv_dirrpl              = ms_mat_lgtyp-zz1_dirrpl_stt
        iv_entitled            = ms_delivery_item-sapext-entitled
      IMPORTING
        ev_change_st_type_mono = mv_pmat_can_be_changed
        ev_pack_mat            = ms_deco_pallets-proposed_packing-pmat
        ev_pack_mat_text       = ms_deco_pallets-proposed_packing-pmat_maktx
        ev_hutype              = ms_deco_pallets-proposed_packing-hu_type
        ev_hutyptext           = ms_deco_pallets-proposed_packing-hu_type_text
        ev_pack_mat_id         = ms_deco_pallets-proposed_packing-pmatid
        ev_nr_internal         = ms_deco_pallets-nr_internal
        ev_no_settings         = ms_deco_pallets-no_pack_settings ).

    IF ms_deco_pallets-no_pack_settings EQ abap_true.
      mv_pmat_can_be_changed = abap_true.
    ENDIF.
    IF mt_proposed_wt[ mv_wt_grp_index ]-putaway_cart_rel EQ abap_true.
      mv_pmat_can_be_changed = abap_false.
    ENDIF.
    IF ms_deco_pallets-nr_internal EQ abap_true AND mt_proposed_wt[ mv_wt_grp_index ]-hus IS INITIAL.
      mt_proposed_wt[ mv_wt_grp_index ]-hus =
                                VALUE #( ( pmatid = ms_deco_pallets-proposed_packing-pmatid
                                           internal_nr = ms_deco_pallets-nr_internal
                                           hutype =  ms_deco_pallets-proposed_packing-hu_type ) ).
    ENDIF.
    ms_deco_pallets-buffer_stbin = mo_sp->get_buffer_stbin( ).

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = ms_defaults-lgnum                  " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zinb_0002                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_lgtyp_tote_rel                  " Parameter ID for process
      IMPORTING
        et_range  = DATA(lt_rng_lgtyp_tote_rel)                 " Parameter-Framework Low
    ).

    IF ms_mat_lgtyp-zz1_dirrpl_stt EQ space
       AND ms_deco_pallets-proposed_wt-nltyp IN lt_rng_lgtyp_tote_rel.
      IF ms_deco_pallets-pc_per_mc NE 0.
        ms_deco_pallets-proposed_wt-qty_per_wt_s = ms_deco_pallets-proposed_wt-qty_per_wt.
        lv_nof_tote = ms_deco_pallets-proposed_wt-qty_per_wt / ms_deco_pallets-pc_per_mc.
        IF lv_nof_tote GT 1.
          ms_deco_pallets-proposed_wt-totes = ceil( lv_nof_tote ).
          mv_displ_pc_per_tot = abap_true.
        ENDIF.
      ENDIF.
      ms_deco_pallets-ptwy_moretone  = 'Put-away in more than one tote'(pam).

      ms_deco_pallets-totes_t        = 'Totes'(tot).
      ms_deco_pallets-pc_per_mc_t    = 'PC/Tote'(pcp).

    ELSEIF ms_mat_lgtyp-zz1_dirrpl_stt EQ abap_true
       AND ms_deco_pallets-proposed_wt-nltyp IN lt_rng_lgtyp_tote_rel.
      ms_deco_pallets-proposed_wt-qty_per_wt_s = ms_deco_pallets-proposed_wt-qty_per_wt.

      ms_deco_pallets-ptwy_moretone  = 'Put-away in more than one master carton'(pac).

      ms_deco_pallets-totes_t        = 'Master Cartons'(mca).
      ms_deco_pallets-pc_per_mc_t    = 'PC/Master Cartons'(pcm).

      lv_nof_tote = ms_deco_pallets-proposed_wt-qty_per_wt / ms_deco_pallets-pc_per_mc.
      IF lv_nof_tote GT 1.
        ms_deco_pallets-proposed_wt-totes = ceil( lv_nof_tote ).
        mv_displ_pc_per_tot = abap_true.
      ENDIF.
    ENDIF.
    ms_deco_pallets-qty_per_wt_uom_s = ms_deco_pallets-qty_per_wt_uom.
    ms_deco_pallets-qty_per_wt_s_t = 'PC'(pct).
  ENDMETHOD.


  METHOD do_create_proposal.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Create proposal and fill the screen fields
**********************************************************************
    DATA: ls_mat_lgtyp           TYPE  /scwm/s_material_lgtyp,
          lv_vsola               TYPE /scwm/de_quantity,
          lv_putawy_qty          TYPE /scwm/de_quantity,
          lv_req_qty             TYPE /scwm/de_quantity,
          lv_total_req_qty       TYPE /scwm/de_quantity,
          lv_total_ltap_qty      TYPE /scwm/de_quantity,
          lv_req_group_qty       TYPE /scwm/de_quantity,
          lv_nof_wts             TYPE i,
          lv_calculated_qty      TYPE /scwm/de_quantity,
          lv_putaway_per_wts_qty TYPE /scwm/de_quantity,
          lt_prel_prop           TYPE zif_ws_deco_sp=>tt_prel_proposal.

    ASSERT iv_change_proposal EQ abap_false
           OR iv_nltyp IS NOT INITIAL
           OR iv_putaway_per_wts_qty IS NOT INITIAL
           OR iv_nof_wts IS NOT INITIAL.

    MODIFY mt_proposed_wt FROM VALUE #( newly_inserted = abap_false modified_in_proposal = abap_false )
           TRANSPORTING newly_inserted modified_in_proposal
           WHERE nof_wts NE 0.

    DATA(lv_original_ptwy_qty) = ms_deco_pallets-putaway_qty.
    IF ms_deco_pallets-uom NE ms_deco_pallets-putaway_uom.
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = ms_mat_global-matid                 " Material GUID16  mit Konvertierungsexit
              iv_quan      = CONV /scwm/de_quantity( ms_deco_pallets-putaway_qty )                 " Mengenfeld
              iv_unit_from = ms_deco_pallets-putaway_uom                 " Mengeneinheit
              iv_unit_to   = ms_deco_pallets-uom                 " Mengeneinheit
              iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
            IMPORTING
              ev_quan      = lv_req_qty.                 " Mengenfeld
        CATCH /scwm/cx_md_interface           " Import Parameter fehlerhaft
              /scwm/cx_md_batch_required     " Charge ist zur Umrechnung notwendig
              /scwm/cx_md_internal_error     " Interner Fehler
              /scwm/cx_md_batch_not_required " Material nicht Chargenpflichtig, Charge nicht benötigt
              /scwm/cx_md_material_exist ##no_handler.     " Material existiert nicht
          "May not happend since it was done before
      ENDTRY.
    ELSE.
      lv_req_qty = ms_deco_pallets-putaway_qty.
    ENDIF.


    "If change proposal in process, then the whole requested quantity is the task quantities together
    IF iv_change_proposal IS NOT INITIAL.
      CLEAR lv_req_qty.
      "Calculate requested quantity for the group
      LOOP AT it_ltap REFERENCE INTO DATA(lr_wts).
        ADD lr_wts->vsolm TO lv_total_ltap_qty.
      ENDLOOP.
      "Calculate total requested quantity
      ADD lv_total_ltap_qty TO lv_total_req_qty.
      ADD lv_total_ltap_qty TO lv_req_qty.
      LOOP AT mt_proposed_wt REFERENCE INTO DATA(lr_prop).
        LOOP AT lr_prop->wts REFERENCE INTO lr_wts.
          ADD lr_wts->vsolm TO lv_total_req_qty.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      lv_total_req_qty = lv_req_qty.
    ENDIF.
    IF lv_total_req_qty GT ms_deco_pallets-open_putaway_qty.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e028(zmc_workstation).
    ENDIF.

    lv_req_group_qty = lv_req_qty.

    IF iv_change_proposal EQ abap_true.
      IF lv_putaway_per_wts_qty IS NOT INITIAL AND iv_putaway_per_wts_uom IS NOT INITIAL
         AND iv_putaway_per_wts_uom NE ms_deco_pallets-uom.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = ms_mat_global-matid                 " Material GUID16  mit Konvertierungsexit
                iv_quan      = CONV /scwm/de_quantity( iv_putaway_per_wts_qty )                 " Mengenfeld
                iv_unit_from = iv_putaway_per_wts_uom                 " Mengeneinheit
                iv_unit_to   = ms_deco_pallets-uom                 " Mengeneinheit
                iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
              IMPORTING
                ev_quan      = lv_putaway_per_wts_qty.                 " Mengenfeld
          CATCH /scwm/cx_md_interface           " Import Parameter fehlerhaft
                /scwm/cx_md_batch_required     " Charge ist zur Umrechnung notwendig
                /scwm/cx_md_internal_error     " Interner Fehler
                /scwm/cx_md_batch_not_required " Material nicht Chargenpflichtig, Charge nicht benötigt
                /scwm/cx_md_material_exist ##no_handler.     " Material existiert nicht
            "May not happend since it was done before
        ENDTRY.
      ELSE.
        lv_putaway_per_wts_qty = iv_putaway_per_wts_qty.
      ENDIF.

      lv_nof_wts = iv_nof_wts.
      IF iv_nof_wts IS INITIAL.
        lv_nof_wts = ms_deco_pallets-nof_wts.
      ENDIF.
      IF lv_putaway_per_wts_qty IS INITIAL.
        lv_putaway_per_wts_qty = ms_deco_pallets-qty_per_wt.
      ENDIF.
      lv_calculated_qty = lv_nof_wts * lv_putaway_per_wts_qty.
      IF lv_calculated_qty GT ms_deco_pallets-open_putaway_qty.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e028(zmc_workstation).
      ENDIF.
      DO lv_nof_wts TIMES.
        APPEND VALUE #( qty = lv_putaway_per_wts_qty uom = ms_deco_pallets-uom nltyp = iv_nltyp )
               TO lt_prel_prop.
      ENDDO.

      "if the quantit in the group has been changed, the the whole proposal must be recalculated
      IF lv_total_ltap_qty NE lv_calculated_qty.
        DATA(lv_complete_recalc) = abap_true.
        lv_req_qty = lv_total_req_qty.
      ENDIF.

      SUBTRACT lv_calculated_qty FROM lv_req_qty.
    ENDIF.

    IF lv_req_qty GT 0.
      IF mv_mono_pallet EQ abap_true. "Split quantities
        IF ms_deco_pallets-pc_per_full_pal IS INITIAL.
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e035(zmc_workstation).
        ENDIF.
        DATA(lv_nof_full_pal) = floor( CONV f( lv_req_qty ) / ms_deco_pallets-pc_per_full_pal ).
        DO lv_nof_full_pal TIMES.
          APPEND VALUE #( qty = ms_deco_pallets-pc_per_full_pal uom = ms_deco_pallets-uom nltyp = iv_nltyp )
                 TO lt_prel_prop.
        ENDDO.
        DATA(lv_rem_from_calc) = lv_req_qty - lv_nof_full_pal * ms_deco_pallets-pc_per_full_pal.
        IF lv_rem_from_calc IS NOT INITIAL.
          APPEND VALUE #( qty = lv_rem_from_calc uom = ms_deco_pallets-uom nltyp = iv_nltyp )
                 TO lt_prel_prop.
        ENDIF.
      ELSE.
        APPEND VALUE #( qty = lv_req_qty  uom = ms_deco_pallets-uom nltyp = iv_nltyp )
               TO lt_prel_prop.
      ENDIF.
    ENDIF.


    mo_sp->create_proposal(
      EXPORTING
        iv_rdocid = ms_delivery_item-docid
        iv_ritmid = ms_delivery_item-itemid
        iv_anfme  = CONV #( ms_deco_pallets-putaway_qty )
        iv_altme  = ms_deco_pallets-putaway_uom
        iv_nltyp  = iv_nltyp
        iv_matid  = ms_delivery_item-product-productid
        iv_hutyp  = ms_deco_pallets-hu_type_from_psp
        it_preliminary_proposal = lt_prel_prop
        iv_puom                 = ms_deco_pallets-puom_wh
        iv_no_lock_release_after = abap_false
        iv_location_mixed       = xsdbool( mv_mono_pallet = abap_false )
      IMPORTING
        et_ltap   = DATA(lt_ltap)
        et_bapiret = DATA(lt_bapiret)
    ).

    mo_function_log->add_log( it_prot = lt_bapiret ).

    IF lt_ltap IS NOT INITIAL.
      SELECT FROM /scwm/lagp
        FIELDS lgpla, lptyp
        FOR ALL ENTRIES IN @lt_ltap
        WHERE lgnum EQ @lt_ltap-lgnum
          AND lgpla EQ @lt_ltap-nlpla
        INTO TABLE @DATA(lt_lagp).

      lock_delivery_item( ).
    ENDIF.
    TRY.
        DATA(lt_stock) = mo_sp->get_stock_data(
          EXPORTING
            iv_matnr        = ms_deco_pallets-matnr                 " Material Number
            iv_only_ws_spec = abap_true
            it_rng_lgpla    = VALUE #( FOR lagp IN lt_lagp ( sign = 'I' option = 'EQ' low = lagp-lgpla ) )                  " Range Table Type for Field Name LGPLA
        ).
      CATCH zcx_workstation ##no_handler. "Not critical error, and may never happen at this point
    ENDTRY.

    IF iv_change_proposal EQ abap_true.
      IF lv_complete_recalc EQ abap_true.
        CLEAR mt_proposed_wt.
        lv_original_ptwy_qty = lv_total_req_qty.
      ELSEIF ms_deco_pallets-uom EQ ms_deco_pallets-putaway_uom.
        lv_original_ptwy_qty = lv_req_group_qty.
      ELSE.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = ms_mat_global-matid                 " Material GUID16  mit Konvertierungsexit
                iv_quan      = lv_req_group_qty                 " Mengenfeld
                iv_unit_from = ms_deco_pallets-uom                 " Mengeneinheit
                iv_unit_to   = ms_deco_pallets-putaway_uom                 " Mengeneinheit
                iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
              IMPORTING
                ev_quan      = lv_putawy_qty.                 " Mengenfeld
            lv_original_ptwy_qty = lv_putawy_qty.
          CATCH /scwm/cx_md_interface           " Import Parameter fehlerhaft
                /scwm/cx_md_batch_required     " Charge ist zur Umrechnung notwendig
                /scwm/cx_md_internal_error     " Interner Fehler
                /scwm/cx_md_batch_not_required " Material nicht Chargenpflichtig, Charge nicht benötigt
                /scwm/cx_md_material_exist ##no_handler.     " Material existiert nicht
            "May not happend since it was done before
        ENDTRY.
      ENDIF.
    ENDIF.

    LOOP AT lt_ltap REFERENCE INTO DATA(lr_ltap).
      READ TABLE lt_lagp INTO DATA(ls_lagp)
           WITH KEY lgpla = lr_ltap->nlpla.
      IF sy-subrc NE 0.
        CLEAR ls_lagp.
      ENDIF.

      READ TABLE lt_stock INTO DATA(ls_stock)
           WITH KEY lgpla = lr_ltap->nlpla.
      IF sy-subrc NE 0.
        CLEAR ls_stock.
      ENDIF.
      IF ls_stock-quan IS NOT INITIAL.
        DATA(lv_addition_to_stock) = CONV zde_addition_to_stock( 'Add to Stock!'(ats) ).
      ELSE.
        CLEAR lv_addition_to_stock.
      ENDIF.


      READ TABLE mt_proposed_wt REFERENCE INTO DATA(lr_prop_wt)
           WITH KEY qty_per_wt = CONV zde_qty_per_wt( lr_ltap->vsola )
                    qty_per_wt_uom = lr_ltap->altme
                    nltyp = lr_ltap->nltyp
                    nlptyp = ls_lagp-lptyp
                    addition_to_stock = lv_addition_to_stock.
      IF sy-subrc NE 0.
        APPEND VALUE #(
                    qty_per_wt = CONV zde_qty_per_wt( lr_ltap->vsola )
                    qty_per_wt_uom = lr_ltap->altme
                    qty_per_wt_b = CONV zde_qty_per_wt( lr_ltap->vsolm )
                    qty_per_wt_uom_b = lr_ltap->meins
                    nltyp = lr_ltap->nltyp
                    nlptyp = ls_lagp-lptyp
                    addition_to_stock = lv_addition_to_stock
                    qty_in_bin = ls_stock-quan
                    qty_in_bin_uom = ls_stock-meins
          ) TO mt_proposed_wt REFERENCE INTO lr_prop_wt.
        lr_prop_wt->newly_inserted = abap_true.
      ELSE.
        lr_prop_wt->modified_in_proposal = abap_true.
      ENDIF.

      APPEND lr_ltap->* TO lr_prop_wt->wts.

      IF lr_ltap->meins NE ms_deco_pallets-putaway_uom.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = lr_ltap->matid                 " Material GUID16  mit Konvertierungsexit
                iv_quan      = lr_ltap->vsolm                 " Mengenfeld
                iv_unit_from = lr_ltap->meins                 " Mengeneinheit
                iv_unit_to   = ms_deco_pallets-putaway_uom                 " Mengeneinheit
                iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
              IMPORTING
                ev_quan      = lv_vsola.                 " Mengenfeld
          CATCH /scwm/cx_md_interface           " Import Parameter fehlerhaft
                /scwm/cx_md_batch_required     " Charge ist zur Umrechnung notwendig
                /scwm/cx_md_internal_error     " Interner Fehler
                /scwm/cx_md_batch_not_required " Material nicht Chargenpflichtig, Charge nicht benötigt
                /scwm/cx_md_material_exist ##no_handler.     " Material existiert nicht
            "May not happend since it was done before
        ENDTRY.
      ELSE.
        lv_vsola = lr_ltap->vsolm.
      ENDIF.
      SUBTRACT lv_vsola FROM lv_original_ptwy_qty.
    ENDLOOP.

    DATA(lt_stor_typ_cust)   = zcl_crud_ztinb_curst_styp=>select_multi_by_lgnum( iv_lgnum = ms_defaults-lgnum ).

    LOOP AT mt_proposed_wt REFERENCE INTO lr_prop_wt.
      "Number of the WTs
      lr_prop_wt->nof_wts = lines( lr_prop_wt->wts ).

      "Adjust values
      IF lr_prop_wt->nof_wts NE 1.
        CLEAR: lr_prop_wt->qty_in_bin,
               lr_prop_wt->qty_in_bin_uom .
      ENDIF.

      "Get the texts
      READ TABLE st_st_type_txt INTO DATA(ls_st_type_text)
           WITH TABLE KEY lgtyp = lr_prop_wt->nltyp.
      IF sy-subrc NE 0.
        SELECT SINGLE FROM /scwm/t301t
               FIELDS *
               WHERE lgnum EQ @ms_defaults-lgnum
                 AND lgtyp EQ @lr_prop_wt->nltyp
                 AND spras EQ @sy-langu
               INTO @ls_st_type_text.
        INSERT ls_st_type_text INTO TABLE st_st_type_txt.
      ENDIF.

      lr_prop_wt->nltypt = ls_st_type_text-ltypt.
      READ TABLE st_st_bin_type_txt INTO DATA(ls_st_bin_type_text)
           WITH TABLE KEY lptyp = lr_prop_wt->nlptyp.
      IF sy-subrc NE 0.
        SELECT SINGLE FROM /scwm/t303t
               FIELDS *
               WHERE lgnum EQ @ms_defaults-lgnum
                 AND lptyp EQ @lr_prop_wt->nlptyp
                 AND spras EQ @sy-langu
               INTO @ls_st_bin_type_text.
        INSERT ls_st_bin_type_text INTO TABLE st_st_type_txt.
      ENDIF.
      lr_prop_wt->nlptypt = ls_st_bin_type_text-ptypt.

      "Fill sort fields
      READ TABLE lt_stor_typ_cust INTO DATA(ls_stor_typ_cust)
           WITH KEY lgtyp = lr_prop_wt->nltyp.
      IF sy-subrc EQ 0 AND ls_stor_typ_cust-wt_sort_rel EQ abap_true.
        lr_prop_wt->nltyp_pos = ls_stor_typ_cust-wt_sort_seq.
      ELSEIF sy-subrc NE 0.
        CLEAR ls_stor_typ_cust.
      ENDIF.
      lr_prop_wt->putaway_cart_rel = ls_stor_typ_cust-putaway_cart_rel.

      DATA(ls_sttype_ctrl) = zcl_crud_scwm_t331=>get_sttype_ctrl_by_sttype( iv_lgnum = ms_defaults-lgnum iv_lgtyp = lr_prop_wt->nltyp ).
      lr_prop_wt->prlet = ls_sttype_ctrl-prlet.
      IF ls_sttype_ctrl-prlet EQ abap_true.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
              EXPORTING
                iv_matid     = ms_delivery_item-product-productid
                iv_entitled  = ms_delivery_item-sapext-entitled
                iv_lgnum     = ms_defaults-lgnum
                iv_lgtyp     = lr_prop_wt->nltyp
              IMPORTING
                es_mat_lgtyp = ls_mat_lgtyp.
          CATCH /scwm/cx_md.
            RAISE EXCEPTION TYPE zcx_workstation.
        ENDTRY.
        DATA(lt_seq) = zcl_crud_scwm_tbintypsq2=>get_seq_by_lgtyp_obintyp(
                         iv_lgnum = ms_defaults-lgnum                 " Warehouse Number/Warehouse Complex
                         iv_lgtyp = lr_prop_wt->nltyp                 " Storage Type
                         iv_obintyp = ls_mat_lgtyp-bintype
                       ).
        READ TABLE lt_seq INTO DATA(ls_seq)
             WITH KEY bintyp = lr_prop_wt->nlptyp.
        IF sy-subrc EQ 0.
          lr_prop_wt->nlptyp_pos = ls_seq-seqno.
        ENDIF.
      ELSE.
        lr_prop_wt->nlptyp_pos = zcl_crud_scwm_tbintyp_sq=>get_seq_for_lgtyp_hutyp_lptyp(
            iv_lgnum = ms_defaults-lgnum                 " Warehouse Number/Warehouse Complex
            iv_lgtyp = lr_prop_wt->nltyp                 " Storage Type
            iv_hutyp = ms_deco_pallets-hu_type_from_psp                 " Handling Unit Type
            iv_lptyp = lr_prop_wt->nlptyp                 " Storage Bin Type
        ).
      ENDIF.

      IF lr_prop_wt->qty_per_wt_uom NE ms_deco_pallets-putaway_uom.
        IF lr_prop_wt->qty_per_wt_uom_b EQ ms_deco_pallets-putaway_uom.
          lr_prop_wt->qty_per_wt = lr_prop_wt->qty_per_wt_b.
          lr_prop_wt->qty_per_wt_uom = ms_deco_pallets-putaway_uom.
        ELSE.
          DATA(lv_qty_to) = CONV /scwm/de_quantity( lr_prop_wt->qty_per_wt ).
          TRY.
              CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                EXPORTING
                  iv_matid     = ms_mat_global-matid                 " Material GUID16  mit Konvertierungsexit
                  iv_quan      = CONV /scwm/de_quantity( lr_prop_wt->qty_per_wt )                " Mengenfeld
                  iv_unit_from = lr_prop_wt->qty_per_wt_uom                 " Mengeneinheit
                  iv_unit_to   = ms_deco_pallets-putaway_uom                 " Mengeneinheit
                  iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
                IMPORTING
                  ev_quan      = lv_qty_to.                 " Mengenfeld
              lr_prop_wt->qty_per_wt = lv_qty_to.
              lr_prop_wt->qty_per_wt_uom = ms_deco_pallets-putaway_uom.
            CATCH /scwm/cx_md_interface           " Import Parameter fehlerhaft
                  /scwm/cx_md_batch_required     " Charge ist zur Umrechnung notwendig
                  /scwm/cx_md_internal_error     " Interner Fehler
                  /scwm/cx_md_batch_not_required " Material nicht Chargenpflichtig, Charge nicht benötigt
                  /scwm/cx_md_material_exist ##no_handler.     " Material existiert nicht
              "May not happend since it was done before
          ENDTRY.
        ENDIF.
      ENDIF.

    ENDLOOP.

    SORT mt_proposed_wt BY nltyp_pos ASCENDING qty_per_wt DESCENDING nlptyp_pos ASCENDING.

    IF lv_original_ptwy_qty LT 0.
      CLEAR lv_original_ptwy_qty.
    ENDIF.
    IF lv_original_ptwy_qty IS NOT INITIAL.
      MESSAGE w034(zmc_workstation).
    ENDIF.
    lock_stock( ).
  ENDMETHOD.


  METHOD do_reset_proposals.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Reset proposals
**********************************************************************
    unlock_delivery_item( ).
    unlock_stock( ).

    CLEAR: mt_proposed_wt, mv_wt_grp_index, ms_deco_pallets-proposed_wt, ms_deco_pallets-proposed_packing,
           ms_mat_lgtyp, mv_displ_pc_per_tot.
    IF iv_keep_putaway_quant EQ abap_false.
      CLEAR ms_deco_pallets-putaway_qty.
    ENDIF.

  ENDMETHOD.


  METHOD fill_product_information.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Fill product dependent information
**********************************************************************
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = ms_delivery_item-product-productid
            iv_entitled   = ms_delivery_item-sapext-entitled
            iv_lgnum      = ms_defaults-lgnum
          IMPORTING
            es_mat_global = ms_mat_global
            es_mat_lgnum  = ms_mat_lgnum.
      CATCH /scwm/cx_md.
        mv_md_ok = abap_false.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e047(zmc_workstation) WITH ms_mat_global-matnr ms_defaults-lgnum.
    ENDTRY.

    SELECT SINGLE FROM mara
           FIELDS mfrpn, ean11
           WHERE matnr EQ @ms_delivery_item-product-productno
           INTO @DATA(ls_mara).


    ms_deco_pallets-matnr = ms_mat_global-matnr.
    ms_deco_pallets-maktx = ms_delivery_item-product-product_text.
    ms_deco_pallets-subst_prod_number = |{ ms_delivery_item-eew-zzmaterp ALPHA = IN }|.
    ms_deco_pallets-mfrpn = ls_mara-mfrpn.
    ms_deco_pallets-ean11 = ls_mara-ean11.
    ms_deco_pallets-uom   = ms_mat_global-meins.

    ms_deco_pallets-entitled = ms_delivery_item-sapext-entitled.

    mo_sp->get_packspec(
      EXPORTING
        iv_matid   = ms_delivery_item-product-productid
      IMPORTING
        et_content = DATA(lt_pack_spec_cont)
        ev_pc_per_fulpal = ms_deco_pallets-pc_per_full_pal
    ).
    IF ms_deco_pallets-pc_per_full_pal EQ 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e067(zmc_workstation) WITH ms_delivery_item-product-productno.
    ENDIF.
    READ TABLE lt_pack_spec_cont REFERENCE INTO DATA(lr_pack_sepc_cont)
         WITH KEY content-matid = ms_delivery_item-product-productid.
    IF sy-subrc  EQ 0.
      READ TABLE lr_pack_sepc_cont->levels REFERENCE INTO DATA(lr_lev1)
           WITH KEY level_seq = '01'.
      IF sy-subrc EQ 0.
        ms_deco_pallets-pc_per_mc = lr_lev1->total_quan.
      ENDIF.
    ENDIF.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = ms_defaults-lgnum                  " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zinb_0002                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_/scwm/de_hutyp                  " Parameter ID for process
      IMPORTING
        ev_constant  = DATA(lv_hu_type)                 " Parameter-Framework Low
    ).
    ms_deco_pallets-hu_type_from_psp = lv_hu_type.

    IF ms_mat_lgnum-zz1_keepcar_whd EQ abap_true.
      ms_deco_pallets-store_as = 'Store as MC'(sam).
      ms_deco_pallets-store_as_mc = abap_true.
    ELSE.
      ms_deco_pallets-store_as = 'Store as single pieces'(sap).
      ms_deco_pallets-store_as_mc = abap_false.
    ENDIF.
    ms_deco_pallets-putaway_uom = ms_mat_global-meins.

    IF ms_deco_pallets-store_as_mc EQ abap_true.
      ms_deco_pallets-puom_wh = ms_mat_lgnum-puom_wh.
      fill_puom_wh_rel_data( ).
    ENDIF.

    TRY.

        CLEAR mt_curr_stock_sit.
        mt_curr_stock_sit = mo_sp->get_current_stock_situation( iv_matnr = ms_deco_pallets-matnr ).
        mo_alv_curr_stock_sit->refresh( ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        MESSAGE lx_ws TYPE 'W' .
    ENDTRY.
  ENDMETHOD.


  METHOD fill_puom_wh_rel_data.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Fill the Prefered alterantiv UoM and dependent data
**********************************************************************
    DATA: lv_pcs_in_1_mc TYPE /scwm/de_quantity.
    CLEAR ms_deco_pallets-puom_wht.
    IF ms_deco_pallets-puom_wh IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE FROM t006a
           FIELDS msehl
           WHERE msehi EQ @ms_deco_pallets-puom_wh
             AND spras EQ @sy-langu
           INTO @ms_deco_pallets-puom_wht.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
          EXPORTING
            iv_matid     = ms_mat_global-matid                 " Material GUID16  mit Konvertierungsexit
            iv_quan      = 1                 " Mengenfeld
            iv_unit_from = ms_deco_pallets-puom_wh                 " Mengeneinheit
            iv_unit_to   = zif_wme_c=>gs_uom-pc                 " Mengeneinheit
            iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
          IMPORTING
            ev_quan      = lv_pcs_in_1_mc.                 " Mengenfeld

        ms_deco_pallets-pc_per_mc = lv_pcs_in_1_mc.
      CATCH /scwm/cx_md.          " Import Parameter fehlerhaft
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e074(zmc_workstation)
              WITH ms_mat_global-matnr ms_deco_pallets-puom_wh zif_wme_c=>gs_uom-pc.
    ENDTRY.
  ENDMETHOD.


  METHOD finish_function_log.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& finish the function log and display it, if needed
**********************************************************************
    DATA: ls_display_profile TYPE  bal_s_prof.
    IF iv_save_only_if_error EQ abap_true AND mo_function_log->get_severity( ) NA 'AEX'.
      RETURN.
    ENDIF.
    mo_function_log->save_applog(
      EXPORTING
        is_log       = VALUE #( extnumber = |Warehouse { ms_defaults-lgnum }, Workst. { ms_defaults-workst_loc }, Function { iv_external_id } |
                                object = zif_wme_c=>gs_msgobj-zewm
                                subobject = zif_wme_c=>gs_msgsubobj-zworkstation )
      IMPORTING
        ev_loghandle = mv_balloghndl ) ##NO_TEXT.                 " Log Handle

    TRY.
        mo_function_log->save_applog2db( iv_loghandle = mv_balloghndl ).
      CATCH /scwm/cx_basics ##NO_HANDLER.
    ENDTRY.

    IF iv_display EQ abap_true AND mo_function_log->get_severity( ) CA 'AEX'.
      TRY.
          CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
            IMPORTING
              e_s_display_profile = ls_display_profile.                  " Display Profile
          mo_function_log->display_log( iv_loghandle = mv_balloghndl is_display_profile = ls_display_profile ).
          ev_displayed = abap_true.
        CATCH /scwm/cx_basics ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD get_delivery_items.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the relevant delivery items for the product
**********************************************************************
    DATA(lo_prd2hum) = NEW /scwm/cl_dlv_prd2hum( ).


    mt_inb_delivery = mo_sp->get_delivery_from_is( CONV #( ms_deco_pallets-inbound_shipment ) ).
    mt_delivery_item = mo_sp->get_delivery_items( iv_matnr = iv_matnr  it_docid = VALUE /scwm/dlv_docid_item_tab( FOR did IN mt_inb_delivery ( docid = did ) ) ).

    LOOP AT mt_delivery_item REFERENCE INTO DATA(lr_itm).
      READ TABLE lr_itm->addmeas INTO DATA(ls_open_ptwy_planned)
             WITH KEY qty_role     = c_planned_ptwy_qty_role
                      qty_category = c_planned_ptwy_qty_cat.
      IF ls_open_ptwy_planned-qty EQ 0.
        DELETE mt_delivery_item.
      ENDIF.
    ENDLOOP.

    "Delete deliveries with no open quantity
    LOOP AT mt_delivery_item REFERENCE INTO lr_itm.
      CALL METHOD lo_prd2hum->map_hu_dlv
        EXPORTING
          iv_docid       = lr_itm->docid
          iv_doccat      = lr_itm->doccat
          iv_with_object = abap_true
        IMPORTING
          et_hu_dlv      = DATA(lt_itmp).
      READ TABLE lt_itmp REFERENCE INTO DATA(lr_imp)
           WITH KEY item_id = lr_itm->itemid.
      IF sy-subrc EQ 0.
        IF lr_imp->open_qty-qty EQ 0.
          DELETE mt_delivery_item.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF mt_delivery_item IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e012(zmc_workstation) WITH ms_deco_pallets-product_ean_mpn.
    ENDIF.
  ENDMETHOD.


  METHOD get_open_putaway.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get open putaway quantity
**********************************************************************
    CLEAR ms_deco_pallets-open_putaway_qty.

    DATA(lo_prd2hum) = NEW /scwm/cl_dlv_prd2hum( ).

    CALL METHOD lo_prd2hum->map_hu_dlv
      EXPORTING
        iv_docid       = ms_delivery_item-docid
        iv_doccat      = ms_delivery_item-doccat
        iv_with_object = abap_true
      IMPORTING
        et_hu_dlv      = DATA(lt_itmp).
    READ TABLE lt_itmp REFERENCE INTO DATA(lr_imp)
         WITH KEY item_id = ms_delivery_item-itemid.
    IF sy-subrc EQ 0.
      ms_deco_pallets-open_putaway_qty = lr_imp->open_qty-qty.
    ENDIF.
  ENDMETHOD.


  METHOD get_packing_instr.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the packing instruction and fill the screen fields
**********************************************************************

    mo_text_handling = zcl_text_handling=>get_instance_for_warehouse( ms_defaults-lgnum ).

    DATA: lt_text        TYPE zif_text_handling=>tt_text,
          lt_section_txt TYPE zif_text_handling=>tt_text,
          lv_section     TYPE string.


    READ TABLE ms_current_dlv_header-partyloc INTO DATA(ls_party_vendor_prof)
         WITH  KEY party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sfprt.
    IF sy-subrc EQ 0.
      lt_section_txt = mo_text_handling->get_texts_from_bp( iv_business_partner = ls_party_vendor_prof-partyno
                                                            it_text_type = VALUE #( ( zif_wme_c=>gc_text_types-vend_unloading ) )
                                                            iv_no_del_note = abap_true ).
      IF lt_section_txt IS NOT INITIAL.
        lv_section = 'Vendor'(cve).
        APPEND VALUE zif_text_handling=>ty_text( text = |{ lv_section }: { ls_party_vendor_prof-partyno ALPHA = OUT }| ) TO lt_text.
        APPEND LINES OF lt_section_txt  TO lt_text.
      ENDIF.
    ENDIF.
*
    LOOP AT it_matnr_entitled INTO DATA(ls_matnr).
      lt_section_txt = mo_text_handling->get_texts_from_prod( iv_product = ls_matnr-matnr
                                                              iv_entitled = ls_matnr-entitled
                                                              iv_lgnum    = ms_deco_pallets-lgnum
                                                              it_text_type = VALUE #( ( zif_wme_c=>gc_text_types-inbound_deco ) ) ).
      IF lt_section_txt IS NOT INITIAL.
        lv_section = 'Product'(prd).
        APPEND VALUE zif_text_handling=>ty_text( text = |{ lv_section }: { ls_matnr-matnr ALPHA = OUT }| ) TO lt_text.
        APPEND LINES OF lt_section_txt  TO lt_text.
      ENDIF.
    ENDLOOP.

    rv_pack_instr = mo_text_handling->create_pack_instr_string( lt_text ).

  ENDMETHOD.


  METHOD gr_dummy_hu_changed.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& IG Dummy HU is changed then change the depedent data
**********************************************************************
    CLEAR ms_deco_pallets-gr_dummy_hu.
    clear_data_when_hu_empty( ).

    mo_sp->get_hu_by_ident(
      EXPORTING
        iv_huident = iv_gr_dummy_hu                  " Handling Unit Identification
      IMPORTING
        es_huhdr   = DATA(ls_huhdr)                  " Internal Structure for Processing HU Headers
        et_huident = DATA(lt_huident)                 " Table of Identifications for a HU
    ).
    IF ls_huhdr IS INITIAL.
      mt_inb_delivery = mo_sp->get_delivery_from_is( CONV #( iv_gr_dummy_hu ) ).
      IF mt_inb_delivery IS INITIAL.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e006(zmc_workstation) WITH iv_gr_dummy_hu.
      ENDIF.
      ms_deco_pallets-inbound_shipment = iv_gr_dummy_hu.
      mv_hu_unknown = abap_true.
      ms_deco_pallets-gr_dummy_hu = 'unknown'(unw).
    ELSE.
      ms_deco_pallets-gr_dummy_hu = iv_gr_dummy_hu.
      READ TABLE lt_huident INTO DATA(ls_hu_i)
           WITH KEY idart = c_idart_inb_ship.
      IF sy-subrc EQ 0.
        mt_inb_delivery = mo_sp->get_delivery_from_is( CONV #( ls_hu_i-huident ) ).
        IF mt_inb_delivery IS INITIAL.
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e004(zmc_workstation) WITH iv_gr_dummy_hu.
        ENDIF.
        ms_deco_pallets-inbound_shipment = ls_hu_i-huident.
      ELSE.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e005(zmc_workstation) WITH iv_gr_dummy_hu.
      ENDIF.
      mv_hu_unknown = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD lock_delivery_item.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Lock delivery item
**********************************************************************
    CALL FUNCTION 'ENQUEUE_/SCDL/E_ID'
      EXPORTING
        mode_/scdl/dl_lock_with_id_str = c_lock_mode_exclusive
        docid                          = ms_delivery_item-docid
        itemid                         = ms_delivery_item-itemid
        _scope                         = c_lock_owner_dialog
      EXCEPTIONS
        foreign_lock                   = 1
        system_failure                 = 2
        OTHERS                         = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e041(zmc_workstation) WITH ms_deco_pallets-ewm_inb_del_item.
    ENDIF.
    mv_locked_docid = ms_delivery_item-docid.
    mv_locked_itemid = ms_delivery_item-itemid.

  ENDMETHOD.


  METHOD lock_stock.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Lock stock for the processing
**********************************************************************
    DATA lv_current TYPE i.
    IF iv_wt_group IS NOT INITIAL.
      DATA(lv_from) = iv_wt_group.
      DATA(lv_nof_grp) = 1.
    ELSE.
      lv_from = 1.
      lv_nof_grp = lines( mt_proposed_wt ).
    ENDIF.
    lv_current = lv_from - 1.
    DO lv_nof_grp TIMES.
      ADD 1 TO lv_current.
      READ TABLE mt_proposed_wt REFERENCE INTO DATA(lr_prop) INDEX lv_current.
      LOOP AT lr_prop->wts REFERENCE INTO DATA(lr_wts).
        "Lock source and target AQUAY
        APPEND VALUE /scwm/aquay(
           flgmove          = space
           vsi              = wmegc_physical_stock
           lgnum            = ms_deco_pallets-lgnum
           loc_type         = wmegc_bin
           location         = lr_wts->vlpla
           vhi              = wmegc_vhi_dummy
           guid_stock       = lr_wts->guid_stock
           quan             = lr_wts->vsolm
           ) TO lr_prop->lock_stbin_y REFERENCE INTO DATA(lr_aqua_y).
        DATA(lv_tabix_y) = sy-tabix.
        CALL FUNCTION 'ENQUEUE_/SCWM/ELAQUAY'
          EXPORTING
            mode_/scwm/aquay = c_lock_mode_shared
            flgmove          = lr_aqua_y->flgmove
            vsi              = lr_aqua_y->vsi
            lgnum            = lr_aqua_y->lgnum
            loc_type         = lr_aqua_y->loc_type
            location         = lr_aqua_y->location
            vhi              = lr_aqua_y->vhi
            guid_stock       = lr_aqua_y->guid_stock
            quan             = lr_aqua_y->quan
            x_flgmove        = 'X'
            x_vsi            = 'X'
            x_huident        = 'X'
            x_vhi            = 'X'
            x_quan           = 'X'
          EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            OTHERS           = 3.
        IF sy-subrc NE 0.
          DELETE lr_prop->lock_stbin_y INDEX lv_tabix_y.
        ENDIF.


        APPEND VALUE /scwm/aquay(
           flgmove          = space
           vsi              = wmegc_reserved_stock
           lgnum            = ms_deco_pallets-lgnum
           loc_type         = wmegc_bin
           location         = lr_wts->vlpla
           vhi              = wmegc_vhi_dummy
           guid_stock       = lr_wts->guid_stock
           quan             = lr_wts->vsolm
           ) TO lr_prop->lock_stbin_y REFERENCE INTO lr_aqua_y.

        lv_tabix_y = sy-tabix.

        CALL FUNCTION 'ENQUEUE_/SCWM/ELAQUAY'
          EXPORTING
            mode_/scwm/aquay = c_lock_mode_shared
            flgmove          = lr_aqua_y->flgmove
            vsi              = lr_aqua_y->vsi
            lgnum            = lr_aqua_y->lgnum
            loc_type         = lr_aqua_y->loc_type
            location         = lr_aqua_y->location
            vhi              = lr_aqua_y->vhi
            guid_stock       = lr_aqua_y->guid_stock
            quan             = lr_aqua_y->quan
            x_flgmove        = 'X'
            x_vsi            = 'X'
            x_huident        = 'X'
            x_vhi            = 'X'
            x_quan           = 'X'
          EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            OTHERS           = 3.
        IF sy-subrc NE 0.
          DELETE lr_prop->lock_stbin_y INDEX lv_tabix_y.
        ENDIF.

        "Target AQAUX
        CALL FUNCTION 'ENQUEUE_/SCWM/ELAQUAX'
          EXPORTING
            mode_/scwm/aquax = c_lock_mode_exclusive
            lgnum            = ms_deco_pallets-lgnum
            loc_type         = wmegc_bin
            location         = lr_wts->nlpla
            type             = 'I'
          EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            OTHERS           = 3.
        IF sy-subrc EQ 0.
          APPEND VALUE /scwm/s_aquay_int(
            lgnum            = ms_deco_pallets-lgnum
            loc_type         = wmegc_bin
            location         = lr_wts->nlpla
             ) TO lr_prop->lock_stbin_x.
        ENDIF.
      ENDLOOP.
    ENDDO.
  ENDMETHOD.


  METHOD next_wt.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Display the next warehouse task group
**********************************************************************
    IF mv_wt_grp_index LT lines( mt_proposed_wt ).
      ADD 1 TO mv_wt_grp_index.
      display_wt_grp( ).
    ENDIF.
  ENDMETHOD.


  METHOD packaging_mat_changed.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& read packing material dependent data
**********************************************************************
    TRY.
        DATA(ls_prod) = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod(  iv_prodno    = iv_pmat ).

      CATCH /scmb/cx_md_access. " Exception Class for Master Data Accesses
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e023(zmc_workstation) WITH iv_pmat.
    ENDTRY.

    ms_deco_pallets-pmatid = ls_prod-prodid.
    mo_sp->get_pack_mat_data(
      EXPORTING
        iv_pack_mat_id         = ls_prod-prodid                 " Packaging Material
        iv_entitled            = ms_delivery_item-sapext-entitled                 " Party Entitled to Dispose
        iv_nltyp               = ms_deco_pallets-nltyp                 " Destination Storage Type
      IMPORTING
        ev_pack_mat            = ms_deco_pallets-proposed_packing-pmat
        ev_pack_mat_text       = ms_deco_pallets-proposed_packing-pmat_maktx
        ev_hutype              = ms_deco_pallets-proposed_packing-hu_type
        ev_hutyptext           = ms_deco_pallets-proposed_packing-hu_type_text
        ev_nr_internal         = ms_deco_pallets-nr_internal ).


  ENDMETHOD.


  METHOD pack_print_hu.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Recreatea proposal if the packing material is not the same as the
*& proposaed one. Then call the pack and print of the model class.
*& Then display the log of error occured, If everything is finm then
*& display the next WT group or leave the processing if there is no more WT group.
*& Finally, display the packing instructions
**********************************************************************
    DATA: lv_ptway_cart     TYPE /scwm/huident,
          lv_ptway_cart_pos TYPE /scwm/huident,
          lv_cancelled      TYPE flag.

    READ TABLE mt_proposed_wt INDEX mv_wt_grp_index REFERENCE INTO DATA(lr_wt_grp).

    IF mv_internal_hu_nr EQ abap_false AND lines( lr_wt_grp->hus ) EQ 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e040(zmc_workstation).
    ENDIF.

    start_function_log( ).

    IF lr_wt_grp->hus IS NOT INITIAL AND ms_deco_pallets-pmatid IS NOT INITIAL.
      MODIFY lr_wt_grp->hus FROM VALUE #( pmatid = ms_deco_pallets-pmatid ) TRANSPORTING pmatid
       WHERE pmatid NE ms_deco_pallets-pmatid.
    ENDIF.

    IF ms_deco_pallets-hu_type_from_psp NE ms_deco_pallets-proposed_packing-hu_type
       AND lr_wt_grp->prlet EQ abap_true.
      DATA(ls_wts_ori) = lr_wt_grp->wts[ 1 ] .
      unlock_delivery_item( ).
      TRY.
          mo_sp->create_proposal(
            EXPORTING
              iv_rdocid                = ms_delivery_item-docid
              iv_ritmid                = ms_delivery_item-itemid
              iv_anfme                 = ls_wts_ori-vsola
              iv_altme                 = ls_wts_ori-altme
              iv_nltyp                 = ls_wts_ori-nltyp
              iv_matid                 = ms_delivery_item-product-productid
              iv_hutyp                 = ms_deco_pallets-proposed_packing-hu_type
              iv_puom                  = ms_deco_pallets-puom_wh
              iv_no_lock_release_after = abap_false
              iv_location_mixed        = xsdbool( mv_mono_pallet = abap_false )
            IMPORTING
              et_ltap                  = DATA(lt_ltap_new)                 " Table Type: Warehouse Tasks Internal
              et_bapiret               = DATA(lt_bapiret_chg)                " Table with BAPI Return Information
          ).
          lock_delivery_item( ).
          unlock_stock( ).
          lock_stock( ).
          IF lt_ltap_new IS NOT INITIAL.
            CLEAR lr_wt_grp->wts.
            APPEND lt_ltap_new[ 1 ] TO lr_wt_grp->wts.
          ELSE.
            RAISE EXCEPTION TYPE zcx_workstation
                  MESSAGE e064(zmc_workstation) WITH ms_deco_pallets-proposed_packing-hu_type ls_wts_ori-nltyp
                     EXPORTING
                       messages = lt_bapiret_chg.
          ENDIF.
        CATCH zcx_workstation INTO DATA(lx_ws).
          /scwm/cl_tm=>cleanup( ).
          ROLLBACK WORK.

          DATA(lv_error) = abap_true.
          mo_function_log->add_log( it_prot = lx_ws->messages ).
          lock_delivery_item( ).
          unlock_stock( ).
          lock_stock( ).
      ENDTRY.
    ENDIF.
    IF lv_error EQ abap_false.
      DATA(lv_alt_uom) = COND /scwm/sp_uom(  WHEN ms_deco_pallets-puom_wh IS NOT INITIAL AND
                                                  ms_deco_pallets-store_as_mc EQ abap_true
                                             THEN ms_deco_pallets-puom_wh
                                             ELSE ms_deco_pallets-putaway_uom
                                            ).
      zcl_bin_determination_ctrl=>sv_location_mixed = xsdbool( mv_mono_pallet = abap_false ).
      TRY.
          IF lr_wt_grp->putaway_cart_rel EQ abap_true.
            mo_ptwy_cart_sp = zcl_ws_ptwy_cart_sp=>create_instance(
                                iv_lgnum          = ms_defaults-lgnum
                                iv_stor_bin_lgpla = ms_defaults-workst_loc
                              ).
            mo_ptwy_cart_sp->interpret_huident(
              EXPORTING
                iv_huident       = ms_deco_pallets-dest_hu                 " Handling Unit Identification
              IMPORTING
                es_cart_settings = DATA(ls_cart_settings)                 " Cart Barcode
            ).
            IF ls_cart_settings-pckg_type EQ mo_ptwy_cart_sp->c_pckg_type_tote.
              CALL FUNCTION 'Z_INB_PTWAY_CART_INFO'
                EXPORTING
                  iv_huident        = ms_deco_pallets-dest_hu
                  io_ptwy_cart_sp   = mo_ptwy_cart_sp
                IMPORTING
                  ev_ptway_cart     = lv_ptway_cart
                  ev_ptway_cart_pos = lv_ptway_cart_pos
                  ev_cancelled      = lv_cancelled.
              IF lv_cancelled EQ abap_true.
                RETURN.
              ENDIF.
            ENDIF.
            mo_ptwy_cart_sp->pack_and_print_deco(
              EXPORTING
                iv_huident         = ms_deco_pallets-dest_hu                 " Handling Unit Identification
                iv_pmatid          = ms_deco_pallets-pmatid                 " Packaging Material
                iv_nof_totes       = ms_deco_pallets-totes
                is_proposed_wt     = lr_wt_grp->wts[ 1 ]                 " Warehouse Task Internal
                iv_docid           = ms_delivery_item-docid                 " Doc. Identification for Document-Related Stocks
                iv_alt_uom         = lv_alt_uom                 " Unit of Measure
                iv_cart_id         = lv_ptway_cart
                iv_pos_on_cart     = lv_ptway_cart_pos
            ).
          ELSE.
            mo_sp->pack_and_print_hus( is_proposed_wt = lr_wt_grp->wts[ 1 ]
                                       is_hu = CORRESPONDING #( mt_proposed_wt[ mv_wt_grp_index ]-hus[ 1 ] )
                                       iv_alt_uom = lv_alt_uom
                                       iv_docid = ms_delivery_item-docid ).
          ENDIF.
        CATCH zcx_workstation INTO lx_ws.
          CLEAR zcl_bin_determination_ctrl=>sv_location_mixed.
          /scwm/cl_tm=>cleanup( ).
          ROLLBACK WORK.

          lv_error = abap_true.
          mo_function_log->add_log( it_prot = lx_ws->messages ).

        CATCH /scwm/cx_sp INTO DATA(lx_sp).
          CLEAR zcl_bin_determination_ctrl=>sv_location_mixed.
          /scwm/cl_tm=>cleanup( ).
          ROLLBACK WORK.

          lv_error = abap_true.
          add_exception_to_fl( lx_sp ).

        CATCH /scwm/cx_core INTO DATA(lx_core).
          CLEAR zcl_bin_determination_ctrl=>sv_location_mixed.
          /scwm/cl_tm=>cleanup( ).
          ROLLBACK WORK.

          lv_error = abap_true.
          add_exception_to_fl( lx_core ).
      ENDTRY.
    ENDIF.
    CLEAR zcl_bin_determination_ctrl=>sv_location_mixed.

    IF lv_error EQ abap_true.
      CLEAR ms_deco_pallets-dest_hu.
      finish_function_log(
        EXPORTING
          iv_external_id        = 'Pack and Print HU' ##no_text
          iv_save_only_if_error = abap_false
          iv_display            = abap_true
        IMPORTING
          ev_displayed          = DATA(lv_displayed)
      ).
      IF lv_displayed EQ abap_false.
        RAISE EXCEPTION lx_ws.
      ENDIF.
      IF lx_ws->no_goods_issue EQ abap_false. "Not a goods issue problem
        RETURN.
      ENDIF.
    ENDIF.

    MESSAGE i021(zmc_workstation).
    refresh_current_dlv_data( ).

    "Laast group last warehouse task was processed
    IF lines( mt_proposed_wt ) EQ 1 AND lines( lr_wt_grp->wts ) EQ 1.
      CLEAR ms_deco_pallets-putaway_qty.
      do_reset_proposals( ).
      IF ms_deco_pallets-open_putaway_qty EQ 0.
        TRY.
            get_delivery_items( iv_matnr = ms_mat_global-matnr ).
            CLEAR ms_deco_pallets-ewm_inb_del_item.
          CATCH zcx_workstation ##no_handler.
        ENDTRY.
      ENDIF.
      IF mt_delivery_item IS INITIAL.
        CLEAR ms_deco_pallets-product_ean_mpn.
        clear_data_when_prod_empty( ).
      ENDIF.
    ELSEIF lines( lr_wt_grp->wts ) EQ 1. "The last WT is processed, but here are more groups
      DELETE mt_proposed_wt INDEX mv_wt_grp_index.
      IF lines( mt_proposed_wt ) LT mv_wt_grp_index.
        mv_wt_grp_index = lines( mt_proposed_wt ).
      ENDIF.

      display_wt_grp( ).

    ELSE. "The group is the same, but the next WT must be processed
      DELETE lr_wt_grp->wts INDEX 1.
      SUBTRACT 1 FROM lr_wt_grp->wt_group_data-nof_wts.
      SUBTRACT 1 FROM ms_deco_pallets-nof_wts.

      IF ms_deco_pallets-no_pack_settings EQ abap_true.
        CLEAR ms_deco_pallets-proposed_packing.
      ELSE.
        CLEAR ms_deco_pallets-dest_hu.
      ENDIF.

    ENDIF.


    CALL FUNCTION 'Z_OUT_DISPLAY_PACK_INST'
      EXPORTING
        iv_pack_inst = get_packing_instr( it_matnr_entitled = VALUE #( ( matnr    = ms_deco_pallets-matnr
                                                                         entitled = ms_delivery_item-sapext-entitled ) ) )
        iv_with_hu   = abap_false.


  ENDMETHOD.


  METHOD prev_wt.
**********************************************************************
*    & Key           : LH-160623
*    & Request No.   : GAP-11 – “SUI Deconsolidation”
*    *********************************************************************
*    & Description (short)
*    & display the previous WT group
*    *********************************************************************
    IF mv_wt_grp_index GT 1.

      SUBTRACT 1 FROM mv_wt_grp_index.
      display_wt_grp( ).

    ENDIF.
  ENDMETHOD.


  METHOD product_changed.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Fill product dependent data, and clear if invalidated
**********************************************************************
    clear_data_when_prod_empty( ).
    ms_deco_pallets-lgnum = ms_defaults-lgnum.

    DATA: lv_matnr TYPE matnr.
    "Determine possible material numbers
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = iv_product_ean_mpn
      IMPORTING
        output = lv_matnr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      lv_matnr = iv_product_ean_mpn.
    ENDIF.

    SELECT FROM mara
           FIELDS matnr
           WHERE matnr EQ @lv_matnr
              OR ean11 EQ @iv_product_ean_mpn
              OR mfrpn EQ @iv_product_ean_mpn
           ORDER BY matnr
           INTO TABLE @DATA(lt_matnr_det)
           UP TO 1 ROWS.                               "#EC CI_NOFIELD.
    IF lt_matnr_det IS INITIAL.
      SELECT FROM mean
             FIELDS matnr
             WHERE ean11 EQ @iv_product_ean_mpn
             ORDER BY matnr
             INTO TABLE @lt_matnr_det
             UP TO 1 ROWS.
      IF sy-subrc NE 0.
        mv_md_ok = abap_false.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e010(zmc_workstation) WITH iv_product_ean_mpn.
      ENDIF.
    ENDIF.

    "Get Delivery Items
    lv_matnr = lt_matnr_det[ 1 ]-matnr.

    get_delivery_items( lv_matnr ).
    DATA(lt_dlv_item) = mt_delivery_item.
    IF lines( lt_dlv_item ) NE 1.
      CALL FUNCTION 'Z_INB_GET_DELIVERY_ITEM'
        CHANGING
          ct_delivery_item = lt_dlv_item.
      IF lt_dlv_item IS INITIAL.
        RETURN.
      ENDIF.
    ELSEIF lt_dlv_item IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e012(zmc_workstation) WITH lv_matnr.
    ENDIF.

    ms_deco_pallets-product_ean_mpn = iv_product_ean_mpn.
    ms_delivery_item = lt_dlv_item[ 1 ].


    fill_product_information( ).
    delivery_item_changed( ).

    check_product( ).

  ENDMETHOD.


  METHOD puom_wh_changed.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& If preferred alternative UoM is changed then fill the dependent data
**********************************************************************
    IF iv_uom IS NOT INITIAL.

      SELECT SINGLE FROM t006
             FIELDS dimid
              WHERE msehi EQ @iv_uom
               INTO @DATA(lv_dimid) ##needed.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e037(zmc_workstation) WITH iv_uom.
      ENDIF.

    ENDIF.

    ms_deco_pallets-puom_wh = iv_uom.
    TRY.
        fill_puom_wh_rel_data( ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        CLEAR ms_deco_pallets-puom_wh.
        RAISE EXCEPTION lx_ws.
    ENDTRY.
  ENDMETHOD.


  METHOD refresh_current_dlv_data.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Refresh delivery dependent data
**********************************************************************
    CHECK ms_delivery_item-docid IS NOT INITIAL.

    DATA(lt_deliv_itm) = mo_sp->get_delivery_items( iv_matnr = ms_mat_global-matnr it_docid = VALUE /scwm/dlv_docid_item_tab( ( docid = ms_delivery_item-docid ) ) ).
    IF ms_delivery_item IS INITIAL.
      ms_delivery_item = lt_deliv_itm[ 1 ].
    ENDIF.
    DELETE mt_delivery_item WHERE docid EQ ms_delivery_item-docid AND itemid EQ ms_delivery_item-itemid.
    APPEND ms_delivery_item TO mt_delivery_item.
    get_open_putaway( ).
  ENDMETHOD.


  METHOD reset_proposals.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Reset proposal with confirmation popup
**********************************************************************
    DATA: lv_answer TYPE flag.

    IF mt_proposed_wt IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Reset Proposals'(rep)
          text_question         = 'Delete existing proposals ?'(dee)                 " Question text in dialog box
          text_button_1         = 'Continue'(con)         " Text on the first pushbutton
          text_button_2         = 'Cancel'(can)         " Text on the second pushbutton
          display_cancel_button = abap_false              " Button for displaying cancel pushbutton
        IMPORTING
          answer                = lv_answer                " Return values: '1', '2', 'A'
        EXCEPTIONS
          OTHERS                = 0.
      IF lv_answer NE '1'.
        RETURN.
      ENDIF.
    ENDIF.
    do_reset_proposals( ).
  ENDMETHOD.


  METHOD start.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Start the application
**********************************************************************
    CALL FUNCTION 'Z_INB_WS_START_UI'
      EXPORTING
        io_controller = NEW zcl_ws_deco_ui( ).                 " Workstation UI Main Screen
  ENDMETHOD.


  METHOD start_function_log.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Start function log
**********************************************************************
    mo_function_log = NEW /scwm/cl_log( iv_lgnum     = ms_defaults-lgnum
                                        iv_balobj    = zif_wme_c=>gs_msgobj-zewm
                                        iv_balsubobj = zif_wme_c=>gs_msgsubobj-zworkstation ).
  ENDMETHOD.


  METHOD unlock_delivery_item.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& unlock delivery item
**********************************************************************
    CHECK mv_locked_docid IS NOT INITIAL.

    CALL FUNCTION 'DEQUEUE_/SCDL/E_ID'
      EXPORTING
        mode_/scdl/dl_lock_with_id_str = c_lock_mode_exclusive
        docid                          = mv_locked_docid
        itemid                         = mv_locked_itemid
        _scope                         = c_lock_owner_dialog.

    CLEAR: mv_locked_docid, mv_locked_itemid.

  ENDMETHOD.


  METHOD unlock_stock.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Unlock stock for the processing
**********************************************************************
    DATA lv_current TYPE i.
    IF iv_wt_group IS NOT INITIAL.
      DATA(lv_from) = iv_wt_group.
      DATA(lv_nof_grp) = 1.
    ELSE.
      lv_from = 1.
      lv_nof_grp = lines( mt_proposed_wt ).
    ENDIF.
    lv_current = lv_from - 1.
    DO lv_nof_grp TIMES.
      ADD 1 TO lv_current.
      READ TABLE mt_proposed_wt REFERENCE INTO DATA(lr_prop) INDEX lv_current.
      LOOP AT lr_prop->lock_stbin_y REFERENCE INTO DATA(lr_aqua_y).
        "unlock source and target AQUAY
        CALL FUNCTION 'DEQUEUE_/SCWM/ELAQUAY'
          EXPORTING
            mode_/scwm/aquay = c_lock_mode_shared
            flgmove          = lr_aqua_y->flgmove
            vsi              = lr_aqua_y->vsi
            lgnum            = lr_aqua_y->lgnum
            loc_type         = lr_aqua_y->loc_type
            location         = lr_aqua_y->location
            vhi              = lr_aqua_y->vhi
            guid_stock       = lr_aqua_y->guid_stock
            quan             = lr_aqua_y->quan
            x_flgmove        = 'X'
            x_vsi            = 'X'
            x_huident        = 'X'
            x_vhi            = 'X'
            x_quan           = 'X'
            _synchron        = 'X'.
      ENDLOOP.
      LOOP AT lr_prop->lock_stbin_x REFERENCE INTO DATA(lr_aqua_x).
        "Target AQAUX
        CALL FUNCTION 'DEQUEUE_/SCWM/ELAQUAX'
          EXPORTING
            mode_/scwm/aquax = c_lock_mode_exclusive
            lgnum            = lr_aqua_x->lgnum
            loc_type         = lr_aqua_x->loc_type
            location         = lr_aqua_x->location
            type             = 'I'
            _synchron        = 'X'
          EXCEPTIONS
            OTHERS           = 0.

      ENDLOOP.
      COMMIT WORK.
    ENDDO.

  ENDMETHOD.


  METHOD warehouse_changed.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Warehouse changed: clear dependent data
**********************************************************************
    CLEAR: st_st_bin_type_txt, st_st_type_txt.
  ENDMETHOD.


  METHOD which_area_is_filled.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& check which area is filled on the UI
**********************************************************************
    CLEAR: ev_dummy_hu_prod_delivery, ev_putaway_input, ev_proposal,
           ev_hu, ev_any_of_them.

    IF ms_deco_pallets-gr_dummy_hu IS NOT INITIAL
       OR ms_deco_pallets-product_ean_mpn IS NOT INITIAL
       OR ms_deco_pallets-ewm_inb_del_item IS NOT INITIAL.
      ev_dummy_hu_prod_delivery = abap_true.
    ENDIF.
    IF ms_deco_pallets-expiration_date IS NOT INITIAL
       OR ms_deco_pallets-putaway_qty IS NOT INITIAL
       OR ms_deco_pallets-puom_wh IS NOT INITIAL.
      ev_dummy_hu_prod_delivery = abap_true.
      ev_putaway_input = abap_true.
    ENDIF.
    IF mt_proposed_wt IS NOT INITIAL.
      ev_dummy_hu_prod_delivery = abap_true.
      ev_putaway_input = abap_true.
      ev_proposal = abap_true.
    ENDIF.
    LOOP AT mt_proposed_wt REFERENCE INTO DATA(lr_prop).
      IF lr_prop->hus IS NOT INITIAL.
        ev_hu = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF ev_dummy_hu_prod_delivery IS NOT INITIAL
       OR ev_putaway_input IS NOT INITIAL
       OR ev_proposal IS NOT INITIAL
       OR ev_hu IS NOT INITIAL.
      ev_any_of_them = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD wt_list.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Display proposed warehouse tasks
**********************************************************************
    DATA: BEGIN OF ls_data,
            nlpla TYPE /scwm/ltap_nlpla,
          END OF ls_data,
          lt_data LIKE STANDARD TABLE OF ls_data.

    IF mv_wt_grp_index IS NOT INITIAL.
      TRY.
          MOVE-CORRESPONDING mt_proposed_wt[ mv_wt_grp_index ]-wts TO lt_data.
        CATCH cx_sy_itab_line_not_found.
          RETURN.
      ENDTRY.
    ENDIF.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_table)
          CHANGING
            t_table      = lt_data ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
        RETURN.
    ENDTRY.

    lo_table->set_screen_popup(
      start_column = 1
      end_column   = 50
      start_line   = 1
      end_line     = 20 ).


*... §5 display the table
    lo_table->display( ).
  ENDMETHOD.


  METHOD zif_ws_deco_ui~check_hu.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Check HU existence and for inbound shipment number
**********************************************************************
    CHECK iv_huident IS NOT INITIAL.

    mo_sp->check_hu(
      EXPORTING
        iv_huident = iv_huident                 " Handling Unit Identification
        iv_inbship = CONV #( ms_deco_pallets-inbound_shipment )                " Inbound Shipment Number (IS)
    ).

  ENDMETHOD.


  METHOD zif_ws_deco_ui~check_target_stype_for_chprop.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Check Monopallet/Mixed pallet allowance
**********************************************************************
    SELECT SINGLE * FROM ztinb_curst_styp
           INTO  @DATA(ls_cust)
           WHERE lgnum EQ @ms_defaults-lgnum
             AND lgtyp EQ @iv_nltyp.
    IF mv_mono_pallet EQ abap_true AND ls_cust-change_stype_mono IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e042(zmc_workstation) WITH iv_nltyp.
    ENDIF.
    IF mv_mono_pallet EQ abap_false AND ls_cust-change_stype_mixed IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e042(zmc_workstation) WITH iv_nltyp.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_deco_ui~get_pack_mat_dat_forf4.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Packing material value help
**********************************************************************
    TRY.
        DATA(ls_prod) = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod(  iv_prodno    = iv_pack_mat ).

      CATCH /scmb/cx_md_access. " Exception Class for Master Data Accesses
        RETURN.
    ENDTRY.

    ev_pack_mat_id = ls_prod-prodid.
    TRY.
        mo_sp->get_pack_mat_data(
          EXPORTING
            iv_pack_mat_id   = ls_prod-prodid                 " Packaging Material
            iv_entitled      = ms_delivery_item-sapext-entitled                 " Party Entitled to Dispose
            iv_nltyp         = ms_deco_pallets-nltyp                 " Destination Storage Type
          IMPORTING
             es_pmat_setting  = es_pmat_setting                  " Workstation Packaging Settings
             ev_pack_mat_text = ev_pack_mat_text                 " Material Description
             ev_hutype        = ev_hutype                        " Handling Unit Type
             ev_hutyptext     = ev_hutyptext                     " Description of Handling Unit Type
             ev_nr_internal   = ev_nr_internal                   " General Flag
        ).
      CATCH zcx_workstation ##no_handler. "This is intended for F4 ->no error message should displayed here
    ENDTRY.
  ENDMETHOD.


  METHOD zif_ws_deco_ui~init_screen.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Initialize UI
**********************************************************************
    mo_ctrl_curr_stock_sit = io_curr_stock_sit.
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = mo_ctrl_curr_stock_sit                          " Abstract Container for GUI Controls
            container_name = mo_ctrl_curr_stock_sit->get_name( )
          IMPORTING
            r_salv_table   = mo_alv_curr_stock_sit                          " Basis Class Simple ALV Tables
          CHANGING
            t_table        = mt_curr_stock_sit
        ).


        mo_alv_curr_stock_sit->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>none ).
        mo_alv_curr_stock_sit->display( ).
        mo_alv_curr_stock_sit->get_functions( )->set_all( abap_false ).
      CATCH cx_salv_msg ##no_handler. "May never happen, only if there is some basic GUI problem
    ENDTRY.
  ENDMETHOD.


  METHOD zif_ws_deco_ui~is_workstation_mixedpallet.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Is Wirkstation a mixed pallet workstation
**********************************************************************
    rv_mixed = mo_sp->is_workstation_mixedpallet( ).
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~get_status.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get UI Status with exluded functions
**********************************************************************
    ev_status = c_main_status.
    et_excludes = mt_excludes.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~get_title.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get titel bar and text
**********************************************************************
    ev_title = c_main_tittle.
    MESSAGE i003(zmc_workstation) WITH ms_defaults-lgnum ms_defaults-workst_loc INTO ev_param.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~init.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Initialize the subscreen
**********************************************************************

    mo_general_func->zif_ws_ui_defaults~get_defaults(
      IMPORTING
        es_defaults = ms_defaults
    ).
    ev_default_needed = xsdbool( NOT mo_general_func->zif_ws_ui_defaults~is_defaults_sufficient( ) ).
    IF ev_default_needed IS INITIAL.
      TRY.
          zif_ws_ui_defaults~set_defaults( is_defaults = ms_defaults ).
        CATCH zcx_workstation. " Workstation errors
          CLEAR ms_defaults.
          ev_default_needed = abap_true.
      ENDTRY.
    ENDIF.
    ev_subscreen_no = c_sub_deco_pallets.
    ev_tab = c_tab_deco_pallets.
    ev_subscreen_prg = c_subscreen_repid.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~leave_screen.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Check whether screen can be left
**********************************************************************
    rv_leave = abap_true.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~pai_tab.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& PAI fo the deconsoladation tab screen
**********************************************************************
    IF iv_ok_code IS INITIAL.
      mv_enter_pressed = abap_true.
    ELSE.
      mv_enter_pressed = abap_false.
    ENDIF.

    DATA ls_deco_pallets TYPE zstr_ws_decon_pallets.
    ls_deco_pallets = is_screen_data.
    DATA(ls_dp) = ms_deco_pallets.
    IF ls_deco_pallets-gr_dummy_hu NE ms_deco_pallets-gr_dummy_hu.
      gr_dummy_hu_changed( ls_deco_pallets-gr_dummy_hu ).
    ELSEIF ls_deco_pallets-product_ean_mpn NE ms_deco_pallets-product_ean_mpn AND mv_enter_pressed EQ abap_true.
      product_changed( ls_deco_pallets-product_ean_mpn ).
    ELSEIF ls_deco_pallets-puom_wh NE ls_dp-puom_wh.
      puom_wh_changed( ls_deco_pallets-puom_wh ).
    ENDIF.


    IF ls_deco_pallets-expiration_date NE ls_dp-expiration_date.
      ms_deco_pallets-expiration_date = ls_deco_pallets-expiration_date.
    ENDIF.
    IF ls_deco_pallets-putaway_qty NE ls_dp-putaway_qty.
      ms_deco_pallets-putaway_qty = ls_deco_pallets-putaway_qty.
    ENDIF.

    IF ls_deco_pallets-pmat NE ls_dp-pmat.
      packaging_mat_changed( ls_deco_pallets-pmat ).
      IF mv_internal_hu_nr EQ abap_true.
        CLEAR ms_deco_pallets-dest_hu.
      ENDIF.
    ENDIF.
    IF ls_deco_pallets-dest_hu NE ls_dp-dest_hu AND mv_enter_pressed EQ abap_true.
      dest_hu_changed( ls_deco_pallets-dest_hu ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~pbo_tab.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& PBO of the tab screen for Deconsolidation
**********************************************************************
    CLEAR mt_excludes.

    IF ms_deco_pallets-product_ean_mpn IS INITIAL.
      CLEAR ms_deco_pallets-md_check.
    ELSEIF mv_md_ok EQ abap_true.
      ms_deco_pallets-md_check = icon_okay.
    ELSE.
      ms_deco_pallets-md_check = icon_cancel.
    ENDIF.

    TRY.
        refresh_current_dlv_data( ).
      CATCH zcx_workstation ##no_handler.
    ENDTRY.

    DATA(lv_exp_date_input) = abap_true.
    DATA(lv_ext_hu_input) = abap_true.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PRD'.
          IF ms_deco_pallets-gr_dummy_hu IS INITIAL.
            screen-input = '0'.
            APPEND c_func_fx_change_prod TO mt_excludes.
          ENDIF.
        WHEN 'MD'.
          IF mv_md_ok EQ abap_false.
            screen-input = '0'.
            APPEND c_func_next_wt TO mt_excludes.
            APPEND c_func_previous_wt TO mt_excludes.
            APPEND c_func_display_hus TO mt_excludes.
            APPEND c_func_change_proposal TO mt_excludes.
            APPEND c_func_reset_proposal TO mt_excludes.
            APPEND c_func_pack_print_hu TO mt_excludes.
            APPEND c_func_save_expire_date TO mt_excludes.
            APPEND c_func_create_proposal TO mt_excludes.
            APPEND c_func_reset_proposal TO mt_excludes.
          ELSEIF ms_deco_pallets-ewm_inb_del_item IS INITIAL.
            IF screen-name EQ 'BTN_FX_CHANGE'.
              IF mt_delivery_item IS INITIAL.
                screen-input = '0'.
                APPEND c_func_fx_change_deliv TO mt_excludes.
              ENDIF.
            ELSE.
              screen-input = '0'.
              APPEND c_func_save_expire_date TO mt_excludes.
              APPEND c_func_create_proposal TO mt_excludes.
              APPEND c_func_reset_proposal TO mt_excludes.
            ENDIF.
          ELSE.
            IF ( ms_mat_global-sled_bbd IS INITIAL OR ms_delivery_item-sapext-bestbefore_date IS NOT INITIAL )
               AND ( screen-name EQ 'ZSTR_WS_DECON_PALLETS-EXPIRATION_DATE'
                  OR screen-name EQ 'BTN_EXP_SAVE' ).
              screen-input = '0'.
              lv_exp_date_input = abap_false.
              APPEND c_func_save_expire_date TO mt_excludes.
            ENDIF.
            IF ( ms_mat_lgnum-zz1_keepcar_whd EQ abap_false
                 OR mt_proposed_wt IS NOT INITIAL )
               AND screen-name EQ 'ZSTR_WS_DECON_PALLETS-PUOM_WH'.
              screen-input = '0'.
            ENDIF.
            IF screen-name EQ 'ZSTR_WS_DECON_PALLETS-PUTAWAY_QTY'
               AND mt_proposed_wt IS NOT INITIAL.
              screen-input = '0'.
              APPEND c_func_create_proposal TO mt_excludes.
            ENDIF.
          ENDIF.
        WHEN 'PRO' OR 'PCK'.
          IF mt_proposed_wt IS INITIAL OR mv_md_ok EQ abap_false.
            screen-input = '0'.
            APPEND c_func_next_wt TO mt_excludes.
            APPEND c_func_previous_wt TO mt_excludes.
            APPEND c_func_display_hus TO mt_excludes.
            APPEND c_func_change_proposal TO mt_excludes.
            APPEND c_func_reset_proposal TO mt_excludes.
            APPEND c_func_pack_print_hu TO mt_excludes.
          ELSE.
            IF screen-name EQ 'ZSTR_WS_DECON_PALLETS-PMAT' AND mv_pmat_can_be_changed EQ abap_false.
              screen-input = '0'.
            ENDIF.
            IF ( ms_deco_pallets-nr_internal EQ abap_true
                 AND ms_deco_pallets-no_pack_settings EQ abap_false )
               AND
               (    screen-name EQ 'ZSTR_WS_DECON_PALLETS-DEST_HU'
                 OR screen-name EQ 'BTN_DISPLAY_HUS' ).
              screen-input = '0'.
              APPEND c_func_display_hus TO mt_excludes.
              lv_ext_hu_input = abap_false.
            ENDIF.
            IF screen-name EQ 'ZSTR_WS_DECON_PALLETS-DEST_HU'
               AND ms_deco_pallets-dest_hu IS NOT INITIAL.
              screen-input = '0'.
            ENDIF.
            IF screen-name EQ 'BTN_PREVIOUS_WT' AND mv_wt_grp_index LE 1 .
              screen-input = '0'.
              APPEND c_func_previous_wt TO mt_excludes.
            ENDIF.
            IF screen-name EQ 'BTN_NEXT_WT' AND mv_wt_grp_index GE lines( mt_proposed_wt ) .
              screen-input = '0'.
              APPEND c_func_next_wt TO mt_excludes.
            ENDIF.
          ENDIF.
        WHEN 'TOT'.
          IF mv_displ_pc_per_tot EQ abap_false.
            screen-active = '0'.
          ENDIF.
      ENDCASE.
      IF screen-name EQ 'BTN_DISPLAY_PRD' AND ms_deco_pallets-matnr IS INITIAL.
        APPEND zcl_ws_main=>c_func_display_prodct TO mt_excludes.
        screen-input = '0'.
      ENDIF.
      IF mv_hu_unknown EQ abap_true.
        CASE screen-name.
          WHEN 'ZSTR_WS_DECON_PALLETS-GR_DUMMY_HU' OR 'BTN_FX_DELETE_HU'.
            screen-input = '0'.
        ENDCASE.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
    IF ms_deco_pallets-gr_dummy_hu IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WS_DECON_PALLETS-GR_DUMMY_HU'.
    ELSEIF ms_deco_pallets-product_ean_mpn IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WS_DECON_PALLETS-PRODUCT_EAN_MPN'.
    ELSEIF lv_exp_date_input EQ abap_true AND ms_deco_pallets-expiration_date IS INITIAL AND ms_deco_pallets-nof_wts IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WS_DECON_PALLETS-EXPIRATION_DATE'.
    ELSEIF ms_deco_pallets-putaway_qty IS INITIAL AND mt_proposed_wt IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WS_DECON_PALLETS-PUTAWAY_QTY'.
    ELSEIF lv_ext_hu_input EQ abap_true.
      SET CURSOR FIELD 'ZSTR_WS_DECON_PALLETS-DEST_HU'.
    ELSEIF mv_pmat_can_be_changed EQ abap_true.
      SET CURSOR FIELD 'ZSTR_WS_DECON_PALLETS-PMAT'.
    ENDIF.

    es_screen_data = ms_deco_pallets.
    SORT mt_excludes.
    DELETE ADJACENT DUPLICATES FROM mt_excludes.
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~process_user_command.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Perform user activities
**********************************************************************
    DATA lv_answer TYPE c LENGTH 1.

    CLEAR: es_bapiret, ev_leave_screen.


    TRY.
        CASE iv_ucomm.
          WHEN c_func_display_log.
            display_log( ).

          WHEN c_func_wt_list.
            wt_list( ).
          WHEN c_func_leave.
            LEAVE TO SCREEN 0.

          WHEN c_func_defaults.
            zif_ws_ui_defaults~call_screen( ).

          WHEN c_func_fx_delete_hu.
            delete_hu( ).

          WHEN c_func_fx_change_deliv.
            change_delivery_item( ).


          WHEN c_func_fx_change_prod.
            IF ms_deco_pallets-product_ean_mpn IS INITIAL.
              CLEAR ms_deco_pallets-gr_dummy_hu.
              clear_data_when_hu_empty( ).
            ELSE.
              CALL FUNCTION 'POPUP_TO_CONFIRM'
                EXPORTING
                  titlebar              = 'Reset Fields'(rfi)
                  text_question         = 'Selected product is not fully processed. Do you want to select new one?'(snp)                 " Question text in dialog box
                  text_button_1         = 'Select new'(sne)         " Text on the first pushbutton
                  text_button_2         = 'Use current'(cwc)         " Text on the second pushbutton
                  display_cancel_button = abap_false              " Button for displaying cancel pushbutton
                IMPORTING
                  answer                = lv_answer                " Return values: '1', '2', 'A'
                EXCEPTIONS
                  OTHERS                = 0.
              IF lv_answer EQ '1'.
                clear_data_when_prod_empty( ).
              ENDIF.
            ENDIF.

          WHEN c_func_save_expire_date.
            change_delivery_data( ).

          WHEN c_func_create_proposal.
            create_proposal( ).

          WHEN c_func_reset_proposal.
            reset_proposals( ).

          WHEN c_func_pack_print_hu.
            pack_print_hu( ).

          WHEN c_func_next_wt.
            next_wt( ).

          WHEN c_func_previous_wt.
            prev_wt( ).

          WHEN c_func_display_hus.
            display_hus( ).

          WHEN c_func_change_proposal.
            change_proposal( ).

          WHEN c_func_change_hus.
            change_hus( ).

          WHEN zcl_ws_main=>c_func_display_prodct.
            display_product( ).
        ENDCASE.

      CATCH zcx_workstation INTO DATA(lx_ws).
        MESSAGE lx_ws TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~call_screen.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Call the defaults pop-up screen
**********************************************************************
    DATA: ls_def_orig TYPE zstr_ws_defaults,
          ls_def_new  TYPE zstr_ws_defaults.
    mo_general_func->zif_ws_ui_defaults~get_defaults(
      IMPORTING
        es_defaults = ls_def_orig
    ).
    mo_general_func->zif_ws_ui_defaults~call_screen( ).
    mo_general_func->zif_ws_ui_defaults~get_defaults(
      IMPORTING
        es_defaults = ls_def_new
    ).
    IF ls_def_new NE ls_def_orig.
      TRY.
          zif_ws_ui_defaults~set_defaults( is_defaults = ls_def_new ).
        CATCH zcx_workstation INTO DATA(lo_cx). " Workstation errors
          MESSAGE lo_cx TYPE 'I' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~get_defaults.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the default screen data
**********************************************************************
    mo_general_func->zif_ws_ui_defaults~get_defaults(
      IMPORTING
        es_defaults = es_defaults
    ).
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~get_default_screen_no.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Get the screen and screen program for default setting popup
**********************************************************************
    ev_dynnr = zcl_general_ui_functions=>c_default_screen.
    ev_repid = zcl_general_ui_functions=>c_default_screen_prog.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~is_defaults_sufficient.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Check whether the defaults are filled completely
**********************************************************************
    rv_yes = mo_general_func->zif_ws_ui_defaults~is_defaults_sufficient( ).
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~set_defaults.
**********************************************************************
*& Key           : LH-160623
*& Request No.   : GAP-11 – “SUI Deconsolidation”
**********************************************************************
*& Description (short)
*& Set defaults from the UI, but first check , whether the screen
*& can be left.
**********************************************************************

    DATA: lv_answer TYPE c LENGTH 1.
    DATA ls_defaults TYPE zstr_ws_defaults .
    ls_defaults = is_defaults.

    which_area_is_filled(
     IMPORTING
       ev_any_of_them = DATA(lv_filled)  ).
    IF lv_filled EQ abap_true AND ls_defaults NE ms_defaults.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = TEXT-rfi "'Set defaults'
          text_question         = 'No data previously entered on main screen will be saved.'(nds)                 " Question text in dialog box
          text_button_1         = 'Continue'(con)         " Text on the first pushbutton
          text_button_2         = 'Cancel'(can)         " Text on the second pushbutton
          display_cancel_button = abap_false              " Button for displaying cancel pushbutton
        IMPORTING
          answer                = lv_answer                " Return values: '1', '2', 'A'
        EXCEPTIONS
          OTHERS                = 0.
      IF lv_answer EQ '1'.
        clear_data_when_hu_empty( ).
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    mo_general_func->zif_ws_ui_defaults~set_defaults( is_defaults = is_defaults ).
    mv_mono_pallet = xsdbool( NOT mo_sp->is_workstation_mixedpallet( ) ).
    mo_general_func->zif_ws_ui_defaults~get_defaults(
      IMPORTING
        es_defaults = ms_defaults
    ).

    IF ms_defaults-lgnum NE ls_defaults-lgnum.
      warehouse_changed( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
