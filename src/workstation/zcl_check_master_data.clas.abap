CLASS zcl_check_master_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS check_product IMPORTING !iv_matnr     TYPE matnr
                                    !iv_warehouse TYPE /scwm/lgnum
                                    !iv_entitled  TYPE /scwm/de_entitled OPTIONAL
                          RETURNING VALUE(rv_ok)  TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_material TYPE matnr.
    DATA mv_warehouse_number TYPE /scwm/lgnum.
    DATA mv_entitled TYPE /scwm/de_entitled.
    DATA mv_material_guid TYPE /sapapo/matid.
    DATA mv_entitled_guid TYPE /scwm/de_entitled_id.

    METHODS skip_check RETURNING VALUE(rv_skip_check) TYPE abap_bool.
    METHODS read_guids RETURNING VALUE(rv_success) TYPE abap_bool.
    METHODS check_warehouse_product RETURNING VALUE(rv_success) TYPE abap_bool.
    METHODS check_replenishment_data RETURNING VALUE(rv_success) TYPE abap_bool.
    METHODS check_unit_of_measure RETURNING VALUE(rv_success) TYPE abap_bool.
    METHODS check_serial_number_obligation RETURNING VALUE(rv_success) TYPE abap_bool.
    METHODS check_packeting_specifications RETURNING VALUE(rv_success) TYPE abap_bool.
ENDCLASS.



CLASS ZCL_CHECK_MASTER_DATA IMPLEMENTATION.


  METHOD check_packeting_specifications.
**********************************************************************
*& Key           : AD-230915
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Check, if packeting specification is missing
*&
**********************************************************************
    DATA lt_ps_keys TYPE /scwm/tt_ps_header_key.
    DATA lt_packspec_content TYPE /scwm/tt_packspec_nested.

    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = VALUE /scwm/s_ps_content_query( matnr_rng = VALUE #( ( sign   = wmegc_sign_inclusive
                                                                                  option = wmegc_option_eq
                                                                                  low    = mv_material ) ) )
        it_psid_rng      = VALUE /scwm/tt_ps_psid_rtab( )
      IMPORTING
        et_ps_keys       = lt_ps_keys.

    CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
      EXPORTING
        iv_guid_ps          = VALUE #( lt_ps_keys[ 1 ]-guid_ps OPTIONAL )
        iv_read_elements    = abap_true
        iv_no_buffer        = abap_false
      IMPORTING
        et_packspec_content = lt_packspec_content
      EXCEPTIONS
        error               = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      " Couldn't read packspec
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF NOT line_exists( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ] ).
      " Packspec is missing
      rv_success = abap_false.
      RETURN.
    ENDIF.

    rv_success = abap_true.
  ENDMETHOD.


  METHOD check_product.
**********************************************************************
*& Key           : AD-230913
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Checks the material master data of the providied material and
*& detemrines, whether the master data is complete (rv_ok = abap_true) or not
*& (rv_ok = abap_false)
**********************************************************************
    mv_material = iv_matnr.
    mv_warehouse_number = iv_warehouse.
    mv_entitled = iv_entitled.

    IF skip_check( ) = abap_true.
      rv_ok = abap_true.
      RETURN.
    ENDIF.

    IF read_guids( ) = abap_false.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    IF check_warehouse_product( ) = abap_false.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    IF check_replenishment_data( ) = abap_false.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    IF check_unit_of_measure( ) = abap_false.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    IF check_serial_number_obligation( ) = abap_false.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    IF check_packeting_specifications( ) = abap_false.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " All checks passed sccusessfully
    rv_ok = abap_true.
  ENDMETHOD.


  METHOD check_replenishment_data.
**********************************************************************
*& Key           : AD-230915
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Check, if the replemnishment data is missing or is incomplete
*&
**********************************************************************
    DATA lt_matlwhst_key TYPE /sapapo/dm_matlwhst_id_tab.
    DATA lt_locprodwhst TYPE /sapapo/dm_matlwhst_tab.

    lt_matlwhst_key = VALUE /sapapo/dm_matlwhst_id_tab( ( matid = mv_material_guid
                                                              entitled_id = mv_entitled_guid ) ).
    CALL FUNCTION '/SAPAPO/MATLWHST_READ_MULTI_2'
      EXPORTING
        it_key              = lt_matlwhst_key
      IMPORTING
        et_locprodwhst      = lt_locprodwhst
      EXCEPTIONS
        data_not_found      = 1
        interface_incorrect = 2
        error_message       = 3
        OTHERS              = 99.

    IF sy-subrc <> 0.
      " Couldn't read replenishemnt data
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF lines( lt_locprodwhst ) = 0.
      " Not replenishment data found
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " Check, if MINQTY, REPQTY or MAXQTY are mssing
    IF lt_locprodwhst[ 1 ]-minqty IS INITIAL OR
       lt_locprodwhst[ 1 ]-repqty IS INITIAL OR
       lt_locprodwhst[ 1 ]-maxqty IS INITIAL.
      " MINQTY (Minimum Quantity ) or REPQTY(Minimum Replenishment Quantity )
      " or MAXQTY(Maximum Quantity ) is empty
      rv_success = abap_false.
      RETURN.
    ENDIF.

    rv_success = abap_true.
  ENDMETHOD.


  METHOD check_serial_number_obligation.
**********************************************************************
*& Key           : AD-230915
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Check, if Serial number obligation is consistent
*&
**********************************************************************
    DATA lv_serial TYPE zde_serial_check.

    " Get product hierarchy and manufacturer
    SELECT SINGLE prdha, mfrnr
      INTO (@DATA(lv_prdha), @DATA(lv_mfrnr) )
      FROM mara
     WHERE matnr = @mv_material.

    IF sy-subrc <> 0.
      " Couldn't read material data
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " Check, if customizing on hierarchy and manufacturer is maintained
    SELECT SINGLE zzserial
      INTO @lv_serial
      FROM ztcross_serialch
     WHERE entitled = @mv_entitled
       AND prdha = @lv_prdha
       AND mfrnr = @lv_mfrnr.

    IF sy-subrc <> 0.
      " Check, if customizing on hierarchy is maintained
      SELECT SINGLE zzserial
        INTO @lv_serial
        FROM ztcross_serialch
       WHERE entitled = @mv_entitled
         AND prdha = @lv_prdha.

      IF sy-subrc <> 0.
        " Check, if customizing on manufacturer is maintained
        SELECT SINGLE zzserial
          INTO @lv_serial
          FROM ztcross_serialch
         WHERE entitled = @mv_entitled
           AND mfrnr = @lv_mfrnr.
      ENDIF.
    ENDIF.

    DATA(lo_erp_plant) = /scwm/cl_erp_plant=>get_instance( ).
    TRY.
        lo_erp_plant->get_plant_by_entitled( EXPORTING iv_entitled = mv_entitled
                                             IMPORTING ev_erp_plant = DATA(lv_plant)  ).
      CATCH  /scwm/cx_stockid_map  INTO DATA(lx_stockid_map).
        rv_success = abap_false.
        RETURN.
    ENDTRY.


    SELECT SINGLE zz1_identtable01_plt, zz1_reasoncode1_plt
      INTO ( @DATA(lv_identtable01_plt), @DATA(lv_reasoncode1_plt) )
      FROM marc
     WHERE matnr = @mv_material
       AND werks = @lv_plant. " <=== DoTo: How to determine the plant?

    IF lv_serial <> '2' OR lv_identtable01_plt <> '001'.
      IF lv_reasoncode1_plt IS INITIAL.
        " Serial number obligation not consistent
        rv_success = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    rv_success = abap_true.
  ENDMETHOD.


  METHOD check_unit_of_measure.
**********************************************************************
*& Key           : AD-230915
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Check, if UOM data for the mastercarton / tote level is missing
*&
**********************************************************************
    DATA lt_matid TYPE TABLE OF /sapapo/dm_material_str.
    DATA lt_mean TYPE TABLE OF /sapapo/mean_str.

    APPEND VALUE /sapapo/dm_material_str( matid = mv_material_guid  ) TO lt_matid.
    CALL FUNCTION '/SAPAPO/DM_PRODUCTS_READ'
      TABLES
        it_matid      = lt_matid
        et_mean       = lt_mean
      EXCEPTIONS
        not_qualified = 1
        error_message = 2
        OTHERS        = 99.

    IF sy-subrc <> 0.
      " Couldn't tread Outbound module
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF NOT line_exists( lt_mean[ matid = mv_material_guid meinh = 'PC' ] ) AND
       NOT line_exists( lt_mean[ matid = mv_material_guid meinh = 'ST' ] ) AND
       NOT line_exists( lt_mean[ matid = mv_material_guid meinh(1) = 'Z' ] ).
      " No UOM data for the mastercarton / tote level found
      rv_success = abap_false.
      RETURN.
    ENDIF.

    rv_success = abap_true.
  ENDMETHOD.


  METHOD check_warehouse_product.
**********************************************************************
*& Key           : AD-230915
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Check, if warehouse porduct for the material is missing or is incomplete
*&
**********************************************************************
    DATA lt_matlwh_key TYPE /sapapo/dm_matlwh_id_tab.
    DATA lt_locprodwh TYPE /sapapo/dm_matlwh_tab.

    lt_matlwh_key = VALUE /sapapo/dm_matlwh_id_tab( ( matid  = mv_material_guid
                                                      entitled_id = mv_entitled_guid ) ).
    CALL FUNCTION '/SAPAPO/MATLWH_READ_MULTI_2'
      EXPORTING
        it_key              = lt_matlwh_key
      IMPORTING
        et_locprodwh        = lt_locprodwh
      EXCEPTIONS
        data_not_found      = 1
        interface_incorrect = 2
        error_message       = 3
        OTHERS              = 99.

    IF sy-subrc <> 0.
      " Couldn't read warehouse product
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF lines( lt_locprodwh ) = 0.
      " No warehouse product found
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF lt_locprodwh[ 1 ]-put_stra IS INITIAL OR
       lt_locprodwh[ 1 ]-lgbkz IS INITIAL.
      " PUT_STRA (Putaway Control) or LGBKZ(Storage Section Indicator) is empty
      rv_success = abap_false.
      RETURN.
    ENDIF.

    rv_success = abap_true.
  ENDMETHOD.


  METHOD read_guids.
**********************************************************************
*& Key           : AD-230915
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determines the required GUIDs for further processing
*&
**********************************************************************
    DATA lt_bapi_return TYPE TABLE OF bapiret2.

    " Determine guid for the provided material number
    CALL FUNCTION '/SAPAPO/DM_MATERIAL_GET_MATID'
      EXPORTING
        iv_matnr        = mv_material
      IMPORTING
        ev_matid        = mv_material_guid
      EXCEPTIONS
        matid_not_found = 1
        error_message   = 2
        OTHERS          = 99.

    IF sy-subrc <> 0.
      " Couldn't read general material master data
      CLEAR mv_material_guid.
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " Determine guid for entitled to dispose
    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner      = mv_entitled
      IMPORTING
        ev_partner_guid = mv_entitled_guid
      TABLES
        et_return       = lt_bapi_return.

    IF sy-subrc <> 0.
      " Couldn't determine the business partner guid for the entitled to
      CLEAR mv_entitled_guid.
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF line_exists( lt_bapi_return[ type = 'E' ] ) OR
       line_exists( lt_bapi_return[ type = 'A' ] ).
      CLEAR mv_entitled_guid.
      rv_success = abap_false.
      RETURN.
    ENDIF.

    rv_success = abap_true.
  ENDMETHOD.


  METHOD skip_check.
**********************************************************************
*& Key           : AD-230915
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Checks the switch framework to determines, if the check should be
*& skiped for the current material.
**********************************************************************
    DATA lv_master_switch TYPE abap_bool.
    DATA lv_material_switch TYPE abap_bool.

    lv_master_switch = zcl_switch=>get_switch_state( iv_lgnum = mv_warehouse_number
                                                     iv_devid = zif_switch_const=>c_zinb_004  ).

    IF lv_master_switch = abap_false.
      " If the master switch is turned off, skip the master data check.
      rv_skip_check = abap_true.
      RETURN.
    ENDIF.

    DATA(lt_switch_value_sets) = VALUE ztt_switch_fields( ( field = zif_switch_const=>c_material_number
                                                            field_value = |{ mv_material ALPHA = OUT }| ) ).

    lv_material_switch = zcl_switch=>get_switch_state( iv_lgnum = mv_warehouse_number
                                                             iv_devid = zif_switch_const=>c_zinb_004
                                                             it_fields = lt_switch_value_sets ).

    IF lv_material_switch = abap_true.
      " If the switch on material level is turned on, skip the master data check for this specific material.
      rv_skip_check = abap_true.
      RETURN.
    ENDIF.

    rv_skip_check = abap_false.
  ENDMETHOD.
ENDCLASS.
