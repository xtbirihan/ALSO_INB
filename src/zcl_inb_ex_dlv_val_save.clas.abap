CLASS zcl_inb_ex_dlv_val_save DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_dlv_val_save .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPE-POOLS wmegc.

    CONSTANTS c_multiplier_even TYPE int8 VALUE 1 ##NO_TEXT.
    CONSTANTS c_multiplier_odd TYPE int8 VALUE 3 ##NO_TEXT.
    DATA gv_warehouse_number TYPE /scwm/lgnum .
    DATA go_bom TYPE REF TO /scdl/cl_bo_management .

    METHODS check_is_needed
      IMPORTING
        !iv_docid        TYPE /scdl/dl_docid
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    METHODS excecute_barcode_check IMPORTING !iv_barcode       TYPE zde_barcode
                                             !io_bo            TYPE REF TO /scdl/if_bo
                                   RETURNING VALUE(rv_message) TYPE bapi_msg .
    METHODS execute_inbound_shipment_check IMPORTING iv_inbound_shipment TYPE zde_inbship
                                           RETURNING VALUE(rv_message)   TYPE bapi_msg.
ENDCLASS.



CLASS ZCL_INB_EX_DLV_VAL_SAVE IMPLEMENTATION.


  METHOD /scwm/if_ex_dlv_val_save~check_header.
**********************************************************************
*& Key           : AD-230307
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_DLV_VAL_SAVE
*& Checks, whether a validation must be performed
**********************************************************************
    BREAK-POINT ID zcg_ex_dlv_val_save.

    " Get warehouse number from global settings
    gv_warehouse_number = /scwm/cl_tm=>sv_lgnum.

    " Get BO manager
    go_bom = /scdl/cl_bo_management=>get_instance( ).
    IF go_bom IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT it_keys REFERENCE INTO DATA(lo_key).
      IF check_is_needed( iv_docid = lo_key->docid ) = abap_true.
        APPEND lo_key->* TO ct_relevant_keys.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD /scwm/if_ex_dlv_val_save~execute_header.
**********************************************************************
*& Key           : AD-230307
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_DLV_VAL_SAVE
*& Performs the validation
**********************************************************************
*    DATA: lv_dummymsg TYPE bapi_msg ##NEEDED.

    DATA: lo_bo TYPE REF TO /scdl/if_bo.
    DATA: lo_header TYPE REF TO /scdl/cl_dl_header_prd.

    BREAK-POINT ID zcg_ex_dlv_val_save.

    " Get BO manager
    go_bom = /scdl/cl_bo_management=>get_instance( ).
    IF go_bom IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT it_relevant_keys REFERENCE INTO DATA(lo_key).
      lo_bo ?= go_bom->get_bo_by_id( iv_docid = lo_key->docid ).
      IF lo_bo IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Get Header (Current Version)
      lo_header ?= lo_bo->get_header( ).
      IF lo_header IS NOT BOUND.
        CONTINUE.
      ENDIF.

      DATA(ls_eew) = lo_header->/scdl/if_dl_head_prd_readonly~get_eew( ).

      " Field Check
      DATA(lv_message) = excecute_barcode_check( iv_barcode =  ls_eew-zzbarcode
                                                 io_bo = lo_bo ).
      IF lv_message IS NOT INITIAL.
        ct_messages = VALUE #(
                           BASE ct_messages (
                               msg = /scwm/cl_dm_message_no=>get_symsg_fields( )
                               docid = lo_key->docid
                              ) ).
      ENDIF.

      DATA(lv_message_is) = execute_inbound_shipment_check( iv_inbound_shipment = ls_eew-zzinbship ).

      IF lv_message_is IS NOT INITIAL.
        ct_messages = VALUE #(
                           BASE ct_messages (
                               msg = /scwm/cl_dm_message_no=>get_symsg_fields( )
                               docid = lo_key->docid
                              ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_is_needed.
**********************************************************************
*& Key           : AD-230307
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_DLV_VAL_SAVE
*& Determines, whether the checks should run
**********************************************************************
    DATA lv_master_switch_active TYPE abap_bool.
    DATA lv_detail_switch_active TYPE abap_bool.
    DATA lo_bo  TYPE REF TO /scdl/if_bo.
    DATA lo_header TYPE REF TO /scdl/cl_dl_header_prd.
    DATA lo_header_old TYPE REF TO /scdl/cl_dl_header_prd.

    rv_result = abap_false.

    " Check master switch for check.
    lv_master_switch_active = zcl_switch=>get_switch_state( iv_lgnum = gv_warehouse_number
                                                     iv_devid = zif_switch_const=>c_zinb_001 ).

    " If master switch is turend off, no check should run at all.
    IF lv_master_switch_active = abap_false.
      RETURN.
    ENDIF.

    " Get BO
    lo_bo = go_bom->get_bo_by_id( iv_docid = iv_docid ).
    IF lo_bo IS NOT BOUND.
      RETURN.
    ENDIF.

    " Get Header (Current Version)
    lo_header ?= lo_bo->get_header( ).
    IF lo_header IS NOT BOUND.
      RETURN.
    ENDIF.

    " Check switch on field level. Run check only when switch is active.
    " Caution! If one field within a combination is de-active the check will fail.
    DATA(lt_switch_fields) = VALUE ztt_switch_fields(
                                   ( field = zif_switch_const=>c_doccat
                                     field_value = lo_header->mv_doccat )
                                   ( field = zif_switch_const=>c_doctype
                                     field_value = lo_header->mv_doctype ) ).

    lv_detail_switch_active = zcl_switch=>get_switch_state( iv_lgnum = gv_warehouse_number
                                                     iv_devid = zif_switch_const=>c_zinb_001
                                                     it_fields = lt_switch_fields ).

    IF lv_detail_switch_active = abap_false.
      RETURN.
    ENDIF.

    " Get Header (DB Version)
    lo_header_old ?= lo_bo->get_header( iv_objectstate = /scdl/if_dl_object_c=>sc_object_state_db ).
    IF lo_header_old IS NOT BOUND.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    " Get custom fields from both versions.
    DATA(ls_eew) = lo_header->/scdl/if_dl_head_prd_readonly~get_eew( ).
    DATA(ls_eew_old) = lo_header_old->/scdl/if_dl_head_prd_readonly~get_eew( ).

    " Check, whether there was a change in the Barcode.
    IF ls_eew-zzbarcode <> ls_eew_old-zzbarcode.
      rv_result = abap_true.
    ENDIF.

    " Check, whether there was a change in the Inbound shipment.
    IF ls_eew-zzinbship <> ls_eew_old-zzinbship.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD excecute_barcode_check.
**********************************************************************
*& Key           : AD-230307
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Executes the barcode check
*&
**********************************************************************
    DATA lo_delivery_check TYPE REF TO zcl_int_delivery_check.
    DATA: lo_item TYPE REF TO /scdl/cl_dl_item_prd.

    lo_delivery_check = NEW zcl_int_delivery_check(  ).

    "----------------------------------
    " Check: Barcode length
    "----------------------------------
    " Do noting, if Barcode is empty.
    " (If we don't skip the barcode check the replication with ERP will fail.)
    IF strlen(  iv_barcode ) = 0.
      RETURN.
    ENDIF.

    IF NOT lo_delivery_check->barcode_lenght_is_valid( iv_barcode ).
      MESSAGE ID '/SCWM/DELIVERY' TYPE /scwm/cl_dm_message_no=>sc_msgty_error
          NUMBER '100' WITH 'Barcode: Wrong length'(001) INTO rv_message.

      RETURN.
    ENDIF.

    "----------------------------------
    " Check: Barcode contains only numbers
    "----------------------------------
    IF NOT lo_delivery_check->barcode_is_numberic( iv_barcode ).
      MESSAGE ID '/SCWM/DELIVERY' TYPE /scwm/cl_dm_message_no=>sc_msgty_error
          NUMBER '100' WITH 'Barcode: Not a valid number'(003) INTO rv_message.

      RETURN.
    ENDIF.

    "----------------------------------
    " Check: Barcode check sum is valid
    "----------------------------------
    IF NOT lo_delivery_check->barcode_checksum_is_valid( iv_barcode ).
      MESSAGE ID '/SCWM/DELIVERY' TYPE /scwm/cl_dm_message_no=>sc_msgty_error
          NUMBER '100' WITH 'Barcode: Invalid check sum'(002) INTO rv_message.

      RETURN.
    ENDIF.

    "----------------------------------
    " Check: Company Code in Barcode (First 4 digits) is valid
    "----------------------------------
    io_bo->get_item_tab(
      IMPORTING
        et_item = DATA(lt_item) ).

    IF NOT line_exists( lt_item[ 1 ] ).
      RETURN.
    ENDIF.

    lo_item ?= lt_item[ 1 ]-item.

    IF lo_item IS NOT BOUND.
      RETURN.
    ENDIF.

    lo_item->/scdl/if_dl_item_prd_readonly~get_sapext(
       IMPORTING
          es_sapext_i = DATA(ls_sapext_i)
          es_sapext_o = DATA(ls_sapext_o)
          es_sapext_p = DATA(ls_sapext_p) ) ##NEEDED.


    SELECT SINGLE comp_code
      INTO @DATA(lv_comp_code)
      FROM ztinb_entitled
      WHERE entitled = @ls_sapext_i-entitled.

    IF sy-subrc <> 0 OR lv_comp_code IS INITIAL.
      MESSAGE ID '/SCWM/DELIVERY' TYPE /scwm/cl_dm_message_no=>sc_msgty_error
          NUMBER '100' WITH 'Barcode: No Company Code for Disposal Party found'(004) INTO rv_message.

      RETURN.
    ENDIF.

    IF lv_comp_code <> iv_barcode(4).
      MESSAGE ID '/SCWM/DELIVERY' TYPE /scwm/cl_dm_message_no=>sc_msgty_error
          NUMBER '100' WITH 'Barcode: Wrong Company Code'(005) INTO rv_message.
    ENDIF.
  ENDMETHOD.


  METHOD execute_inbound_shipment_check.
**********************************************************************
*& Key           : AD-230323
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Executes the Inbound Shipemtn check
*&
**********************************************************************
    DATA lo_delivery_check TYPE REF TO zcl_int_delivery_check.
    DATA lt_huhdr TYPE /scwm/tt_huhdr_int .

    lo_delivery_check = NEW zcl_int_delivery_check(  ).

    "----------------------------------
    " Check: Inbound Shipment length
    "----------------------------------
    " Do noting, if Inbound Shipment is empty.
    " (If we don't skip the Inbound Shipment check the replication with ERP will fail.)
    IF strlen(  iv_inbound_shipment ) = 0.
      RETURN.
    ENDIF.

    " If no matching HU is found => Error
    IF NOT lo_delivery_check->shipmentnumber_has_hu( iv_inbound_shipment ).
      MESSAGE ID '/SCWM/DELIVERY' TYPE /scwm/cl_dm_message_no=>sc_msgty_error
      NUMBER '100' WITH 'Inb. Ship.: No matching HU found'(006) INTO rv_message.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
