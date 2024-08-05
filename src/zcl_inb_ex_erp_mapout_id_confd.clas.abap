CLASS zcl_inb_ex_erp_mapout_id_confd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_mapout_id_confdec .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INB_EX_ERP_MAPOUT_ID_CONFD IMPLEMENTATION.


  METHOD /scwm/if_ex_mapout_id_confdec~mapout.
**********************************************************************
*& Key           : AD-230315
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_ERP_MAPOUT_ID_CONFDEC
*&
**********************************************************************

    ct_bapi_extension2 = VALUE #( BASE ct_bapi_extension2 ( "param  =
                                                            row    = lines( ct_bapi_extension2 ) + 1
                                                            field  = 'ZZBARCODE_EWM'
                                                            value  = is_dlv_info-s_head-s_eew-zzbarcode
                                                            type   = 'CHAR'
                                                            length = 12
                                                           ) ).

    "Replace substitute product
    LOOP AT ct_bapi_handling_unit_item REFERENCE INTO DATA(lr_hu_itm).
      LOOP AT is_dlv_info-t_item REFERENCE INTO DATA(lr_itm).
        READ TABLE lr_itm->t_refdoc REFERENCE INTO DATA(lr_refdoc)
             WITH KEY refdoccat = /scdl/if_dl_doc_c=>sc_doccat_erp.
        IF sy-subrc EQ 0 AND lr_refdoc->refdocno EQ lr_hu_itm->deliv_numb
                         AND lr_refdoc->refitemno EQ lr_hu_itm->deliv_item.
          lr_hu_itm->material_long = lr_itm->s_eew-zzmaterp.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT ct_bapi_inb_item_data_conf  REFERENCE INTO DATA(lr_bapi_itm).
      LOOP AT is_dlv_info-t_item REFERENCE INTO lr_itm.
        READ TABLE lr_itm->t_refdoc REFERENCE INTO lr_refdoc
             WITH KEY refdoccat = /scdl/if_dl_doc_c=>sc_doccat_erp.
        IF sy-subrc EQ 0 AND lr_refdoc->refdocno EQ lr_bapi_itm->deliv_numb
                         AND lr_refdoc->refitemno EQ lr_bapi_itm->deliv_item.
          lr_bapi_itm->material_external = lr_itm->s_eew-zzmaterp.
          lr_bapi_itm->material_long = lr_itm->s_eew-zzmaterp.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
