class ZCL_INB_EX_DLV_UI_SCREEN definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_DLV_UI_SCREEN .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INB_EX_DLV_UI_SCREEN IMPLEMENTATION.


  METHOD /scwm/if_ex_dlv_ui_screen~define_header_extension.
**********************************************************************
*& Key           : AD-230306
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Definies the deleivery custom screen enhancemnt for header.
*&
**********************************************************************
*& Key           : <aahmedov>-23.10.2023
*& Request No.   :
**********************************************************************
*& Description (short)
*& Definies the deleivery custom screen enhancemnt for header.
*&
**********************************************************************
    IF iv_transaction = /scwm/if_ex_dlv_ui_screen=>sc_ta_prdi.
      ev_repid = 'SAPLZFG_PDI_SCREEN_ENH'.
      ev_dynnr = '1000'.
    ENDIF.
    IF iv_transaction = /scwm/if_ex_dlv_ui_screen=>sc_ta_prdo.
      ev_repid = 'SAPLZFG_PDO_SCREEN_ENH'.
      ev_dynnr = '1000'.
    ENDIF.
  ENDMETHOD.


  METHOD /scwm/if_ex_dlv_ui_screen~define_hu_extension.
**********************************************************************
*& Key           : AD-230306
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Definies the deleivery custom screen enhancemnt for HUs.
*&
**********************************************************************
  ENDMETHOD.


  method /SCWM/IF_EX_DLV_UI_SCREEN~DEFINE_ITEM_EXTENSION.
**********************************************************************
*& Key           : AD-230306
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Definies the deleivery custom screen enhancemnt for items.
*&
**********************************************************************
  endmethod.
ENDCLASS.
