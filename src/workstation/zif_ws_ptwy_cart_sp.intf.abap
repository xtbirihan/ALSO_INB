interface ZIF_WS_PTWY_CART_SP
  public .


  constants C_PCKG_TYPE_CART type ZDE_PCKG_TYPE value '10' ##NO_TEXT.
  constants C_PCKG_TYPE_POS_ON_CART type ZDE_PCKG_TYPE value '20' ##NO_TEXT.
  constants C_PCKG_TYPE_TOTE type ZDE_PCKG_TYPE value '30' ##NO_TEXT.
  constants C_PCKG_TYPE_COMPARTMENT type ZDE_PCKG_TYPE value '40' ##NO_TEXT.
  constants C_NO_COLOR type ZDE_COLOR_CODE value 'NO' ##NO_TEXT.

  methods INTERPRET_HUIDENT
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
    exporting
      !ES_CART_SETTINGS type ZTINB_CART_BCODE
      !EV_NO_CART type ABAP_BOOL
      !EV_CART_ID type /SCWM/HUIDENT
      !EV_POS_ON_CART type /SCWM/HUIDENT
      !EV_TOTE_ID type /SCWM/HUIDENT
      !EV_COMP_ON_TOTE type /SCWM/HUIDENT
      !EV_COLOR type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION .
  methods MOVE_TASK_TO_ORDER
    importing
      !IV_TANUM type /SCWM/TANUM
      !IV_FROM_WHO type /SCWM/DE_WHO
      !IV_TO_WHO type /SCWM/DE_WHO
    raising
      ZCX_WORKSTATION
      /SCWM/CX_CORE .
  methods PACK_AND_PRINT_HU
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
      !IV_PMATID type /SCWM/DE_PMATID
      !IS_PROPOSED_WT type /SCWM/LTAP
      !IV_DOCID type /SCWM/DE_DOCID
      !IV_DOCCAT type /SCWM/DE_DOCCAT default 'PDI'
      !IV_ALT_UOM type /SCWM/SP_UOM
      !IV_CART_ID type /SCWM/HUIDENT optional
      !IV_POS_ON_CART type /SCWM/HUIDENT optional
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP
      /SCWM/CX_CORE .
  methods PACK_HU
    importing
      !IV_SOURCE_HU type /SCWM/GUID_HU
      !IV_DEST_HU type /SCWM/GUID_HU
      !IV_LOGPOS type /SCWM/DE_LOGPOS optional
      !IV_TEST_ONLY type ABAP_BOOL optional
    raising
      ZCX_WORKSTATION .
  methods CHECK_AND_CREATE_CART
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
      !IV_PCKG_TYPE type ZDE_PCKG_TYPE optional
      !IV_PMATID type /SCWM/DE_PMATID optional
    exporting
      !EV_STYPE type /SCWM/LGTYP
      !EV_GUID_HU type /SCWM/GUID_HU
      !EV_WHO type /SCWM/DE_WHO
      !ET_TO type /SCWM/TT_TO_DET_MON
      !EV_ALREADY_EXIST type ABAP_BOOL
    raising
      ZCX_WORKSTATION .
  methods MOVE_TOTE_INTO_CART
    importing
      !IV_TOTE_ID type /SCWM/HUIDENT
      !IV_CART_ID type /SCWM/HUIDENT
      !IV_POS_ON_CART type ZDE_POSITION_ON_CART
    raising
      ZCX_WORKSTATION
      /SCWM/CX_CORE .
  methods PACK_AND_PRINT_DECO
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
      !IV_PMATID type /SCWM/DE_PMATID
      !IS_PROPOSED_WT type /SCWM/LTAP
      !IV_DOCID type /SCWM/DE_DOCID
      !IV_DOCCAT type /SCWM/DE_DOCCAT default 'PDI'
      !IV_ALT_UOM type /SCWM/SP_UOM
      !IV_CART_ID type /SCWM/HUIDENT optional
      !IV_POS_ON_CART type /SCWM/HUIDENT optional
      !IV_NOF_TOTES type ZDE_PLAN_TOTES
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP
      /SCWM/CX_CORE .
  methods PACK_AND_PRINT_REPL
    importing
      !IV_TANUM type /SCWM/TANUM
      !IS_CONFIRM type /SCWM/TO_CONF
      !IS_CONF_EXC type /SCWM/S_CONF_EXC
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IV_ANFME type /SCWM/DE_UI_VSOLM
      !IV_ALTME type /SCWM/LRMEI
      !IS_WT_CUR type /SCWM/S_TO_DET_MON
      !IV_PMATID type /SCWM/DE_PMATID
      !IV_CART_ID type /SCWM/HUIDENT
      !IV_POS_ON_CART type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP
      /SCWM/CX_CORE .
endinterface.
