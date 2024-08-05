interface ZIF_WS_DECO_SP
  public .


  interfaces ZIF_WS_DEFAULTS_SP .

  types:
    BEGIN OF ty_prel_proposal,
      qty   TYPE /scwm/de_ui_quan,
      uom   TYPE meins,
      nltyp TYPE /scwm/ltap_nltyp,
      vlpla TYPE /scwm/ltap_vlpla,
    END OF ty_prel_proposal .
  types:
    tt_prel_proposal TYPE STANDARD TABLE OF ty_prel_proposal WITH DEFAULT KEY .
  types:
    BEGIN OF ty_hu_to_be_created,
      huident     TYPE /scwm/huident,
      pmatid      TYPE /scwm/de_pmatid,
      internal_nr TYPE abap_bool,
    END OF ty_hu_to_be_created .
  types:
    BEGIN OF ty_curr_stock_situ,
      lgtyp   TYPE /scwm/lgtyp,
      no_bins TYPE /scwm/de_no_bins,
      quan    TYPE /scwm/de_ui_quan,
    END OF ty_curr_stock_situ .
  types:
    tt_curr_stock_situ TYPE STANDARD TABLE OF ty_curr_stock_situ WITH DEFAULT KEY .
  types:
    BEGIN OF ty_proposed_wt.
      INCLUDE TYPE zstr_ws_proposed_wt AS grp_data.
  TYPES:
      wts TYPE STANDARD TABLE OF /scwm/ltap WITH DEFAULT KEY,
      hus TYPE STANDARD TABLE OF ty_hu_to_be_created WITH DEFAULT KEY,
    END OF ty_proposed_wt .
  types TT_PROPOSED_WT type TY_PROPOSED_WT .

  constants C_ACTION_WORKSTATION type /SCMB/ESDUS-ACTION value 'ZWORKSTATION' ##NO_TEXT.
  constants C_ELEMENT_WAREHOUSE type /SCMB/ESDUS-ELEMENT value 'WAREHOUSE' ##NO_TEXT.
  constants C_ELEMENT_WORKST_LOC type /SCMB/ESDUS-ELEMENT value 'WORKST_LOC' ##NO_TEXT.
  constants:
    BEGIN OF c_pc_or_mc,
      not_relevant            TYPE zde_wst_pcormc VALUE '01',
      store_in_pieces         TYPE zde_wst_pcormc VALUE '02',
      store_in_master_cartons TYPE zde_wst_pcormc VALUE '03',
    END OF c_pc_or_mc .
  constants:
    BEGIN OF c_selection_logic,
      default_pack_mat   TYPE zde_selection_logic VALUE '1',
      pack_specification TYPE zde_selection_logic VALUE '2',
    END OF c_selection_logic .

  methods PACK_AND_PRINT_HUS
    importing
      !IV_DOCID type /SCWM/DE_DOCID
      !IV_DOCCAT type /SCWM/DE_DOCCAT default 'PDI'
      !IV_ALT_UOM type /SCWM/SP_UOM optional
      !IV_NO_MOVE_TO_BUFFER type ABAP_BOOL optional
      !IS_HU type TY_HU_TO_BE_CREATED
      !IS_PROPOSED_WT type /SCWM/LTAP
      !IV_NOF_TOTES type ZDE_PLAN_TOTES optional
      !IV_DEST_HU_TO_TEST type /SCWM/GUID_HU optional
      !IV_LOGPOS_TO_TEST type /SCWM/DE_LOGPOS optional
      !IO_CART_CALLBACK type ref to ZIF_WS_PTWY_CART_SP optional
    exporting
      !EV_GUID_HU type /SCWM/GUID_HU
      !EV_TANUM type /SCWM/TANUM
      !EV_TAPOS type /SCWM/TAPOS
      !EV_WHO type /SCWM/DE_WHO
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP .
  methods CHANGE_DELIVERY
    importing
      !IV_EXPIRATION_DATE type /SCWM/SLED optional
      !IV_BESTBEFORE_DATE type /SCWM/SLED optional
    changing
      !CS_DELIVERY_ITEM type /SCWM/DLV_ITEM_OUT_PRD_STR
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP .
  methods CREATE_PROPOSAL
    importing
      !IV_RDOCID type /SCDL/DL_DOCID
      !IV_RITMID type /SCDL/DL_ITEMID
      !IV_ANFME type /SCWM/RL03TANFME
      !IV_ALTME type /SCWM/LRMEI
      !IV_VLTYP type /SCWM/LTAP_VLTYP optional
      !IV_VLBER type /SCWM/LTAP_VLBER optional
      !IV_VLPLA type /SCWM/LTAP_VLPLA optional
      !IV_MATID type /SCDL/DL_PRODUCTID
      !IV_NLTYP type /SCWM/LTAP_NLTYP optional
      !IV_HUTYP type /SCWM/DE_HUTYP
      !IT_PRELIMINARY_PROPOSAL type TT_PREL_PROPOSAL optional
      !IV_PUOM type /SCWM/DE_PUOM optional
      !IV_NO_LOCK_RELEASE_AFTER type ABAP_BOOL optional
      !IV_LOCATION_MIXED type ABAP_BOOL
    exporting
      !ET_LTAP type /SCWM/TT_LTAP_VB
      !ET_BAPIRET type BAPIRETTAB
    raising
      ZCX_WORKSTATION .
  methods DELETE_HU
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
    raising
      ZCX_WORKSTATION .
  methods GET_CURRENT_STOCK_SITUATION
    importing
      !IV_MATNR type MATNR
    returning
      value(RT_SITU) type TT_CURR_STOCK_SITU
    raising
      ZCX_WORKSTATION .
  methods GET_DELIVERY_FROM_IS
    importing
      !IV_INBSHIP type ZDE_INBSHIP
    returning
      value(RT_DOCID) type /SCDL/DL_ID_TAB .
  methods GET_DELIVERY_ITEMS
    importing
      !IV_MATNR type MATNR
      !IT_DOCID type /SCWM/DLV_DOCID_ITEM_TAB
      !IV_PUTAWAY_OPEN type FLAG default ABAP_TRUE
    returning
      value(RT_DEL_ITEM) type /SCWM/DLV_ITEM_OUT_PRD_TAB
    raising
      ZCX_WORKSTATION .
  methods GET_HU_BY_IDENT
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
    exporting
      !ES_HUHDR type /SCWM/S_HUHDR_INT
      !ET_HUIDENT type /SCWM/TT_IDENT_INT .
  methods GET_PACKSPEC
    importing
      !IV_MATID type /SCWM/DE_MATID
    exporting
      !ES_HEADER type /SCWM/S_PS_HEADER_INT
      !ET_CONTENT type /SCWM/TT_PACKSPEC_NESTED
      !ET_ELEMENTGROUP type /SCWM/TT_PS_ELEMENTGROUP
      !EV_PC_PER_FULPAL type ZDE_PC_PER_FULPAL
    raising
      ZCX_WORKSTATION .
  methods GET_PROP_HU_DATA
    importing
      !IV_NLTYP type /SCWM/LTAP_NLTYP
      !IV_NLPTYP type /SCWM/DE_NLPTYP
      !IV_MATID type /SCWM/DE_MATID
      !IV_KEEPCAR type FLAG
      !IV_DIRRPL type FLAG
      !IV_ENTITLED type /SCWM/DE_ENTITLED
    exporting
      !EV_CHANGE_ST_TYPE_MONO type FLAG
      !EV_PACK_MAT type /SCWM/DE_PMAT
      !EV_PACK_MAT_ID type /SCMB/MDL_PMATID
      !ES_PMAT_SETTING type ZTINB_WSPACK
      !EV_PACK_MAT_TEXT type MAKTX
      !EV_HUTYPE type /SCWM/DE_HUTYP
      !EV_HUTYPTEXT type /SCWM/DE_HUTYPT
      !EV_NR_INTERNAL type FLAG
      !EV_NO_SETTINGS type ABAP_BOOL
    raising
      ZCX_WORKSTATION .
  methods GET_STOCK_DATA
    importing
      !IV_MATNR type MATNR
      !IV_ONLY_WS_SPEC type ABAP_BOOL default ABAP_TRUE
      !IT_RNG_LGPLA type /SCWM/TT_LGPLA_R optional
    returning
      value(RT_STOCK) type /SCWM/TT_STOCK_MON
    raising
      ZCX_WORKSTATION .
  methods IS_WORKSTATION_MIXEDPALLET
    returning
      value(RV_MIXED) type ABAP_BOOL .
  methods GET_PACK_MAT_DATA
    importing
      !IV_PACK_MAT_ID type /SCMB/MDL_PMATID
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_NLTYP type /SCWM/LTAP_NLTYP
    exporting
      !EV_PACK_MAT type /SCWM/DE_PMAT
      !ES_PMAT_SETTING type ZTINB_WSPACK
      !EV_PACK_MAT_TEXT type MAKTX
      !EV_HUTYPE type /SCWM/DE_HUTYP
      !EV_HUTYPTEXT type /SCWM/DE_HUTYPT
      !EV_NR_INTERNAL type FLAG
    raising
      ZCX_WORKSTATION .
  methods CHECK_HU
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
      !IV_INBSHIP type ZDE_INBSHIP optional
    raising
      ZCX_WORKSTATION .
  methods GOODS_RECEIVED
    importing
      !IV_DOCID type /SCDL/DL_DOCID
      !IV_GUID_HU type /SCWM/GUID_HU optional
      !IV_REVERSE_GOODS_RECEIPT type ABAP_BOOL optional
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP .
  methods GET_BUFFER_STBIN
    returning
      value(RV_BUFFER_STBIN) type /SCWM/LGPLA .
  methods HU_EXISTS
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
    returning
      value(RV_EXISTS) type ABAP_BOOL .
endinterface.
