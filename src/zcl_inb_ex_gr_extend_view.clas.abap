CLASS zcl_inb_ex_gr_extend_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_gr_extend_view .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INB_EX_GR_EXTEND_VIEW IMPLEMENTATION.


  METHOD /scwm/if_gr_extend_view~get_form_view_subscreen.
**********************************************************************
*& Key           : AD-230306
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for GRPE Sub screens
*&
**********************************************************************
    IF iv_step = /scwm/if_gr_c=>sc_step_gr_prep AND
       iv_view = /scwm/if_gr_c=>sc_view_hdr.
      "ev_repid = 'SAPLZEWM_INB_PDI_FIELDS_GRPE'.
      ev_repid = 'SAPLZFG_PDI_GRPE_SCR_ENH'.
      ev_dynnr = '1000'.
    ENDIF.
  ENDMETHOD.


  METHOD /scwm/if_gr_extend_view~map_fields.
**********************************************************************
*& Key           : AD-230306
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for GRPE Field mappings
*&
**********************************************************************
    DATA: ls_fieldmap TYPE /scwm/s_gr_fieldmap_ext.

    CLEAR et_field.

    IF iv_step = /scwm/if_gr_c=>sc_step_gr_prep AND
       iv_view = /scwm/if_gr_c=>sc_view_hdr.

      CLEAR ls_fieldmap.
      ls_fieldmap-ui_fieldname = 'ZZBARCODE'.
      ls_fieldmap-logfname = 'ZZBARCODE_H'.
      APPEND ls_fieldmap TO et_field.

      CLEAR ls_fieldmap.
      ls_fieldmap-ui_fieldname = 'ZZINBSHIP'.
      ls_fieldmap-logfname = 'ZZINBSHIP_H'.
      APPEND ls_fieldmap TO et_field.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
