"Name: \PR:/SCWM/SAPLPUT_BIN_DET\FO:DECIDE_SPLITPUT\SE:END\EI
ENHANCEMENT 0 ZIE_INB_PUT_STRATEGY.
**********************************************************************
*& Key           : RM-230316
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************
  DATA:
        lo_impl TYPE REF TO zcl_inb_put_strat_enh.

  lo_impl = NEW zcl_inb_put_strat_enh( ).

  lo_impl->update_put_rule_and_split(
        EXPORTING
          is_t331     = is_t331
          is_penalty  = gs_penalty
          is_ltap     = cs_ltap
        CHANGING
          cv_put_rule = gv_put_rule
          cv_splitput = gv_splitput ).

ENDENHANCEMENT.
