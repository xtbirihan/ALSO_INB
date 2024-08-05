*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_CURST_STYP..................................*
TABLES: ZMV_CURST_STYP, *ZMV_CURST_STYP. "view work areas
CONTROLS: TCTRL_ZMV_CURST_STYP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_CURST_STYP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_CURST_STYP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_CURST_STYP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_CURST_STYP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_CURST_STYP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_CURST_STYP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_CURST_STYP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_CURST_STYP_TOTAL.

*.........table declarations:.................................*
TABLES: ZTINB_CURST_STYP               .
