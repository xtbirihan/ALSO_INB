*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WLOCMIMO....................................*
TABLES: ZMV_WLOCMIMO, *ZMV_WLOCMIMO. "view work areas
CONTROLS: TCTRL_ZMV_WLOCMIMO
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_WLOCMIMO. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WLOCMIMO.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WLOCMIMO_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WLOCMIMO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WLOCMIMO_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WLOCMIMO_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WLOCMIMO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WLOCMIMO_TOTAL.

*.........table declarations:.................................*
TABLES: ZTINB_WLOCMIMO                 .
