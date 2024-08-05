*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WSPACK......................................*
TABLES: ZMV_WSPACK, *ZMV_WSPACK. "view work areas
CONTROLS: TCTRL_ZMV_WSPACK
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_WSPACK. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WSPACK.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WSPACK_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WSPACK.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WSPACK_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WSPACK_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WSPACK.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WSPACK_TOTAL.

*.........table declarations:.................................*
TABLES: ZTINB_WSPACK                   .
