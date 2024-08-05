*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_CART_BCODE..................................*
TABLES: ZMV_CART_BCODE, *ZMV_CART_BCODE. "view work areas
CONTROLS: TCTRL_ZMV_CART_BCODE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_CART_BCODE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_CART_BCODE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_CART_BCODE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_CART_BCODE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_CART_BCODE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_CART_BCODE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_CART_BCODE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_CART_BCODE_TOTAL.

*.........table declarations:.................................*
TABLES: ZTINB_CART_BCODE               .
