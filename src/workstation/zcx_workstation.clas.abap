class ZCX_WORKSTATION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  data MESSAGES type BAPIRET2_T .
  data NO_GOODS_ISSUE type ABAP_BOOL .
  data HU_CREATION_FAILED type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MESSAGES type BAPIRET2_T optional
      !NO_GOODS_ISSUE type ABAP_BOOL optional
      !HU_CREATION_FAILED type ABAP_BOOL optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_WORKSTATION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MESSAGES = MESSAGES .
me->NO_GOODS_ISSUE = NO_GOODS_ISSUE .
me->HU_CREATION_FAILED = HU_CREATION_FAILED .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
