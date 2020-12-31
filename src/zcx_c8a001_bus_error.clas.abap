class ZCX_C8A001_BUS_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  data MV_MSG type STRING .
  data MT_RET2 type BAPIRETTAB .
  data FATAL_ERROR type CHAR1 .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MV_MSG type STRING optional
      !MT_RET2 type BAPIRETTAB optional
      !FATAL_ERROR type CHAR1 optional .
  methods IF_FATAL_ERROR
    returning
      value(RV_VAL) type CHAR1 .
protected section.
private section.
ENDCLASS.



CLASS ZCX_C8A001_BUS_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->MV_MSG = MV_MSG .
me->MT_RET2 = MT_RET2 .
me->FATAL_ERROR = FATAL_ERROR .
endmethod.


METHOD if_fatal_error.
  rv_val = fatal_error.
ENDMETHOD.
ENDCLASS.
