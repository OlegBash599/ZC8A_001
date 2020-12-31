FUNCTION Z_C8A001_ALV_TAB_COLUMN.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(OBJ) TYPE REF TO  ZCL_C8A001_VIEW
*"----------------------------------------------------------------------

  go_view1 ?= obj.

  CALL SCREEN 5100.

ENDFUNCTION.
