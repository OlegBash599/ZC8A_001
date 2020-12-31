FUNCTION Z_C8A001_ALV_ON_DTO.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(OBJ) TYPE REF TO  ZCL_C8A001_VIEW
*"----------------------------------------------------------------------

  go_view_on_dto ?= obj.

  CALL SCREEN 5200.

ENDFUNCTION.
