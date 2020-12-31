FUNCTION z_c8a001_alv_free.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"----------------------------------------------------------------------


  IF go_view_on_dto IS BOUND.
    go_view_on_dto->free( ).
    CLEAR go_view_on_dto.
  ENDIF.

  IF go_view1 IS BOUND.
    go_view1->free( ).
    CLEAR go_view1.
  ENDIF.


ENDFUNCTION.
