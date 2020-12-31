class ZCL_C8A001_ALVTAB_ON_DTO_B definition
  public
  final
  create public .

public section.

  interfaces ZIF_C8A001_ALVTAB_ON_DTO .

  methods CONSTRUCTOR
    importing
      !IO_DTO type ref to ZIF_C8A001_ALV_DTO .
protected section.
private section.
  data mo_dto TYPE REF TO ZIF_C8A001_ALV_DTO.
  data MO_V1 type ref to ZCL_C8A001_VIEW .

ENDCLASS.



CLASS ZCL_C8A001_ALVTAB_ON_DTO_B IMPLEMENTATION.


METHOD CONSTRUCTOR.
  DATA lv_handle_app1 TYPE slis_handl VALUE 'AVV2'.
  mo_dto ?= io_dto.

  CREATE OBJECT mo_v1
    EXPORTING
      iv_alv_struct = mo_dto->get_structure( )
                                                                                                                                                                                                                                                "IV_CUST_AREA
      iv_handle     = lv_handle_app1
      iv_dock_pos   = cl_gui_docking_container=>dock_at_left.

  mo_v1->set_dto( io_dto = mo_dto ).

ENDMETHOD.


METHOD zif_c8a001_alvtab_on_dto~free_obj.
  CALL FUNCTION 'Z_C8A001_ALV_FREE'
    .

  IF mo_v1 IS BOUND.
    mo_v1->free( ).
    CLEAR mo_v1.
  ENDIF.
ENDMETHOD.


METHOD ZIF_C8A001_ALVTAB_ON_DTO~SHOW.

  mo_v1->set_reference2tab(
  CHANGING
    ct_tab = ct_tab
).

  CALL FUNCTION 'Z_C8A001_ALV_ON_DTO'
    EXPORTING
      obj = mo_v1.   " View: for type customizing tcodes


ENDMETHOD.
ENDCLASS.
