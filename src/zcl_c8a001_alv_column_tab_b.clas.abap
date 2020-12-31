class ZCL_C8A001_ALV_COLUMN_TAB_B definition
  public
  final
  create public .

public section.

  interfaces ZIF_C8A001_ALV_COLUMN_TAB .

  methods CONSTRUCTOR
    importing
      !IV_STRUCTURE_NAME type TABNAME .
protected section.
private section.

  data MV_STRUCT_NAME type TABNAME .
  data MO_V1 type ref to ZCL_C8A001_VIEW .
ENDCLASS.



CLASS ZCL_C8A001_ALV_COLUMN_TAB_B IMPLEMENTATION.


METHOD CONSTRUCTOR.
  DATA lv_handle_app1 TYPE slis_handl VALUE 'MOV1'.

  mv_struct_name = iv_structure_name.

  CREATE OBJECT mo_v1
    EXPORTING
      iv_alv_struct = mv_struct_name
                                                                                                                                                                                                                                                "IV_CUST_AREA
      iv_handle     = lv_handle_app1
      iv_dock_pos   = cl_gui_docking_container=>dock_at_left.

ENDMETHOD.


METHOD ZIF_C8A001_ALV_COLUMN_TAB~SHOW.

  mo_v1->set_reference2tab(
    CHANGING
      ct_tab = ct_tab
  ).

  CALL FUNCTION 'Z_C8A001_ALV_TAB_COLUMN'
    EXPORTING
      obj = mo_v1.   " View: for type customizing tcodes

ENDMETHOD.
ENDCLASS.
