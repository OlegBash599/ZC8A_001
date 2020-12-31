interface ZIF_C8A001_ALV_DTO
  public .


  data MS_VARIANT type DISVARIANT read-only .

  methods SET_STRUCTURE
    importing
      !IV_STRUCT type TABNAME .
  methods GET_STRUCTURE
    returning
      value(RV_VAL) type TABNAME .
  methods ON_TOOLBAR
    importing
      !E_OBJECT type ref to CL_ALV_EVENT_TOOLBAR_SET optional .
  methods SET_TOOLBAR_PROCESS
    importing
      !IT_BUTTON type ZTTC8A001_ALV_BUTTON_HANDLER .
  methods HANDLE_USER_COM
    importing
      !IV_UCOMM type SYUCOMM
      !IO_GRID type ref to CL_GUI_ALV_GRID
      !IR_STAN_TAB type STANDARD TABLE optional .
  methods ON_DOUBLE_CLICK
    importing
      !E_ROW type LVC_S_ROW
      !E_COLUMN type LVC_S_COL
      !ES_ROW_NO type LVC_S_ROID
      !IO_GRID type ref to CL_GUI_ALV_GRID
      !ISR_LINE_CLICKED type ref to DATA .
  methods SET_DOUBLE_CLICK_HANDLER
    importing
      !IO_OBJ type ref to OBJECT .
  methods SET_VARIANT
    importing
      !IS_VARIANT type DISVARIANT .
endinterface.
