*----------------------------------------------------------------------*
*       CLASS ZCL_C8A001_ALV_DTO_B DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_c8a001_alv_dto_b DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_c8a001_alv_dto .
  PROTECTED SECTION.
private section.

  data MV_STRUCT type TABNAME .
  data MT_BUTTON type ZTTC8A001_ALV_BUTTON_HANDLER .
  data MO_DOUBLE_CLICK type ref to OBJECT .
ENDCLASS.



CLASS ZCL_C8A001_ALV_DTO_B IMPLEMENTATION.


  METHOD zif_c8a001_alv_dto~get_structure.
    rv_val = mv_struct.
  ENDMETHOD.                    "ZIF_C8A001_ALV_DTO~GET_STRUCTURE


  METHOD zif_c8a001_alv_dto~handle_user_com.

    DATA lr_button  TYPE REF TO zsc8a001_alv_button_handler.

    READ TABLE mt_button REFERENCE INTO lr_button
     WITH KEY function = iv_ucomm.
    IF sy-subrc EQ 0.
      IF lr_button->obj2handl IS BOUND.
        TRY .
            IF ir_stan_tab IS SUPPLIED.
              CALL METHOD lr_button->obj2handl->('USER_COMMAND')
                EXPORTING
                  iv_ucomm    = iv_ucomm
                  io_grid     = io_grid
                  ir_stan_tab = ir_stan_tab.
            ELSE.
              CALL METHOD lr_button->obj2handl->('USER_COMMAND')
                EXPORTING
                  iv_ucomm = iv_ucomm
                  io_grid  = io_grid.
            ENDIF.


          CATCH cx_root.

        ENDTRY.

      ENDIF.
    ENDIF.


  ENDMETHOD.                    "ZIF_C8A001_ALV_DTO~HANDLE_USER_COM


METHOD zif_c8a001_alv_dto~on_double_click.
  IF mo_double_click IS BOUND.
    TRY .
        CALL METHOD mo_double_click->('DOUBLE_CLICK')
          EXPORTING
            e_row            = e_row
            e_column         = e_column
            es_row_no        = es_row_no
            io_grid          = io_grid
            isr_line_clicked = isr_line_clicked.
      CATCH cx_root.

    ENDTRY.

  ENDIF.
ENDMETHOD.


  METHOD zif_c8a001_alv_dto~on_toolbar.
    DATA ls_but TYPE stb_button.
    DATA lr_button TYPE REF TO zsc8a001_alv_button_handler.
    DATA lv_lines TYPE sytabix.

    LOOP AT mt_button REFERENCE INTO lr_button.
      lv_lines = lines( e_object->mt_toolbar ).
      lv_lines = lv_lines + 1.
      CLEAR ls_but.


      ls_but-butn_type = lr_button->butn_type.
      ls_but-function = lr_button->function.
      ls_but-icon = lr_button->icon.
      ls_but-quickinfo  = lr_button->quickinfo.
      ls_but-text  = lr_button->text.
      INSERT ls_but INTO e_object->mt_toolbar INDEX lv_lines.
    ENDLOOP.
  ENDMETHOD.                    "ZIF_C8A001_ALV_DTO~ON_TOOLBAR


METHOD zif_c8a001_alv_dto~set_double_click_handler.
  mo_double_click ?= io_obj.
ENDMETHOD.


  METHOD zif_c8a001_alv_dto~set_structure.
    mv_struct = iv_struct.
  ENDMETHOD.                    "ZIF_C8A001_ALV_DTO~SET_STRUCTURE


  METHOD zif_c8a001_alv_dto~set_toolbar_process.
    mt_button = it_button.
  ENDMETHOD.                    "ZIF_C8A001_ALV_DTO~SET_TOOLBAR_PROCESS


METHOD zif_c8a001_alv_dto~set_variant.
  zif_c8a001_alv_dto~ms_variant = is_variant.
ENDMETHOD.
ENDCLASS.
