*----------------------------------------------------------------------*
*       CLASS zcl_c8a001_db_simple DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_c8a001_db_simple DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_c8a001_db_supply.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.                    "zcl_c8a001_db_simple DEFINITION



*----------------------------------------------------------------------*
*       CLASS zcl_c8a001_db_simple IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_c8a001_db_simple IMPLEMENTATION.
  METHOD zif_c8a001_db_supply~get_dict_fields.
    DATA lo_text_field_long TYPE scrtext_l.
    DATA lv_last_symbol TYPE syindex.
    DATA lv_max_char_input TYPE syindex VALUE '40'.

    DATA lv_pattern_beg_on_star TYPE string VALUE '#**'.
    DATA lv_pattern_end_on_star TYPE string VALUE '#**'.

    IF io_dto_scr->p_freetx IS INITIAL.
      lo_text_field_long = '%'.
    ELSE.
      lo_text_field_long = io_dto_scr->p_freetx.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF lo_text_field_long CP lv_pattern_beg_on_star.
        lo_text_field_long(1) = '%'.
      ELSE.
        lo_text_field_long = '%' && lo_text_field_long.
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      lv_last_symbol = strlen( lo_text_field_long ) - 1.
      IF lo_text_field_long CP lv_pattern_end_on_star
      OR lv_last_symbol EQ lv_max_char_input.
        lo_text_field_long(lv_last_symbol) = '%'.
      ELSE.
        lo_text_field_long = lo_text_field_long && '%'  .
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ENDIF.


        SELECT * FROM dd03vt
      "INTO TABLE mt_dict_info
      INTO TABLE et_dd03vt
      WHERE tabname IN io_dto_scr->mt_tab_ran
        AND fieldname IN io_dto_scr->mt_fld_ran
        AND ddlanguage EQ sy-langu
        AND scrtext_l LIKE lo_text_field_long
      .

  ENDMETHOD.                    "zif_c8a001_db_supply~get_dict_fields
ENDCLASS.                    "zcl_c8a001_db_simple IMPLEMENTATION
