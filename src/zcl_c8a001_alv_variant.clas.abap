class ZCL_C8A001_ALV_VARIANT definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_DISVARIANT type DISVARIANT
      !IV_SAVE type CHAR1 default 'A' .
  methods GET_DEFAULT
    exporting
      !EV_VARIANT type SLIS_VARI .
  methods SEARCH_VARIANT
    exporting
      !EV_VARIANT type SLIS_VARI .
protected section.
private section.

  data MS_DISVARIANT_IN type DISVARIANT .
  data MV_SAVE type CHAR1 .
ENDCLASS.



CLASS ZCL_C8A001_ALV_VARIANT IMPLEMENTATION.


METHOD constructor.
  ms_disvariant_in = is_disvariant.
  mv_save = iv_save.
ENDMETHOD.


METHOD get_default.
  " data def_layout  type disvariant.
  DATA def_layout  TYPE disvariant.
  DATA lv_msg TYPE string.

  def_layout = ms_disvariant_in.

  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = mv_save    " Variants Can be Saved
    CHANGING
      cs_variant    = def_layout    " Variant information
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
    RETURN.
  ENDIF.

  ev_variant = def_layout-variant.
ENDMETHOD.


METHOD search_variant.
  DATA ls_variant  TYPE disvariant.
  DATA ls_variant_out  TYPE disvariant.
  DATA lv_exit TYPE char1.

  DATA lv_msg TYPE string.
  ls_variant = ms_disvariant_in.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      is_variant = ls_variant
      i_save     = mv_save
    IMPORTING
      e_exit     = lv_exit
      es_variant = ls_variant_out
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
  ELSE.
    IF lv_exit NE 'X'.
* set name of layout on selection screen
      ev_variant    = ls_variant_out-variant.
    ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
