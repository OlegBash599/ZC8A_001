*----------------------------------------------------------------------*
*       CLASS ZCL_C8A001_SAMPLE_DTO_SCR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_c8a001_sample_dto_scr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      mt_tab_ran TYPE RANGE OF tabname .
    DATA:
      mt_fld_ran TYPE RANGE OF fieldname .
    DATA  p_freetx TYPE string.
    DATA ms_disvariant TYPE disvariant .

    METHODS constructor .
protected section.
private section.
ENDCLASS.



CLASS ZCL_C8A001_SAMPLE_DTO_SCR IMPLEMENTATION.


METHOD constructor.
  ms_disvariant-report = sy-repid.
  ms_disvariant-username = sy-uname.
ENDMETHOD.
ENDCLASS.
