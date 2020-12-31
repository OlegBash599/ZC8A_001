*----------------------------------------------------------------------*
*       INTERFACE ZIF_C8A001_DB_SUPPLY
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE zif_c8a001_db_supply
  PUBLIC .

  TYPES: BEGIN OF ts_dd03vt.
            INCLUDE TYPE dd03vt.
  TYPES: rank_match_int TYPE p LENGTH 4 DECIMALS 1
         , END OF ts_dd03vt
         .
  TYPES: tt_dd03vt TYPE STANDARD TABLE OF ts_dd03vt WITH DEFAULT KEY.

  METHODS get_dict_fields
    IMPORTING
              !io_dto_scr TYPE REF TO zcl_c8a001_sample_dto_scr
    EXPORTING
              !et_dd03vt  TYPE tt_dd03vt
    RAISING   zcx_c8a001_db_supply.
ENDINTERFACE.                    "ZIF_C8A001_DB_SUPPLY
