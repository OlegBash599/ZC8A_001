*&---------------------------------------------------------------------*
*&  Include           LZFGC8A001_SAMPLE_SCRND01
*&---------------------------------------------------------------------*

DATA gs_dd03vt TYPE dd03vt.

SELECTION-SCREEN BEGIN OF SCREEN 4100.

SELECTION-SCREEN BEGIN OF BLOCK b42 WITH FRAME TITLE text-410.
SELECT-OPTIONS:   s_tab FOR gs_dd03vt-tabname
                , s_field FOR gs_dd03vt-fieldname
                .

PARAMETERS: p_freetx type text1024.

SELECTION-SCREEN END OF BLOCK b42.

SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b41 WITH FRAME TITLE text-411.
PARAMETERS: alv4100 TYPE slis_vari.


SELECTION-SCREEN END OF BLOCK b41.

SELECTION-SCREEN END OF SCREEN 4100.





AT SELECTION-SCREEN OUTPUT.
  IF go_alv_vars IS BOUND.
    go_alv_vars->get_default(
      IMPORTING
        ev_variant = alv4100    " Формат
    ).
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR alv4100.
  IF go_alv_vars IS BOUND.
    go_alv_vars->search_variant(
      IMPORTING
        ev_variant = alv4100    " Формат
    ).
  ENDIF.
