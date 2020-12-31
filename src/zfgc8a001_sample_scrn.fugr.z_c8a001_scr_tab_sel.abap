FUNCTION z_c8a001_scr_tab_sel.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  EXPORTING
*"     REFERENCE(EV_SUBRC) TYPE  SYSUBRC
*"  CHANGING
*"     REFERENCE(DTO_SEL_SCR) TYPE REF TO  ZCL_C8A001_SAMPLE_DTO_SCR
*"----------------------------------------------------------------------

  IF dto_sel_scr IS BOUND.
  ELSE.
    CREATE OBJECT dto_sel_scr.
  ENDIF.

  CREATE OBJECT go_alv_vars
    EXPORTING
      is_disvariant = dto_sel_scr->ms_disvariant.    " АВАР-программа, текущая главная программа



  CALL SELECTION-SCREEN 4100.
  IF sy-subrc EQ 0.
    ev_subrc = sy-subrc.

    dto_sel_scr->mt_tab_ran[] = s_tab[].
    dto_sel_scr->mt_fld_ran[] = s_field[].
    dto_sel_scr->p_freetx = p_freetx.
    dto_sel_scr->ms_disvariant-variant = alv4100.
  ELSE.
    ev_subrc = sy-subrc.
  ENDIF.



ENDFUNCTION.
