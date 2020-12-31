*----------------------------------------------------------------------*
*       CLASS ZCL_C8A001_SAMPLE_REP DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_c8a001_sample_rep DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS sh .
    METHODS double_click
      IMPORTING
        !e_row            TYPE lvc_s_row
        !e_column         TYPE lvc_s_col
        !es_row_no        TYPE lvc_s_roid
        !io_grid          TYPE REF TO cl_gui_alv_grid
        !isr_line_clicked TYPE REF TO data .
    METHODS user_command
      IMPORTING
        !iv_ucomm    TYPE syucomm
        !io_grid     TYPE REF TO cl_gui_alv_grid
        !ir_stan_tab TYPE STANDARD TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_dict_info.
            INCLUDE TYPE zsc8a001_dd03vt_with_rank.
    TYPES: tabcolor TYPE lvc_t_scol
          , END OF ts_dict_info .
    TYPES:
      tt_dict_info TYPE STANDARD TABLE OF ts_dict_info .
    TYPES ts_line TYPE ts_dict_info .

    CONSTANTS mc_output_alv TYPE tabname VALUE 'ZSC8A001_DD03VT_WITH_RANK' ##NO_TEXT.
    DATA mt_dict_info TYPE tt_dict_info .

    METHODS fill_info
      IMPORTING
        !io_dto_scr TYPE REF TO zcl_c8a001_sample_dto_scr
      RAISING
        zcx_c8a001_bus_error .
    METHODS show_tab
      IMPORTING
        !io_dto_scr TYPE REF TO zcl_c8a001_sample_dto_scr
      RAISING
        zcx_c8a001_bus_error .
    METHODS get_alv_dto
      IMPORTING
        !io_dto_scr   TYPE REF TO zcl_c8a001_sample_dto_scr
      RETURNING
        VALUE(ro_obj) TYPE REF TO zif_c8a001_alv_dto .
    METHODS open_se11
      IMPORTING
        !is_line TYPE ts_line .
    METHODS open_se16
      IMPORTING
        !is_line TYPE ts_line .
ENDCLASS.



CLASS zcl_c8a001_sample_rep IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD double_click.
    FIELD-SYMBOLS <fs_line> TYPE ts_line.
    ASSIGN isr_line_clicked->* TO <fs_line>.

    IF <fs_line>-tabname IS NOT INITIAL.
*    set PARAMETER ID 'DTB' FIELD <fs_alv_line>-tabname.
*    call TRANSACTION 'SE11' and SKIP FIRST SCREEN.


      open_se11( is_line = <fs_line> ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_info.

    DATA lo_db_supply TYPE REF TO zif_c8a001_db_supply.

    DATA lt_dd03vt TYPE zif_c8a001_db_supply=>tt_dd03vt.
    FIELD-SYMBOLS <fs_dd03vt> TYPE zif_c8a001_db_supply=>ts_dd03vt.
    FIELD-SYMBOLS <fs_alv_line> TYPE ts_dict_info.
    FIELD-SYMBOLS <fs_cell_color> TYPE lvc_s_scol.

    CLEAR mt_dict_info.


    lo_db_supply ?=
    zcl_c8a001_alv_factory=>get_ins( )->get_db_supply( ).

    IF lo_db_supply IS BOUND.
      TRY.
          lo_db_supply->get_dict_fields( EXPORTING io_dto_scr = io_dto_scr    " DTO: Sample Screen
                                         IMPORTING et_dd03vt  = lt_dd03vt ).   " Generierte Tabelle zu einem View
        CATCH zcx_c8a001_db_supply.
        return.
      ENDTRY.
    ENDIF.


    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_dd03vt ASSIGNING <fs_dd03vt>.
      APPEND INITIAL LINE TO mt_dict_info ASSIGNING <fs_alv_line>.
      MOVE-CORRESPONDING <fs_dd03vt> TO <fs_alv_line>.
      IF <fs_alv_line>-inttype EQ 'C'.
        APPEND INITIAL LINE TO <fs_alv_line>-tabcolor ASSIGNING <fs_cell_color>.
        <fs_cell_color>-fname = 'FIELDNAME'.
        <fs_cell_color>-color-col = 3.
        <fs_cell_color>-color-inv = 0.
        <fs_cell_color>-color-int = 0.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "fill_info


  METHOD get_alv_dto.

    ro_obj ?= zcl_c8a001_alv_factory=>get_ins( )->get_dto4alv( ).
    ro_obj->set_structure( iv_struct = mc_output_alv ).
    ro_obj->set_double_click_handler( me ).


    """"""""""""""""""""""""""""""""""""""""""""""""""""""'
    " alv - buttons - user command
    DATA ls_but TYPE zsc8a001_alv_button_handler.
    DATA lt_but TYPE zttc8a001_alv_button_handler.

    ls_but-function = 'ZOPEN_SE11'.
    ls_but-icon = icon_display.
    ls_but-quickinfo  = 'Open SE11 via GUI'.
    ls_but-text  = 'Open SE11'.
    ls_but-obj2handl ?= me.
    APPEND ls_but TO lt_but .

    CLEAR ls_but.
    ls_but-function = 'ZSEP01'.
    ls_but-butn_type = '3'.
    APPEND ls_but TO lt_but .

    CLEAR ls_but.
    ls_but-function = 'ZOPEN_SE16'.
    ls_but-icon = icon_list.
    ls_but-quickinfo  = 'Open SE16 via GUI'.
    ls_but-text  = 'Open SE16'.
    ls_but-obj2handl ?= me.
    APPEND ls_but TO lt_but .
*
*    ls_but-function = 'ZEDIT_DATA'.
*    ls_but-icon = icon_change.
*    ls_but-quickinfo  = 'Печать с подтверждением'.
*    ls_but-text  = 'Печать с подтверждением'.
*    ls_but-obj2handl ?= mo_handler.
*    APPEND ls_but TO lt_but .
    ro_obj->set_toolbar_process( it_button = lt_but ).
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""'
    " variant management for selection screen and alv inside
    ro_obj->set_variant( is_variant = io_dto_scr->ms_disvariant ).
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""'
  ENDMETHOD.                    "GET_ALV_DTO


  METHOD open_se11.
    DATA: bdcdata_wa  TYPE bdcdata,
          bdcdata_tab TYPE TABLE OF bdcdata.

    DATA opt TYPE ctu_params.

    CLEAR bdcdata_wa.
    bdcdata_wa-program  = 'SAPLSD_ENTRY'.
    bdcdata_wa-dynpro   = '1000'.
    bdcdata_wa-dynbegin = 'X'.
    APPEND bdcdata_wa TO bdcdata_tab.

    CLEAR bdcdata_wa.
    bdcdata_wa-fnam = 'BDC_CURSOR'.
    bdcdata_wa-fval = 'RSRD1-TBMA_VAL'.
    APPEND bdcdata_wa TO bdcdata_tab.

    CLEAR bdcdata_wa.
    bdcdata_wa-fnam = 'BDC_OKCODE'.
    bdcdata_wa-fval = '=WB_DISPLAY'.
    APPEND bdcdata_wa TO bdcdata_tab.

    CLEAR bdcdata_wa.
    bdcdata_wa-fnam = 'RSRD1-TBMA'.
    bdcdata_wa-fval = abap_true.
    APPEND bdcdata_wa TO bdcdata_tab.

    CLEAR bdcdata_wa.
    bdcdata_wa-fnam = 'RSRD1-TBMA_VAL'.
    bdcdata_wa-fval = is_line-tabname.
    APPEND bdcdata_wa TO bdcdata_tab.


    opt-dismode = 'E'.
    "opt-defsize = 'X'.

    CALL TRANSACTION 'SE11' USING bdcdata_tab OPTIONS FROM opt.
  ENDMETHOD.


  METHOD open_se16.

    SET PARAMETER ID 'DTB' FIELD is_line-tabname.
    CALL TRANSACTION 'SE16' AND SKIP FIRST SCREEN.

  ENDMETHOD.


  METHOD sh.

    DATA lv_subrc TYPE sy-subrc.
    DATA lo_dto_sel_scr TYPE REF TO zcl_c8a001_sample_dto_scr .
    DATA lx_error TYPE REF TO zcx_c8a001_bus_error.

    CREATE OBJECT lo_dto_sel_scr.
    lo_dto_sel_scr->ms_disvariant-report = sy-repid.
    lo_dto_sel_scr->ms_disvariant-username = cl_abap_syst=>get_user_name( ).
    lo_dto_sel_scr->ms_disvariant-handle = 'SAMPLEREP'.
    DO 20 TIMES.

      CLEAR lv_subrc.

      CALL FUNCTION 'Z_C8A001_SCR_TAB_SEL'
        IMPORTING
          ev_subrc    = lv_subrc    " Код возврата для ABAP-операторов
        CHANGING
          dto_sel_scr = lo_dto_sel_scr.    " DTO: Sample Screen

      IF lv_subrc EQ 0.
        TRY .
            fill_info( lo_dto_sel_scr ).
            show_tab( lo_dto_sel_scr ).
          CATCH zcx_c8a001_bus_error INTO lx_error.
            IF lx_error->if_fatal_error( ) EQ abap_true.
              EXIT.
            ELSE.

            ENDIF.
        ENDTRY.

      ELSE.
        EXIT.
      ENDIF.


    ENDDO.

  ENDMETHOD.                    "sh


  METHOD show_tab.
    DATA lo_alv_dto TYPE REF TO zif_c8a001_alv_dto.

    DATA lo_alv_on_dto TYPE REF TO zif_c8a001_alvtab_on_dto.


    lo_alv_dto ?= get_alv_dto( io_dto_scr ).

    lo_alv_on_dto ?= zcl_c8a001_alv_factory=>get_ins(
      )->alv_on_dto( lo_alv_dto ).
    lo_alv_on_dto->show( CHANGING ct_tab = mt_dict_info ).

    zcl_c8a001_alv_factory=>get_ins( )->alv_on_dto_free( ).

  ENDMETHOD.                    "show_tab


  METHOD user_command.
    DATA lt_index_rows  TYPE lvc_t_row.
    FIELD-SYMBOLS <fs_row> TYPE lvc_s_row.
    FIELD-SYMBOLS <fs_line> TYPE ts_line.

    io_grid->get_selected_rows(
         IMPORTING
           et_index_rows = lt_index_rows    " Indexes of Selected Rows
*          et_row_no     =     " Numeric IDs of Selected Rows
       ).

    CASE iv_ucomm.
      WHEN 'ZOPEN_SE11'.
        LOOP AT lt_index_rows ASSIGNING <fs_row>.
          READ TABLE ir_stan_tab ASSIGNING <fs_line> INDEX <fs_row>-index.
          IF sy-subrc EQ 0.
            open_se11( <fs_line> ).
          ENDIF.
          EXIT.
        ENDLOOP.

      WHEN 'ZOPEN_SE16'.
        LOOP AT lt_index_rows ASSIGNING <fs_row>.
          READ TABLE ir_stan_tab ASSIGNING <fs_line> INDEX <fs_row>-index.
          IF sy-subrc EQ 0.
            open_se16( <fs_line> ).
          ENDIF.
          EXIT.
        ENDLOOP.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
