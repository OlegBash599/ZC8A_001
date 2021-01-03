
CLASS ZCL_C8A001_DB_ORA11 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_c8a001_db_supply .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
  BEGIN OF ts_named_dref,
    name TYPE string,
    dref TYPE REF TO data,
  END OF ts_named_dref .
    TYPES:
      tt_named_dref TYPE STANDARD TABLE OF ts_named_dref WITH DEFAULT KEY .


    METHODS sample_read.

    METHODS prepare_filter
      IMPORTING io_dto_scr          TYPE REF TO zcl_c8a001_sample_dto_scr
      EXPORTING ev_apply_filter     TYPE string
                ev_apply_filter4tab TYPE string
                ev_contains_search  TYPE string
      RAISING   zcx_c8a001_db_supply.

    METHODS get_query4ora
        IMPORTING iv_apply_filter     TYPE string
                  iv_apply_filter4tab TYPE string
                  iv_contains_search  TYPE string
        RETURNING value(rv_val) TYPE string.

    METHODS get_sql_line_by_sel_tab
        IMPORTING it_named_dref TYPE tt_named_dref
        RETURNING value(rv_val) TYPE string.
ENDCLASS.



CLASS ZCL_C8A001_DB_ORA11 IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_query4ora.
*        IMPORTING iv_apply_filter     TYPE string
*                  iv_apply_filter4tab TYPE string
*                  iv_contains_search  TYPE string
*        RETURNING VALUE(rv_val) TYPE string.
    data lv_similarity_short TYPE syindex VALUE 50.
    data lv_contains_upper TYPE string.
    data skip_records type syindex VALUE 0.
    data num_of_records type syindex VALUE 100.

    lv_contains_upper = iv_contains_search.
    TRANSLATE lv_contains_upper TO UPPER CASE.

    IF iv_contains_search IS INITIAL.
      rv_val =
     | SELECT * from ( |
  && | SELECT |
  && | "TABNAME", "FIELDNAME", "AS4LOCAL", "TABCLASS", "SQLTAB", "POSITION", "KEYFLAG", "MANDATORY",  |
  && | "ROLLNAME", "DOMNAME", "CHECKTABLE", "ADMINFIELD", "INTTYPE", "INTLEN", "REFTABLE", |
  && | "PRECFIELD", "REFFIELD", "CONROUT", "NOTNULL", "LENG",  "DATATYPE", "COMPTYPE",  |
  && | "TABLETYPE", "DEPTH", "AS4USER", "AS4DATE", "MASTERLANG", "DDLANGUAGE", "DDTEXT",  |
  && | "REPTEXT",  "SCRTEXT_S", "SCRTEXT_M", "SCRTEXT_L", "RANK_MATCH_INT",  |
  && | rownum AS row_bottom  |
  && | from ( |
  && | SELECT |
  && | "TABNAME", "FIELDNAME", "AS4LOCAL", "TABCLASS", "SQLTAB", "POSITION", "KEYFLAG", "MANDATORY",  |
  && | "ROLLNAME", "DOMNAME", "CHECKTABLE", "ADMINFIELD", "INTTYPE", "INTLEN", "REFTABLE", |
  && | "PRECFIELD", "REFFIELD", "CONROUT", "NOTNULL", "LENG",  "DATATYPE", "COMPTYPE",  |
  && | "TABLETYPE", "DEPTH", "AS4USER", "AS4DATE", "MASTERLANG", "DDLANGUAGE", "DDTEXT",  |
  && | "REPTEXT",  "SCRTEXT_S", "SCRTEXT_M", "SCRTEXT_L" |
  && | , '100' as "RANK_MATCH_INT", |
  && | rownum AS row_top |
  && | from "DD03VT" |
  && | where { iv_apply_filter } |
  && | ORDER BY "TABNAME", "POSITION", "FIELDNAME" |
  && | ) tab_flike where row_top <= { num_of_records + skip_records } |
  && | ) tab4offset where row_bottom > { skip_records } |
  .
    ELSE.
    rv_val =
     | SELECT * from ( |
  && | SELECT |
  && | "TABNAME", "FIELDNAME", "AS4LOCAL", "TABCLASS", "SQLTAB", "POSITION", "KEYFLAG", "MANDATORY",  |
  && | "ROLLNAME", "DOMNAME", "CHECKTABLE", "ADMINFIELD", "INTTYPE", "INTLEN", "REFTABLE", |
  && | "PRECFIELD", "REFFIELD", "CONROUT", "NOTNULL", "LENG",  "DATATYPE", "COMPTYPE",  |
  && | "TABLETYPE", "DEPTH", "AS4USER", "AS4DATE", "MASTERLANG", "DDLANGUAGE", "DDTEXT",  |
  && | "REPTEXT",  "SCRTEXT_S", "SCRTEXT_M", "SCRTEXT_L", "RANK_MATCH_INT",  |
  && | rownum AS row_bottom  |
  && | from ( |
  && | SELECT |
  && | "TABNAME", "FIELDNAME", "AS4LOCAL", "TABCLASS", "SQLTAB", "POSITION", "KEYFLAG", "MANDATORY",  |
  && | "ROLLNAME", "DOMNAME", "CHECKTABLE", "ADMINFIELD", "INTTYPE", "INTLEN", "REFTABLE", |
  && | "PRECFIELD", "REFFIELD", "CONROUT", "NOTNULL", "LENG",  "DATATYPE", "COMPTYPE",  |
  && | "TABLETYPE", "DEPTH", "AS4USER", "AS4DATE", "MASTERLANG", "DDLANGUAGE", "DDTEXT",  |
  && | "REPTEXT",  "SCRTEXT_S", "SCRTEXT_M", "SCRTEXT_L", |
  && | UTL_MATCH.jaro_winkler_similarity( upper(SCRTEXT_L), '{ lv_contains_upper }') AS "RANK_MATCH_INT", |
  && | rownum AS row_top |
  && | from "DD03VT" |
  && | where UTL_MATCH.jaro_winkler_similarity( upper(SCRTEXT_L), |
  && | '{ lv_contains_upper }') > { lv_similarity_short } and { iv_apply_filter } |
  && | ORDER BY "RANK_MATCH_INT" DESC |
  && | ) tab_flike where row_top <= { num_of_records + skip_records } |
  && | ) tab4offset where row_bottom > { skip_records } |
  .
    ENDIF.

  ENDMETHOD.                    "get_query4ora


  METHOD get_sql_line_by_sel_tab.
*        IMPORTING it_named_dref TYPE tt_named_dref
*        RETURNING VALUE(rv_val) TYPE string.
    DATA lrs_sel_line TYPE REF TO ts_named_dref.
    DATA lo_lib_sel_tab TYPE REF TO cl_lib_seltab.
    DATA lv_where_clause TYPE string.

    FIELD-SYMBOLS <fs_tab> TYPE table.

    LOOP AT it_named_dref REFERENCE INTO lrs_sel_line.
      ASSIGN lrs_sel_line->dref->* TO <fs_tab>.

      IF <fs_tab> IS INITIAL.
        CONTINUE.
      ENDIF.
      CLEAR lv_where_clause.
      lo_lib_sel_tab ?= cl_lib_seltab=>new( it_sel = <fs_tab> ).
      lv_where_clause =
      lo_lib_sel_tab->sql_where_condition( iv_field = lrs_sel_line->name ).

      IF rv_val IS INITIAL.
        rv_val = lv_where_clause.
      ELSE.
        rv_val = rv_val && ` and ` && lv_where_clause.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "get_sql_line_by_sel_tab


  METHOD prepare_filter.

    DATA lt_range_ddlanguage TYPE RANGE OF dd03vt-ddlanguage.
    FIELD-SYMBOLS <fs_language> LIKE LINE OF lt_range_ddlanguage.

    APPEND INITIAL LINE TO lt_range_ddlanguage ASSIGNING <fs_language>.
    <fs_language> = 'IEQ'.
    <fs_language>-low = sy-langu.

    ev_contains_search = io_dto_scr->p_freetx.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA lt_named_dref TYPE tt_named_dref.
    DATA lt_named_dref_tab TYPE tt_named_dref.
    FIELD-SYMBOLS <fs_name_range> TYPE ts_named_dref.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    APPEND INITIAL LINE TO lt_named_dref ASSIGNING <fs_name_range>.
    <fs_name_range>-name = '"TABNAME"'.
    GET REFERENCE OF io_dto_scr->mt_tab_ran[] INTO <fs_name_range>-dref.

    APPEND INITIAL LINE TO lt_named_dref ASSIGNING <fs_name_range>.
    <fs_name_range>-name = '"FIELDNAME"'.
    GET REFERENCE OF io_dto_scr->mt_fld_ran[] INTO <fs_name_range>-dref.

    APPEND INITIAL LINE TO lt_named_dref ASSIGNING <fs_name_range>.
    <fs_name_range>-name = '"DDLANGUAGE"'.
    GET REFERENCE OF lt_range_ddlanguage[] INTO <fs_name_range>-dref.
    ev_apply_filter = get_sql_line_by_sel_tab( lt_named_dref ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    APPEND INITIAL LINE TO lt_named_dref_tab ASSIGNING <fs_name_range>.
    <fs_name_range>-name = 'TABNAME'.
    GET REFERENCE OF io_dto_scr->mt_tab_ran[] INTO <fs_name_range>-dref.

    APPEND INITIAL LINE TO lt_named_dref_tab ASSIGNING <fs_name_range>.
    <fs_name_range>-name = 'DDLANGUAGE'.
    GET REFERENCE OF lt_range_ddlanguage[] INTO <fs_name_range>-dref.
    ev_apply_filter4tab = get_sql_line_by_sel_tab( lt_named_dref ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


  ENDMETHOD.                    "prepare_filter


  METHOD sample_read.
    DATA lo_adbc_query TYPE REF TO zcl_c8a001_adbc_query.
    DATA lv_trg_tab TYPE tabname VALUE 'DD02T'.
    DATA lv_query_ora TYPE string.
    DATA lt_dd02t_db_result TYPE STANDARD TABLE OF dd02t.

    lv_query_ora =
       | SELECT "TABNAME",  "DDLANGUAGE", "AS4LOCAL", "AS4VERS", "DDTEXT" |
    && | from "DD02T" |
    && | where "TABNAME" = '{ lv_trg_tab }' |.

    CREATE OBJECT lo_adbc_query
      EXPORTING
        iv_native_sql = lv_query_ora
        iv_tab_name   = lv_trg_tab.

    lo_adbc_query->run_query( ).
    lo_adbc_query->get_target_tab( IMPORTING et_tab = lt_dd02t_db_result ).
  ENDMETHOD.                    "sample_read


  METHOD zif_c8a001_db_supply~get_dict_fields.

    DATA lv_apply_filter TYPE string.
    DATA lv_apply_filter4tab TYPE string.
    DATA lv_contains_search TYPE string.

    prepare_filter( EXPORTING io_dto_scr = io_dto_scr
                    IMPORTING ev_apply_filter = lv_apply_filter
                              ev_apply_filter4tab = lv_apply_filter4tab
                              ev_contains_search = lv_contains_search ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA lo_adbc_query TYPE REF TO zcl_c8a001_adbc_query.
    DATA lv_trg_tab TYPE tabname VALUE 'DD03VT'.
    DATA lv_trg_output TYPE tabname VALUE 'ZSC8A001_DD03VT_WITH_RANK'.
    "    DATA lv_trg_output TYPE tabname VALUE 'DD03VT'.
    DATA lv_query_ora TYPE string.
    DATA lt_dd03t_db_result TYPE STANDARD TABLE OF zsc8a001_dd03vt_with_rank.
    "   DATA lt_dd03t_db_result TYPE STANDARD TABLE OF DD03VT.

    field-SYMBOLS <fs_db_result> TYPE zsc8a001_dd03vt_with_rank.
    field-SYMBOLS <fs_dd03vt> TYPE zif_c8a001_db_supply=>ts_dd03vt.


    lv_query_ora = get_query4ora( iv_apply_filter = lv_apply_filter
                                  iv_apply_filter4tab = lv_apply_filter4tab
                                  iv_contains_search = lv_contains_search ).

    CREATE OBJECT lo_adbc_query
      EXPORTING
        iv_native_sql = lv_query_ora
        iv_tab_name   = lv_trg_tab
        iv_tab_output = lv_trg_output.

    lo_adbc_query->run_query( ).
    lo_adbc_query->get_target_tab( IMPORTING et_tab = lt_dd03t_db_result ).

   " et_dd03vt[] = lt_dd03t_db_result[].
   loop at lt_dd03t_db_result ASSIGNING <fs_db_result>.
    APPEND INITIAL LINE TO et_dd03vt ASSIGNING <fs_dd03vt>.
    move-CORRESPONDING <fs_db_result> to <fs_dd03vt>.
   ENDLOOP.

  ENDMETHOD.                    "zif_c8a001_db_supply~get_dict_fields
ENDCLASS.
