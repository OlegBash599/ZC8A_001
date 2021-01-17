*----------------------------------------------------------------------*
*       CLASS zcl_c8a001_adbc_query DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_c8a001_adbc_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_native_sql TYPE string
        !iv_tab_name TYPE tabname
        !iv_tab_output TYPE tabname OPTIONAL
        !it_sel_field_cat TYPE lvc_t_fcat OPTIONAL .
    METHODS run_query
      RETURNING
        value(rv_val) TYPE string .
    METHODS get_target_tab
      EXPORTING
        !et_tab TYPE any .
  PROTECTED SECTION.

    TYPES:
      tv_aggfun        TYPE c LENGTH 6 .
    TYPES:
      BEGIN OF ts_outcol_descr,
        name            TYPE adbc_name,   "this column's name
        aggfun          TYPE tv_aggfun,  "specified aggregate function (if any)
        dref            TYPE REF TO data, "reference to the data object into which
                                          "the column value will be retrieved
        ind             TYPE int2,        "indicator
        pos             TYPE sy-colno,    "column position in the output list
        olen            TYPE i,           "the output length of this field
      END OF ts_outcol_descr .
    TYPES:
* internal table c containing the selected output columns
      tt_outcol_descr TYPE STANDARD TABLE OF ts_outcol_descr .

    DATA mv_native_sql TYPE string .
    DATA mv_join_fields TYPE string .
    DATA mv_tab_name TYPE tabname VALUE 'T000' ##no_text.
    "   DATA mt_target_tab TYPE STANDARD TABLE OF konh.
    DATA mt_target_tab TYPE REF TO data .
    DATA mv_con_name TYPE dbcon_name .
    DATA mo_line_type TYPE REF TO cl_abap_structdescr .
    DATA mo_tab_type TYPE REF TO cl_abap_tabledescr .
    DATA mv_tab_output TYPE tabname VALUE 'T000' ##no_text.

    METHODS get_columns
      IMPORTING
        !it_col_descr TYPE adbc_tabcol_descr_tab
      EXPORTING
        !et_columns TYPE tt_outcol_descr .
    METHODS define_output_fields
      IMPORTING
        !io_result_set TYPE REF TO cl_sql_result_set
        !it_coldescr_tab TYPE adbc_tabcol_descr_tab
      CHANGING
        !ct_columns TYPE tt_outcol_descr .
    METHODS parse2table
      IMPORTING
        !it_columns TYPE tt_outcol_descr .
  PRIVATE SECTION.


    DATA mt_field4sel TYPE lvc_t_fcat .

    METHODS create_dyn_tab
      IMPORTING
        !iv_tabname TYPE tabname
      EXPORTING
        !eo_line_type TYPE REF TO cl_abap_structdescr
        !eo_tab_type TYPE REF TO cl_abap_tabledescr .
    METHODS fill_sel_field_catalog .
ENDCLASS.



CLASS ZCL_C8A001_ADBC_QUERY IMPLEMENTATION.


  METHOD constructor.
    mv_native_sql = iv_native_sql.
    mv_con_name = cl_sql_connection=>c_default_connection.
    mv_tab_name = iv_tab_name.
    IF iv_tab_output IS INITIAL.
      mv_tab_output = iv_tab_name.
    ELSE.
      mv_tab_output = iv_tab_output.
    ENDIF.

    " mv_join_fields = iv_join_fields.
    " SPLIT mv_join_fields AT ';' INTO TABLE mt_join_fields.

    IF it_sel_field_cat IS INITIAL.
      fill_sel_field_catalog( ).
    ELSE.
      mt_field4sel[] = it_sel_field_cat[].
    ENDIF.

    create_dyn_tab(
      EXPORTING
"        iv_tabname   = mv_tab_name
        iv_tabname   = mv_tab_output
      IMPORTING
        eo_line_type = mo_line_type
        eo_tab_type  = mo_tab_type
    ).

    CREATE DATA mt_target_tab TYPE HANDLE mo_tab_type.

  ENDMETHOD.                    "constructor


  METHOD create_dyn_tab.

    DATA lo_struct   TYPE REF TO cl_abap_structdescr.
    DATA lt_comp     TYPE cl_abap_structdescr=>component_table.

    DATA lv_msg TYPE string.
    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.

    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.


    cl_abap_typedescr=>describe_by_name(
        EXPORTING p_name = iv_tabname
        RECEIVING p_descr_ref    =  lo_type_descr
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2 ).

    IF sy-subrc <> 0.
      RETURN.
    ELSE.
      lo_struct ?= lo_type_descr.
    ENDIF.

    lt_comp  = lo_struct->get_components( ).

    TRY .
        eo_line_type = cl_abap_structdescr=>create(
                             p_components = lt_comp
*                           p_strict     = true
                           ).
        eo_tab_type = cl_abap_tabledescr=>create( p_line_type  = eo_line_type  ).
      CATCH cx_sy_table_creation. " Exception when Creating a Table Type


    ENDTRY.

  ENDMETHOD.                    "create_dyn_tab


  METHOD define_output_fields.
*     IMPORTING io_result_set TYPE REF TO cl_sql_result_set
*               it_col_descr TYPE adbc_tabcol_descr_tab
*     CHANGING  ct_columns TYPE tt_outcol_descr.

    DATA lr_ind     TYPE REF TO int2.
    DATA lv_dec_len TYPE i.
    DATA lv_aggfun  TYPE tv_aggfun.

    DATA ls_column_descr TYPE adbc_tabcol_descr.
*    DATA lv_ddic_type  TYPE datatype_d.
*    DATA lv_ddic_length         TYPE i.

    FIELD-SYMBOLS <fs_select_column> LIKE LINE OF ct_columns.
    FIELD-SYMBOLS <lv_coldescr> LIKE LINE OF it_coldescr_tab.

    FIELD-SYMBOLS <fs_field4sel> TYPE lvc_s_fcat.

    LOOP AT ct_columns ASSIGNING <fs_select_column>.
      READ TABLE it_coldescr_tab WITH KEY column_name = <fs_select_column>-name
        ASSIGNING <lv_coldescr>.
      IF sy-subrc NE 0 AND <fs_select_column>-aggfun IS INITIAL.
        READ TABLE mt_field4sel ASSIGNING <fs_field4sel> WITH KEY fieldname = <fs_select_column>-name.
        IF sy-subrc EQ 0.
          ls_column_descr-ddic_type = <fs_field4sel>-datatype.
          ls_column_descr-ddic_length = <fs_field4sel>-intlen.
        ELSE.
          ASSERT 1 = 2.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING <lv_coldescr> TO ls_column_descr.
      ENDIF.

      "  CASE <lv_coldescr>-ddic_type.
      CASE ls_column_descr-ddic_type.
        WHEN 'CHAR' or 'LANG'.
          IF <fs_select_column>-aggfun IS NOT INITIAL.
            CREATE DATA <fs_select_column>-dref TYPE i.
          ELSE.
            "    CREATE DATA <fs_select_column>-dref TYPE c LENGTH <lv_coldescr>-ddic_length.
            CREATE DATA <fs_select_column>-dref TYPE c LENGTH ls_column_descr-ddic_length.
          ENDIF.
        WHEN 'NUMC'.
          "CREATE DATA <fs_select_column>-dref TYPE n LENGTH <lv_coldescr>-ddic_length.
          CREATE DATA <fs_select_column>-dref TYPE n LENGTH ls_column_descr-ddic_length.
        WHEN 'INT2' OR 'INT4' OR 'INT1'.
          CASE <fs_select_column>-aggfun.
            WHEN 'SUM'.
              CREATE DATA <fs_select_column>-dref TYPE p.
            WHEN 'AVG'.
              CREATE DATA <fs_select_column>-dref TYPE f.
            WHEN OTHERS.
              CREATE DATA <fs_select_column>-dref TYPE i.
          ENDCASE.
        WHEN 'DATS'.
          CREATE DATA <fs_select_column>-dref TYPE c LENGTH 14.
        WHEN 'DEC'.
          CASE lv_aggfun.
            WHEN 'SUM'.
              CREATE DATA <fs_select_column>-dref
                "TYPE p DECIMALS <lv_coldescr>-ddic_decimals.
                TYPE p DECIMALS ls_column_descr-ddic_decimals.
            WHEN 'AVG'.
              CREATE DATA <fs_select_column>-dref TYPE f.
            WHEN OTHERS.
*             the 'ddic_length' component contains the number of digits,
*             but the length of type P has to be specified in bytes:
*             1 byte representing two digits + one halfbyte for the sign.
              "              lv_dec_len = ( <lv_coldescr>-ddic_length DIV 2 ) + 1.
              lv_dec_len = ( ls_column_descr-ddic_length DIV 2 ) + 1.
              CREATE DATA <fs_select_column>-dref
                "TYPE p LENGTH lv_dec_len DECIMALS <lv_coldescr>-ddic_decimals.
                TYPE p LENGTH lv_dec_len DECIMALS ls_column_descr-ddic_decimals.
          ENDCASE.
        WHEN 'FLTP'.
          CREATE DATA <fs_select_column>-dref TYPE f.
        WHEN 'STRG' OR 'LCHR'.
          CREATE DATA <fs_select_column>-dref TYPE string.
        WHEN 'RSTR' OR 'LRAW'.
          CREATE DATA <fs_select_column>-dref TYPE xstring.
        WHEN 'RAW'.
          "          CREATE DATA <fs_select_column>-dref TYPE x LENGTH <lv_coldescr>-ddic_length.
          CREATE DATA <fs_select_column>-dref TYPE x LENGTH ls_column_descr-ddic_length.
        WHEN OTHERS.
*         shouldn't occur because columns with inappropriate types
*         should have been filtered by CHECK_TYPE routine
          ASSERT 1 = 0.
      ENDCASE.

*     create indicator variable
      GET REFERENCE OF <fs_select_column>-ind INTO lr_ind.

*     add a new output parameter to the result set object
      io_result_set->set_param( data_ref = <fs_select_column>-dref
                                ind_ref  = lr_ind ).
    ENDLOOP.


  ENDMETHOD.                    "define_output_fields


  METHOD fill_sel_field_catalog.
    CLEAR mt_field4sel.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
*       I_BUFFER_ACTIVE        =
        i_structure_name       = mv_tab_output
*       I_CLIENT_NEVER_DISPLAY = 'X'
*       I_BYPASSING_BUFFER     =
*       I_INTERNAL_TABNAME     =
      CHANGING
        ct_fieldcat            = me->mt_field4sel
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      CLEAR me->mt_field4sel.
    ELSE.

    ENDIF.

  ENDMETHOD.                    "FILL_SEL_FIELD_CATALOG


  METHOD get_columns.
*      IMPORTING it_col_descr TYPE adbc_tabcol_descr_tab
*      EXPORTING et_columns TYPE tt_outcol_descr.
    DATA lv_aggfun  TYPE tv_aggfun.
    DATA lt_range_columns TYPE RANGE OF adbc_name.

    FIELD-SYMBOLS <fs_range_col> LIKE LINE OF lt_range_columns.
    FIELD-SYMBOLS <lv_coldescr> LIKE LINE OF it_col_descr.
    FIELD-SYMBOLS  <lv_column> LIKE LINE OF et_columns.

    CLEAR lt_range_columns.
    DATA lo_struct TYPE REF TO cl_abap_structdescr.
    DATA lt_components TYPE abap_component_tab.
    FIELD-SYMBOLS <fs_comp> TYPE abap_componentdescr.
    FIELD-SYMBOLS <fs_field3sel> TYPE lvc_s_fcat.

    "    lo_struct ?= cl_abap_structdescr=>describe_by_name( mv_tab_name ).
    lo_struct ?= cl_abap_structdescr=>describe_by_name( mv_tab_output ).
    "    lt_components = lo_struct->get_components( ).
    "    lt_components = lo_struct->components.
    FIELD-SYMBOLS <fs_comp_all> TYPE abap_compdescr.
    LOOP AT lo_struct->components ASSIGNING <fs_comp_all>.
      APPEND INITIAL LINE TO lt_components ASSIGNING <fs_comp>.
      MOVE-CORRESPONDING <fs_comp_all> TO <fs_comp>.
    ENDLOOP.


    LOOP AT lt_components ASSIGNING <fs_comp>.
      IF mt_field4sel IS INITIAL.
        APPEND INITIAL LINE TO lt_range_columns ASSIGNING <fs_range_col>.
        <fs_range_col>-sign = 'I'.
        <fs_range_col>-option = 'EQ'.
        <fs_range_col>-low = <fs_comp>-name.
      ELSE.
        READ TABLE mt_field4sel TRANSPORTING NO FIELDS WITH KEY fieldname = <fs_comp>-name.
        IF sy-subrc EQ 0.
          APPEND INITIAL LINE TO lt_range_columns ASSIGNING <fs_range_col>.
          <fs_range_col>-sign = 'I'.
          <fs_range_col>-option = 'EQ'.
          <fs_range_col>-low = <fs_comp>-name.
        ENDIF.
      ENDIF.
    ENDLOOP.




    IF  lt_range_columns IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_col_descr ASSIGNING <lv_coldescr>.

      IF <lv_coldescr>-column_name IN lt_range_columns.
      ELSE.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO et_columns ASSIGNING <lv_column>.
      <lv_column>-name = <lv_coldescr>-column_name.
      <lv_column>-aggfun = lv_aggfun.
      TRANSLATE <lv_column>-aggfun TO UPPER CASE.

    ENDLOOP.


    LOOP AT lt_components ASSIGNING <fs_comp>.
      IF  <fs_comp>-name IN lt_range_columns.
      ELSE.
        CONTINUE.
      ENDIF.

      READ TABLE et_columns TRANSPORTING NO FIELDS WITH KEY name = <fs_comp>-name.
      IF sy-subrc NE 0.
*        APPEND INITIAL LINE TO et_columns ASSIGNING <lv_column>.
*        <lv_column>-name = <fs_comp>-name.
*        <lv_column>-aggfun = 'JW'.
*        TRANSLATE <lv_column>-aggfun TO UPPER CASE.
        READ TABLE mt_field4sel ASSIGNING <fs_field3sel> WITH KEY fieldname = <fs_comp>-name.
        IF sy-subrc EQ 0.
          APPEND INITIAL LINE TO et_columns ASSIGNING <lv_column>.
          <lv_column>-name = <fs_comp>-name.
          """"""""""""""""""""""""""""""""""""""'''
          IF <fs_field3sel>-do_sum EQ abap_true.
            <lv_column>-aggfun = 'AGGFUN'.
            TRANSLATE <lv_column>-aggfun TO UPPER CASE.
          ENDIF.
          """"""""""""""""""""""""""""""""""""""'''
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_columns


  METHOD get_target_tab.
    "      EXPORTING et_tab TYPE any.
    FIELD-SYMBOLS <fs_tab> TYPE table.
    ASSIGN mt_target_tab->* TO <fs_tab>.
    IF sy-subrc EQ 0.
      et_tab = <fs_tab>.
    ENDIF.
  ENDMETHOD.                    "get_target_tab


  METHOD parse2table.
    "      IMPORTING it_columns TYPE tt_outcol_descr.
    " DATA lr_oracle_data like REF TO konh.
    DATA lr_oracle_data TYPE REF TO data.
    DATA lr_oracle_data_line TYPE REF TO data.
    DATA lr_columns TYPE REF TO ts_outcol_descr.

    FIELD-SYMBOLS <lv_field_val> TYPE any.
    FIELD-SYMBOLS <lv_oracle_data_cell> TYPE any.

    FIELD-SYMBOLS <fs_tab> TYPE table.
    FIELD-SYMBOLS <fs_oracle_line> TYPE any.

    CREATE DATA lr_oracle_data_line TYPE HANDLE mo_line_type.

    CREATE DATA lr_oracle_data TYPE HANDLE mo_line_type.

    ASSIGN mt_target_tab->* TO <fs_tab>.
    IF sy-subrc EQ 0.
      APPEND INITIAL LINE TO <fs_tab> REFERENCE INTO lr_oracle_data_line.

      LOOP AT it_columns REFERENCE INTO lr_columns.
        ASSIGN lr_columns->dref->* TO <lv_field_val>.

        ASSIGN lr_oracle_data_line->* TO <fs_oracle_line>.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT lr_columns->name OF STRUCTURE <fs_oracle_line>
                    TO <lv_oracle_data_cell>.
          IF sy-subrc EQ 0.
            <lv_oracle_data_cell> = <lv_field_val>.
          ENDIF.
        ENDIF.



      ENDLOOP.
    ENDIF.



  ENDMETHOD.                    "parse2table


  METHOD run_query.

    DATA lv_query      TYPE string.
    DATA sqlerr_ref    TYPE REF TO cx_sql_exception.
    DATA sql_param_inv TYPE REF TO cx_parameter_invalid.
    DATA lo_con2ora    TYPE REF TO cl_sql_connection.
    DATA lo_sql_statement        TYPE REF TO cl_sql_statement.
    DATA lo_result     TYPE REF TO cl_sql_result_set.
    DATA lo_result_set          TYPE REF TO cl_sql_result_set.

    DATA lo_metadata   TYPE REF TO cl_sql_metadata.

    DATA lv_schema  TYPE adbc_name VALUE 'SAPSR3'.
    DATA lv_tabname TYPE adbc_name.


    DATA lt_coldescr_tab      TYPE adbc_tabcol_descr_tab.

    DATA lt_columns TYPE tt_outcol_descr.

    FIELD-SYMBOLS: <lv_schema>      TYPE adbc_name.
    FIELD-SYMBOLS: <lv_tabname>     TYPE adbc_name.

    lv_tabname = mv_tab_name.
    lv_query = mv_native_sql.

    TRY .
        lo_con2ora  = cl_sql_connection=>get_connection( mv_con_name ).
        "   lo_result = lo_con2ora->execute_query( lv_query ).

        lo_metadata = lo_con2ora->get_metadata( ).

        ASSIGN lv_schema TO <lv_schema>.
        ASSIGN lv_tabname TO <lv_tabname>.

*     get all columns of the table/view from the database catalog
        CALL METHOD lo_metadata->get_columns
          EXPORTING
            schema_name = <lv_schema>
            table_name  = <lv_tabname>
          IMPORTING
            column_tab  = lt_coldescr_tab.

        get_columns( EXPORTING it_col_descr = lt_coldescr_tab
                     IMPORTING et_columns = lt_columns ).

        lo_sql_statement = lo_con2ora->create_statement( ).
        lo_result_set = lo_sql_statement->execute_query( lv_query ).

        define_output_fields( EXPORTING io_result_set = lo_result_set
                                        it_coldescr_tab = lt_coldescr_tab
                              CHANGING  ct_columns = lt_columns ).

        CHECK lo_result_set IS BOUND.

        WHILE lo_result_set->next( ) > 0.
          parse2table( lt_columns ).
        ENDWHILE.

        lo_result_set->close( ).

      CATCH cx_sql_exception INTO sqlerr_ref.
        rv_val = 'Ошибка при создании соединения'.
        IF sqlerr_ref->sql_message IS NOT INITIAL.
          rv_val = sqlerr_ref->sql_message.
        ENDIF.

      CATCH cx_parameter_invalid INTO sql_param_inv.
        rv_val = 'Ошибка при обработке параметров'.
    ENDTRY.

  ENDMETHOD.                    "run_query
ENDCLASS.
