CLASS zcl_c8a001_db_hdb2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_c8a001_db_supply.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS prepare_filter
      IMPORTING io_dto_scr          TYPE REF TO zcl_c8a001_sample_dto_scr
      EXPORTING ev_apply_filter     TYPE string
                ev_apply_filter4tab TYPE string
                ev_contains_search  TYPE string
      RAISING   zcx_c8a001_db_supply.
ENDCLASS.



CLASS zcl_c8a001_db_hdb2 IMPLEMENTATION.
  METHOD zif_c8a001_db_supply~get_dict_fields.

    DATA lv_apply_filter TYPE string.
    DATA lv_apply_filter4tab TYPE string.
    DATA lv_contains_search TYPE string.

    prepare_filter( EXPORTING io_dto_scr = io_dto_scr
                    IMPORTING ev_apply_filter = lv_apply_filter
                              ev_apply_filter4tab = lv_apply_filter4tab
                              ev_contains_search = lv_contains_search ).

    TRY .
        NEW zcl_c8a001_amdp_dd03vt(  )->get_list(
                                  EXPORTING iv_filters = lv_apply_filter
                                            iv_filters_tabs = lv_apply_filter4tab
                                            iv_contains_search = lv_contains_search
                               IMPORTING et_dd03vt = et_dd03vt ).
      CATCH cx_amdp_execution_failed INTO DATA(lx_bad_amdp).
        RAISE EXCEPTION TYPE zcx_c8a001_db_supply.
    ENDTRY.
  ENDMETHOD.

  METHOD prepare_filter.

    DATA lt_range_ddlanguage TYPE RANGE OF dd03vt-ddlanguage.
    lt_range_ddlanguage = VALUE #( ( sign = 'I' option = 'EQ' low = sy-langu high = '') ).

    ev_contains_search = io_dto_scr->p_freetx.

    TRY.
        ev_apply_filter = cl_shdb_seltab=>combine_seltabs(
             EXPORTING
               it_named_seltabs = VALUE #(
                                           ( name = 'TABNAME'       dref = REF #( io_dto_scr->mt_tab_ran[] ) )
                                           ( name = 'FIELDNAME'     dref = REF #( io_dto_scr->mt_fld_ran[] ) )
                                           ( name = 'DDLANGUAGE'    dref = REF #( lt_range_ddlanguage ) )
                                           )
*        iv_client_field  =
           ).
      CATCH cx_shdb_exception.

        RAISE EXCEPTION TYPE zcx_c8a001_db_supply.

    ENDTRY.

    TRY.
        ev_apply_filter4tab = cl_shdb_seltab=>combine_seltabs(
             EXPORTING
               it_named_seltabs = VALUE #(
                                           ( name = 'TABNAME'       dref = REF #( io_dto_scr->mt_tab_ran[] ) )
                                           ( name = 'DDLANGUAGE'    dref = REF #( lt_range_ddlanguage ) )
                                           )
*        iv_client_field  =
           ).
      CATCH cx_shdb_exception.

        RAISE EXCEPTION TYPE zcx_c8a001_db_supply.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
