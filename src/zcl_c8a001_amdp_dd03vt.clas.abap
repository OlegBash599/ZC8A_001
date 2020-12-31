CLASS zcl_c8a001_amdp_dd03vt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.

    METHODS get_list
      IMPORTING VALUE(iv_filters)         TYPE string
                VALUE(iv_filters_tabs)    TYPE string
                VALUE(iv_contains_search) TYPE string
      EXPORTING VALUE(et_dd03vt)          TYPE zif_c8a001_db_supply=>tt_dd03vt
      RAISING   cx_amdp_execution_failed.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_tab_n_field
                , tabname TYPE tabname
                , fieldname TYPE fieldname
                , score_match TYPE syindex
          , END OF ts_tab_n_field
          , tt_tab_n_field TYPE STANDARD TABLE OF ts_tab_n_field
          .

ENDCLASS.



CLASS zcl_c8a001_amdp_dd03vt IMPLEMENTATION .
  METHOD get_list BY DATABASE PROCEDURE
                               FOR HDB LANGUAGE SQLSCRIPT
                               OPTIONS READ-ONLY
                               USING dd03vt
                                     dd02t.

    DECLARE text_contain INT := 0;
    DECLARE count_rows INT := 0;
    declare upper_limit4fuzzy DECIMAL(3,2) := 0.4;
    declare fuzzy_long_txt DECIMAL(3,2) := 0.7;
    declare fuzzy_short_txt DECIMAL(3,2) := 0.7;
    declare long_txt_weight DECIMAL(3,2) := 0.6;
    declare short_txt_weight DECIMAL(3,2) := 0.8;

     SELECT LENGTH (:iv_contains_search) into text_contain FROM DUMMY;

    if :text_contain = 0 THEN
* OpenSQL-abap-like selection
      lt_dd03vt_filt =  APPLY_FILTER( dd03vt, :iv_filters);
      et_dd03vt =
        select filt.*, '1.0' as rank_match_int
             from :lt_dd03vt_filt as filt
             ;
    else
    --BEGIN of presearch
        lt_dd03vt_filt =  APPLY_FILTER( dd03vt, :iv_filters);
        lt_DD02T_filt =  APPLY_FILTER( DD02T, :iv_filters_tabs);


-- назначение весов и точности поиска в зависимости от входных данных {
        long_txt_weight = 0.9;
        short_txt_weight = 0.9;

        if text_contain <= 10 THEN
           fuzzy_long_txt = 0.6;
           fuzzy_short_txt = 0.7;

           else
           fuzzy_long_txt = 0.5;
           fuzzy_short_txt = 0.5;
        END IF ;
-- назначение весов и точности поиска в зависимости от входных данных }

        lt_tab_n_field =
          select
            TABNAME as tabname,
            FIELDNAME as fieldname,
            DDLANGUAGE as DDLANGUAGE,
            AS4LOCAL as AS4LOCAL,
            POSITION as POSITION,
            score() as score_match
            from :lt_dd03vt_filt
            where ( contains( SCRTEXT_L, iv_contains_search, FUZZY(:fuzzy_long_txt), WEIGHT(:long_txt_weight))
              or contains( ( SCRTEXT_S, SCRTEXT_M), iv_contains_search, FUZZY(:fuzzy_short_txt), WEIGHT(:short_txt_weight, :long_txt_weight))
              )
             and score() >= :upper_limit4fuzzy
            ;

        select count( * ) into count_rows from :lt_dd03vt_filt;

        if count_rows = 0 Then
        lt_tabs =
            select TABNAME as tabname,
            DDLANGUAGE as DDLANGUAGE,
            AS4LOCAL as AS4LOCAL,
            score() as score_match
            from :lt_DD02T_filt
            WHERE ( contains( DDTEXT, iv_contains_search, FUZZY(:fuzzy_long_txt) ) )
            and score() >= :upper_limit4fuzzy
            ;
        end if;
  -- end of presearch

       et_dd03vt =
        select DISTINCT
            dd03vt_db.*,
            tab_n_field.score_match as rank_match_int
            from dd03vt as dd03vt_db
           inner join :lt_tab_n_field as tab_n_field
            on dd03vt_db.tabname = tab_n_field.tabname
            and dd03vt_db.FIELDNAME = tab_n_field.FIELDNAME
            and dd03vt_db.DDLANGUAGE = tab_n_field.DDLANGUAGE
            and dd03vt_db.AS4LOCAL = tab_n_field.AS4LOCAL
            and dd03vt_db.POSITION = tab_n_field.POSITION
       union
           select DISTINCT
            dd03vt_db.*,
            tabs.score_match as rank_match_int
            from dd03vt as dd03vt_db
           inner join :lt_tabs as tabs
            on dd03vt_db.tabname = tabs.tabname
            and dd03vt_db.DDLANGUAGE = tabs.DDLANGUAGE
            and dd03vt_db.AS4LOCAL = tabs.AS4LOCAL
        ;

    end if;

  ENDMETHOD.
ENDCLASS.
