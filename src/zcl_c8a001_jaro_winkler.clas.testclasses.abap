*"* use this source file for your ABAP unit test classes

CLASS ltc_simple DEFINITION FOR TESTING DURATION SHORT
                                RISK LEVEL HARMLESS
                    INHERITING FROM cl_aunit_assert.

  PUBLIC SECTION.

    METHODS default_match FOR TESTING .
    METHODS with_transpositions FOR TESTING .
    METHODS with_provide_equal_length FOR TESTING .

  PROTECTED SECTION.


  PRIVATE SECTION.

ENDCLASS.

CLASS ltc_simple IMPLEMENTATION.
  METHOD default_match.

    TYPES: BEGIN OF ts_word_check
               , word1 TYPE string
               , word2 TYPE string
               , search_range TYPE syindex
               , common_chars TYPE syindex
               , number_of_transposition TYPE syindex
               , jaro_sim TYPE p LENGTH 7 DECIMALS 3
               , jaro_winkler_sim TYPE decfloat16
               , jaro_sim_pc TYPE syindex
               , jaro_winkler_pc TYPE syindex
          , END OF ts_word_check
          , tt_word_check TYPE STANDARD TABLE OF ts_word_check WITH DEFAULT KEY
          .

    DATA lt_word_check TYPE tt_word_check.

    lt_word_check = VALUE tt_word_check(
    ( word1 = 'Abroms' word2 = 'Abrams' search_range = 2  common_chars = 5 number_of_transposition = 0
        jaro_sim = '0.8889' jaro_sim_pc = 89  jaro_winkler_pc = 92 )
    ( word1 = 'Dunningham' word2 = 'Cunnigham' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )

    ( word1 = 'Lampley' word2 = 'Campley' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )

    ( word1 = 'Marhta' word2 = 'Martha' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )

    ( word1 = 'Jonathon' word2 = 'Jonathan' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )

    ( word1 = 'Jeraldine' word2 = 'Geraldine' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )

    ( word1 = 'Dwayne' word2 = 'Duane' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )

    ( word1 = 'Shackleford' word2 = 'Shackelford' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )

    ( word1 = 'Алексей' word2 = 'Алексий' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )
    ).

    DATA lo_jaro_winkler TYPE REF TO zcl_c8a001_jaro_winkler.


    DATA lv_jaro_winkler_sim TYPE decfloat16.
    DATA lv_jaro_sim TYPE decfloat16.
    DATA lv_jaro_sim_pc TYPE syindex.
    DATA lv_jw_sim_pc TYPE decfloat16.
    DATA lv_j_sim_pc TYPE decfloat16.

    FIELD-SYMBOLS <fs_word_check> TYPE ts_word_check.

    LOOP AT lt_word_check ASSIGNING <fs_word_check>.
      CREATE OBJECT lo_jaro_winkler
        EXPORTING
          src_str = <fs_word_check>-word1
          trg_str = <fs_word_check>-word2.

      lo_jaro_winkler->calc(
        IMPORTING  ev_sim_jaro_winkler = <fs_word_check>-jaro_winkler_sim
                   ev_sim_jaro         = lv_jaro_sim ).

      "  lv_jaro_sim_pc = lv_jaro_sim * 100.
      lv_jaro_sim_pc =
        <fs_word_check>-jaro_sim_pc = lv_jaro_sim * 100.

      assert_equals( EXPORTING exp = <fs_word_check>-jaro_sim_pc
                               act = lv_jaro_sim_pc
                               msg = | jaro sim: { <fs_word_check>-word1 } { <fs_word_check>-word2 } should be { <fs_word_check>-jaro_sim_pc }  | ).

    ENDLOOP.

    BREAK-POINT.

  ENDMETHOD.

  METHOD with_transpositions.
    TYPES: BEGIN OF ts_word_check
             , word1 TYPE string
             , word2 TYPE string
             , search_range TYPE syindex
             , common_chars TYPE syindex
             , number_of_transposition TYPE syindex
             , jaro_sim TYPE p LENGTH 7 DECIMALS 3
             , jaro_winkler_sim TYPE decfloat16
             , jaro_sim_pc TYPE syindex
             , jaro_winkler_pc TYPE syindex
        , END OF ts_word_check
        , tt_word_check TYPE STANDARD TABLE OF ts_word_check WITH DEFAULT KEY
        .

    DATA lt_word_check TYPE tt_word_check.

    lt_word_check = VALUE tt_word_check(
    ( word1 = 'Marhta' word2 = 'Martha' search_range = 2  common_chars = 6 number_of_transposition = 1
        jaro_sim = '0.9444' jaro_sim_pc = 94  jaro_winkler_pc = 97 )
    ).

    DATA lo_jaro_winkler TYPE REF TO zcl_c8a001_jaro_winkler.


    DATA lv_jaro_winkler_sim TYPE decfloat16.
    DATA lv_jaro_sim TYPE decfloat16.
    DATA lv_jaro_sim_pc TYPE syindex.
    DATA lv_jw_sim_pc TYPE decfloat16.
    DATA lv_j_sim_pc TYPE decfloat16.

    FIELD-SYMBOLS <fs_word_check> TYPE ts_word_check.

    LOOP AT lt_word_check ASSIGNING <fs_word_check>.
      CREATE OBJECT lo_jaro_winkler
        EXPORTING
          src_str = <fs_word_check>-word1
          trg_str = <fs_word_check>-word2.

      lo_jaro_winkler->calc(
        EXPORTING  iv_compare_length   = 0
                   iv_adjust4long      = abap_true
                   iv_case_insensitive = abap_true
        IMPORTING  ev_sim_jaro_winkler = lv_jaro_winkler_sim
                   ev_sim_jaro         = lv_jaro_sim ).

      lv_jaro_sim_pc = lv_jaro_sim * 100.

      assert_equals( EXPORTING exp = <fs_word_check>-jaro_sim_pc
                               act = lv_jaro_sim_pc
                               msg = | jaro sim: { <fs_word_check>-word1 } { <fs_word_check>-word2 } should be { <fs_word_check>-jaro_sim_pc }  | ).

    ENDLOOP.


  ENDMETHOD.

  METHOD with_provide_equal_length.
    TYPES: BEGIN OF ts_word_check
           , word1 TYPE string
           , word2 TYPE string
           , search_range TYPE syindex
           , common_chars TYPE syindex
           , number_of_transposition TYPE syindex
           , jaro_sim TYPE p LENGTH 7 DECIMALS 3
           , jaro_winkler_sim TYPE decfloat16
           , jaro_sim_pc TYPE syindex
           , jaro_winkler_pc TYPE syindex
      , END OF ts_word_check
      , tt_word_check TYPE STANDARD TABLE OF ts_word_check WITH DEFAULT KEY
      .

    DATA lt_word_check TYPE tt_word_check.

    lt_word_check = VALUE tt_word_check(
    ( word1 = 'Abroms' word2 = 'Abrams' search_range = 2  common_chars = 5 number_of_transposition = 0
        jaro_sim = '0.8889' jaro_sim_pc = 89  jaro_winkler_pc = 93 )
    ( word1 = 'Dunningham' word2 = 'Cunnigham' search_range = 4  common_chars = 8 number_of_transposition = 0
        jaro_sim = '0.8667' jaro_sim_pc = 87 )
    ).

    DATA lo_jaro_winkler TYPE REF TO zcl_c8a001_jaro_winkler.


    DATA lv_jaro_winkler_sim TYPE decfloat16.
    DATA lv_jaro_sim TYPE decfloat16.
    DATA lv_jaro_sim_pc TYPE syindex.
    DATA lv_jw_sim_pc TYPE decfloat16.
    DATA lv_j_sim_pc TYPE decfloat16.

    FIELD-SYMBOLS <fs_word_check> TYPE ts_word_check.

    LOOP AT lt_word_check ASSIGNING <fs_word_check>.
      CREATE OBJECT lo_jaro_winkler
        EXPORTING
          src_str              = <fs_word_check>-word1
          trg_str              = <fs_word_check>-word2
          provide_equal_length = abap_true.

      lo_jaro_winkler->calc(
        EXPORTING  iv_compare_length   = 0
                   iv_adjust4long      = abap_true
                   iv_case_insensitive = abap_true
        IMPORTING  ev_sim_jaro_winkler = lv_jaro_winkler_sim
                   ev_sim_jaro         = lv_jaro_sim ).

      lv_jaro_sim_pc = lv_jaro_sim * 100.

      assert_equals( EXPORTING exp = <fs_word_check>-jaro_sim_pc
                               act = lv_jaro_sim_pc
                               msg = | jaro sim: { <fs_word_check>-word1 } { <fs_word_check>-word2 } should be { <fs_word_check>-jaro_sim_pc }  | ).

    ENDLOOP.


  ENDMETHOD.

ENDCLASS.
