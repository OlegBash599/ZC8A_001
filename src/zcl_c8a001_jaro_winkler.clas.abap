CLASS zcl_c8a001_jaro_winkler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !src_str              TYPE string
        !trg_str              TYPE string
        !provide_equal_length TYPE abap_bool DEFAULT abap_false .
    METHODS calc
      IMPORTING
        !iv_compare_length   TYPE syindex DEFAULT 0
        !iv_adjust4long      TYPE abap_bool DEFAULT abap_false
        !iv_case_insensitive TYPE abap_bool DEFAULT abap_true
        !iv_phonetic_adjust  TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !ev_sim_jaro_winkler TYPE decfloat16
        !ev_sim_jaro         TYPE decfloat16 .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ts_word_pair
            , word_src TYPE string
            , word_trg TYPE string
            , min_length TYPE syindex
          , END OF ts_word_pair
          .

    TYPES: BEGIN OF ts_word_meta
              , symb TYPE char1
              , pos_num TYPE syindex
              , is_common TYPE abap_bool
              , paired_pos_num TYPE syindex
         , END OF ts_word_meta
         , tt_word_meta TYPE STANDARD TABLE OF ts_word_meta WITH DEFAULT KEY
         .

    TYPES: BEGIN OF ts_word
            , symb TYPE char1
        , END OF ts_word
        , tt_word TYPE STANDARD TABLE OF ts_word WITH DEFAULT KEY
        .

    TYPES: tv_ratio TYPE p LENGTH 7 DECIMALS 4.

    CONSTANTS mc_prefix_normal_limit TYPE syindex VALUE 4.

    DATA ms_word_pair TYPE ts_word_pair .
    DATA mv_src_init TYPE string.
    DATA mv_trg_init TYPE string.
    DATA mv_provide_equal_length TYPE abap_bool.

    DATA mt_word_meta_src TYPE tt_word_meta.
    DATA mt_word_meta_trg TYPE tt_word_meta.

    DATA mv_winkler_limit4adjust TYPE p DECIMALS 2 VALUE '0.7'.
    DATA winkler_ratio4equal_prefix TYPE p DECIMALS 2 VALUE '0.1'.



    METHODS get_word_meta
      IMPORTING
        !iv_str       TYPE string
      RETURNING
        VALUE(rt_val) TYPE tt_word_meta .

    METHODS word2array
      IMPORTING
        !iv_str       TYPE string
      RETURNING
        VALUE(rt_val) TYPE tt_word .

    METHODS get_count_matched_pairs
      IMPORTING !iv_search_range        TYPE syindex
      RETURNING VALUE(rv_matched_pairs) TYPE syindex.


    METHODS get_transpositions
      EXPORTING ev_transpositions TYPE syindex.

    METHODS get_half_of_transposition
      RETURNING VALUE(rv_val) TYPE syindex.

    METHODS delete_lead_trail_spaces.

    METHODS do_upper_case
      IMPORTING !iv_case_insensitive TYPE abap_bool.

    METHODS cut_the_part2be_compare
      IMPORTING !iv_compare_length TYPE syindex.

    METHODS get_search_range
      RETURNING VALUE(rv_val) TYPE syindex.

    METHODS get_phonetic_ratio
      IMPORTING !iv_phonetic_adjust TYPE abap_bool
      RETURNING VALUE(rv_val)       TYPE tv_ratio.

    METHODS adjust_sim_by_common_prefix
      EXPORTING !ev_common_prefix    TYPE syindex
      CHANGING  !cv_sim_jaro_winkler TYPE decfloat16.

    METHODS do_adjust4long_strings
      IMPORTING !iv_adjust4long        TYPE abap_bool
                !iv_common_chars_count TYPE syindex
                !iv_common_prefix      TYPE syindex
      RETURNING VALUE(rv_val)          TYPE abap_bool.

    METHODS adjust_similarity4long_strings
      IMPORTING !iv_common_chars_count TYPE syindex
                !iv_common_prefix      TYPE syindex
      CHANGING  !cv_sim_jaro_winkler   TYPE decfloat16.



ENDCLASS.



CLASS zcl_c8a001_jaro_winkler IMPLEMENTATION.


  METHOD adjust_similarity4long_strings.
*      IMPORTING !iv_common_chars_count TYPE syindex
*                !iv_common_prefix    TYPE syindex
*      CHANGING  !cv_sim_jaro_winkler   TYPE decfloat16.
    DATA lv_proportion_coef TYPE decfloat16.
    DATA lv_part1 TYPE decfloat16.
    DATA lv_part2 TYPE decfloat16.

    DATA adjustment4long_strings TYPE decfloat16.

    lv_proportion_coef = 1 - cv_sim_jaro_winkler.

    lv_part1 = ( iv_common_chars_count - iv_common_prefix - 1 ).
    lv_part2 =
    ( strlen( ms_word_pair-word_src ) + strlen( ms_word_pair-word_trg ) - iv_common_prefix * 2 + 2 ).

    adjustment4long_strings = lv_proportion_coef * ( lv_part1 / lv_part2 ).

    cv_sim_jaro_winkler = cv_sim_jaro_winkler + adjustment4long_strings.

  ENDMETHOD.


  METHOD adjust_sim_by_common_prefix.
*exporting !ev_common_prefix TYPE syindex
*      CHANGING !cv_sim_jaro_winkler TYPE decfloat16.
    DATA upper_limit2compare_prefix TYPE syindex.

    DATA symbol_position_prefix TYPE syindex.
    DATA num_of_equal_char_in_prefix TYPE syindex.


    IF ms_word_pair-min_length GE mc_prefix_normal_limit.
      upper_limit2compare_prefix = mc_prefix_normal_limit.
    ELSE.
      upper_limit2compare_prefix = ms_word_pair-min_length.
    ENDIF.

    num_of_equal_char_in_prefix = 0.
    DO.
      symbol_position_prefix = sy-index - 1.
      IF ms_word_pair-word_src+symbol_position_prefix(1) EQ ms_word_pair-word_trg+symbol_position_prefix(1).
        num_of_equal_char_in_prefix = num_of_equal_char_in_prefix + 1.
      ELSE.
        EXIT.
      ENDIF.
      IF num_of_equal_char_in_prefix GE upper_limit2compare_prefix.
        EXIT.
      ENDIF.
    ENDDO.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ev_common_prefix = num_of_equal_char_in_prefix.
    IF num_of_equal_char_in_prefix EQ 0.
      RETURN.
    ENDIF.
    " корректируем похожесть по Jaro-Winkler
    cv_sim_jaro_winkler =
    cv_sim_jaro_winkler + ( num_of_equal_char_in_prefix * winkler_ratio4equal_prefix ) * ( 1 - cv_sim_jaro_winkler ).
  ENDMETHOD.


  METHOD calc.
    DATA search_range  TYPE syindex.
    DATA common_chars_count TYPE syindex.
    DATA half_of_transpositions TYPE syindex.
    DATA common_chars_incl_phonetic TYPE p LENGTH 7 DECIMALS 4.
    DATA common_prefix_count TYPE syindex.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    delete_lead_trail_spaces(  ).
    do_upper_case( iv_case_insensitive ).
    cut_the_part2be_compare( iv_compare_length ).
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " calculation search range / matching limit
    search_range = get_search_range(  ).
    "" calculation characters in common
    common_chars_count = get_count_matched_pairs( search_range  ).
    half_of_transpositions = get_half_of_transposition(  ).
    " phonetic adjustment (if needed)
    common_chars_incl_phonetic = common_chars_count + ( get_phonetic_ratio( iv_phonetic_adjust ) / 10 ).
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " рассчитываем похожесть по Jaro
    ev_sim_jaro_winkler =
    ev_sim_jaro = (   ( common_chars_incl_phonetic / strlen( ms_word_pair-word_src ) )
                    + ( common_chars_incl_phonetic / strlen( ms_word_pair-word_trg ) )
                    + ( ( common_chars_count - half_of_transpositions ) / common_chars_count )
                   ) / 3.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Делаем дополнения от Winkler
    " Continue to boost the weight if the strings are similar

    IF ev_sim_jaro GT mv_winkler_limit4adjust.
      adjust_sim_by_common_prefix(  IMPORTING ev_common_prefix = common_prefix_count
                                    CHANGING cv_sim_jaro_winkler = ev_sim_jaro_winkler ).
      IF do_adjust4long_strings( iv_adjust4long = iv_adjust4long
                                 iv_common_chars_count = common_chars_count
                                 iv_common_prefix = common_prefix_count ) EQ abap_true.
        adjust_similarity4long_strings( EXPORTING iv_common_chars_count = common_chars_count
                                                  iv_common_prefix = common_prefix_count
                                        CHANGING cv_sim_jaro_winkler = ev_sim_jaro_winkler ).

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mv_src_init = src_str.
    mv_trg_init = trg_str.
    ms_word_pair-word_src = src_str.
    ms_word_pair-word_trg = trg_str.
    ms_word_pair-min_length =
    nmin( val1 = strlen( ms_word_pair-word_src ) val2 = strlen( ms_word_pair-word_trg ) ).
    mv_provide_equal_length = provide_equal_length.

  ENDMETHOD.


  METHOD cut_the_part2be_compare.
    "        IMPORTING !iv_compare_length TYPE syindex.

    DATA lv_compare_length TYPE syindex.
    IF iv_compare_length GT 0.
      IF strlen( ms_word_pair-word_src ) LT lv_compare_length.
        ms_word_pair-word_src = ms_word_pair-word_src(lv_compare_length).
      ENDIF.

      IF strlen( ms_word_pair-word_trg ) LT lv_compare_length.
        ms_word_pair-word_trg = ms_word_pair-word_trg(lv_compare_length).
      ENDIF.

    ENDIF.

    ms_word_pair-min_length = nmin( val1 = strlen( ms_word_pair-word_src ) val2 = strlen( ms_word_pair-word_trg ) ).

    IF mv_provide_equal_length EQ abap_true.

      IF strlen( ms_word_pair-word_src ) GT strlen( ms_word_pair-word_trg ).
        DO ( strlen( ms_word_pair-word_src ) - strlen( ms_word_pair-word_trg ) ) TIMES.
          ms_word_pair-word_trg = ms_word_pair-word_trg && ` `.
        ENDDO.
      ENDIF.

*    ELSE.
*      ms_word_pair-word_src_length = strlen( ms_word_pair-word_src ).
*      ms_word_pair-word_trg_length = strlen( ms_word_pair-word_trg ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_lead_trail_spaces.
    SHIFT ms_word_pair-word_src LEFT DELETING LEADING space.
    SHIFT ms_word_pair-word_src RIGHT DELETING TRAILING space.
    SHIFT ms_word_pair-word_trg LEFT DELETING LEADING space.
    SHIFT ms_word_pair-word_trg RIGHT DELETING TRAILING space.
  ENDMETHOD.


  METHOD do_adjust4long_strings.
*        IMPORTING !iv_adjust4long type abap_bool
*                  !iv_common_chars_count TYPE syindex
*        RETURNING VALUE(rv_val) TYPE abap_bool.
    rv_val = abap_false.
    IF iv_adjust4long EQ abap_true.
      IF ms_word_pair-min_length GE mc_prefix_normal_limit.
        IF iv_common_chars_count GT ( iv_common_prefix + 1 ).
          IF ( 2 * iv_common_chars_count ) GE ( iv_common_prefix + 1 ).
            IF ms_word_pair-word_src(1) NA '0123456789'.
              rv_val = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD do_upper_case.
    IF iv_case_insensitive EQ abap_true.
      TRANSLATE ms_word_pair-word_src TO UPPER CASE.
      TRANSLATE ms_word_pair-word_trg TO UPPER CASE.
    ENDIF.
  ENDMETHOD.


  METHOD get_count_matched_pairs .

    DATA lv_symbol_num_trg TYPE syindex.
    DATA lv_symbol_num_src TYPE syindex.

    DATA lv_low_lim_char_pos TYPE syindex.
    DATA lv_high_lim_char_pos TYPE syindex.

    FIELD-SYMBOLS <fs_word_meta_trg> TYPE ts_word_meta.
    FIELD-SYMBOLS <fs_word_meta_src> TYPE ts_word_meta.

    mt_word_meta_src = get_word_meta( ms_word_pair-word_src ).
    mt_word_meta_trg = get_word_meta( ms_word_pair-word_trg ).

    lv_symbol_num_src = 0.
    lv_symbol_num_trg = 0.
    rv_matched_pairs = 0.
    LOOP AT mt_word_meta_src ASSIGNING <fs_word_meta_src>.
      lv_symbol_num_src  = 1 + lv_symbol_num_src.
      IF lv_symbol_num_src  GT iv_search_range.
        lv_low_lim_char_pos = lv_symbol_num_src - iv_search_range.
      ELSE.
        lv_low_lim_char_pos = 0.
      ENDIF.

      IF ( lv_symbol_num_src + iv_search_range ) LT strlen( ms_word_pair-word_trg ).
        lv_high_lim_char_pos = lv_symbol_num_src + iv_search_range.
      ELSE.
        lv_high_lim_char_pos = strlen( ms_word_pair-word_trg ).
      ENDIF.

      LOOP AT mt_word_meta_trg ASSIGNING <fs_word_meta_trg> FROM lv_low_lim_char_pos TO lv_high_lim_char_pos.
        " if the symbol is already paired - do not try to pair the symbol
        IF <fs_word_meta_trg>-is_common EQ abap_true.
          CONTINUE.
        ENDIF.
        IF <fs_word_meta_src>-symb EQ <fs_word_meta_trg>-symb.

          <fs_word_meta_src>-is_common = abap_true.
          <fs_word_meta_trg>-is_common = abap_true.
          <fs_word_meta_src>-paired_pos_num = <fs_word_meta_trg>-pos_num.
          <fs_word_meta_trg>-paired_pos_num = <fs_word_meta_src>-pos_num.
          rv_matched_pairs = 1 + rv_matched_pairs.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_half_of_transposition.
    "        RETURNING VALUE(rv_val) TYPE syindex.
    DATA lv_transpositions TYPE syindex.
    get_transpositions( IMPORTING ev_transpositions = lv_transpositions ).
    rv_val = lv_transpositions / 2.

  ENDMETHOD.


  METHOD get_phonetic_ratio.
*IMPORTING !iv_phonetic_adjust  TYPE abap_bool
*      RETURNING VALUE(rv_val) TYPE tv_ratio.

    CLEAR rv_val.
    IF iv_phonetic_adjust EQ abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD get_search_range.
    "        RETURNING VALUE(rv_val) TYPE syindex.
    rv_val  =
    floor( nmax( val1 = strlen( ms_word_pair-word_trg )
                 val2 = strlen( ms_word_pair-word_src ) )
                 / 2 )
      - 1 .
  ENDMETHOD.


  METHOD get_transpositions.
*      IMPORTING it_word_meta_src TYPE tt_word_meta
*                it_word_meta_trg TYPE tt_word_meta
*      EXPORTING ev_transpositions TYPE syindex.

    "" точка отсчета для поиска во втором слове
    DATA from4second_word TYPE syindex.

    FIELD-SYMBOLS <fs_word_meta_trg> TYPE ts_word_meta.
    FIELD-SYMBOLS <fs_word_meta_src> TYPE ts_word_meta.

    from4second_word = 0.
    CLEAR ev_transpositions.
    LOOP AT mt_word_meta_src ASSIGNING <fs_word_meta_trg>
        WHERE is_common = abap_true.

      LOOP AT mt_word_meta_trg ASSIGNING <fs_word_meta_src> FROM from4second_word.
        IF <fs_word_meta_src>-is_common EQ abap_true.
          from4second_word = sy-tabix + 1.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF sy-subrc EQ 0.
        IF <fs_word_meta_trg>-symb NE <fs_word_meta_src>-symb.
          ev_transpositions = 1 + ev_transpositions.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_word_meta.
*      IMPORTING
*        !iv_str       TYPE string
*      RETURNING
*        VALUE(rt_val) TYPE tt_word_meta .

    DATA lt_word_array TYPE tt_word.
    DATA lv_symbol_num_src TYPE syindex.

    FIELD-SYMBOLS <fs_word> TYPE ts_word.
    FIELD-SYMBOLS <fs_word_meta> TYPE ts_word_meta.

    lt_word_array = word2array( iv_str ).

    lv_symbol_num_src = 0.
    LOOP AT lt_word_array ASSIGNING <fs_word>.
      lv_symbol_num_src = 1 + lv_symbol_num_src.
      APPEND INITIAL LINE TO rt_val ASSIGNING <fs_word_meta>.
      <fs_word_meta>-symb = <fs_word>-symb.
      <fs_word_meta>-pos_num = lv_symbol_num_src.
    ENDLOOP.

  ENDMETHOD.


  METHOD word2array.
    DATA lv_char_pos TYPE syindex.
    FIELD-SYMBOLS <fs_word_symb> TYPE ts_word.
    DO strlen( iv_str ) TIMES.
      lv_char_pos = sy-index - 1.
      APPEND INITIAL LINE TO rt_val ASSIGNING <fs_word_symb>.
      <fs_word_symb>-symb = iv_str+lv_char_pos(1).
    ENDDO.
  ENDMETHOD.
ENDCLASS.
