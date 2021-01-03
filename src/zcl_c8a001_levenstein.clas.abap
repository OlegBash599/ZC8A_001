CLASS zcl_c8a001_levenstein DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_src_str TYPE string
        !iv_trg_str TYPE string.

    METHODS calc_CUSTOM
      EXPORTING
        !ev_distance   TYPE syindex
        !ev_similarity TYPE syindex .

    METHODS calc_std
      EXPORTING
        !ev_distance   TYPE syindex
        !ev_similarity TYPE syindex .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_src_str TYPE string.
    DATA mv_trg_str TYPE string.

ENDCLASS.



CLASS zcl_c8a001_levenstein IMPLEMENTATION.
  METHOD constructor.
    mv_src_str = iv_src_str.
    mv_trg_str = iv_trg_str.
  ENDMETHOD.

  METHOD calc_std.
    DATA lv_edit_distance TYPE syindex.

    ev_distance = distance( val1 = mv_src_str val2 = mv_trg_str ).
    IF ev_distance IS INITIAL.
      ev_similarity = 0.
    ELSE.
      ev_similarity =
      100 - ( 100 * ev_distance / nmax( val1 = strlen( mv_src_str ) val2 = strlen( mv_trg_str ) ) ) .
    ENDIF.
  ENDMETHOD.

  METHOD calc_CUSTOM.

    TYPES: BEGIN OF ts_matrix_cell
            , i TYPE syindex
            , j TYPE syindex
            , src_symbol TYPE char1
            , trg_symbol TYPE char1
            , from_to_dis TYPE syindex
          , END OF ts_matrix_cell
          , tt_matrix TYPE SORTED TABLE OF ts_matrix_cell
            WITH UNIQUE KEY primary_key COMPONENTS i j
          .

    DATA lt_matrix TYPE tt_matrix.
    DATA ls_matrix_cell TYPE ts_matrix_cell.

    DATA src_str TYPE string VALUE 'стoлб'.
    DATA trg_str TYPE string VALUE 'стyл'.
    DATA src_str_length TYPE syindex.
    DATA trg_str_length TYPE syindex.
    DATA cost4change TYPE syindex.

    DATA symbol_index_position_i TYPE syindex.
    DATA symbol_index_position_j TYPE syindex.

    FIELD-SYMBOLS <fs_matrix> TYPE ts_matrix_cell.

    src_str = mv_src_str.
    trg_str = mv_trg_str.

    src_str_length = strlen( src_str ).
    trg_str_length = strlen( trg_str ).

    CLEAR lt_matrix.
    " строим матрицу для поэлементного расчета
    DO ( src_str_length + 1 ) TIMES.
      ls_matrix_cell-i = sy-index - 1.
      DO ( trg_str_length + 1 ) TIMES.
        ls_matrix_cell-j = sy-index - 1.
        INSERT ls_matrix_cell INTO TABLE lt_matrix.
      ENDDO.
    ENDDO.

    " рассчитываем расстояние для каждого элемента матрицы
    LOOP AT lt_matrix ASSIGNING <fs_matrix>.
      """"""""""""""""""""""""""""""""""
      """ { <<нулевые>> случаи - нужны для начально заполнения (0,j) и (i,0). {{{{{
      IF <fs_matrix>-j EQ 0
        AND <fs_matrix>-i EQ 0.
        <fs_matrix>-from_to_dis = 0.
        <fs_matrix>-src_symbol = space.
        <fs_matrix>-trg_symbol = space.
        CONTINUE.
      ENDIF.
      """"""""""""""""""""""""""""""""""
      IF <fs_matrix>-i EQ 0.
        <fs_matrix>-from_to_dis = <fs_matrix>-j.
        <fs_matrix>-src_symbol = space.
        <fs_matrix>-trg_symbol = trg_str(<fs_matrix>-j).
        CONTINUE.
      ENDIF.
      """"""""""""""""""""""""""""""""""
      IF <fs_matrix>-j EQ 0.
        <fs_matrix>-from_to_dis = <fs_matrix>-i.
        <fs_matrix>-src_symbol = src_str(<fs_matrix>-i).
        <fs_matrix>-trg_symbol = space.
        CONTINUE.
      ENDIF.
      """ <<нулевые>> случаи - нужны для начально заполнения (0,j) и (i,0). }}}}}
      """"""""""""""""""""""""""""""""""

      """" ненулевые случаи {{{  рекурентный расчет
      symbol_index_position_i = <fs_matrix>-i - 1.
      symbol_index_position_j = <fs_matrix>-j - 1.
      <fs_matrix>-src_symbol = src_str+symbol_index_position_i(1).
      <fs_matrix>-trg_symbol = trg_str+symbol_index_position_j(1).
      cost4change = 0.
      IF <fs_matrix>-src_symbol EQ <fs_matrix>-trg_symbol.
        cost4change = 0.
      ELSE.
        cost4change = 1.
      ENDIF.
      <fs_matrix>-from_to_dis =
      nmin( val1 = ( lt_matrix[ i = ( <fs_matrix>-i - 1 ) j = <fs_matrix>-j         ]-from_to_dis + cost4change )
            val2 = ( lt_matrix[ i = <fs_matrix>-i         j = ( <fs_matrix>-j - 1 ) ]-from_to_dis + cost4change )
            val3 = ( lt_matrix[ i = ( <fs_matrix>-i - 1 ) j = ( <fs_matrix>-j - 1 ) ]-from_to_dis + cost4change ) ).


    ENDLOOP.

    ev_distance = lt_matrix[ lines( lt_matrix ) ]-from_to_dis.

    IF ev_distance IS INITIAL.
      ev_similarity = 0.
    ELSE.
      ev_similarity = 100 - ( 100 * ev_distance / nmax( val1 = src_str_length val2 = trg_str_length ) ) .
    ENDIF.
  ENDMETHOD.

ENDCLASS.
