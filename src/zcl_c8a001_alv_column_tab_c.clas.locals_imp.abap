*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*CLASS lcl_view1 DEFINITION INHERITING FROM zcl_sas02_view.
*
*  PUBLIC SECTION.
*    METHODS on_toolbar REDEFINITION.
*    METHODS on_user_com REDEFINITION.
*    METHODS set_changeable REDEFINITION.
*
*  PROTECTED SECTION.
*    METHODS build_layout REDEFINITION.
*    METHODS on_menu_bt_handle REDEFINITION.
*  PRIVATE SECTION.
*
*
*ENDCLASS.                    "lcl_view1 DEFINITION
*
**----------------------------------------------------------------------*
**       CLASS lcl_view1 IMPLEMENTATION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS lcl_view1 IMPLEMENTATION.
*
*  METHOD on_toolbar .
*
*    "super->on_toolbar( ).
*
*    DATA ls_but TYPE stb_button.
*    DATA lv_lines TYPE sytabix.
*
*    DELETE e_object->mt_toolbar WHERE function EQ '&DETAIL'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&&SEP00'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&&SEP01'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&&SEP02'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&&SEP03'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&FIND'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&FIND_MORE'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&PRINT_BACK'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&MB_VIEW'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&&SEP06'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&&GRAPH'.
*
***0  Кнопка (стандартная)
***1  Меню + кнопка по умолчанию
***2  Меню
***3  Разделитель
***4  Зависимая кнопка
***5  Зависимая кнопка (Checkbox)
***6  Пункт меню
**
***    lv_lines = lines( e_object->mt_toolbar ).
***    lv_lines = lv_lines + 1.
***    CLEAR ls_but.
***    ls_but-function = 'ZSEND2CHK'.
***    ls_but-icon = icon_intensify_undo.
***    ls_but-quickinfo  = 'Отправить КК на проверку'.
***    ls_but-text  = 'На проверку'.
***    INSERT ls_but INTO e_object->mt_toolbar INDEX lv_lines.
***
**    lv_lines = lines( e_object->mt_toolbar ).
**    lv_lines = lv_lines + 1.
**    CLEAR ls_but.
**    ls_but-function = 'ZACTIONS'.
**    ls_but-icon = icon_intensify_undo.
**    ls_but-butn_type = '2'.
**    ls_but-quickinfo  = 'Действия'.
**    ls_but-text  = 'Действия'.
**    INSERT ls_but INTO e_object->mt_toolbar INDEX lv_lines.
**
***    lv_lines = lines( e_object->mt_toolbar ).
***    lv_lines = lv_lines + 1.
***    CLEAR ls_but.
***    ls_but-butn_type = '3'.
***    INSERT ls_but INTO e_object->mt_toolbar INDEX lv_lines.
***
***    lv_lines = lines( e_object->mt_toolbar ).
***    lv_lines = lv_lines + 1.
***    CLEAR ls_but.
***    ls_but-function = 'ZFROMARMUP'.
***    ls_but-icon = icon_previous_hierarchy_level.
***    ls_but-quickinfo  = 'Поднять приоритет забора с АРМ'.
***    ls_but-text  = 'Поднять приотритет задания'.
***    INSERT ls_but INTO e_object->mt_toolbar INDEX lv_lines.
**
**    lv_lines = lines( e_object->mt_toolbar ).
**    lv_lines = lv_lines + 1.
**    CLEAR ls_but.
**    ls_but-butn_type = '3'.
**    INSERT ls_but INTO e_object->mt_toolbar INDEX lv_lines.
**
**    lv_lines = lines( e_object->mt_toolbar ).
**    lv_lines = lv_lines + 1.
**    CLEAR ls_but.
**    ls_but-function = 'ZRUNZLTAK'.
**    ls_but-icon = icon_previous_hierarchy_level.
**    ls_but-quickinfo  = 'Открыть ZLTAK'.
**    ls_but-text  = 'Открыть ZLTAK MONITOR'.
**    INSERT ls_but INTO e_object->mt_toolbar INDEX lv_lines.
*
*  ENDMETHOD.                    "on_toolbar
*
*
*  METHOD on_menu_bt_handle.
**    IF e_ucomm EQ 'ZACTIONS'.
**      CALL METHOD e_object->add_function
**        EXPORTING
**          fcode = 'ZSEND2CHK'
**          text  = 'Отправить КК на проверку'.
**
**      CALL METHOD e_object->add_function
**        EXPORTING
**          fcode = 'ZFROMARMUP'
**          text  = 'Поднять приоритет забора с АРМ'.
**
**      CALL METHOD e_object->add_function
**        EXPORTING
**          fcode = 'ZCANSARM'
**          text  = 'Отменить проверку на АРМ'.
**
***      CALL METHOD e_object->add_function
***        EXPORTING
***          fcode = 'ZCLEAR_RESARM'
***          text  = 'Очистить результат на АРМ'.
**
**    ENDIF.
*  ENDMETHOD.                    "on_menu_bt_handle
*
*  METHOD on_user_com.
**    DATA lv_ucomm_str TYPE string.
**    CASE e_ucomm.
**      WHEN 'ZCLEAR'.
***        RAISE EVENT ev1 EXPORTING ev_type = 'ZCLEAR'.
***
**      WHEN 'ZREMLINE'.
***        me->raise_remline( ).
***
**
**      WHEN 'ZFILESCHNG' OR 'ZCNHSAVE' OR 'ZCNHCNCL' .
**        lv_ucomm_str = e_ucomm.
**        RAISE EVENT ev1 EXPORTING ev_type = lv_ucomm_str.
**
**      WHEN OTHERS.
**        me->raise_normal_event( EXPORTING iv_ucomm = e_ucomm ).
**    ENDCASE.
*  ENDMETHOD.                    "on_user_com
*
*
*
*  METHOD set_changeable .
*  ENDMETHOD.                    "set_changeable
*  METHOD build_layout .
*    FIELD-SYMBOLS <fs_fcat> TYPE lvc_s_fcat.
*
**    LOOP AT ct_fieldcat ASSIGNING <fs_fcat>.
**
**      CASE <fs_fcat>-fieldname.
**        WHEN 'NUMOF_BSKNR'.
**          <fs_fcat>-scrtext_s =
**          <fs_fcat>-scrtext_m =
**          <fs_fcat>-scrtext_l =
**          'Кол-во корзин'.
**
**        WHEN OTHERS.
**      ENDCASE.
**
**    ENDLOOP.
*
*  ENDMETHOD.                    "build_layout
*
*ENDCLASS.                    "lcl_view1 IMPLEMENTATION
