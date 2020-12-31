class ZCL_C8A001_VIEW definition
  public
  create public .

public section.

  types:
    BEGIN OF ts_4alv
              , alv_struct TYPE tabname
              , cust_area TYPE C LENGTH 50
              , handle TYPE slis_handl
              , stable TYPE lvc_s_stbl
              , grid_title TYPE LVC_TITLE
            , END OF ts_4alv .
  types:
    tt_4alv TYPE STANDARD TABLE OF ts_4alv .
  types:
    BEGIN OF ts_line_mvc
              , line_mvc TYPE sytabix
            , END OF ts_line_mvc .
  types:
    tt_line_mvc TYPE SORTED TABLE OF ts_line_mvc
              WITH UNIQUE KEY primary_key COMPONENTS line_mvc .

  data MT_LINE_MVC type TT_LINE_MVC read-only .
  data MS_LINE_MVC type TS_LINE_MVC .
  data MT_FIELDCAT type LVC_T_FCAT .
  data MS_VARIANT type DISVARIANT .
  data MO_DOCKING type ref to CL_GUI_DOCKING_CONTAINER .
  data MO_GRID type ref to CL_GUI_ALV_GRID .
  data MS_TY_LAY type LVC_S_LAYO .
  data MO_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data MO_DOCK_CONT type ref to DATA .
  data MT_TAB_REF type ref to DATA .

  events EV1
    exporting
      value(EV_TYPE) type STRING .

  methods CONSTRUCTOR
    importing
      !IV_ALV_STRUCT type TABNAME
      !IV_CUST_AREA type STRING optional
      !IV_HANDLE type SLIS_HANDL
      !IV_DOCK_POS type I default 1 .
  methods SET_VIEW
    importing
      !IT_TAB type ANY TABLE .
  methods SYUCOMM_PROC
    importing
      !IV_SYUCOMM type SYUCOMM
    returning
      value(R_VAL) type SAP_BOOL .
  methods FREE .
  methods MOD_STAT_OUT
    changing
      !CT_TAB type STANDARD TABLE .
  methods GUI_FUNC_ZREFRESH .
  methods GUI_FUNC_ZLOG .
  methods ON_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods ON_USER_COM
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods SET_CHANGEABLE
    importing
      !IV_MODE type CHAR1 default 'A' .
  methods RAISE_NORMAL_EVENT
    importing
      !IV_UCOMM type SYUCOMM .
  type-pools ABAP .
  methods FILL_SEL_LINES2MVC
    importing
      !IV_SEL_ALL type CHAR1 default ABAP_FALSE .
  methods OPEN_PANEL
    importing
      !IV_MODE type CHAR1 optional .
  methods IS_PANEL_OPEN
    returning
      value(RV_VAL) type CHAR1 .
  methods SELECTED_LINE_SAVE .
  methods SET_SELECT_LINE .
  methods REFRESH_TAB_DISPLAY .
  methods FLUSH
    importing
      !IV_UCOMM type SYUCOMM default 'TRIG_PAI' .
  methods SYUCOMM_SCREEN
    importing
      !IV_SYUCOMM type SYUCOMM
    returning
      value(R_VAL) type SAP_BOOL .
  methods DO_TAB .
  methods SET_REFERENCE2TAB
    changing
      !CT_TAB type STANDARD TABLE .
  methods SET_DTO
    importing
      !IO_DTO type ref to ZIF_C8A001_ALV_DTO .
protected section.

  data MS_4ALV type TS_4ALV .
  data MV_GRID_WAS_CREATED type CHAR1 .
  data MV_DOC_EXTENSION type I value 2500. "#EC NOTEXT
  type-pools ABAP .
  data MV_PANEL_OPEN type SAP_BOOL value ABAP_FALSE. "#EC NOTEXT

  methods ON_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO .
  methods BUILD_LAYOUT
    changing
      !CT_FIELDCAT type LVC_T_FCAT .
  methods ALV_LAYOUT
    changing
      !CS_LAYOUT type LVC_S_LAYO .
  methods ON_MENU_BT_HANDLE
    for event MENU_BUTTON of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_UCOMM .
private section.

  CONSTANTS mc_char1_a TYPE char1 VALUE 'A'.

  data MT_VIEW type ref to DATA .
  data MT_INDEX_ROWS type LVC_T_ROW .
  data MT_ROW_NO type LVC_T_ROID .
  data MO_DTO type ref to ZIF_C8A001_ALV_DTO .

  methods CREATE_DOCK .
  type-pools ABAP .
  methods CREATE_GRID
    importing
      !IV_BCGR type CHAR1 default ABAP_FALSE .
  methods FILL_FIELD_CATALOG
    returning
      value(RV_RETRN) type CHAR1 .
  methods HANDLERS_AND_VARIANT .
  methods SET_TAB4_1ST_DISPLAY
    changing
      !CT_TAB type STANDARD TABLE .
  methods CLEAR_MVC_NUM .
ENDCLASS.



CLASS ZCL_C8A001_VIEW IMPLEMENTATION.


METHOD alv_layout.
  cs_layout-sel_mode = mc_char1_a.
  cs_layout-zebra = abap_true.
  cs_layout-grid_title = me->ms_4alv-grid_title.

  cs_layout-col_opt = abap_true.
  cs_layout-ctab_fname = 'TABCOLOR'.


ENDMETHOD.


METHOD BUILD_LAYOUT.
  FIELD-SYMBOLS: <fs_fcat> TYPE lvc_s_fcat.

  LOOP AT ct_fieldcat ASSIGNING <fs_fcat>.
    CASE <fs_fcat>-fieldname.
      WHEN 'LINE_MVC' OR 'UPDATE'.
        "<fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech = abap_true.

      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.
ENDMETHOD.


METHOD CLEAR_MVC_NUM.
  CLEAR me->mt_line_mvc[].
  CLEAR me->ms_line_mvc.
ENDMETHOD.


METHOD CONSTRUCTOR.
  ms_4alv-alv_struct = iv_alv_struct.
  ms_4alv-cust_area = iv_cust_area.
  ms_4alv-handle = iv_handle.
  ms_4alv-stable = 'XX'.
ENDMETHOD.


METHOD CREATE_DOCK.
  IF me->ms_4alv-cust_area IS INITIAL.
    IF me->mo_docking IS INITIAL.
      CREATE OBJECT me->mo_docking
        EXPORTING
*           parent    =
*           repid     =
*           dynnr     =
*           side      = DOCK_AT_LEFT
"          extension = 2000
          extension = me->mv_doc_extension
*           style     =
*           lifetime  = lifetime_default
*           caption   =
*           metric    = 0
*           ratio     =
*           no_autodef_progid_dynnr     =
*           name      =
*      exceptions
*           cntl_error                  = 1
*           cntl_system_error           = 2
*           create_error                = 3
*           lifetime_error              = 4
*           lifetime_dynpro_dynpro_link = 5
*           others    = 6
        .
      IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      GET REFERENCE OF me->mo_docking INTO me->mo_dock_cont.
    ELSE.

      CALL METHOD me->mo_docking->set_adjust_design
        EXPORTING
          adjust_design = 1
*        exceptions
*           cntl_error    = 1
*           cntl_system_error = 2
*           others        = 3
        .
      IF sy-subrc <> 0.
*       Implement suitable error handling here
      ENDIF.


    ENDIF.
  ELSE.
    IF mo_container IS INITIAL.
      CREATE OBJECT mo_container
        EXPORTING
"          parent         = cl_gui_custom_container=>screen0
          container_name = me->ms_4alv-cust_area
        EXCEPTIONS
          others         = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        GET REFERENCE OF me->mo_container INTO me->mo_dock_cont.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD CREATE_GRID.
  IF me->mo_grid IS INITIAL.
    IF iv_bcgr EQ abap_true.
      CREATE OBJECT me->mo_grid
          EXPORTING
*                   i_shellstyle      = 0
*                   i_lifetime        =
            i_parent          = cl_gui_docking_container=>default_screen "
            "i_parent          = CL_GUI_DOCKING_CONTAINER=>SCREEN0 "
        "        i_parent          = me->mo_docking
*                   i_appl_events     = space
*                   i_parentdbg       =
*                   i_applogparent    =
*                   i_graphicsparent  =
*                   i_name            =
*                   i_fcat_complete   = SPACE
          EXCEPTIONS
            error_cntl_create = 1
            error_cntl_init   = 2
            error_cntl_link   = 3
            error_dp_create   = 4
            OTHERS            = 5.
    ELSE.
      IF me->ms_4alv-cust_area IS INITIAL.
        CREATE OBJECT me->mo_grid
          EXPORTING
*                   i_shellstyle      = 0
*                   i_lifetime        =
            "i_parent          = CL_GUI_DOCKING_CONTAINER=>DEFAULT_SCREEN "
            "i_parent          = CL_GUI_DOCKING_CONTAINER=>SCREEN0 "
            i_parent          = me->mo_docking
*                   i_appl_events     = space
*                   i_parentdbg       =
*                   i_applogparent    =
*                   i_graphicsparent  =
*                   i_name            =
*                   i_fcat_complete   = SPACE
          EXCEPTIONS
            error_cntl_create = 1
            error_cntl_init   = 2
            error_cntl_link   = 3
            error_dp_create   = 4
            OTHERS            = 5.
      ELSE.
        CREATE OBJECT me->mo_grid
          EXPORTING
*                   i_shellstyle      = 0
*                   i_lifetime        =
            "i_parent          = CL_GUI_DOCKING_CONTAINER=>DEFAULT_SCREEN "
            "i_parent          = CL_GUI_DOCKING_CONTAINER=>SCREEN0 "
            i_parent          = me->mo_container
*                   i_appl_events     = space
*                   i_parentdbg       =
*                   i_applogparent    =
*                   i_graphicsparent  =
*                   i_name            =
*                   i_fcat_complete   = SPACE
          EXCEPTIONS
            error_cntl_create = 1
            error_cntl_init   = 2
            error_cntl_link   = 3
            error_dp_create   = 4
            OTHERS            = 5.
      ENDIF.
    ENDIF.

    IF sy-subrc EQ 0.
      me->mv_grid_was_created = abap_true.

      " event for double click
      ""[[[[
      CALL METHOD me->mo_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      """]]]]
    ENDIF.
  ELSE.
    me->mv_grid_was_created = abap_false.
  ENDIF.
ENDMETHOD.


METHOD DO_TAB.

  FIELD-SYMBOLS <fs_stan_tab> TYPE STANDARD TABLE.

  ASSIGN me->mt_tab_ref->* TO <fs_stan_tab>.
  IF sy-subrc EQ 0.
    me->mod_stat_out( CHANGING ct_tab = <fs_stan_tab> ).
  ENDIF.
ENDMETHOD.


METHOD FILL_FIELD_CATALOG.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = me->ms_4alv-alv_struct
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = me->mt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    rv_retrn = 'E'.
  ELSE.
    CLEAR rv_retrn.
  ENDIF.


ENDMETHOD.


METHOD FILL_SEL_LINES2MVC.
  DATA lt_row TYPE lvc_t_row.

  DATA lt_index_rows  TYPE lvc_t_row.
  DATA lt_row_no  TYPE lvc_t_roid.
  DATA ls_line_mvc TYPE  ts_line_mvc.

  FIELD-SYMBOLS <fs_line_mvc> TYPE ts_line_mvc.
  FIELD-SYMBOLS <fs_row_no> TYPE lvc_s_roid.
  FIELD-SYMBOLS <fs_ref> TYPE any.
  FIELD-SYMBOLS <fs_line_mvc_comp> TYPE any.
  FIELD-SYMBOLS <fs_ref_tab> TYPE ANY TABLE.

  IF iv_sel_all EQ abap_true.

  ELSE.
    me->mo_grid->get_selected_rows(
     IMPORTING et_index_rows = lt_index_rows
            et_row_no     =  lt_row_no ).

  ENDIF.


  IF me->mt_tab_ref IS BOUND.
    ASSIGN me->mt_tab_ref->* TO  <fs_ref_tab>.
  ELSE.
    RETURN.
  ENDIF.

  CLEAR me->mt_line_mvc.
  IF iv_sel_all EQ abap_true.
    " режим выделения всех записей
    LOOP AT <fs_ref_tab> ASSIGNING <fs_ref>.
      ASSIGN COMPONENT 'LINE_MVC' OF STRUCTURE <fs_ref> TO <fs_line_mvc_comp>.
      IF sy-subrc EQ 0.
        ls_line_mvc-line_mvc = <fs_line_mvc_comp>.
        INSERT ls_line_mvc INTO TABLE me->mt_line_mvc.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT lt_row_no ASSIGNING <fs_row_no>.

*      READ TABLE <fs_ref_tab> ASSIGNING <fs_ref> INDEX <fs_row_no>-row_id.
*      IF sy-subrc EQ 0.
*
*      ENDIF.
      LOOP AT <fs_ref_tab> ASSIGNING <fs_ref>.
        IF sy-tabix EQ <fs_row_no>-row_id.
          ASSIGN COMPONENT 'LINE_MVC' OF STRUCTURE <fs_ref> TO <fs_line_mvc_comp>.
          IF sy-subrc EQ 0.
            "APPEND INITIAL LINE TO me->mt_line_mvc ASSIGNING <fs_line_mvc>.
            "<fs_line_mvc>-line_mvc = <fs_line_mvc_comp>.
            ls_line_mvc-line_mvc = <fs_line_mvc_comp>.
            INSERT ls_line_mvc INTO TABLE me->mt_line_mvc.


          ENDIF.
          EXIT.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD FLUSH.
  CALL METHOD cl_gui_cfw=>flush.

  call METHOD cl_gui_cfw=>set_new_ok_code( NEW_CODE = iv_ucomm ).

ENDMETHOD.


METHOD FREE.
  IF me->mo_grid IS BOUND.
    me->mo_grid->free( ).
  ENDIF.

  IF me->mo_docking IS BOUND.
    me->mo_docking->free( ).
  ENDIF.

*  IF me->mo_dock_cont IS BOUND.
*    me->mo_dock_cont->free( ).
*  ENDIF.

  IF me->mo_container IS BOUND.
    me->mo_container->free( ).
  ENDIF.

  free: me->mo_grid, me->mo_docking, me->mo_container.

ENDMETHOD.


METHOD GUI_FUNC_ZLOG.
  RAISE EVENT ev1 EXPORTING ev_type = 'ZLOG'.
ENDMETHOD.


METHOD GUI_FUNC_ZREFRESH.
  RAISE EVENT ev1 EXPORTING ev_type = 'ZREFRESH'.
ENDMETHOD.


METHOD handlers_and_variant.

  IF mo_dto IS BOUND.
    IF mo_dto->ms_variant IS NOT INITIAL.
      me->ms_variant = mo_dto->ms_variant.
    ENDIF.
  ENDIF.

  IF me->ms_variant IS INITIAL.
    me->ms_variant-report = sy-cprog.
    me->ms_variant-handle = ms_4alv-handle.
    me->ms_variant-username = sy-uname.
  ENDIF.


  SET HANDLER me->on_double_click FOR me->mo_grid.
  SET HANDLER me->on_toolbar FOR me->mo_grid.
  SET HANDLER me->on_user_com FOR me->mo_grid.
  SET HANDLER me->on_menu_bt_handle FOR me->mo_grid.
ENDMETHOD.


METHOD IS_PANEL_OPEN.

  rv_val = me->mv_panel_open.


ENDMETHOD.


METHOD MOD_STAT_OUT.
  CLEAR me->mv_grid_was_created.

  IF cl_gui_alv_grid=>offline( ) IS INITIAL.
    me->create_dock( ).
    me->create_grid( ).
  ELSE.
    me->create_grid( iv_bcgr = abap_true ).
  ENDIF.

  IF me->fill_field_catalog( ) EQ 'E'.
    RETURN.
  ENDIF.

  IF me->mv_grid_was_created EQ abap_true.
    me->handlers_and_variant(  ).
    me->build_layout( CHANGING ct_fieldcat = me->mt_fieldcat ).
    me->alv_layout( CHANGING cs_layout = me->ms_ty_lay ).
    me->set_tab4_1st_display( CHANGING ct_tab = ct_tab ).
  ELSE.
    me->refresh_tab_display( ).
  ENDIF.


ENDMETHOD.


METHOD on_double_click.


  FIELD-SYMBOLS <fs_tab> TYPE any.
  FIELD-SYMBOLS <fs_tab_line> TYPE any.
  ASSIGN me->mt_view->* TO <fs_tab>.
  "FIELD-SYMBOLS <fs_view> LIKE LINE OF <fs_tab>.

  IF e_row IS INITIAL
      OR es_row_no IS INITIAL
      .
    RETURN.
  ENDIF.

  " clear_mvc_num( ).

  " me->ms_line_mvc-line_mvc = e_row-index.
  " APPEND me->ms_line_mvc TO me->mt_line_mvc.

  " RAISE EVENT ev1 EXPORTING ev_type = 'ZDOUBLE_CLICK'.



  IF mo_dto IS BOUND.
    FIELD-SYMBOLS <fs_stan_tab> TYPE STANDARD TABLE.
    data lr_tab_line_clicked TYPE REF TO data.
    ASSIGN me->mt_tab_ref->* TO <fs_stan_tab>.
    IF sy-subrc EQ 0.
      READ TABLE <fs_stan_tab> REFERENCE INTO lr_tab_line_clicked INDEX e_row-index.
      IF sy-subrc EQ 0.
        mo_dto->on_double_click( EXPORTING e_row = e_row
                                               e_column = e_column
                                               es_row_no = es_row_no
                                               io_grid = mo_grid
                                               isr_line_clicked = lr_tab_line_clicked ).
      ELSE.

      ENDIF.

    ENDIF.


  ENDIF.


ENDMETHOD.


method ON_MENU_BT_HANDLE.
endmethod.


METHOD ON_TOOLBAR.
  DATA ls_but TYPE stb_button.
  DATA lv_lines TYPE sytabix.

  DELETE e_object->mt_toolbar WHERE function EQ '&DETAIL'.
  DELETE e_object->mt_toolbar WHERE function EQ '&&SEP00'.
  DELETE e_object->mt_toolbar WHERE function EQ '&&SEP01'.
  DELETE e_object->mt_toolbar WHERE function EQ '&&SEP02'.
  DELETE e_object->mt_toolbar WHERE function EQ '&&SEP03'.
  DELETE e_object->mt_toolbar WHERE function EQ '&FIND'.
  DELETE e_object->mt_toolbar WHERE function EQ '&FIND_MORE'.
  DELETE e_object->mt_toolbar WHERE function EQ '&PRINT_BACK'.
  DELETE e_object->mt_toolbar WHERE function EQ '&MB_VIEW'.
  DELETE e_object->mt_toolbar WHERE function EQ '&&SEP06'.
  DELETE e_object->mt_toolbar WHERE function EQ '&&GRAPH'.


*  lv_lines = lines( e_object->mt_toolbar ).
*  lv_lines = lv_lines + 1.
*  CLEAR ls_but.
*  ls_but-function = 'ZSHOW_DOC'.
*  ls_but-icon = icon_system_possible_entries.
*  ls_but-quickinfo  = 'Просмотр документа'.
*  ls_but-text  = 'Просмотр'.
*  INSERT ls_but INTO e_object->mt_toolbar INDEX lv_lines.
  IF mo_dto IS BOUND.
    mo_dto->on_toolbar( e_object ).
  ENDIF.

ENDMETHOD.


METHOD ON_USER_COM.

  FIELD-SYMBOLS <fs_stan_tab> TYPE STANDARD TABLE.
  IF mo_dto IS BOUND.

    ASSIGN me->mt_tab_ref->* TO <fs_stan_tab>.
    IF sy-subrc EQ 0.
      mo_dto->handle_user_com( iv_ucomm = e_ucomm
                               io_grid = mo_grid
                               ir_stan_tab = <fs_stan_tab> ).
    ELSE.
      mo_dto->handle_user_com( iv_ucomm = e_ucomm
                             io_grid = mo_grid ).
    ENDIF.


  ENDIF.

ENDMETHOD.


METHOD OPEN_PANEL.
  DATA:  lv_ext TYPE i
       , lv_ratio TYPE i
       , lv_ratio100 TYPE i VALUE '100'
       .

  CALL METHOD me->mo_docking->get_ratio(
    IMPORTING
      ratio = lv_ratio ).

  CALL METHOD me->mo_docking->get_extension
    IMPORTING
      extension = lv_ext.

  CASE iv_mode.
    WHEN 'A'.
      mv_panel_open = abap_true.
      IF lv_ratio = 100.
        "lv_ext = lv_ext * lv_ratio / 200.
        lv_ext = lv_ext * lv_ratio / 400.
      ELSE.
        " lv_ext = 2000.
      ENDIF.

    WHEN 'B'.
      mv_panel_open = abap_false.
      IF lv_ratio >= 95 .
        "lv_ext = lv_ext * lv_ratio / 200.
        lv_ext = lv_ext * lv_ratio100 / 400.
      ELSE.
        "lv_ext = 2500.
        " значит под экран уже свернутый
        " и в этом режиме нам не нужно его открытьва повторно

      ENDIF.

    WHEN 'Z'.
      lv_ext = me->mv_doc_extension.

      mv_panel_open = abap_false.

    WHEN OTHERS.
      IF lv_ratio < 90.
        lv_ext = me->mv_doc_extension.
        mv_panel_open = abap_false.
      ELSE.
        "lv_ext = lv_ext  * lv_ratio / 200.
        "lv_ext = lv_ext  * lv_ratio / 400.
        lv_ext = lv_ext  * lv_ratio / 800.
        mv_panel_open = abap_true.
      ENDIF.
  ENDCASE.
  CALL METHOD me->mo_docking->set_extension(
    EXPORTING
      extension = lv_ext ).

  CALL METHOD me->set_select_line( ).
ENDMETHOD.


METHOD RAISE_NORMAL_EVENT.
  DATA lv_str_ucomm TYPE string.
  DATA lv_sel_all TYPE char1.
  CLEAR me->mt_line_mvc[].
  IF iv_ucomm EQ 'ZSAVEALL'.
    lv_sel_all = abap_true.
  ELSE.
    lv_sel_all = abap_false.
  ENDIF.
  me->fill_sel_lines2mvc( iv_sel_all = lv_sel_all ).
  IF me->mt_line_mvc[] IS INITIAL.
    message s037.
  ELSE.
    lv_str_ucomm = iv_ucomm.
    RAISE EVENT ev1 EXPORTING ev_type = lv_str_ucomm.
  ENDIF.
ENDMETHOD.


METHOD REFRESH_TAB_DISPLAY.
  CALL METHOD me->mo_grid->refresh_table_display
    EXPORTING
      is_stable      = me->ms_4alv-stable
*     i_soft_refresh =
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*       Implement suitable error handling here
  ENDIF.
ENDMETHOD.


METHOD SELECTED_LINE_SAVE.
  DATA lt_row TYPE lvc_t_row.

  DATA lt_index_rows  TYPE lvc_t_row.
  DATA lt_row_no  TYPE lvc_t_roid.


  me->mo_grid->get_selected_rows(
    IMPORTING et_index_rows = mt_index_rows
              et_row_no     =  mt_row_no ).

ENDMETHOD.


METHOD SET_CHANGEABLE.
  DATA lt_fc  TYPE lvc_t_fcat.
  " get layout

  CALL METHOD me->mo_grid->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fc.


  " set layout
  CALL METHOD me->mo_grid->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = lt_fc.
ENDMETHOD.


METHOD SET_DTO.
  mo_dto ?= io_dto.
ENDMETHOD.


METHOD SET_REFERENCE2TAB.

  GET REFERENCE OF ct_tab INTO me->mt_tab_ref.


ENDMETHOD.


METHOD SET_SELECT_LINE.
  IF me->mt_row_no[] IS INITIAL.
  ELSE.
    me->mo_grid->set_selected_rows(
    EXPORTING it_row_no =  me->mt_row_no
      ).
  ENDIF.

ENDMETHOD.


METHOD SET_TAB4_1ST_DISPLAY.

  FIELD-SYMBOLS <fs_stan_tab> TYPE STANDARD TABLE.

  set_reference2tab( CHANGING ct_tab = ct_tab ).

  ASSIGN me->mt_tab_ref->* TO <fs_stan_tab>.
  IF sy-subrc EQ 0.

    CALL METHOD me->mo_grid->set_table_for_first_display
      EXPORTING
*       i_buffer_active               =
*       i_bypassing_buffer            =
*       i_consistency_check           =
*       i_structure_name              =
        is_variant                    = me->ms_variant
        i_save                        = 'A' "CONV char01( text-cca )
        i_default                     = abap_true
        is_layout                     = me->ms_ty_lay
*       is_print                      =
*       it_special_groups             =
*       it_toolbar_excluding          =
*       it_hyperlink                  =
*       it_alv_graphics               =
*       it_except_qinfo               =
*       ir_salv_adapter               =
      CHANGING
*       it_outtab                     = mt_view->*[]
*       it_outtab                     = <fs_stan_tab>
                    "it_outtab                     = ct_tab
        it_outtab                     = <fs_stan_tab>[]
        it_fieldcatalog               = me->mt_fieldcat
*       it_sort                       =
*       it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*             Implement suitable error handling here
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD SET_VIEW.
  GET REFERENCE OF it_tab INTO mt_view.
ENDMETHOD.


METHOD SYUCOMM_PROC.
  DATA lv_save TYPE syucomm.
  lv_save = iv_syucomm.
  IF mo_grid IS BOUND.
    me->mo_grid->set_function_code( CHANGING c_ucomm = lv_save ).
  ENDIF.
*  IF lv_save eq 'TRIG_PAI'.
*    clear lv_save.
*  ENDIF.
  IF lv_save IS INITIAL.
    r_val = abap_true.
  ELSE.
    r_val = abap_false.
  ENDIF.
ENDMETHOD.


METHOD SYUCOMM_SCREEN.

  IF iv_syucomm IS INITIAL.
    RETURN.
  ENDIF.

*    CASE abap_true.
*      WHEN ms_scr_cntx-r1.

  " есть ли функция в стандартном ALV или в Custom
  IF me->syucomm_proc( iv_syucomm = iv_syucomm ) EQ abap_true.
  ELSE.
    " если нет - то это специфичная фунцкия
    "    me->call_spec_func( iv_syucomm = iv_syucomm ).
  ENDIF.

  "me->clear_syucomm( ).

ENDMETHOD.
ENDCLASS.
