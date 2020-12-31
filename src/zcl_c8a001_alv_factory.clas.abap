*----------------------------------------------------------------------*
*       CLASS ZCL_C8A001_ALV_FACTORY DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_c8a001_alv_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_ins
      RETURNING
        value(ro_obj) TYPE REF TO zcl_c8a001_alv_factory .
    METHODS alv_column_tab
      IMPORTING
        !iv_struct TYPE tabname OPTIONAL
      RETURNING
        value(ro_obj) TYPE REF TO zif_c8a001_alv_column_tab .
    METHODS alv_on_dto
      IMPORTING
        !io_dto4alv TYPE REF TO zif_c8a001_alv_dto
      RETURNING
        value(ro_obj) TYPE REF TO zif_c8a001_alvtab_on_dto .
    METHODS get_dto4alv
      RETURNING
        value(ro_obj) TYPE REF TO zif_c8a001_alv_dto .
    METHODS alv_on_dto_free .
    METHODS get_db_supply
       RETURNING
        value(ro_obj) TYPE REF TO zif_c8a001_db_supply .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_obj
                , obj_n TYPE seoclsname
                , obj TYPE REF TO object
            , END OF ts_obj .
    TYPES:
      tt_obj TYPE SORTED TABLE OF ts_obj
              WITH UNIQUE KEY primary_key COMPONENTS obj_n .

    CLASS-DATA mo_obj TYPE REF TO zcl_c8a001_alv_factory .
    DATA mt_obj TYPE tt_obj .

    METHODS constructor .
ENDCLASS.                    "ZCL_C8A001_ALV_FACTORY DEFINITION



*----------------------------------------------------------------------*
*       CLASS ZCL_C8A001_ALV_FACTORY IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_c8a001_alv_factory IMPLEMENTATION.


  METHOD alv_column_tab.
    DATA lv_cls_name TYPE seoclsname VALUE 'ZCL_C8A001_ALV_COLUMN_TAB_B'.

    DATA lr_obj TYPE REF TO ts_obj.

    READ TABLE mt_obj REFERENCE INTO lr_obj
      WITH KEY obj_n = lv_cls_name.
    IF sy-subrc EQ 0.
      ro_obj ?= lr_obj->obj.
    ELSE.
      "APPEND INITIAL LINE TO mt_obj ASSIGNING <fs_obj>.
      CREATE DATA lr_obj.
      lr_obj->obj_n = lv_cls_name.
      CREATE OBJECT ro_obj TYPE (lv_cls_name)
        EXPORTING
          iv_structure_name = iv_struct.
      lr_obj->obj ?= ro_obj.


      INSERT lr_obj->* INTO TABLE mt_obj.

    ENDIF.


  ENDMETHOD.                    "ALV_COLUMN_TAB


  METHOD alv_on_dto.
    DATA lv_cls_name TYPE seoclsname VALUE 'ZCL_C8A001_ALVTAB_ON_DTO_B'.

    DATA lr_obj TYPE REF TO ts_obj.

    READ TABLE mt_obj REFERENCE INTO lr_obj
      WITH KEY obj_n = lv_cls_name.
    IF sy-subrc EQ 0.
      ro_obj ?= lr_obj->obj.
    ELSE.
      "APPEND INITIAL LINE TO mt_obj ASSIGNING <fs_obj>.
      CREATE DATA lr_obj.
      lr_obj->obj_n = lv_cls_name.
      CREATE OBJECT ro_obj TYPE (lv_cls_name)
        EXPORTING
          io_dto = io_dto4alv.
      lr_obj->obj ?= ro_obj.


      INSERT lr_obj->* INTO TABLE mt_obj.

    ENDIF.
  ENDMETHOD.                    "ALV_ON_DTO


  METHOD alv_on_dto_free.
    DATA lv_cls_name TYPE seoclsname VALUE 'ZCL_C8A001_ALVTAB_ON_DTO_B'.

    DATA lr_obj TYPE REF TO ts_obj.
    DATA lo_alv_on_dto TYPE REF TO zif_c8a001_alvtab_on_dto.

    LOOP AT mt_obj REFERENCE INTO lr_obj
      WHERE obj_n = lv_cls_name.

      lo_alv_on_dto ?= lr_obj->obj.
      lo_alv_on_dto->free_obj( ).
      CLEAR lo_alv_on_dto.

    ENDLOOP.

    DELETE mt_obj WHERE obj_n = lv_cls_name.
  ENDMETHOD.                    "alv_on_dto_free


  METHOD constructor.
    "
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_dto4alv.
    DATA lv_cls TYPE seoclsname VALUE 'ZCL_C8A001_ALV_DTO_B'.
    CREATE OBJECT ro_obj TYPE (lv_cls).
  ENDMETHOD.                    "GET_DTO4ALV

  METHOD get_db_supply.
*     returning
*      value(RO_OBJ) type ref to ZIF_C8A001_DB_SUPPLY .
    DATA lv_database_release TYPE char32.
    DATA lv_class_name TYPE seoclsname VALUE 'ZCL_C8A001_DB_SIMPLE'.

    CASE sy-dbsys.
      WHEN 'ORACLE'.

        CALL FUNCTION 'DB_GET_RELEASE'
          IMPORTING
            release = lv_database_release.
        IF strlen( lv_database_release ) GE 2.
          IF lv_database_release(2) EQ '11'.
            lv_class_name = 'ZCL_C8A001_DB_ORA11'.
          ENDIF.

          IF lv_database_release(2) EQ '12'.
            lv_class_name = 'ZCL_C8A001_DB_ORA12'.
          ENDIF.
        ENDIF.

      WHEN 'HDB'.
        lv_class_name = 'ZCL_C8A001_DB_HDB2'.
      WHEN OTHERS.

    ENDCASE.

    IF lv_class_name IS INITIAL.
      lv_class_name = 'ZCL_C8A001_DB_SIMPLE'.
    ENDIF.

    TRY.
        CREATE OBJECT ro_obj TYPE (lv_class_name).
      CATCH cx_root.
        lv_class_name = 'ZCL_C8A001_DB_SIMPLE'.
        CREATE OBJECT ro_obj TYPE (lv_class_name).
    ENDTRY.

  ENDMETHOD.                    "get_db_supply

  METHOD get_ins.
    IF mo_obj IS BOUND.
    ELSE.
      CREATE OBJECT mo_obj.
    ENDIF.
    ro_obj ?= mo_obj.
  ENDMETHOD.                    "get_ins
ENDCLASS.                    "ZCL_C8A001_ALV_FACTORY IMPLEMENTATION
