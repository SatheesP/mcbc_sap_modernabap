*&---------------------------------------------------------------------*
*& Report  ZMCBC_REUSE_OO_MULTI_INST_CAR
*&
*&---------------------------------------------------------------------*
*& Architecture, Design & Developed by Sathees Kumar P.
*&
*& Training Example for MCBC - Modern ABAP, ABAP OO
*& Description : Show-case the multiple instances of Objects
*&               which is not possible by Function Group
*&---------------------------------------------------------------------*
REPORT zmcbc_reuse_oo_multi_inst_car.

*--  Car Class Declaration  --*

CLASS lcl_car DEFINITION CREATE PUBLIC.

* Access to attributes & methods of this section for end users this class
  PUBLIC SECTION.
*   Method/Function Declaration  (Action/Behavior of Object)
    METHODS:
      constructor IMPORTING iv_name        TYPE  string,

*     Access or Setters/Getters methods for attribute 'mv_name'
      set_name  IMPORTING iv_name          TYPE  string,
      get_name  RETURNING VALUE(rv_name)   TYPE  string,
      get_speed RETURNING VALUE(rv_speed)  TYPE  i,

*     Object/Behavioral methods
      increase_speed IMPORTING iv_speed         TYPE  i,
      decrease_speed IMPORTING iv_speed         TYPE  i,
      stop,

      show_log_messages.

* Access to attributes & methods of this section within this class & subclasses only
  PROTECTED SECTION.


* Access to attributes & methods of this section within this class only
  PRIVATE SECTION.

*   Data/Attribute/Member Variable Declaration  (Characteristic of Object)
    DATA:
      mv_name        TYPE  string,
      mv_speed       TYPE  i,
      mt_log_message TYPE strtable.
*   Method/Function Declaration  (Action/Behavior of Object)

ENDCLASS.


CLASS lcl_car IMPLEMENTATION.

* Initialization of Object Attributes and other processing
  METHOD constructor.

    me->mv_name = iv_name.
    APPEND VALUE #( str = |Car { iv_name } instance created| ) TO me->mt_log_message.

  ENDMETHOD.

  METHOD set_name.

    me->mv_name = iv_name.

  ENDMETHOD.

  METHOD get_name.

    rv_name = me->mv_name.

  ENDMETHOD.

  METHOD get_speed.

    rv_speed = me->mv_speed.

  ENDMETHOD.

  METHOD increase_speed.

    DATA(lv_log) = |Car speed increased from { me->mv_speed }|.
    me->mv_speed = me->mv_speed + iv_speed.
    APPEND VALUE #( str = |{ lv_log } to { me->mv_speed }| ) TO me->mt_log_message.

  ENDMETHOD.

  METHOD decrease_speed.

    DATA(lv_log) = |Car speed decreased from { me->mv_speed }|.
    me->mv_speed = me->mv_speed - iv_speed.
    APPEND VALUE #( str = |{ lv_log } to { me->mv_speed }| ) TO me->mt_log_message.

  ENDMETHOD.

  METHOD stop.

    APPEND VALUE #( str = |Car stopped from speed of { me->mv_speed }| ) TO me->mt_log_message.
    me->mv_speed = 0.

  ENDMETHOD.

  METHOD show_log_messages.

    DATA: lr_table  TYPE REF TO cl_salv_table.

    LOOP AT me->mt_log_message ASSIGNING FIELD-SYMBOL(<fs_log>).
      <fs_log>-row_index = sy-tabix.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lr_table
          CHANGING
            t_table      = me->mt_log_message ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    DATA: lr_functions TYPE REF TO cl_salv_functions_list.

    lr_functions = lr_table->get_functions( ).
    lr_functions->set_all( abap_true ).

    DATA: lr_columns TYPE REF TO cl_salv_columns.

    lr_columns = lr_table->get_columns( ).
    lr_columns->set_optimize( abap_true ).

    lr_table->set_screen_popup(
      start_column = 1
      end_column   = 100
      start_line   = 1
      end_line     = 20 ).

    lr_table->display( ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************

DATA: go_car1 TYPE  REF TO lcl_car,
      go_car2 TYPE  REF TO lcl_car.


INITIALIZATION.


START-OF-SELECTION.

  PERFORM create_cars.

FORM create_cars.

  DATA: lv_name  TYPE  string,
        lv_speed TYPE  i.

*  CREATE OBJECT go_car1
*    EXPORTING
*      iv_name = 'Car ABC'.

  go_car1 = NEW #( 'Car ABC' ). " ABAP Expression oriented syntax

  CREATE OBJECT go_car2
    EXPORTING
      iv_name = 'Car XYZ'.

  lv_name = go_car1->get_name( ).
  WRITE:  / '     ', lv_name COLOR COL_HEADING.
  lv_name = go_car2->get_name( ).
  WRITE: 25 '     ', lv_name COLOR COL_GROUP.

  go_car1->increase_speed( 40 ).
  lv_speed = go_car1->get_speed( ).
  WRITE: / lv_speed.
  go_car2->increase_speed( 50 ).
  lv_speed = go_car2->get_speed( ).
  WRITE 25 lv_speed.

  go_car1->decrease_speed( 10 ).
  lv_speed = go_car1->get_speed( ).
  WRITE: / lv_speed.
  go_car2->decrease_speed( 10 ).
  lv_speed = go_car2->get_speed( ).
  WRITE 25 lv_speed.

  go_car1->stop( ).
  lv_speed = go_car1->get_speed( ).
  WRITE: / lv_speed.
  go_car2->stop( ).
  lv_speed = go_car2->get_speed( ).
  WRITE 25 lv_speed.

  go_car1->show_log_messages( ).
  go_car2->show_log_messages( ).

ENDFORM.
