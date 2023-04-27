*&---------------------------------------------------------------------*
*& Report  ZMCBC_REUSE_OO_LCL_REPORT
*&
*&---------------------------------------------------------------------*
*& Architecture, Design & Developed by Sathees Kumar P.
*&
*& Training Example for MCBC - Modern ABAP, ABAP OO
*& Description : Show-case the report logic in to local classes,
*&               ensures not able to use Obsolete Language Elements
*&---------------------------------------------------------------------*
REPORT zmcbc_reuse_oo_lcl_report.

*----------------------------------------------------------------------*
*-- Global Data & Selection Screen declaration                       --*
*----------------------------------------------------------------------*
DATA:
  gv_airlineid TYPE scarr-carrid,
  gv_currency  TYPE scarr-currcode.

SELECT-OPTIONS: so_airln FOR gv_airlineid,
                so_curnc FOR gv_currency.

*----------------------------------------------------------------------*
*-- Local Class Definitions                                          --*
*----------------------------------------------------------------------*

CLASS lcl_main DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      init,
      valid_airline,
      run.
ENDCLASS.

CLASS lcl_top5_airlines DEFINITION.

  PUBLIC SECTION.
    TYPES: tt_airlineid_rng TYPE RANGE OF scarr-carrid,
           tt_currcode_rng  TYPE RANGE OF scarr-currcode,
           tt_airlines TYPE  STANDARD TABLE OF scarr WITH EMPTY KEY.

    METHODS:
      set_selscr_values IMPORTING it_airlineid TYPE tt_airlineid_rng
                                  it_currcode  TYPE tt_currcode_rng,
      display,

      get_data RETURNING VALUE(rt_airlines) TYPE tt_airlines.

  PRIVATE SECTION.
    DATA:
      mt_airlineid TYPE  tt_airlineid_rng,
      mt_currcode  TYPE  tt_currcode_rng,
      mt_airlines  TYPE  tt_airlines.

    METHODS:
      prepare_data.
ENDCLASS.


*----------------------------------------------------------------------*
*-- Program initialization                                           --*
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_main=>init( ).

*----------------------------------------------------------------------*
*-- Selection screen validations                                     --*
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON so_airln.
  lcl_main=>valid_airline( ).

*----------------------------------------------------------------------*
*-- Program execution starts here                                    --*
*----------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_main=>run( ).

*----------------------------------------------------------------------*
*-- Local Class Implementations                                      --*
*----------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.

  METHOD run.

    DATA(lo_to5_airlines) = NEW lcl_top5_airlines( ).
    lo_to5_airlines->set_selscr_values( it_airlineid = so_airln[]
                                        it_currcode = so_curnc[] ).
    lo_to5_airlines->display( ).

  ENDMETHOD.

  METHOD init.

    so_airln[] = VALUE #( sign = 'I' option = 'EQ' ( low = 'AA' ) ( low = 'IG' ) ( low = 'VT' ) ).
    so_curnc[] = VALUE #( sign = 'I' option = 'EQ' ( low = 'INR' ) ( low = 'USD' ) ).

  ENDMETHOD.

  METHOD valid_airline.

    IF so_airln[] IS INITIAL.
      MESSAGE 'Please enter/select at least one airline' TYPE 'E'.

      RETURN.
    ENDIF.

    SELECT COUNT( * )
      FROM scarr
     WHERE carrid IN so_airln.
    IF sy-dbcnt = 0.
      MESSAGE 'Please enter/select at least one valid airline' TYPE 'E'.

      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_top5_airlines IMPLEMENTATION.

  METHOD set_selscr_values.

    mt_airlineid = it_airlineid.
    mt_currcode  = it_currcode.

  ENDMETHOD.

  METHOD display.

    DATA: lr_table  TYPE REF TO cl_salv_table.

    me->prepare_data( ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lr_table
          CHANGING
            t_table      = me->mt_airlines ).
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

  METHOD prepare_data.

    SELECT *
      INTO TABLE @me->mt_airlines
      FROM scarr UP TO 5 ROWS
     ORDER BY carrid DESCENDING.
*     WHERE carrid in @me->mt_airlineid
*       AND currcode in @me->mt_currcode.

  ENDMETHOD.

  METHOD get_data.

    me->prepare_data( ).
    rt_airlines = me->mt_airlines.

  ENDMETHOD.

ENDCLASS.
