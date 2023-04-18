CLASS zcl_mcbc_mdrnabap_inlinedeclar DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcbc_modernabap
  FINAL
  CREATE PRIVATE GLOBAL FRIENDS zcl_mcbc_modernabap.

  PUBLIC SECTION.
    METHODS:
      old_elementary_data_types,
      new_elementary_data_types,

      old_select_single_stmt,
      new_select_single_stmt,

      old_select_stmt_itab,
      new_select_stmt_itab,

      old_loop_at_into,
      new_loop_at_into,

      old_loop_at_assigning,
      new_loop_at_assigning,

      old_method_calling,
      new_method_calling.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MCBC_MDRNABAP_INLINEDECLAR IMPLEMENTATION.


  METHOD new_elementary_data_types.

    DATA(int_variable) = 10.            " Integer
    DATA(char_variable) = '12345.123'.  " Char length 9
    DATA(str_variable) = `Hello World`. " String

    out->write( EXPORTING data = str_variable ).

  ENDMETHOD.


  METHOD new_loop_at_assigning.

    SELECT carrid, carrname, url
      FROM scarr
      INTO TABLE @DATA(airlines).

    LOOP AT airlines ASSIGNING FIELD-SYMBOL(<airline>).
      ...
    ENDLOOP.

  ENDMETHOD.


  METHOD new_loop_at_into.

    SELECT carrid, carrname, url
      FROM scarr
      INTO TABLE @DATA(airlines).

    LOOP AT airlines INTO DATA(airline).
      ...
    ENDLOOP.

  ENDMETHOD.


  METHOD new_method_calling.

    TRY.
        cl_abap_random=>create( )->intinrange(    " method chaining based on return object
          EXPORTING
            low   = 1
            high  = 10
          RECEIVING
            value = DATA(random_integer)        " inline declaration at RECEIVING / IMPORT parameter
       ).
      CATCH cx_abap_random.
    ENDTRY.

  ENDMETHOD.


  METHOD new_select_single_stmt.

    SELECT SINGLE carrid, carrname
      FROM scarr
     WHERE carrid = 'AA'
      INTO @DATA(airline1).    " Structure DataObject

    out->write( airline1-carrid ).
    out->write( airline1-carrname ).

  ENDMETHOD.


  METHOD new_select_stmt_itab.

    SELECT carrid, carrname, url
      FROM scarr
      INTO TABLE @DATA(airlines).

    out->write( airlines ).

  ENDMETHOD.


  METHOD old_elementary_data_types.

    DATA int_variable TYPE i.
    DATA char_variable TYPE c LENGTH 9.
    DATA str_variable TYPE string.

    int_variable = 10.
    char_variable = '12345.123'.
    str_variable = `Hello World`.

    out->write( EXPORTING data = str_variable ).

  ENDMETHOD.


  METHOD old_loop_at_assigning.

    TYPES: BEGIN OF ty_airline,
             airline_id   TYPE scarr-carrid,
             airline_name TYPE scarr-carrname,
             airline_url  TYPE scarr-url,
           END OF ty_airline,

           tt_airline TYPE STANDARD TABLE OF ty_airline.

    DATA airlines TYPE tt_airline.
    FIELD-SYMBOLS <airline1> TYPE ty_airline.

    SELECT carrid carrname url
      FROM scarr
      INTO TABLE airlines.

    LOOP AT airlines ASSIGNING <airline1>.
      ...
    ENDLOOP.

  ENDMETHOD.


  METHOD old_loop_at_into.

    TYPES: BEGIN OF ty_airline,
             airline_id   TYPE scarr-carrid,
             airline_name TYPE scarr-carrname,
             airline_url  TYPE scarr-url,
           END OF ty_airline,

           tt_airline TYPE STANDARD TABLE OF ty_airline.

    DATA airlines TYPE tt_airline.
    DATA airline TYPE ty_airline.

    SELECT carrid carrname url
      FROM scarr
      INTO TABLE airlines.

    LOOP AT airlines INTO airline.
      ...
    ENDLOOP.

  ENDMETHOD.


  METHOD old_method_calling.

    DATA random TYPE REF TO cl_abap_random.
    DATA random_integer TYPE i.

    random = cl_abap_random=>create( ).

    TRY.
        random->intinrange(
          EXPORTING
            low   = 1
            high  = 10
          RECEIVING
            value = random_integer
        ).
      CATCH cx_abap_random.
    ENDTRY.

  ENDMETHOD.


  METHOD old_select_single_stmt.

    DATA airline_id TYPE scarr-carrid.
    DATA airline_name TYPE scarr-carrname.

    SELECT SINGLE carrid carrname
      FROM scarr
      INTO (airline_id, airline_name)
     WHERE carrid = 'AA'.

    out->write( airline_id ).
    out->write( airline_name ).

  ENDMETHOD.


  METHOD old_select_stmt_itab.

    TYPES: BEGIN OF ty_airline,
             airline_id   TYPE scarr-carrid,
             airline_name TYPE scarr-carrname,
             airline_url  TYPE scarr-url,
           END OF ty_airline,

           tt_airline TYPE STANDARD TABLE OF ty_airline.

    DATA airlines TYPE tt_airline.

    SELECT carrid carrname url
      FROM scarr
      INTO TABLE airlines.

  ENDMETHOD.
ENDCLASS.
