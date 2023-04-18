CLASS zcl_mcbc_mdrnabap_const_exp DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcbc_modernabap
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_mcbc_modernabap .

  PUBLIC SECTION.

    METHODS:
      new_operator,
      value_operator,
      ref_operator,
      exact_operator,
      conv_operator,
      cast_operator,
      cond_operator,
      switch_operator,
      filter_operator,
      corresponding_operator.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MCBC_MDRNABAP_CONST_EXP IMPLEMENTATION.


  METHOD cast_operator.

*-- Up/Down casting with data reference variable
    DATA: dr_generic TYPE REF TO data,
          dr_integer TYPE REF TO i,
          dr_string  TYPE REF TO string,
          lv_int     TYPE i.

    dr_generic = NEW i( ).

    dr_integer = CAST i( dr_generic ).      "by CAST expr => Downcasting/Narrowing cast
    dr_integer ?= dr_generic.               "by CAST oper => Downcasting/Narrowing cast
    dr_integer->* = 10.
    out->write( dr_integer->* ).

    dr_string = NEW string( ).
    dr_string->* = 'New ABAP Syntax'.
    dr_generic = CAST #( dr_string ).       "by CAST expr => Upcasting/Widening cast
    dr_generic = dr_string.                 "by CAST oper => Upcasting/Widening cast
    out->write( CAST string( dr_generic )->* ).

  ENDMETHOD.


  METHOD cond_operator.

    SELECT SINGLE netwr
      FROM vbak
      INTO @DATA(lv_netwr)
     WHERE vbeln = '0000000010'.

    DATA(lv_priority) = COND #( WHEN lv_netwr > 7000
                                THEN 'High Priority'
                                WHEN lv_netwr > 5000 AND lv_netwr <= 7000
                                THEN 'Medium Priority'
                                ELSE 'Less Priority' ).

    out->write( lv_priority ).

  ENDMETHOD.


  METHOD conv_operator.

    "The two calculations produce different results. In the first case,
    "the calculation type is f and the end result is converted to i.
    "In the second case, CONV converts each intermediate result to the calculation type i.
    DATA int TYPE i.

    int = sqrt( 7 ) + sqrt( 8 ).
    out->write( int ).

    int = CONV i( sqrt( 7 ) ) + CONV i( sqrt( 8 ) ).
    out->write( int ).

    "String and character/boolean comparison using CONV
    DATA text TYPE char1 VALUE space.
    DATA bool TYPE abap_bool  VALUE abap_false.
    DATA str TYPE string VALUE ` `.

    IF text = str.
      out->write( 'TEXT = STR' ).
    ELSE.
      out->write( 'TEXT # STR' ).
    ENDIF.

    IF text = CONV char1( str ).
      out->write( 'TEXT = STR' ).
    ELSE.
      out->write( 'TEXT # STR' ).
    ENDIF.

    IF bool = str.
      out->write( 'BOOL = STR' ).
    ELSE.
      out->write( 'BOOL # STR' ).
    ENDIF.

    IF bool = CONV abap_bool( str ).
      out->write( 'BOOL = STR' ).
    ELSE.
      out->write( 'BOOL # STR' ).
    ENDIF.

  ENDMETHOD.


  METHOD corresponding_operator.

    TYPES: BEGIN OF str1,       " Source Structure
             name TYPE string,
             age  TYPE int2,
             city TYPE string,
           END OF str1,

           BEGIN OF str2,       " Target Structure
             name    TYPE string,
             age     TYPE int2,
             gender  TYPE char1,
             address TYPE string,
             country TYPE string,
           END OF str2.

    DATA: struct1 TYPE str1,
          struct2 TYPE str2,
          tab1    TYPE TABLE OF str1,  " Source ITAB
          tab2    TYPE TABLE OF str2.  " Target ITAB

*-- Instead of MOVE-CORRESPONDING statement, using Built-In Function
    struct1 = VALUE #( name = 'Swathi' age = '25' city = 'Coimbatore' ).
    struct2 = CORRESPONDING #( struct1 ).  " moves name and age from Struct1
    out->write( struct2 ).

*-- BASE syntax keeps target structure data fields which doesn't exist in source structure
    CLEAR struct2.
    struct2 = VALUE #( gender = 'F' address = 'Coimbatore' country = 'India' ).
    struct2 = CORRESPONDING #( BASE ( struct2 ) struct1 ).  " Keeps gender, address and country; moves name and age from Struct1
    out->write( struct2 ).

*-- BASE syntax keeps target structure data fields which doesn't exist in source structure
*-- with Mapping we can change the source and target data movement with different field names
    CLEAR struct2.
    struct2 = VALUE #( gender = 'F' country = 'India' ).
    struct2 = CORRESPONDING #( BASE ( struct2 ) struct1 MAPPING address = city ).
    out->write( struct2 ).

*-- We can use same for Tables
    tab1 = VALUE #( ( name = 'Krishiv' age = '2' city = 'Coimbatore' )
                    ( name = 'Lakshmi' age = '25' city = 'Coimbatore' ) ).

    tab2 = VALUE #( ( name = 'Krishiv' gender = 'M' country = 'India' )
                    ( name = 'Lakshmi' gender = 'F' country = 'India' ) ).

    tab2 = CORRESPONDING #( BASE ( tab2 ) tab1 MAPPING address = city ).
    out->write( tab1 ).
    out->write( tab2 ).

    tab2 = CORRESPONDING #( tab1 MAPPING address = city ).
    out->write( tab2 ).

  ENDMETHOD.


  METHOD exact_operator.

    " Lossless assignment
    DATA number TYPE n LENGTH 5.

    number = '12345abcd'.
    out->write( number ).
    TRY.
        number = EXACT #( '12345abcd' ).
      CATCH cx_sy_conversion_error INTO DATA(err).
        out->write( | Error Meg: { err->get_text( ) } | ).
    ENDTRY.

    DATA char_length2 TYPE c LENGTH 2.

    char_length2 = sy-abcde.
    out->write( sy-abcde ).
    out->write( char_length2 ).

    TRY.
        char_length2 = EXACT #( sy-abcde ).
      CATCH cx_sy_conversion_error INTO DATA(error1).
        out->write( | Error Meg: { error1->get_text( ) } | ).
    ENDTRY.

    " Lossless calculation
    TRY.
        DATA(result) = EXACT #( 3 * ( 1 / 3 ) ).
        out->write( result ).
      CATCH cx_sy_conversion_rounding INTO DATA(error2).
        out->write( | Error Meg: { error2->get_text( ) } Value: { error2->value } | ).
    ENDTRY.

    TRY.
        DATA(result2) = EXACT decfloat34( 1 / 3 * 3 ).
        out->write( result2 ).
      CATCH cx_sy_conversion_rounding INTO DATA(error3).
        out->write( | Error Meg: { error3->get_text( ) } Value: { error3->value } | ).
    ENDTRY.

  ENDMETHOD.


  METHOD filter_operator.

    DATA lt_airlines TYPE SORTED TABLE OF scarr WITH UNIQUE KEY carrid.

    SELECT *
      FROM scarr
      INTO TABLE @lt_airlines.

    "*-- Whenever using only WHERE condition, source table must be of type sorted or hashed.
    "*-- WHERE condition supported operators are 'AND' and '='
    "*-- Key columns are allowed and only once in WHERE condition
    DATA(lt_vt_airline) = FILTER #( lt_airlines WHERE carrid = 'VT ' ).
    out->write( lt_vt_airline ).

    DATA(lt_except_vt_airlines) = FILTER #( lt_airlines EXCEPT WHERE carrid = 'VT ' ).
    out->write( lt_except_vt_airlines ).

    "*-- By seeing the above examples, only WHERE condition means, supports partial/full key with/without EXCEPT
    "*-- When we want to get set of keys, then we have to use 'ftab' syntax
    "*-- When using 'ftab' syntax, source table need not be sorted/hashed type. But 'ftab' must be sorted/hashed
    "*-- !! When using 'ftab' all key columns must be mentioned in WHERE condition !!

    SELECT *
      FROM scarr
      INTO TABLE @DATA(lt_all_airlines).

    TYPES: BEGIN OF ty_ftab_airline,
             airline_id TYPE scarr-carrid,
           END OF ty_ftab_airline.
    DATA  lt_ftab_airline TYPE SORTED TABLE OF ty_ftab_airline WITH NON-UNIQUE KEY airline_id.

    lt_ftab_airline = VALUE #( ( airline_id = 'VT ' )
                               ( airline_id = 'IG ' ) ).

    DATA(lt_indian_airlines) = FILTER #( lt_all_airlines IN lt_ftab_airline
                                         WHERE carrid = airline_id ).
    out->write( lt_indian_airlines ).

    DATA(lt_except_indian_airlines) = FILTER #( lt_all_airlines EXCEPT IN lt_ftab_airline
                                                WHERE carrid = airline_id ).
    out->write( lt_except_indian_airlines ).

    "*-- Generic column access using elementary data as internal table type (without structure)
    DATA lt_ftab_asia_airlines TYPE SORTED TABLE OF scarr-currcode WITH UNIQUE KEY table_line.
    lt_ftab_asia_airlines = VALUE #( ( 'INR  ' )
                                     ( 'JPY  ' )
                                     ( 'SGD  ' ) )  .
    DATA(lt_asian_airlines) = FILTER #( lt_all_airlines IN lt_ftab_asia_airlines
                                        WHERE currcode = table_line ).
    out->write( lt_asian_airlines ).

  ENDMETHOD.


  METHOD new_operator.

*-- Constructor expression elementary data type
    DATA(integer_ref) = NEW i( 2020 ).
    DATA(string_ref) = NEW string( `Hello World` ).

    out->write( name = 'Constructor Expressions: ' data = integer_ref->* ).
    out->write( data = string_ref->* ).

*-- Constructor expression structure data type
    DATA: airline_ref_aa TYPE REF TO scarr,
          airline_ref_ia TYPE REF TO scarr.

    " Instead of statement 'CREATE DATA dref TYPE dtype'
    airline_ref_aa = NEW scarr( carrid = 'AA'
                                carrname = 'American Airlines' ).
    out->write( airline_ref_aa->* ).

    " Instead of statement 'CREATE DATA dref'
    airline_ref_ia = NEW #( carrid = 'IA'
                            carrname = 'Indian Airlines' ).   " Context Type
    out->write( airline_ref_ia->* ).

    TYPES tt_scarr TYPE STANDARD TABLE OF scarr WITH EMPTY KEY.
    DATA airlines TYPE REF TO tt_scarr.

    airlines = NEW #( ( carrid = 'AA' carrname = 'American Airlines' )
                      ( carrid = 'IA' carrname = 'Indian Airlines'   ) ).
    out->write( airlines->* ).

  ENDMETHOD.


  METHOD ref_operator.

    DATA age TYPE i VALUE '50'.
    DATA int_ref TYPE REF TO i.

    int_ref = REF #( age ).  " Instead of GET REFERENCE OF dobj INTO dref
    out->write( name = 'Constructor Expression Ref.' data = int_ref->* ).

    DATA: itab  TYPE  STANDARD TABLE OF i WITH EMPTY KEY.
    itab = VALUE #( FOR i = 1 THEN i + 1 UNTIL i > 11 ( i ) ).

    int_ref = REF #( itab[ 5 ] ).
    out->write( int_ref->* ).

  ENDMETHOD.


  METHOD switch_operator.

    DATA day  TYPE  cind.

    DO 7 TIMES.
      DATA(date) = CONV d( sy-datum + ( sy-index - 1 ) ).

      CALL FUNCTION 'DATE_COMPUTE_DAY'
        EXPORTING
          date = date
        IMPORTING
          day  = day.

      DATA(weekday) = SWITCH string( day
                        WHEN '1' THEN 'Monday'
                        WHEN '2' THEN 'Tuesday'
                        WHEN '3' THEN 'Wednesday'
                        WHEN '4' THEN 'Thursday'
                        WHEN '5' THEN 'Friday'
                        WHEN '6' THEN 'Saturday'
                        WHEN '7' THEN 'Sunday' ).

      " Date variable used: Embedded Expressions - format options
      out->write( |Date: { date DATE = USER }, Weekday: { weekday }| ).
    ENDDO.

  ENDMETHOD.


  METHOD value_operator.

    DATA string_variable TYPE string VALUE `test`. " OR |test|.

    " If No value passing for generic types,
    " It automatically initializes default/empty value for type
    DATA(integer_variable) = VALUE i( ).
    string_variable = VALUE #( ).
    out->write( integer_variable ).
    out->write( string_variable ).

    " Value passing allowed for structure and internal table types
    DATA airline1  TYPE scarr.
    airline1 = VALUE #( carrid = 'AA' carrname = 'American Airlines' currcode = 'USD' ).

    DATA(airline2) = VALUE scarr( carrid = 'EW' carrname = 'EURO Wings' currcode = 'EUR' ).

    " Copy values from airline2, required fields could be changed via BASE syntax
    DATA(airline3) = VALUE scarr( BASE airline2 carrid = 'BA' carrname = 'Berlin Airlines' ).

    out->write( airline1 ).
    out->write( airline2 ).
    out->write( airline3 ).

    " Value constructor expression with FOR iterator & LET expression
    TYPES: BEGIN OF ty_random_number,
             serial_number TYPE i,
             random_number TYPE i,
           END OF ty_random_number.

    DATA random_numbers TYPE STANDARD TABLE OF ty_random_number.

    DATA(random) = cl_abap_random=>create( ).
    random_numbers = VALUE #( FOR i = 1 THEN i + 1 WHILE i <= 10
                                 LET idx = i
                                     rno = random->intinrange( low = 1
                                                               high = 10 )
                                 IN ( serial_number = idx
                                      random_number = rno ) ).
    out->write( random_numbers ).

  ENDMETHOD.
ENDCLASS.
