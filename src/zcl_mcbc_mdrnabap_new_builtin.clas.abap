CLASS zcl_mcbc_mdrnabap_new_builtin DEFINITION
  PUBLIC INHERITING FROM zcl_mcbc_modernabap
  FINAL CREATE PRIVATE
  GLOBAL FRIENDS zcl_mcbc_modernabap .

  PUBLIC SECTION.

    METHODS ipow_nmin_nmax .
    METHODS boolean_functions .
    METHODS itab_functions .
    METHODS condense_function .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MCBC_MDRNABAP_NEW_BUILTIN IMPLEMENTATION.


  METHOD boolean_functions.

    " BOOLC - returns string with 'X' for true and ' ' blank for false
    " result is not suitable for relational expression

    DATA: answerr1 TYPE string VALUE 'RED',
          answerr2 TYPE string VALUE 'MUMBAI'.

    DATA right_answers TYPE i.

    right_answers = strlen( condense( boolc( answerr1 = 'GREEN' ) ) ) +
                    strlen( condense( boolc( answerr2 = 'MUMBAI' ) ) ).

    out->write( right_answers ).

    " BOOLX - bit function

    DATA: answer1 TYPE string VALUE 'RED',
          answer2 TYPE string VALUE 'MUMBAI',
          answer3 TYPE string VALUE 'APJ'.

    DATA x_option TYPE x.
    DATA i_option TYPE int1.
    DATA result TYPE string.

    x_option = boolx( bool = answer1 = 'RED' bit = 1 )
                BIT-OR boolx( bool = answer2 = 'DELHI' bit = 2 )
                BIT-OR boolx( bool = answer3 = 'APJ' bit = 3 ).

    i_option = x_option. " Binary to Integer conversion
    CASE i_option.
      WHEN 1 OR 2 OR 4. " Binary values 001 / 010 / 100
        result = '1/3 is correctly answered'.

      WHEN 7. " Binary value 111
        result = '3/3 is correctly answered'.

      WHEN OTHERS. " Binary values 011 / 101 / 110 (3/5/6)
        result = '2/3 is correctly answered'.
    ENDCASE.

    out->write( result ).

    " XSDBOOL - returns CHAR of length 1 and 'X' for true and ' ' blank for false
    " Result is suitable for relational expression with ABAP_TRUE and ABAP_FALSE

    IF xsdbool( answer1 = 'RED' ) = abap_true.
      out->write( 'Right Answer' ).
    ENDIF.

  ENDMETHOD.


  METHOD condense_function.

    DATA(text) = `  Good   Night   `.

*--Normal usage: Removes all leading and trailing spaces( works like 'trim' function in other languages)

    DATA(condense) = condense( text ). "If only text has to be condensed, no parameter names needed
    out->write( condense ).

*--Only Delete: When del = text to delete

    DATA(condense_del_char) = condense( val = text del = `Good ` ).
    out->write( 'Only Delete : '  && condense_del_char ).

*--Only Replace: when 'del' = space, from = char/string to replace,
*--to = text to replace with (takes only first character from text)

    DATA(condense_replace_char) = condense( val = text
                                            del = ` `
                                            from = `N` to = `F*` ).
    out->write( 'Only Replace : ' && condense_replace_char ).

*--The argument 'from' can be a single character or a text

    DATA(condense_replace_char1) = condense( val = text
                                             del = ` `
                                             from = `Good` to = ` ` ).
    out->write( 'Only Replace : ' && condense_replace_char1 ).

*--Delete & Replace: When del = text to delete, from = old text to replace ,
*--to = text to replace with(takes only first character from text)

    DATA(condense_del_replace) = condense( val = text
                                           del = `Good`
                                           from = `N` to = `F` ).
    out->write( 'Delete & Replace : ' && condense_del_replace ).

  ENDMETHOD.


  METHOD ipow_nmin_nmax.

    DATA: num1  TYPE  i,
          num2  TYPE  i,
          date1 TYPE  i,
          date2 TYPE  i,
          date3 TYPE  i.

    date2 = CONV d( '99991231' ).
    date1 = sy-datum.
    date3 = CONV d( sy-datum(6) && '01' ).

    num1 = 45. num2 = 99.

    " ipow function returns more precise and more performance than ** operator for calculating the Power

    out->write( |1.2 ** 2 \n   **: { '1.2' ** 2 } \n ipow: { ipow( base = '1.2' exp = 2 ) }| ).
    " Find minimum value
    out->write( |Date started { CONV d( nmin( val1 = date1 val2 = date2 val3 = date3 ) ) DATE = ISO }| ).

    " Find maximum value
    out->write( |Max number is { nmax( val1 = num1 val2 = num2 ) }| ).

  ENDMETHOD.


  METHOD itab_functions.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    " To find no. of rows in the ITAB lt_scarr
    DATA(lv_lines) = lines( lt_scarr ).

    " To check line existence based on predicate function
    IF line_exists( lt_scarr[ carrid = 'SQ' ] ).
      out->write( |Airline id 'SQ' exist| ).
    ENDIF.

    " To check line existence based on predicate function by using secondary key for binary search
    IF line_exists( lt_scarr[ KEY name carrname = 'Singapore Airlines' ] ).

      out->write( |Airline 'Singapore Airlines' exist| ).
      DATA(lv_idx) = line_index( lt_scarr[ KEY name carrname = 'Singapore Airlines' ] ).
      out->write( |Airline 'Singapore Airlines' exist in index { lv_idx } sec. key 'NAME' | ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
