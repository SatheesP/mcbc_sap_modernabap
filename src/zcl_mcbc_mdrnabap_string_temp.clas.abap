CLASS zcl_mcbc_mdrnabap_string_temp DEFINITION
  PUBLIC INHERITING FROM zcl_mcbc_modernabap
  FINAL CREATE PRIVATE
  GLOBAL FRIENDS zcl_mcbc_modernabap .

  PUBLIC SECTION.

    METHODS:
      literal_text,
      format_options,
      control_characters.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MCBC_MDRNABAP_STRING_TEMP IMPLEMENTATION.


  METHOD control_characters.

    out->write( 'Control characters' ).
    out->write( |line-1\nline-2\rline-3\tline-3:section1| ).

  ENDMETHOD.


  METHOD format_options.

    DATA lv_case LIKE cl_abap_format=>c_upper.
    DATA idx1 TYPE i VALUE -10.
    DATA idx2 TYPE i VALUE 1234567.

    IF CONV i( sy-datum+4(2) ) MOD 2 = 1.   " Odd months upper case
      lv_case = cl_abap_format=>c_upper.
    ELSE.
      lv_case = cl_abap_format=>c_lower.
    ENDIF.

    " DATA(out) = out->next_section(
    out->write( 'Embedded expressions - Format options' ).
    out->write( |{ sy-datlo WIDTH = 20 ALIGN = CENTER PAD = '=' }| ).
    out->write( |{ CONV decfloat34( idx2 / 10 ) WIDTH = 15 DECIMALS = 2 ALIGN = RIGHT }| ).
    out->write( |{ sy-abcde }| ).
    out->write( |{ sy-abcde CASE = LOWER }| ).
    out->write( |{ sy-abcde CASE = (lv_case) }| ).

  ENDMETHOD.


  METHOD literal_text.

    DATA idx1 TYPE i VALUE -10.
    DATA idx2 TYPE i VALUE 1234567.

    out->write( 'Embedded expressions - Predefined format' ).
    out->write( | Current sys date:{ sy-datum " Referring system variable ( comment needs enter/newline )
                                  } | ).
    out->write( |{ idx1 }| ).
    out->write( |{ idx2 }| ).
    out->write( |{ idx1 WIDTH = 10 },{ idx2 WIDTH = 10 }<-| ).
    out->write( |{ CONV decfloat34( idx2 / 10 ) WIDTH = 15 }| ).
    out->write( |Local Date:{ sy-datlo }, local time:{ sy-timlo }| ).

  ENDMETHOD.
ENDCLASS.
