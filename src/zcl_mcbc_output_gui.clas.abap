CLASS zcl_mcbc_output_gui DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcbc_output
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING io_out TYPE REF TO if_demo_output.

    METHODS:
      if_oo_adt_intrnl_classrun~write REDEFINITION,
      if_oo_adt_intrnl_classrun~write_data REDEFINITION,
      if_oo_adt_intrnl_classrun~write_text REDEFINITION,
      if_oo_adt_intrnl_classrun~display REDEFINITION,
      if_oo_adt_intrnl_classrun~begin_section REDEFINITION,
      if_oo_adt_intrnl_classrun~end_section REDEFINITION,
      if_oo_adt_intrnl_classrun~line REDEFINITION,
      if_oo_adt_intrnl_classrun~next_section REDEFINITION,
      if_oo_adt_intrnl_classrun~get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_out TYPE REF TO if_demo_output.

ENDCLASS.



CLASS ZCL_MCBC_OUTPUT_GUI IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    me->mo_out = io_out.

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~begin_section.

    me->mo_out->begin_section(
       title = title
    ).

    output = me.

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~display.

    me->mo_out->display( ).

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~end_section.

    me->mo_out->end_section( ).
    output = me.

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~get.

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~line.

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~next_section.

    me->mo_out->next_section( title = title  ).
    output = me.

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~write.

    IF name IS SUPPLIED.
      me->mo_out->write(
        EXPORTING
          data   = data    " Text or Data
          name   = name    " Name
      ).
    ELSE.
      me->mo_out->write(
        EXPORTING
          data   = data    " Text or Data
      ).
    ENDIF.
    output = me.

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~write_data.

  ENDMETHOD.


  METHOD if_oo_adt_intrnl_classrun~write_text.

  ENDMETHOD.
ENDCLASS.
