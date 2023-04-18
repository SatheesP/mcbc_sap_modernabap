CLASS zcl_mcbc_output DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_intrnl_classrun
      ALL METHODS ABSTRACT .

    ALIASES:
      begin_section FOR if_oo_adt_intrnl_classrun~begin_section,
      display FOR if_oo_adt_intrnl_classrun~display .

    ALIASES:
       end_section FOR if_oo_adt_intrnl_classrun~end_section,
       get FOR if_oo_adt_intrnl_classrun~get,
       line FOR if_oo_adt_intrnl_classrun~line,
       next_section FOR if_oo_adt_intrnl_classrun~next_section,
       write FOR if_oo_adt_intrnl_classrun~write,
       write_data FOR if_oo_adt_intrnl_classrun~write_data,
       write_text FOR if_oo_adt_intrnl_classrun~write_text.

    CLASS-METHODS factory
      IMPORTING
        !io_out       TYPE REF TO object
      RETURNING
        VALUE(ro_out) TYPE REF TO zcl_mcbc_output .

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_MCBC_OUTPUT IMPLEMENTATION.


  METHOD factory.

    DATA lo_gui_out TYPE REF TO if_demo_output.
    DATA lo_adt_out TYPE REF TO if_oo_adt_intrnl_classrun.

    TRY.
        lo_gui_out = CAST if_demo_output( io_out ).
        ro_out = NEW zcl_mcbc_output_gui( io_out = lo_gui_out ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

    IF lo_gui_out IS NOT BOUND.
      TRY.
          lo_adt_out = CAST if_oo_adt_intrnl_classrun( io_out ).
          ro_out = NEW zcl_mcbc_output_adt( io_out = lo_adt_out ).
        CATCH cx_sy_move_cast_error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
