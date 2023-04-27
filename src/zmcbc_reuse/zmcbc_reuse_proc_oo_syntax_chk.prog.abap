*&---------------------------------------------------------------------*
*& Report  ZMCBC_REUSE_PROC_OO_SYNTAX_CHK
*&
*&---------------------------------------------------------------------*
*& Architecture, Design & Developed by Sathees Kumar P.
*&
*& Training Example for MCBC - Modern ABAP, ABAP OO
*& Description : Show-case the how ABAP syntax check behaves with
*&               Procedural coding and modern OO coding
*&               (Obsolete Language Elements)
*&---------------------------------------------------------------------*
REPORT zmcbc_reuse_proc_oo_syntax_chk.


CLASS lcl_obsolete_syntax_chk DEFINITION.

  PUBLIC SECTION.
    METHODS:
      method1,
      method2.

ENDCLASS.

CLASS lcl_obsolete_syntax_chk IMPLEMENTATION.

  METHOD method1.

    DATA lv_month TYPE string.

    CASE sy-datum+3(2).
        "CLEAR lv_month.
      WHEN '01'.
        lv_month = 'Jan'.

      WHEN '02'.
        lv_month = 'Feb'.

      WHEN OTHERS.
        lv_month = 'Not found'.
    ENDCASE.

  ENDMETHOD.

  METHOD method2.

*    DATA lt_scarr TYPE scarr OCCURS 10.
    DATA lt_scarr TYPE STANDARD TABLE OF scarr INITIAL SIZE 10.

    SELECT *
      FROM scarr
      INTO TABLE @lt_scarr.


  ENDMETHOD.

ENDCLASS.


FORM obsolete_syntax1.

  DATA lv_month TYPE string.

  CASE sy-datum+3(2).
      CLEAR lv_month.
    WHEN '01'.
      lv_month = 'Jan'.

    WHEN '02'.
      lv_month = 'Feb'.

    WHEN OTHERS.
      lv_month = 'Not found'.
  ENDCASE.

ENDFORM.

FORM obsolete_syntax2.

  DATA lt_scarr TYPE scarr OCCURS 10.


ENDFORM.
