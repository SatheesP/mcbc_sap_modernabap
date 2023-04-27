*&---------------------------------------------------------------------*
*& Report  ZMCBC_REUSE_PROC_OO_COMPILER
*&
*&---------------------------------------------------------------------*
*& Architecture, Design & Developed by Sathees Kumar P.
*&
*& Training Example for MCBC - Modern ABAP, ABAP OO
*& Description : Show-case the how ABAP compiler behaves with
*&               Procedural coding and modern OO coding
*&---------------------------------------------------------------------*
REPORT zmcbc_reuse_proc_oo_compiler.

DATA:
  gv_int TYPE i VALUE '1001',
  gv_day TYPE week_day.

INITIALIZATION.
  gv_int = '1234'.

START-OF-SELECTION.

"---------------------------------------------"
**--  Procedural code how compiler treats  --**
"---------------------------------------------"
  CALL FUNCTION 'POPUP_TO_INFORM1'
    EXPORTING
      titel = sy-title    " Title line of dialog box
      txt12  = 'How Compiler behaves'
      txt2  = 'for Procedural programming and'
      txt3  = 'for Object Oriented programming'.

  CALL FUNCTION 'DATE_TO_DAY'
    EXPORTING
      date    = sy-datum   " Date of weekday to be calculated
    IMPORTING
      weekday = gv_day.    " Weekday

  WRITE /: gv_day.

"-------------------------------------"
**--  OO code how compiler treats  --**
"-------------------------------------"
  cl_reca_date=>get_weekday(
    EXPORTING
      id_date    = sy-datum    " Current Date of Application Server
    RECEIVING
      rd_weekday = gv_day    " Day
  ).

  IF sy-subrc = 0.
    WRITE /: gv_day.
  ENDIF.
