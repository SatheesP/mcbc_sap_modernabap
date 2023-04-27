CLASS zcl_mcbc_mdrnabap_table_exp DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcbc_modernabap
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_mcbc_modernabap .

  PUBLIC SECTION.

    METHODS read_old_itab_index .
    METHODS read_new_itab_index .
    METHODS read_old_itab_key .
    METHODS read_new_itab_key .
    METHODS read_old_itab_idx_defkey .
    METHODS read_new_itab_idx_defkey .
    METHODS read_old_itab_comp_defkey .
    METHODS read_new_itab_comp_defkey .
    METHODS read_old_itab_row_exists .
    METHODS read_new_itab_row_exists .
    METHODS read_old_itab_search .
    METHODS read_new_itab_search .
    METHODS exception_raising .
    METHODS lwg_old_loopatcmd_wosort .
    METHODS lwg_new_loop_groupby_atgroup .
    METHODS lwg_old_loop_at_wth_sorting .
    METHODS lwg_old_loop_itab_unique_rec .
    METHODS lwg_new_loop_itab_unique_rec .
    METHODS lwg_old_loop_at_multiple_keys .
    METHODS lwg_new_loop_at_multiple_keys .
    METHODS for_itr_new_for_groups .
    METHODS for_itr_new_for_in_itab .
    METHODS for_itr_new_reduce .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_MCBC_MDRNABAP_TABLE_EXP IMPLEMENTATION.


  METHOD exception_raising.

    SELECT *
      FROM scarr
      INTO TABLE @DATA(lt_scarr).

    TRY.
        out->write( lt_scarr[ 100 ] ).
      CATCH cx_sy_itab_line_not_found INTO DATA(cx_err).
        out->write( cx_err->get_text( ) ).
    ENDTRY.

    " Default value works with VALUE / REF operators
    out->write( VALUE #( lt_scarr[ 1 ] DEFAULT VALUE #( carrid = 'XX' carrname = 'Not found' ) ) ).
    out->write( VALUE #( lt_scarr[ 100 ] DEFAULT VALUE #( carrid = 'XX' carrname = 'Not found' ) ) ).
    out->write( VALUE #( lt_scarr[ carrid = '$$' ] OPTIONAL ) ).  " OPTOINAL sets initial value

    ASSIGN lt_scarr[ carrid = 'VT' ] TO FIELD-SYMBOL(<fs_flight>).
    IF sy-subrc = 0.
      out->write( <fs_flight> ).
    ENDIF.

    IF line_exists( lt_scarr[ carrid = 'SG' ] ).
      out->write( lt_scarr[ carrid = 'SG' ] ).
    ELSE.
      out->write( |carrid 'SG' is not found.| ).
    ENDIF.

    IF line_index( lt_scarr[ carrid = 'SQ' ] ) > 0.
      out->write( lt_scarr[ carrid = 'SQ' ] ).
    ENDIF.

  ENDMETHOD.


  METHOD for_itr_new_for_groups.

*data :  tt_mark_sheet  (  )
*    * Output - Student Mark sheet preparation using FOR GROUPS ... OF and FOR ... IN GROUP

    TYPES:

      BEGIN OF ty_student,
        rollno TYPE n LENGTH 4,
        name   TYPE string,
        std    TYPE i,
      END OF ty_student,
      tt_student TYPE SORTED TABLE OF ty_student WITH UNIQUE KEY rollno,

      BEGIN OF ty_exam,
        examid TYPE c LENGTH 1,  " Q-Quarterly; H-Half Yearly; F-Final
        name   TYPE string,
      END OF ty_exam,
      tt_exam TYPE SORTED TABLE OF ty_exam WITH UNIQUE KEY examid,

      BEGIN OF ty_mark,

        examid  TYPE c LENGTH 1,
        rollno  TYPE n LENGTH 4,
        subject TYPE  string,
        mark    TYPE i,
      END OF ty_mark,
      tt_mark TYPE STANDARD TABLE OF ty_mark WITH EMPTY KEY,

      BEGIN OF ty_subj_mark,
        subject TYPE  string,
        mark    TYPE i,
      END OF ty_subj_mark,
      tt_subj_mark TYPE STANDARD TABLE OF ty_subj_mark WITH EMPTY KEY,

      BEGIN OF ty_mark_sheet,
        exam_name TYPE string,
        standard  TYPE string,
        stu_name  TYPE string,
        tot_marks TYPE i,
        percent   TYPE i,
*           subj_mark TYPE tt_subj_mark,
      END OF ty_mark_sheet,
      tt_mark_sheet TYPE STANDARD TABLE OF ty_mark_sheet WITH EMPTY KEY,

      BEGIN OF ty_subject_mark,
        exam_name TYPE string,
        standard  TYPE string,
        stu_name  TYPE string,
        mark      TYPE i,
      END OF ty_subject_mark,

      tt_subject_mark TYPE STANDARD TABLE OF ty_subject_mark WITH EMPTY KEY.

* Master data
    DATA(students) = VALUE tt_student( ( rollno = '1001' name = 'Sathees' std = 10 )
                                       ( rollno = '1002' name = 'Kumar'   std = 10 )
                                       ( rollno = '1003' name = 'Ram'     std = 10 ) ).

    DATA(exams) = VALUE tt_exam( ( examid = 'Q' name = 'Quarterly' )
                                 ( examid = 'H' name = 'Half Yearly' )
                                 ( examid = 'F' name = 'Final' ) ).

* Transaction data
    DATA(marks) = VALUE tt_mark( ( examid = 'Q' rollno = '1001' subject = 'tamil' mark = '70' )
                                 ( examid = 'Q' rollno = '1001' subject = 'english' mark = '55' )
                                 ( examid = 'Q' rollno = '1001' subject = 'maths' mark = '85' )
                                 ( examid = 'Q' rollno = '1001' subject = 'science' mark = '75' )
                                 ( examid = 'Q' rollno = '1001' subject = 'history' mark = '82' )

                                 ( examid = 'Q' rollno = '1002' subject = 'tamil' mark = '70' )
                                 ( examid = 'Q' rollno = '1002' subject = 'english' mark = '50' )
                                 ( examid = 'Q' rollno = '1002' subject = 'maths' mark = '70' )
                                 ( examid = 'Q' rollno = '1002' subject = 'science' mark = '64' )
                                 ( examid = 'Q' rollno = '1002' subject = 'history' mark = '71' )

                                 ( examid = 'Q' rollno = '1003' subject = 'tamil' mark = '77' )
                                 ( examid = 'Q' rollno = '1003' subject = 'english' mark = '49' )
                                 ( examid = 'Q' rollno = '1003' subject = 'maths' mark = '72' )
                                 ( examid = 'Q' rollno = '1003' subject = 'science' mark = '59' )
                                 ( examid = 'Q' rollno = '1003' subject = 'history' mark = '69' )

                                 ( examid = 'H' rollno = '1001' subject = 'tamil' mark = '75' )
                                 ( examid = 'H' rollno = '1001' subject = 'english' mark = '60' )
                                 ( examid = 'H' rollno = '1001' subject = 'maths' mark = '90' )
                                 ( examid = 'H' rollno = '1001' subject = 'science' mark = '80' )
                                 ( examid = 'H' rollno = '1001' subject = 'history' mark = '87' )

*                               * Erroneous data
                                 ( examid = 'W' rollno = '1010' subject = ' ' mark = '99' ) ).

    DATA(mark_sheets) = VALUE tt_mark_sheet( FOR GROUPS student OF mark IN marks
                                                 GROUP BY ( examid = mark-examid rollno = mark-rollno subjects = GROUP SIZE )

                                                 LET total = REDUCE i( INIT t = 0
                                                                        FOR m IN GROUP student
                                                                       NEXT t = t + m-mark ) IN

                                                 ( exam_name = VALUE #( exams[ examid = student-examid ]-name DEFAULT '???' )
                                                   standard  = VALUE #( students[ rollno = student-rollno ]-std DEFAULT 0 )
                                                   stu_name  = VALUE #( students[ rollno = student-rollno ]-name DEFAULT '???' )
                                                   tot_marks = total
                                                   percent   = total / student-subjects
*                                                 subj_mark = VALUE #( FOR m IN GROUP student
*                                                                    ( subject = m-subject mark = m-mark ) )


                                                 ) ).
    out->write( mark_sheets ).

  ENDMETHOD.


  METHOD for_itr_new_for_in_itab.

*   Find specific subject mark using FOR ... IN itab with where condition
*   Master data row is not found, fill default value otherwise DUMP is produced

    TYPES:

      BEGIN OF ty_student,
        rollno TYPE n LENGTH 4,
        name   TYPE string,
        std    TYPE i,
      END OF ty_student,
      tt_student TYPE SORTED TABLE OF ty_student WITH UNIQUE KEY rollno,

      BEGIN OF ty_exam,
        examid TYPE c LENGTH 1,  " Q-Quarterly; H-Half Yearly; F-Final
        name   TYPE string,
      END OF ty_exam,
      tt_exam TYPE SORTED TABLE OF ty_exam WITH UNIQUE KEY examid,

      BEGIN OF ty_mark,

        examid  TYPE c LENGTH 1,
        rollno  TYPE n LENGTH 4,
        subject TYPE  string,
        mark    TYPE i,
      END OF ty_mark,
      tt_mark TYPE STANDARD TABLE OF ty_mark WITH EMPTY KEY,

      BEGIN OF ty_subj_mark,
        subject TYPE  string,
        mark    TYPE i,
      END OF ty_subj_mark,
      tt_subj_mark TYPE STANDARD TABLE OF ty_subj_mark WITH EMPTY KEY,

      BEGIN OF ty_mark_sheet,
        exam_name TYPE string,
        standard  TYPE string,
        stu_name  TYPE string,
        tot_marks TYPE i,
        percent   TYPE i,
      END OF ty_mark_sheet,
      tt_mark_sheet TYPE STANDARD TABLE OF ty_mark_sheet WITH EMPTY KEY,

      BEGIN OF ty_subject_mark,
        exam_name TYPE string,
        standard  TYPE string,
        stu_name  TYPE string,
        mark      TYPE i,
      END OF ty_subject_mark,

      tt_subject_mark TYPE STANDARD TABLE OF ty_subject_mark WITH EMPTY KEY.

*   Master data
    DATA(students) = VALUE tt_student( ( rollno = '1001' name = 'Sathees' std = 10 )
                                       ( rollno = '1002' name = 'Kumar'   std = 10 )
                                       ( rollno = '1003' name = 'Ram'     std = 10 ) ).

    DATA(exams) = VALUE tt_exam( ( examid = 'Q' name = 'Quarterly' )
                                 ( examid = 'H' name = 'Half Yearly' )
                                 ( examid = 'F' name = 'Final' ) ).

*   Transaction data
    DATA(marks) = VALUE tt_mark( ( examid = 'Q' rollno = '1001' subject = 'tamil' mark = '70' )
                                 ( examid = 'Q' rollno = '1001' subject = 'english' mark = '55' )
                                 ( examid = 'Q' rollno = '1001' subject = 'maths' mark = '85' )
                                 ( examid = 'Q' rollno = '1001' subject = 'science' mark = '75' )
                                 ( examid = 'Q' rollno = '1001' subject = 'history' mark = '82' )

                                 ( examid = 'Q' rollno = '1002' subject = 'tamil' mark = '70' )
                                 ( examid = 'Q' rollno = '1002' subject = 'english' mark = '50' )
                                 ( examid = 'Q' rollno = '1002' subject = 'maths' mark = '70' )
                                 ( examid = 'Q' rollno = '1002' subject = 'science' mark = '64' )
                                 ( examid = 'Q' rollno = '1002' subject = 'history' mark = '71' )

                                 ( examid = 'Q' rollno = '1003' subject = 'tamil' mark = '77' )
                                 ( examid = 'Q' rollno = '1003' subject = 'english' mark = '49' )
                                 ( examid = 'Q' rollno = '1003' subject = 'maths' mark = '72' )
                                 ( examid = 'Q' rollno = '1003' subject = 'science' mark = '59' )
                                 ( examid = 'Q' rollno = '1003' subject = 'history' mark = '69' )

                                 ( examid = 'H' rollno = '1001' subject = 'tamil' mark = '75' )
                                 ( examid = 'H' rollno = '1001' subject = 'english' mark = '60' )
                                 ( examid = 'H' rollno = '1001' subject = 'maths' mark = '90' )
                                 ( examid = 'H' rollno = '1001' subject = 'science' mark = '80' )
                                 ( examid = 'H' rollno = '1001' subject = 'history' mark = '87' )

*                               * Erroneous data
                                 ( examid = 'W' rollno = '1010' subject = ' ' mark = '99' ) ).
    DATA(tamil_marks) = VALUE tt_subject_mark( FOR mark IN marks WHERE ( subject = 'tamil' )
                                               ( exam_name = VALUE #( exams[ examid = mark-examid ]-name DEFAULT '???' )
                                                 standard  = VALUE #( students[ rollno = mark-rollno ]-std DEFAULT 0 )
                                                 stu_name  = VALUE #( students[ rollno = mark-rollno ]-name DEFAULT '???' )
                                                 mark = mark-mark ) ).

    out->write( tamil_marks ).

  ENDMETHOD.


  METHOD for_itr_new_reduce.

    TYPES:

      BEGIN OF ty_student,
        rollno TYPE n LENGTH 4,
        name   TYPE string,
        std    TYPE i,
      END OF ty_student,
      tt_student TYPE SORTED TABLE OF ty_student WITH UNIQUE KEY rollno,

      BEGIN OF ty_exam,
        examid TYPE c LENGTH 1,  " Q-Quarterly; H-Half Yearly; F-Final
        name   TYPE string,
      END OF ty_exam,
      tt_exam TYPE SORTED TABLE OF ty_exam WITH UNIQUE KEY examid,

      BEGIN OF ty_mark,
        examid  TYPE c LENGTH 1,
        rollno  TYPE n LENGTH 4,
        subject TYPE  string,
        mark    TYPE i,
      END OF ty_mark,
      tt_mark TYPE STANDARD TABLE OF ty_mark WITH EMPTY KEY,

      BEGIN OF ty_subj_mark,
        subject TYPE  string,
        mark    TYPE i,
      END OF ty_subj_mark,
      tt_subj_mark TYPE STANDARD TABLE OF ty_subj_mark WITH EMPTY KEY,

      BEGIN OF ty_mark_sheet,
        exam_name TYPE string,
        standard  TYPE string,
        stu_name  TYPE string,
        tot_marks TYPE i,
        percent   TYPE i,
      END OF ty_mark_sheet,
      tt_mark_sheet TYPE STANDARD TABLE OF ty_mark_sheet WITH EMPTY KEY,

      BEGIN OF ty_subject_mark,
        exam_name TYPE string,
        standard  TYPE string,
        stu_name  TYPE string,
        mark      TYPE i,
      END OF ty_subject_mark,

      tt_subject_mark TYPE STANDARD TABLE OF ty_subject_mark WITH EMPTY KEY.

*   Master data
    DATA(students) = VALUE tt_student( ( rollno = '1001' name = 'Sathees' std = 10 )
                                       ( rollno = '1002' name = 'Kumar'   std = 10 )
                                       ( rollno = '1003' name = 'Ram'     std = 10 ) ).

    DATA(exams) = VALUE tt_exam( ( examid = 'Q' name = 'Quarterly' )
                                 ( examid = 'H' name = 'Half Yearly' )
                                 ( examid = 'F' name = 'Final' ) ).

*   Transaction data
    DATA(marks) = VALUE tt_mark( ( examid = 'Q' rollno = '1001' subject = 'tamil' mark = '70' )
                                 ( examid = 'Q' rollno = '1001' subject = 'english' mark = '55' )
                                 ( examid = 'Q' rollno = '1001' subject = 'maths' mark = '85' )
                                 ( examid = 'Q' rollno = '1001' subject = 'science' mark = '75' )
                                 ( examid = 'Q' rollno = '1001' subject = 'history' mark = '82' )

                                 ( examid = 'Q' rollno = '1002' subject = 'tamil' mark = '70' )
                                 ( examid = 'Q' rollno = '1002' subject = 'english' mark = '50' )
                                 ( examid = 'Q' rollno = '1002' subject = 'maths' mark = '70' )
                                 ( examid = 'Q' rollno = '1002' subject = 'science' mark = '64' )
                                 ( examid = 'Q' rollno = '1002' subject = 'history' mark = '71' )

                                 ( examid = 'Q' rollno = '1003' subject = 'tamil' mark = '77' )
                                 ( examid = 'Q' rollno = '1003' subject = 'english' mark = '49' )
                                 ( examid = 'Q' rollno = '1003' subject = 'maths' mark = '72' )
                                 ( examid = 'Q' rollno = '1003' subject = 'science' mark = '59' )
                                 ( examid = 'Q' rollno = '1003' subject = 'history' mark = '69' )

                                 ( examid = 'H' rollno = '1001' subject = 'tamil' mark = '75' )
                                 ( examid = 'H' rollno = '1001' subject = 'english' mark = '60' )
                                 ( examid = 'H' rollno = '1001' subject = 'maths' mark = '90' )
                                 ( examid = 'H' rollno = '1001' subject = 'science' mark = '80' )
                                 ( examid = 'H' rollno = '1001' subject = 'history' mark = '87' )

*                               * Erroneous data
                                 ( examid = 'W' rollno = '1010' subject = ' ' mark = '99' ) ).

    " To find subject wise total marks using REDUCE operator
    TYPES: BEGIN OF ty_subwise_marks,
             tamil   TYPE i,
             english TYPE i,
             maths   TYPE i,
             science TYPE i,
             history TYPE i,
           END OF ty_subwise_marks.

    DATA(sub_total) = REDUCE ty_subwise_marks( INIT subtot = VALUE ty_subwise_marks( )
                                 FOR I = 1 UNTIL I > LINES( marks )
                                NEXT subtot-tamil = subtot-tamil + COND #( WHEN marks[ I ]-subject = 'tamil' THEN marks[ I ]-MARK ELSE 0 )
                                     subtot-english = subtot-english + COND #( WHEN marks[ I ]-subject = 'english' THEN marks[ I ]-MARK ELSE 0 )
                                     subtot-maths = subtot-maths + COND #( WHEN marks[ I ]-subject = 'maths' THEN marks[ I ]-MARK ELSE 0 )
                                     subtot-science = subtot-science + COND #( WHEN marks[ I ]-subject = 'science' THEN marks[ I ]-MARK ELSE 0 )
                                     subtot-history = subtot-history + COND #( WHEN marks[ I ]-subject = 'history' THEN marks[ I ]-MARK ELSE 0 )
                             ).

    out->write( sub_total ).

  ENDMETHOD.


  METHOD lwg_new_loop_at_multiple_keys.

    TYPES: BEGIN OF ty_expenses,
             exp_date     TYPE string,
             person_name  TYPE string,
             exp_category TYPE string,  " AT NEW we use this column, but person name also considered
             exp_amount   TYPE i,
           END OF ty_expenses,
           tt_expenses TYPE STANDARD TABLE OF ty_expenses.

    DATA: itab_expenses TYPE tt_expenses,
          total         TYPE i.

    itab_expenses = VALUE #(
      ( exp_date = '15.03.2023' person_name = 'swathi'  exp_category = 'Tickets' exp_amount = 500 )
      ( exp_date = '01.03.2023' person_name = 'sathees' exp_category = 'Fuel'    exp_amount = 200 )
      ( exp_date = '12.03.2023' person_name = 'kiran'   exp_category = 'Fuel'    exp_amount = 500 )
      ( exp_date = '01.03.2023' person_name = 'swathi'  exp_category = 'Tickets' exp_amount = 800 )
      ( exp_date = '16.03.2023' person_name = 'sathees' exp_category = 'Fuel'    exp_amount = 500 )
      ( exp_date = '12.03.2023' person_name = 'swathi'  exp_category = 'Food'    exp_amount = 1000 )
      ( exp_date = '15.03.2023' person_name = 'kiran'   exp_category = 'Food'    exp_amount = 700 )
      ( exp_date = '08.03.2023' person_name = 'sathees' exp_category = 'Fuel'    exp_amount = 200 )
      ( exp_date = '11.03.2023' person_name = 'swathi'  exp_category = 'Tickets' exp_amount = 700 )
      ( exp_date = '01.03.2023' person_name = 'kiran'   exp_category = 'Food'    exp_amount = 500 )
      ( exp_date = '13.03.2023' person_name = 'swathi'  exp_category = 'Tickets' exp_amount = 500 )
    ).

    out->write( `Multiple Keys` ).
    LOOP AT itab_expenses   INTO DATA(expenses)
      GROUP BY ( name = expenses-person_name category = expenses-exp_category
                                            grpsize = GROUP SIZE grpidx = GROUP INDEX )
                                             WITHOUT MEMBERS INTO DATA(grp).
      " LOOP + GROUP with Multiple keys example

      out->write( |Idx { grp-grpidx }; records { grp-grpsize }; Name: { grp-name } Category: { grp-category }| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD lwg_new_loop_groupby_atgroup.

    TYPES: BEGIN OF ty_expenses,
             exp_date     TYPE string,
             exp_category TYPE string,
             exp_amount   TYPE i,
           END OF ty_expenses,
           tt_expenses TYPE STANDARD TABLE OF ty_expenses.

    DATA: itab_expenses TYPE tt_expenses,
          total         TYPE i.

    itab_expenses = VALUE #( ( exp_date = '01.03.2023' exp_category = 'Food'    exp_amount = 500 )
                             ( exp_date = '01.03.2023' exp_category = 'Fuel'    exp_amount = 200 )
                             ( exp_date = '01.03.2023' exp_category = 'Tickets' exp_amount = 800 )
                             ( exp_date = '12.03.2023' exp_category = 'Food'    exp_amount = 1000 )
                             ( exp_date = '12.03.2023' exp_category = 'Fuel'    exp_amount = 500 )
                             ( exp_date = '15.03.2023' exp_category = 'Food'    exp_amount = 700 )
                             ( exp_date = '15.03.2023' exp_category = 'Tickets' exp_amount = 500 )
                           ).

    LOOP AT itab_expenses INTO DATA(ls_expense) GROUP BY ( key = ls_expense-exp_category )
                                        INTO DATA(exp_grp).
      total = 0.
      LOOP AT GROUP exp_grp INTO DATA(ls_expgrp).
        total = total + ls_expgrp-exp_amount.
      ENDLOOP.
      out->write( | Total Expense Amount on { ls_expgrp-exp_category } is: { total }.| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD lwg_new_loop_itab_unique_rec.

    TYPES: BEGIN OF ty_expenses,
             exp_date     TYPE string,
             exp_category TYPE string,
             exp_amount   TYPE i,
           END OF ty_expenses,
           tt_expenses TYPE STANDARD TABLE OF ty_expenses.
    DATA: itab_expenses TYPE tt_expenses.

    itab_expenses = VALUE #(
      ( exp_date = '15.03.2023' exp_category = 'Tickets' exp_amount = 500 )
      ( exp_date = '01.03.2023' exp_category = 'Fuel'    exp_amount = 200 )
      ( exp_date = '12.03.2023' exp_category = 'Fuel'    exp_amount = 500 )
      ( exp_date = '01.03.2023' exp_category = 'Tickets' exp_amount = 800 )
      ( exp_date = '12.03.2023' exp_category = 'Food'    exp_amount = 1000 )
      ( exp_date = '15.03.2023' exp_category = 'Food'    exp_amount = 700 )
      ( exp_date = '01.03.2023' exp_category = 'Food'    exp_amount = 500 )
    ).

    " LOOP + GROUP to find unique records
    " To find unique records without using temp internal table
    " assignment with sorting and delete adjacent duplicates stmt.
    " Additionally, you can bring index as serial number
    " and count of records in group, below is the example

    out->write( `Unique Expense Categories` ).
    LOOP AT itab_expenses INTO DATA(expense)
                               GROUP BY ( key = expense-exp_category grpsize = GROUP SIZE grpidx = GROUP INDEX )
                               WITHOUT MEMBERS INTO DATA(grp).
      out->write( |{ grp-grpidx }. Category: { grp-key } - { grp-grpsize } rows| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD lwg_old_loopatcmd_wosort.

    TYPES: BEGIN OF ty_expenses,
             exp_category TYPE string,
             exp_date     TYPE string,
             exp_amount   TYPE i,
           END OF ty_expenses,
           tt_expenses TYPE STANDARD TABLE OF ty_expenses.

    DATA: itab_expenses TYPE tt_expenses,
          total         TYPE i.

    itab_expenses = VALUE #( ( exp_category = 'Food'    exp_date = '01.03.2023' exp_amount = 500 )
                             ( exp_category = 'Fuel'    exp_date = '01.03.2023' exp_amount = 200 )
                             ( exp_category = 'Tickets' exp_date = '01.03.2023' exp_amount = 800 )
                             ( exp_category = 'Food'    exp_date = '12.03.2023' exp_amount = 1000 )
                             ( exp_category = 'Fuel'    exp_date = '12.03.2023' exp_amount = 500 )
                             ( exp_category = 'Food'    exp_date = '15.03.2023' exp_amount = 700 )
                             ( exp_category = 'Tickets' exp_date = '15.03.2023' exp_amount = 500 )
                           ).

    out->write( `LOOP + AT command grouping` ).

    LOOP AT itab_expenses INTO DATA(ls_expense).
      AT NEW exp_category.
        " out->write( `Expense Date: ` && ls_expenses-exp_date ).
        total = 0.
      ENDAT.

      total = total + ls_expense-exp_amount.
      " out->write( |{ ls_expenses-exp_category } { ls_expenses-exp_amount }| ).

      AT END OF exp_date.
        out->write( | Total expenses on Category { ls_expense-exp_category } is: { total }| ).
      ENDAT.

    ENDLOOP.

  ENDMETHOD.


  METHOD lwg_old_loop_at_multiple_keys.

    TYPES: BEGIN OF ty_expenses,
             person_name  TYPE string,
             exp_category TYPE string,
             exp_date     TYPE string,
             exp_amount   TYPE i,
           END OF ty_expenses,
           tt_expenses TYPE STANDARD TABLE OF ty_expenses.

    DATA: itab_expenses TYPE tt_expenses,
          result        TYPE tt_expenses,
          count         TYPE i,
          total         TYPE i.

    itab_expenses = VALUE #(
      ( person_name = 'swathi'  exp_category = 'Tickets' exp_date = '15.03.2023' exp_amount = 500 )
      ( person_name = 'sathees' exp_category = 'Fuel'    exp_date = '01.03.2023' exp_amount = 200 )
      ( person_name = 'kiran'   exp_category = 'Fuel'    exp_date = '12.03.2023' exp_amount = 500 )
      ( person_name = 'swathi'  exp_category = 'Tickets' exp_date = '01.03.2023' exp_amount = 800 )
      ( person_name = 'sathees' exp_category = 'Fuel'    exp_date = '16.03.2023' exp_amount = 500 )
      ( person_name = 'swathi'  exp_category = 'Food'    exp_date = '12.03.2023' exp_amount = 1000 )
      ( person_name = 'kiran'   exp_category = 'Food'    exp_date = '15.03.2023' exp_amount = 700 )
      ( person_name = 'sathees' exp_category = 'Fuel'    exp_date = '08.03.2023' exp_amount = 200 )
      ( person_name = 'swathi'  exp_category = 'Tickets' exp_date = '11.03.2023' exp_amount = 700 )
      ( person_name = 'kiran'   exp_category = 'Food'    exp_date = '01.03.2023' exp_amount = 500 )
      ( person_name = 'swathi'  exp_category = 'Tickets' exp_date = '13.03.2023' exp_amount = 500 )
    ).

    SORT itab_expenses ASCENDING BY person_name exp_category.
    LOOP AT itab_expenses INTO DATA(expense).
      AT NEW exp_category.
        total = 0.
        count = 0.
      ENDAT.
      total = total + expense-exp_amount.
      count = count + 1.
      AT END OF exp_category.
        out->write( |Total Expenses for Person { expense-person_name }, Category { expense-exp_category } is: { total }; { count } rows| ).
      ENDAT.

    ENDLOOP.

  ENDMETHOD.


  METHOD lwg_old_loop_at_wth_sorting.

    TYPES: BEGIN OF ty_expenses,
             exp_category TYPE string,
             exp_date     TYPE string,
             exp_amount   TYPE i,
           END OF ty_expenses,
           tt_expenses TYPE STANDARD TABLE OF ty_expenses.

    DATA: itab_expenses TYPE tt_expenses,
          total         TYPE i.

    itab_expenses = VALUE #(
      ( exp_category = 'Tickets' exp_date = '15.03.2023' exp_amount = 500 )
      ( exp_category = 'Fuel'    exp_date = '01.03.2023' exp_amount = 200 )
      ( exp_category = 'Fuel'    exp_date = '12.03.2023' exp_amount = 500 )
      ( exp_category = 'Tickets' exp_date = '01.03.2023' exp_amount = 800 )
      ( exp_category = 'Food'    exp_date = '12.03.2023' exp_amount = 1000 )
      ( exp_category = 'Food'    exp_date = '15.03.2023' exp_amount = 700 )
      ( exp_category = 'Food'    exp_date = '01.03.2023' exp_amount = 500 )
    ).

    SORT itab_expenses BY  exp_category.
    out->write( `LOOP + AT command grouping with sorting` ).
    LOOP AT  itab_expenses  INTO DATA(ls_expence).

      AT NEW exp_category.
        total = 0.
      ENDAT.
      total  = total  + ls_expence-exp_amount.
      AT END OF exp_category.
        out->write( |Total Expense on Category { ls_expence-exp_category } is: { total }.| ).
      ENDAT.

    ENDLOOP.

  ENDMETHOD.


  METHOD lwg_old_loop_itab_unique_rec.

    TYPES: BEGIN OF ty_expenses,
             exp_category TYPE string,
             exp_date     TYPE string,
             exp_amount   TYPE i,
           END OF ty_expenses,
           tt_expenses TYPE STANDARD TABLE OF ty_expenses.

    DATA: itab_expenses TYPE tt_expenses,
          count         TYPE i,
          idx           TYPE i.

    itab_expenses = VALUE #(
       ( exp_category = 'Tickets' exp_date = '15.03.2023' exp_amount = 500 )
       ( exp_category = 'Fuel'    exp_date = '01.03.2023' exp_amount = 200 )
       ( exp_category = 'Fuel'    exp_date = '12.03.2023' exp_amount = 500 )
       ( exp_category = 'Tickets' exp_date = '01.03.2023' exp_amount = 800 )
       ( exp_category = 'Food'    exp_date = '12.03.2023' exp_amount = 1000 )
       ( exp_category = 'Food'    exp_date = '15.03.2023' exp_amount = 700 )
       ( exp_category = 'Food'    exp_date = '01.03.2023' exp_amount = 500 )
    ).

    SORT itab_expenses ASCENDING BY exp_category.
    LOOP AT itab_expenses INTO DATA(expense).

      AT NEW exp_category.
        idx = idx + 1.
        count = 0.
      ENDAT.

      count = count + 1.

      AT END OF exp_category.
        out->write( |{ idx }. Category { expense-exp_category } - { count } rows| ).
      ENDAT.

    ENDLOOP.

  ENDMETHOD.


  METHOD read_new_itab_comp_defkey.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    idx = line_index( lt_scarr[ KEY name COMPONENTS carrname = 'American Airlines' ] ).
    ls_scarr = lt_scarr[ KEY name COMPONENTS carrname = 'American Airlines' ].
    out->write( |{ idx } { ls_scarr-carrname }| ).

  ENDMETHOD.


  METHOD read_new_itab_idx_defkey.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    idx = 9.
    ls_scarr = lt_scarr[ KEY name INDEX idx ].
    out->write( |{ idx } { ls_scarr-carrname }| ).

  ENDMETHOD.


  METHOD read_new_itab_index.
    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    idx = 9.
    ls_scarr = lt_scarr[ idx ].
    out->write( |{ idx } { ls_scarr-carrname }| ).

  ENDMETHOD.


  METHOD read_new_itab_key.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    "   Table Expr. sy-tabix will not be updated except Assign usage
    ls_scarr = lt_scarr[ carrid = 'AB' currcode = 'EUR' ].
    idx = line_index( lt_scarr[ carrid = 'AB' currcode = 'EUR' ] ).
    out->write( |{ idx } { ls_scarr-carrname }| ).

  ENDMETHOD.


  METHOD read_new_itab_row_exists.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    IF line_exists( lt_scarr[ carrid = 'AA' ] ).
      out->write( |Carrier Id 'AA' exist.| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_new_itab_search.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    idx = line_index( lt_scarr[ carrid = 'AA' ] ).

  ENDMETHOD.


  METHOD read_old_itab_comp_defkey.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    READ TABLE lt_scarr WITH TABLE KEY name COMPONENTS carrname = 'American Airlines' INTO ls_scarr.
    IF sy-subrc = 0.
      idx = sy-tabix.
      out->write( |{ idx } { ls_scarr-carrname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_old_itab_idx_defkey.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    "   Using defined key (Primary or Secondary keys)
    READ TABLE lt_scarr INDEX 9 USING KEY name INTO ls_scarr.
    IF sy-subrc = 0.
      idx = sy-tabix.
      out->write( |{ idx } { ls_scarr-carrname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_old_itab_index.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    READ TABLE lt_scarr INDEX 9 INTO ls_scarr.
    IF sy-subrc = 0.
      idx = sy-tabix.
      out->write( |{ idx } { ls_scarr-carrname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_old_itab_key.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    "   Using key (Primary Table Index not a primary key)
    READ TABLE lt_scarr WITH KEY carrid = 'AB' currcode = 'EUR' INTO ls_scarr.
    IF sy-subrc = 0.
      idx = sy-tabix.
      out->write( |{ idx } { ls_scarr-carrname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_old_itab_row_exists.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    READ TABLE lt_scarr WITH KEY carrid = 'AA' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      out->write( |Carrier Id 'AA' exist.| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_old_itab_search.

    DATA: ls_scarr TYPE  scarr,
          lt_scarr TYPE  STANDARD TABLE OF scarr
                            WITH KEY primary_key COMPONENTS carrid
                            WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname,
          idx      TYPE i.

    SELECT * FROM scarr
      INTO TABLE @lt_scarr.

    READ TABLE lt_scarr WITH KEY carrid = 'AA' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      idx = sy-tabix.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
