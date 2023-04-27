CLASS zcl_mcbc_modernabap DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_modernabap,
             topic_key       TYPE  zmcbc_modernabap_topics,
             topic_text      TYPE  string,
             subtopic_key    TYPE  c LENGTH 3, "A0..A9, AA...AZ
             subtopic_text   TYPE  string,

             class_key       TYPE  seoclskey,
             old_method_name TYPE  seocpdname,
             new_method_name TYPE  seocpdname,
             execute_method  TYPE  abap_bool,
           END OF ty_modernabap,

           tt_modernabap TYPE STANDARD TABLE OF ty_modernabap WITH EMPTY KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS set_output
      IMPORTING
        !io_out TYPE REF TO object .

    CLASS-METHODS factory
      IMPORTING
        !iv_modern_abap_topic_key   TYPE zmcbc_modernabap_topics
        !io_out                     TYPE REF TO object OPTIONAL
      RETURNING
        VALUE(ro_modern_abap_topic) TYPE REF TO zcl_mcbc_modernabap .

    CLASS-DATA gt_modernabap_info TYPE tt_modernabap READ-ONLY.
  PROTECTED SECTION.
    DATA:
      out TYPE REF TO zcl_mcbc_output.

  PRIVATE SECTION.
    CLASS-DATA:
      go_out TYPE REF TO zcl_mcbc_output.

ENDCLASS.



CLASS ZCL_MCBC_MODERNABAP IMPLEMENTATION.


  METHOD class_constructor.

    DATA lt_dd07 TYPE STANDARD TABLE OF dd07v.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZMCBC_MODERNABAP_TOPICS'    " Domain name
        text           = abap_true
      TABLES
        dd07v_tab      = lt_dd07
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    gt_modernabap_info = VALUE #(
      ( topic_key = 'A'                                  "--------- Itab filling for topic A: Inline Declaration
        topic_text = lt_dd07[ domvalue_l = 'A' ]-ddtext
        subtopic_key = 'A0'
        subtopic_text = 'Elementaty Data Types'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_INLINEDECLAR'
        old_method_name = 'OLD_ELEMENTARY_DATA_TYPES'
        new_method_name = 'NEW_ELEMENTARY_DATA_TYPES'
        execute_method = abap_true )
      ( topic_key = 'A'
        topic_text = lt_dd07[ domvalue_l = 'A' ]-ddtext
        subtopic_key = 'A1'
        subtopic_text = 'SELECT SINGLE Statement'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_INLINEDECLAR'
        old_method_name = 'OLD_SELECT_SINGLE_STMT'
        new_method_name = 'NEW_SELECT_SINGLE_STMT'
        execute_method = abap_true )
      ( topic_key = 'A'
        topic_text = lt_dd07[ domvalue_l = 'A' ]-ddtext
        subtopic_key = 'A2'
        subtopic_text = 'SELECT Statement ITAB'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_INLINEDECLAR'
        old_method_name = 'OLD_SELECT_STMT_ITAB'
        new_method_name = 'NEW_SELECT_STMT_ITAB'
        execute_method = abap_true )
      ( topic_key = 'A'
        topic_text = lt_dd07[ domvalue_l = 'A' ]-ddtext
        subtopic_key = 'A3'
        subtopic_text = 'LOOP AT INTO'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_INLINEDECLAR'
        old_method_name = 'OLD_LOOP_AT_INTO'
        new_method_name = 'NEW_LOOP_AT_INTO'
        execute_method = abap_true )
      ( topic_key = 'A'
        topic_text = lt_dd07[ domvalue_l = 'A' ]-ddtext
        subtopic_key = 'A4'
        subtopic_text = 'LOOP AT ASSIGNING'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_INLINEDECLAR'
        old_method_name = 'OLD_LOOP_AT_ASSIGNING'
        new_method_name = 'NEW_LOOP_AT_ASSIGNING'
        execute_method = abap_true )
      ( topic_key = 'A'
        topic_text = lt_dd07[ domvalue_l = 'A' ]-ddtext
        subtopic_key = 'A5'
        subtopic_text = 'Method Calling'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_INLINEDECLAR'
        old_method_name = 'OLD_METHOD_CALLING'
        new_method_name = 'NEW_METHOD_CALLING'
        execute_method = abap_true )

      ( topic_key = 'B'                                   "----------- Itab filling for topic B: constr exp
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B0'
        subtopic_text = 'NEW-Instance Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'NEW_OPERATOR'
        execute_method = abap_true )
      ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B1'
        subtopic_text = 'VALUE-Value Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'VALUE_OPERATOR'
        execute_method = abap_true )
      ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B2'
        subtopic_text = 'REF-Reference Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'REF_OPERATOR'
        execute_method = abap_true )
      ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B3'
        subtopic_text = 'EXACT-Lossless Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'EXACT_OPERATOR'
        execute_method = abap_true )
      ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B4'
        subtopic_text = 'CONV-Conversion Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'CONV_OPERATOR'
        execute_method = abap_true )
      ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B5'
        subtopic_text = 'CAST-Casting Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'CAST_OPERATOR'
        execute_method = abap_true )
      ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B6'
        subtopic_text = 'COND-Conditional Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'COND_OPERATOR'
        execute_method = abap_true )
      ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B7'
        subtopic_text = 'SWITCH-Conditional Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'SWITCH_OPERATOR'
        execute_method = abap_true )
      ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B8'
        subtopic_text = 'FILTER-Filter Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'FILTER_OPERATOR'
        execute_method = abap_true )
     ( topic_key = 'B'
        topic_text = lt_dd07[ domvalue_l = 'B' ]-ddtext
        subtopic_key = 'B9'
        subtopic_text = 'CORRESPONDING-Component Operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_CONST_EXP'
        old_method_name = ''
        new_method_name = 'CORRESPONDING_OPERATOR'
        execute_method = abap_true )

      ( topic_key = 'C'                                   "----- Itab filling for topic C: Table Expressions
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C0'
        subtopic_text = 'Read itab with Index Access'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'READ_OLD_ITAB_INDEX'
        new_method_name = 'READ_NEW_ITAB_INDEX'
        execute_method = abap_true )
      ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C1'
        subtopic_text = 'Read itab with key'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'READ_OLD_ITAB_KEY'
        new_method_name = 'READ_NEW_ITAB_KEY'
        execute_method = abap_true )
      ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C2'
        subtopic_text = 'Read itab with Index Accessvia Defined Key'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'READ_OLD_ITAB_IDX_DEFKEY'
        new_method_name = 'READ_NEW_ITAB_IDX_DEFKEY'
        execute_method = abap_true )
      ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C3'
        subtopic_text = 'Read itab with components of Defined Key'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'READ_OLD_ITAB_COMP_DEFKEY'
        new_method_name = 'READ_NEW_ITAB_COMP_DEFKEY'
        execute_method = abap_true )
      ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C4'
        subtopic_text = 'Check itab row exists or not'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'READ_OLD_ITAB_ROW_EXISTS'
        new_method_name = 'READ_NEW_ITAB_ROW_EXISTS'
        execute_method = abap_true )
      ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C5'
        subtopic_text = 'Check itab row index for Search Criteria'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'READ_OLD_ITAB_SEARCH'
        new_method_name = 'READ_NEW_ITAB_SEARCH'
        execute_method = abap_true )
      ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C6'
        subtopic_text = 'Exception CX_SY_ITAB_LINE_NOT_FOUND raising'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = ''
        new_method_name = 'EXCEPTION_RAISING'
        execute_method = abap_true )

       ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C7a'
        subtopic_text = 'LOOP .. GROUP BY + AT GROUP w/o sorting'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'LWG_OLD_LOOPATCMD_WOSORT'
        new_method_name = 'LWG_NEW_LOOP_GROUPBY_ATGROUP'
        execute_method = abap_true )
       ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C7b'
        subtopic_text = 'LOOP .. GROUP BY + AT GROUP with sorting'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'LWG_OLD_LOOP_AT_WTH_SORTING'
        new_method_name = 'LWG_NEW_LOOP_GROUPBY_ATGROUP'
        execute_method = abap_true )
       ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C7c'
        subtopic_text = 'LOOP AT itab with GROUP BY to find unique records'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'LWG_OLD_LOOP_ITAB_UNIQUE_REC'
        new_method_name = 'LWG_NEW_LOOP_ITAB_UNIQUE_REC'
        execute_method = abap_true )
       ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C7d'
        subtopic_text = 'LOOP AT itab with GROUP BY with Multiple keys'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = 'LWG_OLD_LOOP_AT_MULTIPLE_KEYS'
        new_method_name = 'LWG_NEW_LOOP_AT_MULTIPLE_KEYS'
        execute_method = abap_true )

 ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C8a'
        subtopic_text = 'FOR GROUPS … OF and FOR … IN GROUP'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = ''
        new_method_name = 'FOR_ITR_NEW_FOR_GROUPS'
        execute_method = abap_true )

  ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C8b'
        subtopic_text = 'FOR … IN itab'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = ''
        new_method_name = 'FOR_ITR_NEW_FOR_IN_ITAB'
        execute_method = abap_true )

 ( topic_key = 'C'
        topic_text = lt_dd07[ domvalue_l = 'C' ]-ddtext
        subtopic_key = 'C8c'
        subtopic_text = 'REDUCE operator'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_TABLE_EXP'
        old_method_name = ''
        new_method_name = 'FOR_ITR_NEW_REDUCE'
        execute_method = abap_true )



"""""""""""""""""""""""""""""""""""""""""""""""
      ( topic_key = 'D'                                   "--- Itab filling for topic D: New Built-in Functions
        topic_text = lt_dd07[ domvalue_l = 'D' ]-ddtext
        subtopic_key = 'D0'
        subtopic_text = 'New Built-in Functions ipow,nmin,nmax'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_NEW_BUILTIN'
        old_method_name = ''
        new_method_name = 'IPOW_NMIN_NMAX'
        execute_method = abap_true )
      ( topic_key = 'D'
        topic_text = lt_dd07[ domvalue_l = 'D' ]-ddtext
        subtopic_key = 'D1'
        subtopic_text = 'Boolean Built-in Functions BOOLC, BOOLX, XSDBOOL'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_NEW_BUILTIN'
        old_method_name = ''
        new_method_name = 'BOOLEAN_FUNCTIONS'
        execute_method = abap_true )
      ( topic_key = 'D'
        topic_text = lt_dd07[ domvalue_l = 'D' ]-ddtext
        subtopic_key = 'D2'
        subtopic_text = 'Itab Built-in Functions LINES, LINE_INDEX, LINE_EXISTS'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_NEW_BUILTIN'
        old_method_name = ''
        new_method_name = 'ITAB_FUNCTIONS'
        execute_method = abap_true )
      ( topic_key = 'D'
        topic_text = lt_dd07[ domvalue_l = 'D' ]-ddtext
        subtopic_key = 'D3'
        subtopic_text = 'CONDENSE Built-in Function'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_NEW_BUILTIN'
        old_method_name = ''
        new_method_name = 'CONDENSE_FUNCTION'
        execute_method = abap_true )
      ( topic_key = 'E'                                  "---- Itab filling for topic E: Meshes
        topic_text = lt_dd07[ domvalue_l = 'E' ]-ddtext
        subtopic_key = 'E0'
        subtopic_text = 'Forward and Inverse Association'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_MESHES'
        old_method_name = ''
        new_method_name = 'FORWARD_INVERSE_ASSOCIATION'
        execute_method = abap_true )
      ( topic_key = 'E'
        topic_text = lt_dd07[ domvalue_l = 'E' ]-ddtext
        subtopic_key = 'E1'
        subtopic_text = 'Reflexive Association'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_MESHES'
        old_method_name = ''
        new_method_name = 'REFLEXIVE_ASSOCIATION'
        execute_method = abap_true )
      ( topic_key = 'F'
        topic_text = lt_dd07[ domvalue_l = 'F' ]-ddtext
        subtopic_key = 'F0'
        subtopic_text = 'Literal Text'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_STRING_TEMP'
        old_method_name = ''
        new_method_name = 'LITERAL_TEXT'
        execute_method = abap_true )
      ( topic_key = 'F'
        topic_text = lt_dd07[ domvalue_l = 'F' ]-ddtext
        subtopic_key = 'F1'
        subtopic_text = 'Format Options'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_STRING_TEMP'
        old_method_name = ''
        new_method_name = 'FORMAT_OPTIONS'
        execute_method = abap_true )
      ( topic_key = 'F'
        topic_text = lt_dd07[ domvalue_l = 'F' ]-ddtext
        subtopic_key = 'F2'
        subtopic_text = 'Control Characters'
        class_key-clsname = 'ZCL_MCBC_MDRNABAP_STRING_TEMP'
        old_method_name = ''
        new_method_name = 'CONTROL_CHARACTERS'
        execute_method = abap_true )
      ).

  ENDMETHOD.


  METHOD factory.

    IF io_out IS SUPPLIED.
      set_output( io_out = io_out ).
    ENDIF.

    CASE iv_modern_abap_topic_key.
      WHEN 'A'. " Inline Declarations
        ro_modern_abap_topic = NEW zcl_mcbc_mdrnabap_inlinedeclar( ).

      WHEN 'B'. " Constructor Expressions
        ro_modern_abap_topic = NEW zcl_mcbc_mdrnabap_const_exp( ).

      WHEN 'C'. " Table Expressions
        ro_modern_abap_topic = NEW zcl_mcbc_mdrnabap_table_exp( ).

      WHEN 'D'. " New Built-In Functions
        ro_modern_abap_topic = NEW zcl_mcbc_mdrnabap_new_builtin( ).

      WHEN 'E'. " Meshes
        ro_modern_abap_topic = NEW zcl_mcbc_mdrnabap_meshes( ).

      WHEN 'F'. " String Templates
        ro_modern_abap_topic = NEW zcl_mcbc_mdrnabap_string_temp( ).

    ENDCASE.
    IF ro_modern_abap_topic IS BOUND.
      ro_modern_abap_topic->out = go_out.
    ENDIF.

  ENDMETHOD.


  METHOD set_output.

    IF go_out IS NOT BOUND.
      go_out = zcl_mcbc_output=>factory( io_out = io_out ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
