*&---------------------------------------------------------------------*
*& Report  ZMCBC_MODERNABAP_GUI
*&
*&---------------------------------------------------------------------*
*&
*& Architected by: SATHEES KUMAR P.
*& Development Team: SWATHI & KIRAN
*& Training Example for MCBC - Modern ABAP
*& Description: Example coding display based on topic and
*&              sub topic selection
*&---------------------------------------------------------------------*
REPORT zmcbc_modernabap_gui MESSAGE-ID zmcbc_mdrnabap_msgs.

TYPE-POOLS: vrm.

PARAMETERS ip_topic TYPE zmcbc_modernabap_topics AS LISTBOX VISIBLE LENGTH 30.
PARAMETERS ip_subtp TYPE char70 LOWER CASE.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR ip_subtp.
  PERFORM set_subtopic.

AT SELECTION-SCREEN.
  PERFORM validations.  "" validations""

START-OF-SELECTION.

  DATA(go_out) = cl_demo_output=>new( ).
  DATA gv_sec_heading TYPE string.
  DATA gt_source TYPE rswsourcet.

  ASSIGN zcl_mcbc_modernabap=>gt_modernabap_info[ topic_key = ip_topic subtopic_key = ip_subtp(3) ]
    TO FIELD-SYMBOL(<fs_modernabap>).
  IF sy-subrc <> 0 OR <fs_modernabap> IS NOT ASSIGNED.
    RETURN.
  ENDIF.

*-Print Heading
  gv_sec_heading = |{ <fs_modernabap>-topic_text } - { <fs_modernabap>-subtopic_text }|.
  go_out->begin_section( title = gv_sec_heading ).

*-To get OO include names for class methods
  cl_oo_include_naming=>get_instance_by_cifkey(
    EXPORTING
      cifkey = <fs_modernabap>-class_key     " Object Type Key
    RECEIVING
      cifref = DATA(lo_clas_intf_ref)
    EXCEPTIONS
      no_objecttype  = 1
      internal_error = 2
      OTHERS         = 3
  ).
  IF sy-subrc = 0.
    DATA(lo_class_ref) = CAST if_oo_class_incl_naming( lo_clas_intf_ref ).
  ENDIF.

  zcl_mcbc_modernabap=>factory(
    EXPORTING
      iv_modern_abap_topic_key = ip_topic
      io_out                   = go_out
    RECEIVING
      ro_modern_abap_topic     = DATA(lo_modern_abap)
  ).

  IF lo_modern_abap IS NOT BOUND.
    go_out->write(
      EXPORTING
        data = |{ <fs_modernabap>-subtopic_text } - !!! Modern ABAP Sub Class implementation not found !!!|
    ).
    go_out->display( ).
    RETURN.
  ENDIF.

** Display the source code of the Old Method
  IF <fs_modernabap>-old_method_name IS NOT INITIAL.
    DATA(lv_include_name) = lo_class_ref->get_include_by_mtdname( <fs_modernabap>-old_method_name ).
    READ REPORT lv_include_name INTO gt_source.

    go_out->write(
      EXPORTING
        data   = gt_source    " Text or Data
        name   = CONV string( 'Old Code'(001) )   " Name
    ).

**- Display the output of the Old Method
    IF <fs_modernabap>-execute_method = abap_true AND <fs_modernabap>-old_method_name IS NOT INITIAL.
      CALL METHOD lo_modern_abap->(<fs_modernabap>-old_method_name).
    ENDIF.

    go_out->line( ).
  ENDIF.

** Display the source code of the New Method
  lv_include_name = lo_class_ref->get_include_by_mtdname( <fs_modernabap>-new_method_name ).
  READ REPORT lv_include_name INTO gt_source.

  go_out->write(
    EXPORTING
      data   = gt_source    " Text or Data
      name   = CONV string( 'New Code'(002) )   " Name
  ).

** Display the output of the New Method
  IF <fs_modernabap>-execute_method = abap_true AND <fs_modernabap>-new_method_name IS NOT INITIAL.
    CALL METHOD lo_modern_abap->(<fs_modernabap>-new_method_name).
  ENDIF.

  go_out->end_section( ).
  go_out->line( ).

  go_out->display( ).

*&---------------------------------------------------------------------*
*&      Form  SET_SUBTOPIC
*&---------------------------------------------------------------------*
*       Sets subtopic based on Main Topic selection
*----------------------------------------------------------------------*
FORM set_subtopic.

  DATA lv_topic_key TYPE zmcbc_modernabap_topics.
  DATA lv_subtopic_key TYPE c LENGTH 6.
  DATA lt_dynp_fields TYPE STANDARD TABLE OF dynpread.
  DATA lt_dfies TYPE STANDARD TABLE OF dfies.
  DATA lt_rettab TYPE STANDARD TABLE OF ddshretval.
  DATA lt_vrm TYPE vrm_values.

  lt_dynp_fields = VALUE #( ( fieldname = 'IP_TOPIC' ) ).

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid    " Program Name
      dynumb               = sy-dynnr    " Screen Number
    TABLES
      dynpfields           = lt_dynp_fields    " Table for Reading Current Screen Values
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.
  IF sy-subrc = 0.
    lv_topic_key = lt_dynp_fields[ fieldname = 'IP_TOPIC' ]-fieldvalue.
  ENDIF.

  lt_vrm = VALUE #( FOR row IN zcl_mcbc_modernabap=>gt_modernabap_info WHERE ( topic_key = lv_topic_key ) ( key = row-subtopic_key text = row-subtopic_text ) ).
  lt_dfies = VALUE #( ( fieldname = 'KEY'  lfieldname = 'KEY'  datatype = 'CHAR' leng = 40 offset =  0 outputlen = 40 intlen =  80 position = 1 )
                      ( fieldname = 'TEXT' lfieldname = 'TEXT' datatype = 'CHAR' leng = 80 offset = 80 outputlen = 80 intlen = 160 position = 2 ) ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KEY'       " Name of return field in FIELD_TAB
      dynpprog        = sy-repid    " Current program
      dynpnr          = sy-dynnr    " Screen number
      value_org       = 'S'         " Value return: C: cell by cell, S: structured
    TABLES
      value_tab       = lt_vrm      " Table of values: entries cell by cell
      field_tab       = lt_dfies    " Fields of the hit list
      return_tab      = lt_rettab   " Return the selected value
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0 AND line_exists( lt_rettab[ fieldname = 'KEY' ] ).
    lv_subtopic_key = lt_rettab[ fieldname = 'KEY' ]-fieldval.
    ip_subtp = |{ lv_subtopic_key } { lt_vrm[ key = lv_subtopic_key ]-text }|.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDATIONS
*&---------------------------------------------------------------------*
*       Selection Common Validations
*----------------------------------------------------------------------*
FORM validations.

  IF ip_topic IS INITIAL.
    MESSAGE e001.
  ENDIF.

  IF ip_subtp IS INITIAL.
    MESSAGE e002.
  ENDIF.

  IF ip_topic <> ip_subtp(1).
    MESSAGE e003.
  ENDIF.

  IF NOT line_exists( zcl_mcbc_modernabap=>gt_modernabap_info[ topic_key = ip_topic subtopic_key = ip_subtp(3) ] ).
    MESSAGE e003.
  ENDIF.

ENDFORM.
