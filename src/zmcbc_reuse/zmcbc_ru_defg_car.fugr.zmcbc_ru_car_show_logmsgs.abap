FUNCTION zmcbc_ru_car_show_logmsgs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_CLEAR_LOGMSGS) TYPE  ABAP_BOOL OPTIONAL
*"----------------------------------------------------------------------
  DATA: lr_table  TYPE REF TO cl_salv_table.

  LOOP AT gt_log_message ASSIGNING FIELD-SYMBOL(<fs_log>).
    <fs_log>-row_index = sy-tabix.
  ENDLOOP.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lr_table
        CHANGING
          t_table      = gt_log_message ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = lr_table->get_functions( ).
  lr_functions->set_all( abap_true ).

  DATA: lr_columns TYPE REF TO cl_salv_columns.

  lr_columns = lr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  lr_table->set_screen_popup(
    start_column = 1
    end_column   = 100
    start_line   = 1
    end_line     = 20 ).

  lr_table->display( ).

  IF iv_clear_logmsgs = abap_true.
    CLEAR gt_log_message.
  ENDIF.

ENDFUNCTION.
