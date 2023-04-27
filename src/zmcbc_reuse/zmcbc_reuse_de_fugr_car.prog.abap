*&---------------------------------------------------------------------*
*& Report  ZMCBC_REUSE_DE_FUGR_CAR
*&
*&---------------------------------------------------------------------*
*& Architecture, Design & Developed by Sathees Kumar P.
*&
*& Training Example for MCBC - Modern ABAP, ABAP OO
*& Description : Show-case the Data Encapsulation in Function Group.
*&               Manipulate Function Group's global data objects
*&               via indirect dynamic access.
*&---------------------------------------------------------------------*
REPORT zmcbc_reuse_de_fugr_car.

DATA : gv_car  TYPE  string  VALUE ' -Altroz-'.

* No direct access to 'gv_speed' in Func Group 'ZMCBC_RU_DEFG_CAR'

* Use of function module to manipulate the data 'gv_speed'

INITIALIZATION.


START-OF-SELECTION.

  WRITE: 'Car: ' COLOR COL_HEADING, gv_car COLOR COL_HEADING.

  PERFORM drive_car.
  PERFORM access_internal_data.


FORM drive_car.

  DATA : lv_speed  TYPE  i.

  CALL FUNCTION 'ZMCBC_RU_CAR_INC_SPEED'
    EXPORTING
      iv_speed = 30.

  CALL FUNCTION 'ZMCBC_RU_CAR_GET_SPEED'
    IMPORTING
      ev_car_speed = lv_speed.

  WRITE: lv_speed.

  CALL FUNCTION 'ZMCBC_RU_CAR_DEC_SPEED'
    EXPORTING
      iv_speed = 10.

  CALL FUNCTION 'ZMCBC_RU_CAR_GET_SPEED'
    IMPORTING
      ev_car_speed = lv_speed.

  WRITE: lv_speed.

  CALL FUNCTION 'ZMCBC_RU_CAR_STOP'.

  CALL FUNCTION 'ZMCBC_RU_CAR_GET_SPEED'
    IMPORTING
      ev_car_speed = lv_speed.

  WRITE: lv_speed.

  CALL FUNCTION 'ZMCBC_RU_CAR_SHOW_LOGMSGS'
    EXPORTING
      iv_clear_logmsgs = abap_true.

ENDFORM.

* Indirect access & change to Function Group's Global Data
* Function Group's FM called once then data available in internal session memory
* No control over global data once Function Group loaded to internal session memory
FORM access_internal_data.

  DATA : lv_speed        TYPE  i,
         lv_indir_access TYPE  string VALUE '(SAPLZMCBC_RU_DEFG_CAR)gv_speed'.

  FIELD-SYMBOLS: <lv_speed>  TYPE  i.

  CALL FUNCTION 'ZMCBC_RU_CAR_INC_SPEED'
    EXPORTING
      iv_speed = 30.

  CALL FUNCTION 'ZMCBC_RU_CAR_GET_SPEED'
    IMPORTING
      ev_car_speed = lv_speed.

  WRITE: /, 'Before Indirect Access' COLOR COL_POSITIVE.
  WRITE: lv_speed.

  ASSIGN (lv_indir_access) TO <lv_speed>.
  IF sy-subrc = 0 AND <lv_speed> IS ASSIGNED.
    <lv_speed> = 0.
  ENDIF.

  CALL FUNCTION 'ZMCBC_RU_CAR_GET_SPEED'
    IMPORTING
      ev_car_speed = lv_speed.

  WRITE: /, 'After Indirect Change' COLOR COL_NEGATIVE.
  WRITE: lv_speed.

  CALL FUNCTION 'ZMCBC_RU_CAR_SHOW_LOGMSGS'.

ENDFORM.
