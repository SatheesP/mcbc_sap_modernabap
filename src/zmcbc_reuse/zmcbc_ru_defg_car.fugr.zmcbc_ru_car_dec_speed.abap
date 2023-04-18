FUNCTION zmcbc_ru_car_dec_speed.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SPEED) TYPE  I
*"----------------------------------------------------------------------
  DATA(lv_log) = |Car speed decreased from { gv_speed }|.
  gv_speed = gv_speed - iv_speed.

  APPEND VALUE #( str = |{ lv_log } to { gv_speed }| ) TO gt_log_message.

ENDFUNCTION.
