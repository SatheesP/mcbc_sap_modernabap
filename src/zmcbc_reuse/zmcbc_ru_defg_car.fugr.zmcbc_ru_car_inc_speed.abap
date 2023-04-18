FUNCTION ZMCBC_RU_CAR_INC_SPEED.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SPEED) TYPE  I OPTIONAL
*"----------------------------------------------------------------------
  DATA(lv_log) = |Car speed increased from { gv_speed }|.
  gv_speed = gv_speed + iv_speed.

  APPEND VALUE #( str = |{ lv_log } to { gv_speed }| ) TO gt_log_message.

ENDFUNCTION.
