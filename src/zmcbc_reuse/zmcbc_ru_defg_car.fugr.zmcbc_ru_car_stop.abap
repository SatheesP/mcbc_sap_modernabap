FUNCTION ZMCBC_RU_CAR_STOP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  APPEND VALUE #( str = |Car stopped from speed of { gv_speed }| ) TO gt_log_message.
  gv_speed = 0.

ENDFUNCTION.
