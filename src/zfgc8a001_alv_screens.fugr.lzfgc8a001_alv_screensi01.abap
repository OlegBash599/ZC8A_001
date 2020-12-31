*&---------------------------------------------------------------------*
*&  Include           LZSAS02_ALV_SCREENSI01
*&---------------------------------------------------------------------*

 MODULE user_command_5100 INPUT.
   go_view1->syucomm_screen( EXPORTING iv_syucomm =  sy-ucomm ).
 ENDMODULE.                    "USER_COMMAND_9000 INPUT

*----------------------------------------------------------------------*
*  MODULE user_command_5200 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
 MODULE user_command_5200 INPUT.
   go_view_on_dto->syucomm_screen( EXPORTING iv_syucomm =  sy-ucomm ).
 ENDMODULE.                    "USER_COMMAND_9000 INPUT


*----------------------------------------------------------------------*
*  MODULE user_comexit_9000 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
 MODULE user_comexit_5100 INPUT.
   SET SCREEN 0.
   LEAVE SCREEN.
 ENDMODULE.                    "user_comexit_9000 INPUT
