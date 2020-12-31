*&---------------------------------------------------------------------*
*&  Include           LZSAS02_ALV_SCREENSO01
*&---------------------------------------------------------------------*

 MODULE status_5100 OUTPUT.
   SET TITLEBAR 'ZC8A001_T' WITH sy-datum sy-uzeit.
   SET PF-STATUS 'ZC8A001_PF1'.

   IF go_view1 IS BOUND.
     go_view1->do_tab( ).
   ENDIF.
 ENDMODULE.                    "status_9000 OUTPUT


*----------------------------------------------------------------------*
*  MODULE status_5200 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
 MODULE status_5200 OUTPUT.
   SET TITLEBAR 'ZC8A001_T' WITH sy-datum sy-uzeit.
   SET PF-STATUS 'ZC8A001_PF1'.

   IF go_view_on_dto IS BOUND.
     go_view_on_dto->do_tab( ).
   ENDIF.
 ENDMODULE.                    "status_5200 OUTPUT
