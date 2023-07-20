       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 LOOP_SAMPLE02.
       *>
       ENVIRONMENT                 DIVISION.
       DATA                        DIVISION.
       *>
       WORKING-STORAGE             SECTION.
           01 WK-SUJI-I              PIC 99.
           01 WK-SUJI-TOTAL          PIC 99.
       PROCEDURE DIVISION.
       MOVE  ZERO TO  WK-SUJI-I.
       MOVE  ZERO TO  WK-SUJI-TOTAL.
       ADD  1 TO  WK-SUJI-I.
       *>指定回数繰り返す処理
           PERFORM UNTIL WK-SUJI-I > 10
               ADD WK-SUJI-I TO  WK-SUJI-TOTAL
               ADD 1 TO WK-SUJI-I
           END-PERFORM.
       *>
           DISPLAY WK-SUJI-TOTAL.
           STOP RUN.
