       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 LOOP_SAMPLE01.
       *>
       ENVIRONMENT                 DIVISION.
       DATA                        DIVISION.
       *>
       WORKING-STORAGE             SECTION.
           01 WK-SUJI              PIC 99 VALUE 1.
       PROCEDURE DIVISION.
       *>指定回数繰り返す処理
           PERFORM WK-SUJI TIMES
               ADD 2 3 4 5 6 7 8 9 10 TO WK-SUJI
           END-PERFORM.
       *>
           DISPLAY WK-SUJI.
           STOP RUN.
       END PROGRAM LOOP_SAMPLE01.
