       IDENTIFICATION                DIVISION.
       PROGRAM-ID.                   ADD_SAMPLE01.

       ENVIRONMENT                   DIVISION.
       CONFIGURATION                 SECTION.
       DATA                          DIVISION.
       WORKING-STORAGE               SECTION.
         01 WORK-AREA.
           03 WORK-GROUP-1.
             05 WORK-1             PIC 9(3).
             05 WORK-2             PIC 9(3).
             05 WORK-SUM           PIC 9(3).

       PROCEDURE                     DIVISION.
       MAIN                          SECTION.

       MOVE 100 TO WORK-1.
       MOVE 200 TO WORK-2.

       ADD WORK-1 WORK-2 TO WORK-SUM.

       DISPLAY "合計: "WORK-SUM.

       STOP RUN.
