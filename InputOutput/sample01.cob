       IDENTIFICATION DIVISION.
       PROGRAM-ID. INOUT_OUTPUT01.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN TO 'test.txt'
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
               FD TEST-FILE.
                  01 OUT-FILE-REC PIC 99.
           WORKING-STORAGE SECTION.
               01 TEST_NUM PIC 9(2).
       PROCEDURE DIVISION.
           MAIN SECTION.
                OPEN OUTPUT TEST-FILE.

                MOVE 10 TO TEST_NUM.
                MOVE TEST_NUM TO OUT-FILE-REC.

                WRITE OUT-FILE-REC.
                DISPLAY OUT-FILE-REC
                CLOSE TEST-FILE.
       STOP RUN.
