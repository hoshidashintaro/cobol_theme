       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE01.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT TEST-FILE ASSIGN TO 'test01.txt'
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS IN-FILE-STATUS.

       DATA DIVISION.
           FILE SECTION.
           FD TEST-FILE.
               01 IN-FILE-REC PIC X(10).
           WORKING-STORAGE SECTION.
               01 IN-FILE-STATUS PIC XX.
       PROCEDURE DIVISION.
           OPEN INPUT TEST-FILE.
           PERFORM UNTIL IN-FILE-STATUS NOT = "00"
               READ TEST-FILE
                   AT END
                      DISPLAY "READ END";
                   NOT AT END
                      DISPLAY IN-FILE-REC
               END-READ
           END-PERFORM.
           CLOSE TEST-FILE.
       STOP RUN.
