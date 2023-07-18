       IDENTIFICATION DIVISION.
       PROGRAM-ID. INPUT02.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT TEST-FILE ASSIGN TO 'test02.txt'
              ORGANIZATION IS LINE SEQUENTIAL
              STATUS IN-FILE-STATUS.

       DATA DIVISION.
           FILE SECTION.
               FD TEST-FILE.
                  01 IN-FILE-REC PIC X(20).
           WORKING-STORAGE SECTION.
               01 IN-FILE-STATUS PIC XX.
       PROCEDURE DIVISION.
           MAIN SECTION.
                OPEN INPUT TEST-FILE.

                PERFORM UNTIL IN-FILE-STATUS NOT = "00"

                    READ TEST-FILE

                        AT END
                            DISPLAY "READ END"

                        NOT AT END
                            DISPLAY IN-FILE-STATUS
                    END-READ
                END-PERFORM.
                CLOSE TEST-FILE.
       STOP RUN.
