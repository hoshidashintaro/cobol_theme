       *>FC課題2 プリント処理のテストプログラム
       *>************************************************************************
       *>見出し部
       *>************************************************************************
       IDENTIFICATION                DIVISION.
       PROGRAM-ID.                   PLENT002.
       *>************************************************************************
       *>環境部
       *>************************************************************************
       ENVIRONMENT                   DIVISION.
       CONFIGURATION                 SECTION.
       INPUT-OUTPUT                  SECTION.
       FILE-CONTROL.
       *>************************************************************************
       *>[入力]受注ファイル
       *>************************************************************************
       SELECT   IN01-TEST-FILE     ASSIGN        TO "IN01.txt"
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   STATUS IN-FILE-STATUS.
       *>************************************************************************
       SELECT   PRT-TEST-FILE     ASSIGN        TO "PR01.txt"
                                  ORGANIZATION IS LINE SEQUENTIAL.
       *>************************************************************************
       *>データ部
       *>************************************************************************
       DATA                          DIVISION.
       FILE                          SECTION.
       *>************************************************************************
       *>IN01-FILEのレイアウト定義
       *>************************************************************************
       FD   IN01-FILE.
       01   IN01-RECODE.
          03   IN01-ZYUTYU-BANGOU.
                05   IN01-MISEBAN                   PIC X(003).
                05   IN01-TYUMON-BANDOU             PIC 9(005).
       *>************************************************************************
       *>一覧表のレイアウト定義
       *>************************************************************************
       FD   PRT-TEST-FILE.
       01   PRT-RECODE                              PIC ZZZ,ZZ9.
       *>************************************************************************
       *>作業領域の定義
       *>************************************************************************
       WORKING-STORAGE               SECTION.
       *>
       01   WRK-WOEK-AREA.
             03   WRK-OUT-COUNT                     PIC 9(006).
       *>
       *>ステータスの領域を定義を設定する
       01  IN-FILE-STATUS                           PIC XX.
       *>************************************************************************
       *>印刷用パーツ定義
       *>************************************************************************
       01   HD01-OUT-NUMBER.
             03   OUT-NUMBER                        PIC ZZZ,ZZ9.
       *>************************************************************************
       *>手続き部
       *>************************************************************************
       PROCEDURE                     DIVISION.
       *>
             PERFORM   INIT-PROC.
       *>
             PERFORM   TERM-PROC.
       *>
       STOP RUN.
       *>************************************************************************
       *>初期処理
       *>************************************************************************
       INIT-PROC                     SECTION.
       *>
       *>  ファイルのオープン
           OPEN   INPUT    IN01-FILE
                  OUTPUT   PRT-TEST-FILE.
       *>
       *>  [入力]受注ファイルの読み込み
           PERFORM    IN01-FILE-READ-AND-WRITE-PROC.
       *>
       INIT-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>終了処理
       *>************************************************************************
       TERM-PROC                     SECTION.
       *>
       *>  ファイルのクローズ
           CLOSE   IN01-TEST-FILE
                   PRT-TEST-FILE.
       *>
       *>  入力件数の表示
       *>
       *>  プログラムが終了したことを表示する
           *>DISPLAY   MS1-MESSAGE-AREA   UPON   CONSOLE.
       *>
       TERM-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>[入力]受注ファイルの読み込みと書き込み
       *>************************************************************************
       IN01-FILE-READ-AND-WRITE-PROC       SECTION.
       *>
       PERFORM UNTIL IN-FILE-STATUS NOT = "00"
           READ IN01-TEST-FILE
               AT    END
                     DISPLAY "READ END"
       *>
               NOT   AT     END
               IF IN01-RECODE = SPACE THEN
               *>DISPLAY IN01-RECODE
               ADD   ZERO   TO   WRK-OUT-COUNT
               *>
               ELSE IF IN01-RECODE >= 1 THEN
                     DISPLAY IN01-RECODE
                     ADD   1   TO   WRK-OUT-COUNT
       *>
           END-READ
       END-PERFORM.
       *>
       *>      件数の代入と印刷処理
               MOVE      WRK-OUT-COUNT        TO   OUT-NUMBER.
               *>DISPLAY"WRK-OUT-COUNT:"WRK-OUT-COUNT
               *>DISPLAY"OUT-NUMBER:"OUT-NUMBER
       *>
               WRITE     PRT-RECODE         FROM   HD01-OUT-NUMBER.
       *>
       IN01-FILE-READ-AND-WRITE-PROC-EXIT.
       *>
           EXIT.
