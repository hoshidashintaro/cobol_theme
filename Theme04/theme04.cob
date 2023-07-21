       *>----------------------------------------------------------------------------
       *>課題３ 条件一致のみ出力
       *>----------------------------------------------------------------------------
       *>見出し部
       *>----------------------------------------------------------------------------
       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                        SAMPLE03.
       *>----------------------------------------------------------------------------
       *>環境部
       *>----------------------------------------------------------------------------
       ENVIRONMENT                        DIVISION.
       INPUT-OUTPUT                       SECTION.
       FILE-CONTROL.
       *>----------------------------------------------------------------------------
       *>入力ファイル
       *>----------------------------------------------------------------------------
       SELECT    IN01-ZYUTYU-FILE   ASSIGN       TO   "IN01.txt"
                                    ORGANIZATION IS LINE SEQUENTIAL
                                    STATUS IN-FILE-STATUS.
       *>----------------------------------------------------------------------------
       *>[出力]受注ファイル店番一致分のみ
       *>----------------------------------------------------------------------------
       SELECT    OT01-ZYUTYU-FILE   ASSIGN       TO   "OT01.txt"
                                    ORGANIZATION IS LINE SEQUENTIAL.
       *>----------------------------------------------------------------------------
       *>データ部
       *>----------------------------------------------------------------------------
       DATA                               DIVISION.
       FILE                               SECTION.
       *>----------------------------------------------------------------------------
       *>入力ファイルのレイアウト定義
       *>----------------------------------------------------------------------------
       FD   IN01-ZYUTYU-FILE.
       01   IN01-RECODE.
            03   IN01-MISEBAN            PIC X(003).
            03   IN01-TYUMON-BANGOU      PIC 9(005).
       *>----------------------------------------------------------------------------
       *>出力ファイルのレイアウト定義
       *>----------------------------------------------------------------------------
       FD   OT01-ZYUTYU-FILE.
       01   OT01-RECODE.
            03   OT01-MISEBAN            PIC X(003).
            03   OT01-TYUMON-BANGOU      PIC 9(005).
       *>----------------------------------------------------------------------------
       *>作業領域の定義
       *>----------------------------------------------------------------------------
       WORKING-STORAGE                    SECTION.
       *>
       *>--手続き部で「MAIN-PROC」を終了させる際の定数となる--
       77   CST-END                           PIC X(004) VALUE "END ".
       *>
       *>
       01   WRK-WORK-AREA.
            *>--手続き部で「MAIN-PROC」を終了させる際の変数となる--
            *>03   WRK-AT-END                   PIC X(004).
            *>--書き込み件数をカウントする変数--
            03   WRK-OUT-COUNT                PIC 9(006).
            03   WRK-MISEBAN                  PIC X(003).
       *>
       *>商品マスタファイルマッチング領域
       01   KY01-SHOHIN-M.
            03   KY01-STATUS                  PIC 9(001).
            03   KY01-MISEBAN                 PIC X(003).
       *>
       *>--処理が終了したときに終了したことを証明するメッセージを表記する--
       01   MS1-MESSAGE-AREA.
            03   FILLER                       PIC X(040)
                          VALUE "SAMPLE03の出力結果".
       *>
       *>--処理が終了した際に出力件数を表示する--
       01   MS2-MESSAGE-AREA.
            03   FILLER                       PIC X(030)
                                 VALUE "出力ファイル件数：".
            *>--ZZZ,ZZ9：整数部3けた（ゼロサプレス）＋カンマ＋整数部3けた（ゼロサプレス）--
            *>--ゼロサプレス：数値としての本来の表示に直す時に使う--
            03   MSG2-COUNT                   PIC ZZZ,ZZ9.
       01   IN-FILE-STATUS PIC XX.
       *>----------------------------------------------------------------------------
       *>手続き部
       *>----------------------------------------------------------------------------
       PROCEDURE                         DIVISION.
       *>
           PERFORM   INIT-PROC.
       *>
           *>PERFORM   MAIN-PROC  UNTIL   WRK-AT-END   =   CST-END.
       *>
           PERFORM   TERM-PROC.
       *>
           STOP RUN.
       *>----------------------------------------------------------------------------
       *>初期処理
       *>----------------------------------------------------------------------------
       INIT-PROC                         SECTION.
       *>
       *>  作業領域の初期化
           *>MOVE   SPACE      TO   WRK-AT-END.
       *>
           MOVE   ZERO       TO   WRK-OUT-COUNT.
       *>
       *>  マッチングキーの初期化（ステータス）
           MOVE   ZERO       TO KY01-STATUS.
       *>
       *>ファイルのオープン
           OPEN   INPUT    IN01-ZYUTYU-FILE
                  OUTPUT   OT01-ZYUTYU-FILE.
       *>
       *>受注ファイルの読み込み
           PERFORM ZYUTYU-FILE-READ-PROC.
       *>
       INIT-PROC-EXIT.
       *>
           EXIT.
       *>----------------------------------------------------------------------------
       *>終了処理
       *>----------------------------------------------------------------------------
       TERM-PROC                         SECTION.
       *>
       *>ファイルのクローズ
           CLOSE   IN01-ZYUTYU-FILE
                   OT01-ZYUTYU-FILE.
       *>
       *>入出力件数の表示
           MOVE   WRK-OUT-COUNT TO MSG2-COUNT.
       *>
           DISPLAY   MS1-MESSAGE-AREA UPON CONSOLE.
           DISPLAY   MS2-MESSAGE-AREA UPON CONSOLE.
       *>
       TERM-PROC-EXIT.
       *>
           EXIT.
       *>----------------------------------------------------------------------------
       *>主処理
       *>----------------------------------------------------------------------------
       MAIN-PROC                          SECTION.
       *>
           IF   KY01-MISEBAN  =  "T01"   THEN
       *>
               PERFORM   WRITE-PROC
       *>
               PERFORM   ZYUTYU-FILE-READ-PROC
       *>
           ELSE   IF   KY01-MISEBAN NOT =  "T01"   THEN
       *>
               PERFORM   ZYUTYU-FILE-READ-PROC
       *>
           END-IF.
       *>
       MAIN-PROC-EXIT.
       *>
           EXIT.
       *>----------------------------------------------------------------------------
       *>書き込み処理
       *>----------------------------------------------------------------------------
       WRITE-PROC                         SECTION.
       *>
       *>
           MOVE    IN01-MISEBAN   TO   OT01-MISEBAN.
       *>
           WRITE   OT01-RECODE.
       *>
           ADD   1                TO   WRK-OUT-COUNT.
       *>
       WRITE-PROC-EXIT.
       *>
           EXIT.
       *>----------------------------------------------------------------------------
       *>受注ファイルの読み込み
       *>----------------------------------------------------------------------------
       ZYUTYU-FILE-READ-PROC SECTION.
       *>
           READ   IN01-ZYUTYU-FILE
             AT   END
                  *>MOVE   "END"   TO   WRK-AT-END
                  MOVE   9   TO   KY01-STATUS
       *>
            NOT   AT   END
                MOVE   IN01-MISEBAN   TO   KY01-MISEBAN
            END-READ.
       *>
       ZYUTYU-FILE-READ-PROC-EXIT.
       *>
           EXIT.
