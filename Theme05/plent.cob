       *>----------------------------------------------------------------------------
       *>課題２プリント出力
       *>----------------------------------------------------------------------------
       *>見出し部
       *>----------------------------------------------------------------------------
       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                        PRENT_SAMPLE01.
       *>----------------------------------------------------------------------------
       *>環境部
       *>----------------------------------------------------------------------------
       ENVIRONMENT                        DIVISION.
       INPUT-OUTPUT                       SECTION.
       FILE-CONTROL.
       *>----------------------------------------------------------------------------
       *>[入力]商品マスタファイル
       *>----------------------------------------------------------------------------
       SELECT    IN01-SHOHIN-MASTER    ASSIGN       TO   "IN01.txt"
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       STATUS IN-FILE-STATUS.
       *>----------------------------------------------------------------------------
       *>[出力]商品マスタファイル
       *>----------------------------------------------------------------------------
       SELECT    OT01-SHOHIN-MASTER    ASSIGN       TO   "OT01.txt"
                                       ORGANIZATION IS LINE SEQUENTIAL.
       *>----------------------------------------------------------------------------
       *>データ部
       *>----------------------------------------------------------------------------
       DATA                               DIVISION.
       FILE                               SECTION.
       *>----------------------------------------------------------------------------
       *>[入力]商品マスタファイルのレイアウト定義
       *>----------------------------------------------------------------------------
       FD   IN01-SHOHIN-MASTER.
       01   IN01-RECODE.
            03   IN01-SHOHIN-ZYOHOU.
                 05   IN01-SHOHIN-CODE        PIC X(002).
                 05   IN01-SHOHIN-NO          PIC 9(004).
            03   IN01-SHOHIN-MEI              PIC X(010).
            03   IN01-ZAIKO-SU                PIC 9(004).
       *>----------------------------------------------------------------------------
       *>[出力]商品マスタファイルのレイアウト定義
       *>----------------------------------------------------------------------------
       FD   OT01-SHOHIN-MASTER.
       01   OT01-RECODE.
            03   OT01-SHOHIN-ZYOHOU.
                 05   OT01-SHOHIN-CODE        PIC X(002).
                 05   OT01-SHOHIN-NO          PIC 9(004).
            03   OT01-SHOHIN-MEI              PIC X(010).
            03   OT01-ZAIKO-SU                PIC 9(004).
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
            03   WRK-AT-END                   PIC X(004).
            *>--書き込み件数をカウントする変数--
            03   WRK-OUT-COUNT                PIC 9(006).
       *>
       *>商品マスタファイルマッチング領域
       01   KY01-SHOHIN-M.
            03   KY01-STATUS                  PIC 9(001).
            03   KY01-SHOHIN-CODE             PIC X(002).
            03   KY01-SHOHIN-NO               PIC 9(004).
            03   KY01-SHOHIN-MEI              PIC X(010).
            03   KY01-ZAIKO-SU                PIC 9(004).
       *>
       *>--処理が終了したときに終了したことを証明するメッセージを表記する--
       01   MS1-MESSAGE-AREA.
            03   FILLER                       PIC X(040)
                          VALUE "PRENT_SAMPLE01の出力結果".
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
           PERFORM   MAIN-PROC  UNTIL   WRK-AT-END   =   CST-END.
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
           MOVE   SPACE      TO   WRK-AT-END.
       *>
           MOVE   ZERO       TO   WRK-OUT-COUNT.
       *>
       *>  マッチングキーの初期化（ステータス）
           MOVE   ZERO       TO KY01-STATUS.
       *>
       *>ファイルのオープン
           OPEN   INPUT    IN01-SHOHIN-MASTER
                  OUTPUT   OT01-SHOHIN-MASTER.
       *>
       *>商品マスタファイルの読み込み
           PERFORM SHOHIN-MASTER-READ-PROC.
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
           CLOSE   IN01-SHOHIN-MASTER
                   OT01-SHOHIN-MASTER.
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
       PERFORM   UNTIL   KY01-SHOHIN-CODE =   ""
       PERFORM SHOHIN-MASTER-READ-PROC
       END-PERFORM.
       MAIN-PROC-EXIT.
       *>----------------------------------------------------------------------------
       *>書き込み処理
       *>----------------------------------------------------------------------------
       WRITE-PROC                         SECTION.
       *>
       *>
           MOVE   IN01-SHOHIN-CODE   TO   OT01-SHOHIN-CODE.
           MOVE   IN01-SHOHIN-NO     TO   OT01-SHOHIN-NO.
           MOVE   IN01-SHOHIN-MEI    TO   OT01-SHOHIN-MEI.
           MOVE   IN01-ZAIKO-SU      TO   OT01-ZAIKO-SU.
       *>
           WRITE OT01-RECODE.
       *>
           ADD   1   TO   WRK-OUT-COUNT.
       *>
       WRITE-PROC-EXIT.
       *>
           EXIT.
       *>----------------------------------------------------------------------------
       *>商品マスタファイルの読み込み
       *>----------------------------------------------------------------------------
       SHOHIN-MASTER-READ-PROC SECTION.
       *>
       PERFORM   UNTIL   IN-FILE-STATUS NOT =   "00"
           READ   IN01-SHOHIN-MASTER
             AT   END
                  MOVE   "END"   TO   WRK-AT-END
                  MOVE   9   TO   KY01-STATUS
       *>
            NOT   AT   END
                MOVE   IN01-SHOHIN-CODE   TO   KY01-SHOHIN-CODE
                *>----MOVE   IN01-SHOHIN-NO     TO   KY01-SHOHIN-NO
                *>----MOVE   IN01-SHOHIN-MEI    TO   KY01-SHOHIN-MEI
                *>----MOVE   IN01-ZAIKO-SU      TO   KY01-ZAIKO-SU
                DISPLAY IN01-RECODE
            END-READ
       END-PERFORM.
       *>
       SHOHIN-MASTER-READ-PROC-EXIT.
       *>
           EXIT.
       
