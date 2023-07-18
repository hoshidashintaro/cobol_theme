       *>************************************************************************
       *>課題１ マッチング処理のテストプログラム
       *>************************************************************************
       *>見出し部
       *>************************************************************************
       IDENTIFICATION                DIVISION.
       PROGRAM-ID.                   TEST0001.
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
       SELECT   IN01-ZYUTYU-FILE     ASSIGN        TO "IN01.txt"
                                     ORGANIZATION IS LINE SEQUENTIAL.
       *>************************************************************************
       *>[入力]商品マスタファイル
       *>************************************************************************
       SELECT   IN02-SHOHIN-MASTER   ASSIGN       TO "IN02.txt"
                                     ORGANIZATION IS LINE SEQUENTIAL.
       *>************************************************************************
       *>[出力]受注ファイル
       *>************************************************************************
       SELECT   OT01-ZYUTYU-FILE   ASSIGN       TO "OT01.txt"
                                   ORGANIZATION IS LINE SEQUENTIAL.
       *>************************************************************************
       *>データ部
       *>************************************************************************
       DATA                          DIVISION.
       FILE                          SECTION.
       *>************************************************************************
       *>[入力]受注ファイルのレイアウト定義
       *>************************************************************************
       FD   IN01-ZYUTYU-FILE.
       01   IN01-RECODE.
          03   IN01-ZYUTYU-BANGOU.
                05   IN01-MISEBAN                   PIC X(003).
                05   IN01-TYUMON-BANGOU             PIC 9(005).
          03   IN01-SHOHIN-ZYOHOU.
                05   IN01-SHOHIN-CODE.
                      07   IN01-BUNRUI-CODE         PIC X(002).
                      07   IN01-SHOHIN-NO           PIC 9(004).
          03   IN01-TYUMON-ZYOHOU.
                05   IN01-TYUMON-SU                 PIC 9(003).
                05   IN01-UKETUKE-NICHIZI.
                      07   IN01-HIDUKE              PIC 9(006).
                      07   IN01-ZIKAN               PIC 9(004).
                05   IN01-TANTOSYA-CODE.
                      07   IN01-BUSHO-CODE          PIC X(003).
                      07   IN01-TANTOSYA-BANGOU     PIC 9(004).
       *>************************************************************************
       *>商品マスタファイルのレイアウト定義
       *>************************************************************************
       FD   IN02-SHOHIN-MASTER.
       01   IN02-RECODE.
          03   IN02-SHOHIN-CODE.
                05 IN02-BUNRUI-CODE                 PIC X(002).
                05 IN02-SHOHIN-NO                   PIC 9(004).
          03   IN02-SHOHIN-MI                       PIC X(020).
          03   IN02-ZAIKO-SU                        PIC 9(004).
       *>************************************************************************
       *>[出力]受注ファイルのレイアウト定義
       *>************************************************************************
       FD   OT01-ZYUTYU-FILE.
       01   OT01-RECODE.
          03   OT01-ZYUTYU-BANGOU.
                05   OT01-MISEBAN                   PIC X(003).
                05   OT01-TYUMON-BANGOU             PIC 9(005).
          03   OT01-SHOHIN-ZYOHOU.
                05   OT01-SHOHIN-CODE.
                      07   OT01-BUNRUI-CODE         PIC X(002).
                      07   OT01-SHOHIN-NO           PIC 9(004).
                05   OT02-SHOHIN-MI                 PIC X(020).
          03   OT01-TYUMON-ZYOHOU.
                05   OT01-TYUMON-SU                 PIC 9(003).
                05   OT01-UKETUKE-NICHIZI.
                      07   OT01-HIDUKE              PIC 9(006).
                      07   OT01-ZIKAN               PIC 9(004).
                05   OT01-TANTOSYA-CODE.
                      07   OT01-BUSHO-CODE          PIC X(003).
                      07   OT01-TANTOSYA-BANGOU     PIC 9(004).
       *>************************************************************************
       *>作業領域の定義
       *>************************************************************************
       WORKING-STORAGE               SECTION.
       *>
       77   CST-END                     PIC X(004) VALUE "END".
       *>
       01   WRK-WOEK-AREA.
             03   WRK-AT-END                        PIC X(004).
             03   WRK-OUT-COUNT                     PIC 9(006).
             03   WRK-SHOHIN-MEI                    PIC X(020).
             03   WRK-BUNRUI-CODE                   PIC X(002).
             03   WRK-SHOHIN-NO                     PIC 9(004).
       *>
       01   MSG-TAIHI-AREA.
             03   MSG-SHOUHIN                       PIC X(015).
             03   MSG-GAITONASHI                    PIC X(028).

       *>
       *>[入力]受注ファイルマッチングキー領域
       01   KY1-ZYUTYU-FILE.
             03   KY1-STATUS                        PIC 9(001).
             03   KY1-BUNRUI-CODE                   PIC X(002).
             03   KY1-SHOHIN-NO                     PIC 9(004).
             03   KY1-TYUMON-SU                     PIC 9(003).
       *>
       *>商品マスタファイルマッチングキー領域
       01   KY2-SHOHIN-M.
           03   KY2-STATUS                          PIC 9(001).
           03   KY2-BUNRUI-CODE                     PIC X(002).
           03   KY2-SHOHIN-NO                       PIC 9(004).
           03   KY2-ZAIKO-SU                        PIC 9(004).
       *>
       01   MS1-MESSAGE-AREA.
           03   FILLER                              PIC X(030)
                                              VALUE "処理が終了".
       *>
       01   MS2-MESSAGE-AREA.
           03   MSG2-MESSAGE-ERROR                  PIC X(050).
       *>
       *>01   MS1-MESSAGE-AREA.
       *>    03   SAISYU-MESSAGE PIC X(060).
       *>
       *>************************************************************************
       *>手続き部
       *>************************************************************************
       PROCEDURE                     DIVISION.
       *>
       PERFORM   INIT-PROC.
       *>
       PERFORM   MAIN-PROC   UNTIL   WRK-AT-END = CST-END.
       *>
       PERFORM   TERM-PROC.
       *>
       STOP RUN.
       *>************************************************************************
       *>初期処理
       *>************************************************************************
       INIT-PROC                     SECTION.
       *>
       *>作業領域の初期化
           MOVE   SPACE   TO   WRK-AT-END.
       *>
           MOVE   ZERO    TO   WRK-OUT-COUNT.
       *>
       *>マッチングキーの初期化（ステータス）
           MOVE   ZERO    TO   KY1-STATUS
                               KY2-STATUS.
       *>
       *>ファイルのオープン
           OPEN   INPUT    IN01-ZYUTYU-FILE
                           IN02-SHOHIN-MASTER
                  OUTPUT   OT01-ZYUTYU-FILE.
       *>
       *>[入力]受注ファイルの読み込み
           PERFORM    ZYUTYU-FILE-IN01-READ-PROC.
       *>
       *>商品マスタファイルの読み込み
           PERFORM    SHOHIN-MASTER-READ-PROC.
       *>
       INIT-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>終了処理
       *>************************************************************************
       TERM-PROC                     SECTION.
       *>
       *>ファイルのクローズ
           CLOSE   IN01-ZYUTYU-FILE
                   IN02-SHOHIN-MASTER
                   OT01-ZYUTYU-FILE.
       *>
       *>入出力件数の表示
       *>
           DISPLAY   MS1-MESSAGE-AREA   UPON   CONSOLE.

       *>
       TERM-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>主処理
       *>************************************************************************
       MAIN-PROC                     SECTION.
       *>
       *>  キーが一致
           IF    KY1-BUNRUI-CODE    =   KY2-BUNRUI-CODE
           AND   KY1-SHOHIN-NO      =   KY2-SHOHIN-NO
           AND   KY1-TYUMON-SU     <=   KY2-ZAIKO-SU  THEN
       *>
       *>      商品名を一時（退避）領域へ待避
               MOVE   IN02-SHOHIN-MI   TO   WRK-SHOHIN-MEI
       *>
       *>      [出力]受注ファイルの編集と出力
               PERFORM   ZYUTYU-FILE-0UT01-WRITE-PROC
       *>
       *>      [入力]受注ファイルの読み込み
               PERFORM   ZYUTYU-FILE-IN01-READ-PROC
       *>
       *>      商品マスタの読み込み
               PERFORM   SHOHIN-MASTER-READ-PROC
       *>
       *>  [入力]受注ファイルの注文数が商品マスタよりも多い場合
           ELSE   IF   KY1-BUNRUI-CODE   =   KY2-BUNRUI-CODE
           AND         KY1-SHOHIN-NO     =   KY2-SHOHIN-NO
           AND         KY1-TYUMON-SU     >   KY2-ZAIKO-SU THEN
       *>
       *>      一時（退避）領域をスペースでクリア
               MOVE   KY1-BUNRUI-CODE   TO   WRK-BUNRUI-CODE
               MOVE   KY1-SHOHIN-NO     TO   WRK-SHOHIN-NO
               MOVE   "商品コード"       TO   MSG-SHOUHIN
               MOVE   " 在庫不足"        TO   MSG-GAITONASHI
       *>
               STRING
                  MSG-SHOUHIN        DELIMITED   BY   SIZE
                  WRK-BUNRUI-CODE    DELIMITED   BY   SIZE
                  WRK-SHOHIN-NO      DELIMITED   BY   SIZE
                  MSG-GAITONASHI     DELIMITED   BY   SIZE
                  INTO MSG2-MESSAGE-ERROR
               END-STRING
       *>
                 DISPLAY   MSG2-MESSAGE-ERROR     UPON   CONSOLE
       *>
       *>      [入力]受注ファイルの編集と出力
               *>*---PERFORM   ZYUTYU-FILE-0UT01-WRITE-PROC
       *>
       *>      [入力]受注ファイルの読み込み
               PERFORM   ZYUTYU-FILE-IN01-READ-PROC
       *>
       *>      商品マスタの読み込み
               PERFORM   SHOHIN-MASTER-READ-PROC
       *>
       *>  [入力]受注ファイルと商品マスタの商品マッチしないとき
           ELSE IF KY1-BUNRUI-CODE  NOT  =   KY2-BUNRUI-CODE
           OR      KY1-SHOHIN-NO    NOT  =   KY2-SHOHIN-NO     THEN
           *>DISPLAY"マッチしない"
       *>
       *>      一時（退避）領域をスペースでクリア
               MOVE   KY1-BUNRUI-CODE   TO   WRK-BUNRUI-CODE
               MOVE   KY1-SHOHIN-NO     TO   WRK-SHOHIN-NO
               MOVE   "商品コード"       TO   MSG-SHOUHIN
               MOVE   " 該当マスタなし"   TO   MSG-GAITONASHI
               *>
               STRING
                  MSG-SHOUHIN        DELIMITED   BY   SIZE
                  WRK-BUNRUI-CODE    DELIMITED   BY   SIZE
                  WRK-SHOHIN-NO      DELIMITED   BY   SIZE
                  MSG-GAITONASHI     DELIMITED   BY   SIZE
                  INTO MSG2-MESSAGE-ERROR
               END-STRING
               *>
                 DISPLAY   MSG2-MESSAGE-ERROR     UPON   CONSOLE
       *>
       *>      [入力]受注ファイルの読み込み
               PERFORM   ZYUTYU-FILE-IN01-READ-PROC
       *>      商品マスタの読み込み
               PERFORM   SHOHIN-MASTER-READ-PROC
       *>
       *>  [入力]受注ファイルだけの場合
           ELSE   IF   KY1-ZYUTYU-FILE < KY2-SHOHIN-M   THEN
       *>
       *>      商品マスタファイルの読み込み
               PERFORM ZYUTYU-FILE-IN01-READ-PROC
               *>PERFORM SHOHIN-MASTER-READ-PROC
       *>
       *>  商品マスタファイルだけの場合
           ELSE   IF   KY1-ZYUTYU-FILE > KY2-SHOHIN-M   THEN
       *>
               *>PERFORM ZYUTYU-FILE-IN01-READ-PROC
       *>      商品マスタファイルの読み込み
               PERFORM SHOHIN-MASTER-READ-PROC
       *>
           END-IF.
       *>
       MAIN-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>[出力]受注ファイルの編集・書き込み処理
       *>************************************************************************
       ZYUTYU-FILE-0UT01-WRITE-PROC     SECTION.
       *>
       *>  受注番号の書き込み
           MOVE   IN01-MISEBAN           TO   OT01-MISEBAN.
           MOVE   IN01-TYUMON-BANGOU     TO   OT01-TYUMON-BANGOU.
       *>
       *>  商品情報の書き込み
           MOVE   IN01-BUNRUI-CODE       TO   OT01-SHOHIN-CODE.
           MOVE   IN01-SHOHIN-NO         TO   OT01-SHOHIN-NO.
       *>
       *>  注文情報の書き込み
           MOVE   IN01-TYUMON-SU         TO   OT01-TYUMON-SU.
           MOVE   IN01-HIDUKE            TO   OT01-HIDUKE.
           MOVE   IN01-ZIKAN             TO   OT01-ZIKAN.
           MOVE   IN01-BUSHO-CODE        TO   OT01-BUSHO-CODE.
           MOVE   IN01-TANTOSYA-BANGOU   TO   OT01-TANTOSYA-BANGOU.
       *>
       *>  商品名は一時領域の内容セット
           MOVE   WRK-SHOHIN-MEI         TO   OT02-SHOHIN-MI.
       *>
       *>  出力ファイルへ書き込む
           WRITE OT01-RECODE.
       *>
       *>  書き込み件数のカウント
           ADD   1                       TO   WRK-OUT-COUNT.
       *>
       ZYUTYU-FILE-0UT01-WRITE-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>[入力]受注ファイルの読み込み
       *>************************************************************************
       ZYUTYU-FILE-IN01-READ-PROC       SECTION.
       *>
           READ IN01-ZYUTYU-FILE
               AT    END
                     MOVE   "END"              TO WRK-AT-END
                     MOVE   9                  TO KY1-STATUS
       *>
               NOT   AT     END
                     MOVE   IN01-BUNRUI-CODE   TO   KY1-BUNRUI-CODE
                     MOVE   IN01-SHOHIN-NO     TO   KY1-SHOHIN-NO
                     MOVE   IN01-TYUMON-SU     TO   KY1-TYUMON-SU
       *>
           END-READ.
       *>
       ZYUTYU-FILE-IN01-READ-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>商品マスタファイルの読み込み
       *>************************************************************************
       SHOHIN-MASTER-READ-PROC      SECTION.
       *>
           READ IN02-SHOHIN-MASTER
               AT    END
                     MOVE   "END"              TO WRK-AT-END
                     MOVE   9                  TO   KY2-STATUS
       *>
               NOT   AT     END
                     MOVE   IN02-BUNRUI-CODE   TO   KY2-BUNRUI-CODE
                     MOVE   IN02-SHOHIN-NO     TO   KY2-SHOHIN-NO
                     MOVE   IN02-ZAIKO-SU      TO   KY2-ZAIKO-SU
       *>
           END-READ.
       *>
       SHOHIN-MASTER-READ-PROC-EXIT.
       *>
           EXIT.
