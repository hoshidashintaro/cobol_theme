       *>************************************************************************
       *>課題１ マッチング処理
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
                05   IN01-UKETSUKE-NICHIZI.
                      07   IN01-DATE                PIC 9(006).
                      07   IN01-TIME                PIC 9(004).
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
                05   OT01-UKETSUKE-NICHIZI.
                      07 OT01-DATE                  PIC 9(006).
                      07 OT01-TIME                  PIC 9(004).
                05   OT01-TANTOSYA-CODE.
                      07 OT01-BUSHO-CODE            PIC X(003).
                      07 OT01-TANTOSYA-BANGOU       PIC 9(004).
       *>************************************************************************
       *>作業領域の定義
       *>************************************************************************
       WORKING-STORAGE               SECTION.
       *>
       77   CST-END                    PIC X(004) VALUE "END ".
       *>
       01   WRK-WOEK-AREA.
             03   WRK-AT-END                        PIC X(004).
             03   WRK-OUT-COUNT                     PIC 9(006).
             03   WRK-SHOHIN-MEI                    PIC X(020).
             03   WRK-BUNRUI-CODE                   PIC X(002).
             03   WRK-SHOHIN-NO                     PIC 9(004).
       *>
       *>[入力]受注ファイルマッチングキー領域
       01   KY1-ZYUTYU-FILE.
             03   KY1-STATUS                        PIC 9(001).
             *>*---03   KY1-SHOHIN-CODE.
                   03   KY1-BUNRUI-CODE             PIC 9(005).
                   03   KY1-SHOHIN-NO               PIC 9(003).
       *>
       *>商品マスタファイルマッチングキー領域
       01   KY2-SHOHIN-M.
           03   KY2-STATUS                          PIC 9(001).
           *>*---03   KY2-SHOHIN-CODE.
                 03   KY2-BUNRUI-CODE               PIC 9(005).
                 03   KY2-SHOHIN-NO                 PIC 9(003).
       *>
       01   MS1-MESSAGE-AREA.
           03   FILLER                              PIC X(030)
                                              VALUE "処理の結果".
       *>
       01   MS2-MESSAGE-AREA.
           03   FILLER                              PIC X(030) VALUE "出力ファイル件数".
           03   MSG2-COUNT                          PIC ZZZ,ZZ9.
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
           MOVE   WRK-OUT-COUNT     TO   MSG2-COUNT.
       *>
           DISPLAY   MS1-MESSAGE-AREA   UPON   CONSOLE.
           DISPLAY   MS2-MESSAGE-AREA   UPON   CONSOLE.
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
           IF KY1-SHOHIN-CODE = KY2-SHOHIN-CODE THEN
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
       *>  [入力]受注ファイルだけの場合
           ELSE   IF   KY1-ZYUTYU-FILE > KY2-SHOHIN-M THEN
           *>----ELSE   IF   TYUMON-SU-IN01 > ZAIKO-SU-IN02 THEN
           *>--エラーメッセージの表示
       *>
       *>      一時（退避）領域をスペースでクリア
               MOVE   KY1-BUNRUI-CODE   TO   WRK-BUNRUI-CODE
       *>
       *>      [入力]受注ファイルの編集と出力
               *>*---PERFORM   ZYUTYU-FILE-0UT01-WRITE-PROC
       *>
       *>      [入力]受注ファイルの読み込み
               PERFORM   ZYUTYU-FILE-IN01-READ-PROC
       *>
       *>  商品マスタにない時
           *>*---ELSE IF IN01-TYUMON-SU NOT = IN02-ZAIKO-SU THEN
       *>
       *>      一時（退避）領域をスペースでクリア
               *>*---MOVE   KY1-BUNRUI-CODE   TO   WRK-BUNRUI-CODE
       *>
       *>      [入力]受注ファイルの編集と出力
               *>*---PERFORM   ZYUTYU-FILE-0UT01-WRITE-PROC
       *>
       *>      [入力]受注ファイルの読み込み
               *>*---PERFORM   ZYUTYU-FILE-IN01-READ-PROC
       *>
       *>
       *>  商品マスタファイルだけの場合
           ELSE   IF   KY1-ZYUTYU-FILE > KY2-SHOHIN-M   THEN
       *>
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
           MOVE   IN01-SHOHIN-CODE     TO   OT01-SHOHIN-CODE.
           *>*---MOVE   SHOHIN-NO-IN01          TO   SHOHIN-NO-OT01.
       *>
       *>  商品名は一時領域の内容セット
           MOVE   WRK-SHOHIN-MEI     TO   OT02-SHOHIN-MI.
       *>
       *>  出力ファイルへ書き込む
           WRITE OT01-RECODE.
       *>
       *>  書き込み件数のカウント
           ADD   1                    TO   WRK-OUT-COUNT.
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
                     MOVE   IN01-BUNRUI-CODE    TO   KY1-BUNRUI-CODE
                     MOVE   IN01-SHOHIN-NO    TO   KY1-SHOHIN-NO

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
                     MOVE   9                  TO   KY2-STATUS
       *>
               NOT   AT     END
                     MOVE   IN02-BUNRUI-CODE    TO   KY2-BUNRUI-CODE
                     MOVE   IN02-SHOHIN-NO    TO   KY2-SHOHIN-NO
       *>
           END-READ.
       *>
       SHOHIN-MASTER-READ-PROC-EXIT.
       *>
           EXIT.
