       *>*********************************************************
       *>課題１のロジックテスト用
       *>*********************************************************
       *>見出し部
       *>*********************************************************
       IDENTIFICATION                DIVISION.
       PROGRAM-ID.                   TEST0001.
       *>*********************************************************
       *>環境部
       *>*********************************************************
       ENVIRONMENT                   DIVISION.
       CONFIGURATION                 SECTION.
       INPUT-OUTPUT                  SECTION.
       FILE-CONTROL.
       *>*********************************************************
       *>[入力]売上明細ファイル
       *>*********************************************************
       SELECT IN1-URIAGE-MEISAI ASSIGN        TO "ID01.txt"
                                 ORGANIZATION IS LINE SEQUENTIAL.
       *>*********************************************************
       *>[入力]商品マスタファイル
       *>*********************************************************
       SELECT IN2-SHOUHIN-MASTER ASSIGN       TO "ID02.txt"
                                 ORGANIZATION IS LINE SEQUENTIAL.
       *>*********************************************************
       *>[出力]売上詳細ファイル
       *>*********************************************************
       SELECT OUT-URIAGE-SHOUSAI ASSIGN       TO "OD01.txt"
                                 ORGANIZATION IS LINE SEQUENTIAL.
       *>*********************************************************
       *>データ部
       *>*********************************************************
       DATA                          DIVISION.
       FILE                          SECTION.
       *>*********************************************************
       *>売上明細ファイルのレイアウト定義
       *>*********************************************************
       FD IN1-URIAGE-MEISAI.
       01 IN1-RECODE.
          03 IN1-SHOHIN.
              05 IN1-SHOHIN-CODE     PIC 9(005).
              05 IN1-SHURUI          PIC 9(003).
          03 IN1-SUURYO-TOTAL        PIC 9(003).
          03 IN1-KAKAKU              PIC 9(005).
          03 FILLER                  PIC X(024).
       *>*********************************************************
       *>商品マスタファイルのレイアウト定義
       *>*********************************************************
       FD IN2-SHOUHIN-MASTER.
       01 IN2-RECODE.
          03 IN2-SHOHIN-CODE         PIC 9(005).
          03 IN2-SHURUI              PIC 9(003).
          03 IN2-SHOHIN-MEI          PIC X(040).
          03 FILLER                  PIC X(012).
       *>*********************************************************
       *>売上詳細ファイルのレイアウト定義
       *>*********************************************************
       FD OUT-URIAGE-SHOUSAI.
       01 OUT-RECODE.
          03 OUT-SHOHIN-CODE         PIC 9(005).
          03 OUT-SHURUI              PIC 9(003).
          03 OUT-SHOHIN-MEI          PIC X(040).
          03 OUT-SUURYO-TOTAL        PIC 9(003).
          03 OUT-KAKAKU              PIC 9(005).
          03 OUT-KINGAKU-TOTAL       PIC 9(008).
       *>*********************************************************
       *>作業領域の定義
       *>*********************************************************
       WORKING-STORAGE               SECTION.
       *>
       77 CST-END                    PIC X(004) VALUE "END ".
       *>
       01 WRK-WOEK-AREA.
           03 WRK-AT-END             PIC X(004).
           03 WRK-OUT-COUNT          PIC 9(004).
           03 WRK-SHOUHIN-MEI        PIC X(040).
       *>
       *>売上明細ファイルマッチングキー領域
       01 KY1-URI-MEISAI.
           03 KY1-STATUS             PIC 9(001).
           03 KY1-SHOHIN-CODE        PIC 9(005).
           03 KY1-SHURUI             PIC 9(003).
       *>
       *>商品マスタファイルマッチングキー領域
       01 KY2-SHOUHIN-M.
           03 KY2-STATUS             PIC 9(001).
           03 KY2-SHOHIN-CODE        PIC 9(005).
           03 KY2-SHURUI             PIC 9(003).
       *>
       01 MS1-MESSAGE-AREA.
           03 FILLER                 PIC X(026)
                       VALUE "EXPGM003の処理結果".
       *>
       01 MS2-MESSAGE-AREA.
           03 FILLER                 PIC X(028)
                        VALUE "出力ファイル件数：".
           03 MSG2-COUNT             PIC ZZZ,ZZ9.
       *>*********************************************************
       *>手続き部
       *>*********************************************************
       PROCEDURE                     DIVISION.
       *>
       PERFORM   INIT-PROC.
       *>
       PERFORM   MAIN-PROC   UNTIL   WRK-AT-END = CST-END.
       *>
       PERFORM   TERM-PROC.
       *>
       STOP RUN.
       *>*********************************************************
       *>初期処理
       *>*********************************************************
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
           OPEN   INPUT    IN1-URIAGE-MEISAI
                           IN2-SHOUHIN-MASTER
                  OUTPUT   OUT-URIAGE-SHOUSAI.
       *>
       *>売上明細ファイルの読み込み
           PERFORM    URIAGE-MEISAI-READ-PROC.
       *>
       *>商品マスタファイルの読み込み
           PERFORM    SHOUHIN-MASTER-READ-PROC.
       *>
       INIT-PROC-EXIT.
       *>
           EXIT.
       *>*********************************************************
       *>終了処理
       *>*********************************************************
       TERM-PROC                     SECTION.
       *>
       *>ファイルのクローズ
           CLOSE   IN1-URIAGE-MEISAI
                   IN2-SHOUHIN-MASTER
                   OUT-URIAGE-SHOUSAI.
       *>
       *>入出力件数の表示 （ここがエラー表示に変わる）
           MOVE   WRK-OUT-COUNT   TO   MSG2-COUNT.
       *>
           DISPLAY   MS1-MESSAGE-AREA   UPON   CONSOLE.
           DISPLAY   MS2-MESSAGE-AREA   UPON   CONSOLE.
       *>
       TERM-PROC-EXIT.
       *>
           EXIT.
       *>*********************************************************
       *>主処理
       *>*********************************************************
       MAIN-PROC                     SECTION.
       *>
       *>  キーが一致
           IF KY1-URI-MEISAI = KY2-SHOUHIN-M THEN
       *>
       *>      商品名を一時（退避）領域へ待避
               MOVE   IN2-SHOHIN-MEI   TO   WRK-SHOUHIN-MEI
       *>
       *>      売上詳細ファイルの編集と出力
               PERFORM   URIAGE-SHOUSAI-WRITE-PROC
       *>
       *>      売上明細の読み込み
               PERFORM   URIAGE-MEISAI-READ-PROC
       *>
       *>      売上明細の読み込み
               PERFORM   SHOUHIN-MASTER-READ-PROC
       *>
       *>  売上明細ファイルだけの場合
           ELSE   IF   KY1-URI-MEISAI < KY2-SHOUHIN-M THEN
       *>
       *>      一時（退避）領域をスペースでクリア
               MOVE   SPACE   TO   WRK-SHOUHIN-MEI
       *>
       *>      売上詳細ファイルの編集と出力
               PERFORM   URIAGE-SHOUSAI-WRITE-PROC
       *>
       *>      売上明細ファイルの読み込み
               PERFORM   URIAGE-MEISAI-READ-PROC
       *>
       *>  商品マスタファイルだけの場合
           ELSE   IF   KY1-URI-MEISAI > KY2-SHOUHIN-M   THEN
       *>
       *>      商品マスタファイルの読み込み
               PERFORM SHOUHIN-MASTER-READ-PROC
       *>
           END-IF.
       *>
       MAIN-PROC-EXIT.
       *>
           EXIT.
       *>*********************************************************
       *>売上詳細ファイルの編集・書き込み処理
       *>*********************************************************
       URIAGE-SHOUSAI-WRITE-PROC     SECTION.
       *>
           MOVE   IN1-SHOHIN-CODE     TO   OUT-SHOHIN-CODE.
           MOVE   IN1-SHURUI          TO   OUT-SHURUI.
       *>
       *>  商品名は一時領域の内容セット
           MOVE   WRK-SHOUHIN-MEI     TO   OUT-SHOHIN-MEI.
       *>
           MOVE   IN1-SUURYO-TOTAL    TO   OUT-SUURYO-TOTAL.
           MOVE   IN1-KAKAKU          TO   OUT-KAKAKU.
       *>
       *>  金額の計算
           COMPUTE   OUT-KINGAKU-TOTAL = IN1-SUURYO-TOTAL * IN1-KAKAKU.
       *>
       *>  出力ファイルへ書き込む
           WRITE OUT-RECODE.
       *>
       *>  書き込み件数のカウント
           ADD   1                    TO   WRK-OUT-COUNT.
       *>
       URIAGE-SHOUSAI-WRITE-PROC-EXIT.
       *>
           EXIT.
       *>*********************************************************
       *>売上明細ファイルの読み込み
       *>*********************************************************
       URIAGE-MEISAI-READ-PROC       SECTION.
       *>
           READ IN1-URIAGE-MEISAI
               AT    END
                     MOVE   "END"              TO WRK-AT-END
                     MOVE   9                  TO KY1-STATUS
       *>
               NOT   AT     END
                     MOVE   IN1-SHOHIN-CODE   TO   KY1-SHOHIN-CODE
                     MOVE   IN1-SHURUI         TO   KY1-SHURUI
       *>
           END-READ.
       *>
       URIAGE-MEISAI-READ-PROC-EXIT.
       *>
           EXIT.
       *>*********************************************************
       *>商品マスタファイルの読み込み
       *>*********************************************************
       SHOUHIN-MASTER-READ-PROC      SECTION.
       *>
           READ IN2-SHOUHIN-MASTER
               AT    END
                     MOVE   9                  TO   KY2-STATUS
       *>
               NOT   AT     END
                     MOVE   IN2-SHOHIN-CODE    TO   KY2-SHOHIN-CODE
                     MOVE   IN2-SHURUI         TO   KY2-SHURUI
       *>
           END-READ.
       *>
       SHOUHIN-MASTER-READ-PROC-EXIT.
       *>
           EXIT.
