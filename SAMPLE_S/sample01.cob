       *>************************************************************************
       *>ソート処理のテストプログラム０１
       *>************************************************************************
       *>見出し部
       *>************************************************************************
       IDENTIFICATION                DIVISION.
       PROGRAM-ID.                   SAMPLE0001.
       *>************************************************************************
       *>環境部
       *>************************************************************************
       ENVIRONMENT                   DIVISION.
       CONFIGURATION                 SECTION.
       INPUT-OUTPUT                  SECTION.
       FILE-CONTROL.
       *>************************************************************************
       *>[入力]ファイル （SORT対象ファイル）
       *>************************************************************************
       SELECT   IN-HIBETSU-URIAGE     ASSIGN       TO "IN01.txt"
                                      ORGANIZATION IS LINE SEQUENTIAL.
       *>************************************************************************
       *>中間ソートファイル （作業領域）
       *>************************************************************************
       SELECT   ST-HIBETSU-URIAGE     ASSIGN       TO "ST".
       *>************************************************************************
       *>[出力]ファイル （結果出力ファイル）
       *>************************************************************************
       SELECT   OUT-URIAGE-MEISAI     ASSIGN       TO "OT01.txt"
                                      ORGANIZATION IS LINE SEQUENTIAL.
       *>************************************************************************
       *>データ部
       *>************************************************************************
       DATA                          DIVISION.
       FILE                          SECTION.
       *>************************************************************************
       *>日別売上明細ファイル
       *>************************************************************************
       FD   IN-HIBETSU-URIAGE.
       01   IN-HIBETSU-URIAGE-REC.
            03   IN-URIAGE-DATA        PIC 9(008).
            03   IN-SHOHIN.
                 05   IN-SHOHIN-CODE   PIC 9(005).
                 05   IN-SHURUI        PIC 9(003).
            03   IN-SUURYO             PIC 9(003).
            03   IN-TANKA              PIC 9(005).
            03   FILLER                PIC X(016).
       *>************************************************************************
       *>ソートファイルのレイアウト定義
       *>************************************************************************
       FD   ST-HIBETSU-URIAGE.
       01   ST-HIBETSU-URIAGE-REC.
            03   ST-URIAGE-DATA        PIC 9(008).
            03   ST-SHOHIN.
                 05   ST-SHOHIN-CODE   PIC 9(005).
                 05   ST-SHURUI        PIC 9(003).
            03   ST-SUURYO             PIC 9(003).
            03   ST-TANKA              PIC 9(005).
            03   FILLER                PIC X(016).
       *>************************************************************************
       *>[出力]ファイルのレイアウト定義
       *>************************************************************************
       FD   OUT-URIAGE-MEISAI.
       01   OUT-URIAGE-MEISAI-REC.
            03   OUT-SHOHIN.
                 05   OUT-SHOHIN-CODE   PIC 9(005).
                 05   OUT-SHURUI        PIC 9(003).
            03   OUT-SUURYO-TOTAL       PIC 9(003).
            03   OUT-KAKAKU             PIC 9(005).
            03   FILLER                 PIC X(024).
       *>************************************************************************
       *>作業領域の定義
       *>************************************************************************
       WORKING-STORAGE               SECTION.
       *>
       77   CST-END                     PIC X(004) VALUE "END".
       *>
       *>退避領域
       01   WRK-WOEK-AREA.
            03   WRK-AT-END             PIC X(004).
            03   WRK-IN-COUNT           PIC 9(006).
            03   WRK-OUT-COUNT          PIC 9(006).
            03   WRK-SUURYO-TOTAL       PIC 9(003).
            03   WRK-URIAGE-TOTAL       PIC 9(008).
            03   WRK-INCOUNT            PIC 9(006).
       *>
       *>前レコードの集計キー保存用
       01   KEY-SUMMARY.
            03   KEY-SHOHIN-CODE        PIC X(004).
            03   KEY-SHURUI             PIC 9(006).
       *>
       *>メッセージ１：SORTの処理結果の表示
       01   MS1-MESSAGE-AREA.
            03   FILLER                 PIC X(028)
                              VALUE "SORTの処理結果".
       *>
       *>メッセージ２：入力ファイル件数:の表示
       01   MS2-MESSAGE-AREA.
            03   FILLER                 PIC X(040)
                              VALUE "入力ファイル件数:".
            03   MSG2-COUNT             PIC ZZZ,ZZ9.
       *>
       *>メッセージ３：出力ファイル件数:の表示
       01   MS3-MESSAGE-AREA.
            03   FILLER                 PIC X(040)
                              VALUE "出力ファイル件数:".
            03   MSG3-COUNT             PIC ZZZ,ZZ9.
       *>************************************************************************
       *>手続き部
       *>************************************************************************
       PROCEDURE                     DIVISION.
       *>
       *>  日別売上明細ファイルのソートとソート後の集計
           SORT   ST-HIBETSU-URIAGE
                  ON   ASCENDING   KEY   ST-SHOHIN-CODE
                  ON   ASCENDING   KEY   ST-SHURUI
               INPUT   PROCEDURE   RELEASE-CONTROL-PROC
               OUTPUT  PROCEDURE   RETURN-CONTROL-PROC.
       *>
           PERFORM   TERM-PROC.
       *>
       STOP RUN.
       *>************************************************************************
       *>終了処理
       *>************************************************************************
       TERM-PROC                     SECTION.
       *>
       *>入出力件数の表示
           MOVE   WRK-IN-COUNT    TO  MSG2-COUNT.
           MOVE   WRK-OUT-COUNT   TO  MSG3-COUNT.
       *>
           DISPLAY   MS1-MESSAGE-AREA   UPON   CONSOLE.
           DISPLAY   MS2-MESSAGE-AREA   UPON   CONSOLE.
           DISPLAY   MS3-MESSAGE-AREA   UPON   CONSOLE.
       *>
       TERM-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>ソート前主処理（ソートファイルへのデータ書き込み）
       *>************************************************************************
       RELEASE-CONTROL-PROC          SECTION.
       *>
       *>  入力ファイルのオープン
           OPEN   INPUT   IN-HIBETSU-URIAGE.
       *>
       *>  作業領域の初期
           MOVE   SPACE     TO   WRK-AT-END.
       *>
           PERFORM   RELEASE-MAIN-PROC
                                     UNTIL   WRK-AT-END = CST-END.
       *>
       *>  入力ファイルをクローズ
           CLOSE   IN-HIBETSU-URIAGE.
       *>
       RELEASE-CONTROL-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>日別売上明細ファイルの読み込みと整列併合用ファイルへの書き込み
       *>************************************************************************
       RELEASE-MAIN-PROC              SECTION.
       *>
       *>  入力ファイルの読み込み
           READ   IN-HIBETSU-URIAGE
             AT   END
                  MOVE "END"   TO   WRK-AT-END
       *>
            NOT   AT   END
       *>         整列併合用ファイルの書き込み
                  MOVE   IN-HIBETSU-URIAGE-REC
                                         TO   ST-HIBETSU-URIAGE-REC
                  RELEASE  ST-HIBETSU-URIAGE-REC
       *>
           END-READ.
       *>
       RELEASE-MAIN-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>ソート後主処理（データの集計処理制御）
       *>************************************************************************
       RETURN-CONTROL-PROC              SECTION.
       *>
       *>  出力ファイルのオープン
           OPEN   OUTPUT   OUT-URIAGE-MEISAI.
       *>
       *>  作業領域の初期化
           MOVE   SPACE   TO   WRK-AT-END.
           MOVE   ZERO    TO   WRK-IN-COUNT
                               WRK-OUT-COUNT
                               WRK-SUURYO-TOTAL
                               WRK-URIAGE-TOTAL.
       *>
       *>  データの読み込み（先読み）
           PERFORM  FILE-RETURN-PROC.
       *>
       *>  前レコードの集計保存
           MOVE   ST-SHOHIN-CODE   TO   KEY-SHOHIN-CODE.
           MOVE   ST-SHURUI        TO   KEY-SHURUI.
       *>
       *>  集計処理の呼び出し
           PERFORM  SUMMARY-MAIN-PROC
                                    UNTIL   WRK-AT-END = CST-END.
       *>
       *>  入力件数が0よりも大きければ、売上詳細ファイルの
       *>  編集・出力を行う（最終データの対策）
           IF   WRK-IN-COUNT > ZERO   THEN
                PERFORM   URIAGE-SHOUSAI-WRITE-PROC
           END-IF.
       *>
       *>  出力ファイルのクローズ
           CLOSE   OUT-URIAGE-MEISAI.
       *>
       RETURN-CONTROL-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>ソート後日別売上明細ファイルの集計
       *>************************************************************************
       SUMMARY-MAIN-PROC              SECTION.
       *>
       *>  集計キー変わりを判定
           IF   ST-SHOHIN-CODE NOT = KEY-SHOHIN-CODE OR
                ST-SHURUI      NOT = KEY-SHURUI      THEN
       *>
       *>       売上詳細ファイルの編集・出力
                PERFORM  URIAGE-SHOUSAI-WRITE-PROC
       *>
       *>       集計項目の初期化
                MOVE   ZERO             TO   WRK-SUURYO-TOTAL
                                             WRK-URIAGE-TOTAL
       *>
       *>       現レコードの集計キー保存
                MOVE   ST-SHOHIN-CODE   TO   KEY-SHOHIN-CODE
                MOVE   ST-SHURUI        TO   KEY-SHURUI
           END-IF.
       *>
       *>  数量、売上金額の集計
           ADD   ST-SUURYO              TO   WRK-SUURYO-TOTAL.
           COMPUTE   WRK-URIAGE-TOTAL = WRK-URIAGE-TOTAL
                                      + (ST-SUURYO * ST-TANKA).
       *>
       *>  ソート後売上明細ファイルの読み込み
           PERFORM   FILE-RETURN-PROC.
       *>
       SUMMARY-MAIN-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>売上詳細ファイルの編集・書き込み処理
       *>************************************************************************
       URIAGE-SHOUSAI-WRITE-PROC              SECTION.
       *>
       *>  商品コード、商品種類は対象の内容をセット
           MOVE   KEY-SHOHIN-CODE    TO   OUT-SHOHIN-CODE.
           MOVE   KEY-SHURUI         TO   OUT-SHURUI.
       *>
       *>  合計数量
           MOVE   WRK-SUURYO-TOTAL   TO   OUT-SUURYO-TOTAL.
       *>
       *>  加重平均単価の算出
           COMPUTE   OUT-KAKAKU = WRK-URIAGE-TOTAL / WRK-SUURYO-TOTAL.
       *>
       *>  出力ファイルへ書き込む
           WRITE   OUT-URIAGE-MEISAI-REC.
       *>
       *>  書き込み件数のカウント
           ADD   1                   TO  WRK-OUT-COUNT.
       *>
       URIAGE-SHOUSAI-WRITE-PROC-EXIT.
       *>
           EXIT.
       *>************************************************************************
       *>ソート後日別売上明細ファイル読み込み
       *>************************************************************************
       FILE-RETURN-PROC              SECTION.
       *>
           RETURN   ST-HIBETSU-URIAGE
              AT   END
                   MOVE   "END"   TO   WRK-AT-END
       *>
             NOT   AT   END
                   ADD   1        TO   WRK-IN-COUNT
       *>
           END-RETURN.
       *>
       FILE-RETURN-PROC-EXIT.
       *>
           EXIT.
