       *>1.B..+....2....+....3....+....4....+....5....+....6....+....7....+....8
       *>-----------------------------------------------------------------------
       *>サマリーのプログラムサンプル01
       *>SUMMARY_SAMPLE0001
       *>-----------------------------------------------------------------------
       *>-----------------------------------------------------------------------
       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                SUMMARY_SAMPLE01.
       *>-----------------------------------------------------------------------
       *>環境部
       *>-----------------------------------------------------------------------
       ENVIRONMENT                        DIVISION.
       INPUT-OUTPUT                       SECTION.
       FILE-CONTROL.
       *>-----------------------------------------------------------------------
       *>入力ファイル
       *>-----------------------------------------------------------------------
       SELECT    IN01-ZYUTYU-FILE   ASSIGN       TO   "IN01.txt"
                                    ORGANIZATION IS LINE SEQUENTIAL.
       *>-----------------------------------------------------------------------
       *>出力ファイル
       *>-----------------------------------------------------------------------
       SELECT    OT01-ZYUTYU-FILE   ASSIGN       TO   "OT01.txt"
                                    ORGANIZATION IS LINE SEQUENTIAL.
       *>-----------------------------------------------------------------------
       *>データ部
       *>-----------------------------------------------------------------------
       DATA                               DIVISION.
       FILE                               SECTION.
       *>-----------------------------------------------------------------------
       *>IN01-ZYUTYU-FILEの定義
       *>-----------------------------------------------------------------------
       FD   IN01-ZYUTYU-FILE.
       01   IN01-ZYUTYU-FILE-REC.
            03   IN01-ZYUTYU-BANGOU.
                 05   IN01-MISEBAN            PIC X(003).
                 05   IN01-TYUMON-BANGOU      PIC 9(005).
            03   IN01-SHOHIN-ZYOUHOU.
                 05   IN01-BUNRUI-CODE        PIC X(002).
                 05   IN01-SHOHIN-NO          PIC 9(004).
            03   IN01-TYUMON-SU               PIC 9(003).
            03   IN01-UKETSUKE-NICHIZI.
                 05   IN01-HIDUKE             PIC 9(006).
                 05   IN01-ZIKAN              PIC 9(004).
            03   IN01-TANTOSYA-CODE.
                 05   IN01-BUSHO-CODE         PIC X(003).
                 05   IN01-TANTOSYA-BANGOU    PIC 9(004).
       *>-----------------------------------------------------------------------
       *>OT01-TYUMON-SU-FILEの定義
       *>-----------------------------------------------------------------------
       FD   OT01-TYUMON-SU-FILE.
       01   OT01-TYUMON-SU-FILE-REC.
            03   OT01-SHOHIN-ZYOUHOU.
                 05   OT01-BUNRUI-CODE        PIC X(002).
                 05   OT01-SHOHIN-NO          PIC 9(004).
            03   OT01-TYUMON-SU               PIC 9(004).
       *>-----------------------------------------------------------------------
       *>作業領域の定義
       *>-----------------------------------------------------------------------
       WORKING-STORAGE                    SECTION.
       *>
       77   CST-END                           PIC X(004) VALUE "END ".
       *>
       01   WRK-WORK-AREA.
            03   WRK-AT-END                   PIC X(004).
            *>後ほど出力確認で使うかも？使わなければ削除03   WRK-IN-COUNT                 PIC 9(006).
            *>後ほど出力確認で使うかも？使わなければ削除03   WRK-OUT-COUNT                PIC 9(006).
            03   WRK-TYUMON-SU-TOTAL          PIC 9(004).
            03   WRK-INCOUNT                  PIC 9(006).
            03   WRK-TYUMON-SU                PIC 9(003).
       *>
       *>前レコードの集計キー保存用
       01   KEY-SUMMARY.
            03   KEY-BUNRUI-CODE              PIC X(002).
            03   KEY-SHOHIN-NO                PIC 9(004).
       *>
       01   MS1-MESSAGE-AREA.
            03   FILLER                       PIC X(018) VALUE "正常終了".
       *>-----------------------------------------------------------------------
       *>手続き部
       *>-----------------------------------------------------------------------
       PROCEDURE                          DIVISION.
       *>
             PERFORM   MAIN-PROC.
       *>
       *>
       *>-----------------------------------------------------------------------
       *>主処理
       *>-----------------------------------------------------------------------
       MAIN-PROM                          SECTION.
       *>
       *>    初期処理を実行
             PERFORM   INIT-PROC.
       *>
       *>    ファイルのオープン
             OPEN   INPUT    IN01-ZYUTYU-FILE
                    OUTPUT   OT01-TYUMON-SU-FILE.
       *>
       *>    入力ファイルの読み込み
             PERFPRM   IN01-ZYUTYU-FILE-READ-PROC.
       MAIN-PROC-EXIT.
       *>
           EXIT.
       *>-----------------------------------------------------------------------
       *>初期処理
       *>-----------------------------------------------------------------------
       INIT-PROM                          SECTION.
       *>
             MOVE   SPACE   TO   WRK-AT-END.
             MOVE   ZERO    TO   WRK-TYUMON-SU-TOTAL.
       *>
       INIT-PROC-EXIT.
       *>
           EXIT.
       *>-----------------------------------------------------------------------
       *>終了処理
       *>-----------------------------------------------------------------------
       TERM-PROM                          SECTION.
       *>
           DISPLAY   MS1-MESSAGE-AREA UPON   CONSOLE.
       *>
       TERM-PROC-EXIT.
       *>
           EXIT.
       *>-----------------------------------------------------------------------
       *>ファイル読み込み処理
       *>-----------------------------------------------------------------------
       IN01-ZYUTYU-FILE-READ-PROC                          SECTION.
       *>
           READ IN01-ZYUTYU-FILE
                AT     END
                MOVE   "END"             TO   WRK-AT-END
                DISPLAY "READ END"
           *>
               NOT   AT   END
               MOVE   IN01-BUNRUI-CODE   TO   KEY-BUNRUI-CODE
               MOVE   IN01-SHOHIN-NO     TO   KEY-SHOHIN-NO
           END-READ.

       *>
       IN01-ZYUTYU-FILE-READ-PROC-EXIT.
       *>
           EXIT.
       *>-----------------------------------------------------------------------
