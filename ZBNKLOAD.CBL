000100***************************************************************** ZBNKLOAD
000200*                                                               * ZBNKLOAD
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * ZBNKLOAD
000400*   This demonstration program is provided for use by users     * ZBNKLOAD
000500*   of Micro Focus products and may be used, modified and       * ZBNKLOAD
000600*   distributed as part of your application provided that       * ZBNKLOAD
000700*   you properly acknowledge the copyright of Micro Focus       * ZBNKLOAD
000800*   in this material.                                           * ZBNKLOAD
000900*                                                               * ZBNKLOAD
001000***************************************************************** ZBNKLOAD
001100                                                                  ZBNKLOAD
001200***************************************************************** ZBNKLOAD
001300* Prgram:      ZBNKLOAD.CBL                                     * ZBNKLOAD
001400* Function:    Load sequential data into indexed files          * ZBNKLOAD
001500***************************************************************** ZBNKLOAD
001600 IDENTIFICATION DIVISION.                                         ZBNKLOAD
001700 PROGRAM-ID.                                                      ZBNKLOAD
001800     ZBNKLOAD.                                                    ZBNKLOAD
001900 DATE-WRITTEN.                                                    ZBNKLOAD
002000     September 2002.                                              ZBNKLOAD
002100 DATE-COMPILED.                                                   ZBNKLOAD
002200     Today.                                                       ZBNKLOAD
002300 ENVIRONMENT DIVISION.                                            ZBNKLOAD
002400 INPUT-OUTPUT   SECTION.                                          ZBNKLOAD
002500   FILE-CONTROL.                                                  ZBNKLOAD
002600     SELECT BNKACC-SEQ                                            ZBNKLOAD
002700            ASSIGN       TO SEQACC                                ZBNKLOAD
002800            ORGANIZATION IS SEQUENTIAL                            ZBNKLOAD
002900            ACCESS MODE  IS SEQUENTIAL                            ZBNKLOAD
003000            FILE STATUS  IS WS-BNKACC-SEQ-STATUS.                 ZBNKLOAD
003100                                                                  ZBNKLOAD
003200     SELECT BNKATYPE-SEQ                                          ZBNKLOAD
003300            ASSIGN       TO SEQATYP                               ZBNKLOAD
003400            ORGANIZATION IS SEQUENTIAL                            ZBNKLOAD
003500            ACCESS MODE  IS SEQUENTIAL                            ZBNKLOAD
003600            FILE STATUS  IS WS-BNKATYPE-SEQ-STATUS.               ZBNKLOAD
003700                                                                  ZBNKLOAD
003800     SELECT BNKCUST-SEQ                                           ZBNKLOAD
003900            ASSIGN       TO SEQCUST                               ZBNKLOAD
004000            ORGANIZATION IS SEQUENTIAL                            ZBNKLOAD
004100            ACCESS MODE  IS SEQUENTIAL                            ZBNKLOAD
004200            FILE STATUS  IS WS-BNKCUST-SEQ-STATUS.                ZBNKLOAD
004300                                                                  ZBNKLOAD
004400     SELECT BNKTXN-SEQ                                            ZBNKLOAD
004500            ASSIGN       TO SEQTXN                                ZBNKLOAD
004600            ORGANIZATION IS SEQUENTIAL                            ZBNKLOAD
004700            ACCESS MODE  IS SEQUENTIAL                            ZBNKLOAD
004800            FILE STATUS  IS WS-BNKTXN-SEQ-STATUS.                 ZBNKLOAD
004900                                                                  ZBNKLOAD
005000     SELECT BNKHELP-SEQ                                           ZBNKLOAD
005100            ASSIGN       TO SEQHELP                               ZBNKLOAD
005200            ORGANIZATION IS SEQUENTIAL                            ZBNKLOAD
005300            ACCESS MODE  IS SEQUENTIAL                            ZBNKLOAD
005400            FILE STATUS  IS WS-BNKHELP-SEQ-STATUS.                ZBNKLOAD
005500                                                                  ZBNKLOAD
005600     SELECT BNKACC-NDX                                            ZBNKLOAD
005700            ASSIGN       TO NDXACC                                ZBNKLOAD
005800            ORGANIZATION IS INDEXED                               ZBNKLOAD
005900            ACCESS MODE  IS SEQUENTIAL                            ZBNKLOAD
006000            RECORD KEY   IS BAC-REC-ACCNO                         ZBNKLOAD
006100            ALTERNATE KEY IS BAC-REC-PID WITH DUPLICATES          ZBNKLOAD
006200            FILE STATUS  IS WS-BNKACC-NDX-STATUS.                 ZBNKLOAD
006300                                                                  ZBNKLOAD
006400     SELECT BNKCUST-NDX                                           ZBNKLOAD
006500            ASSIGN       TO NDXCUST                               ZBNKLOAD
006600            ORGANIZATION IS INDEXED                               ZBNKLOAD
006700            ACCESS MODE  IS RANDOM                                ZBNKLOAD
006800            RECORD KEY   IS BCS-REC-PID                           ZBNKLOAD
006900            ALTERNATE KEY IS BCS-REC-NAME WITH DUPLICATES         ZBNKLOAD
007000            ALTERNATE KEY IS BCS-REC-NAME-FF WITH DUPLICATES      ZBNKLOAD
007100            FILE STATUS  IS WS-BNKCUST-NDX-STATUS.                ZBNKLOAD
007200                                                                  ZBNKLOAD
007300     SELECT BNKATYPE-NDX                                          ZBNKLOAD
007400            ASSIGN       TO NDXATYP                               ZBNKLOAD
007500            ORGANIZATION IS INDEXED                               ZBNKLOAD
007600            ACCESS MODE  IS RANDOM                                ZBNKLOAD
007700            RECORD KEY   IS BAT-REC-TYPE                          ZBNKLOAD
007800            FILE STATUS  IS WS-BNKATYPE-NDX-STATUS.               ZBNKLOAD
007900                                                                  ZBNKLOAD
008000     SELECT BNKTXN-NDX                                            ZBNKLOAD
008100            ASSIGN       TO NDXTXN                                ZBNKLOAD
008200            ORGANIZATION IS INDEXED                               ZBNKLOAD
008300            ACCESS MODE  IS SEQUENTIAL                            ZBNKLOAD
008400            RECORD KEY   IS BTX-REC-TIMESTAMP                     ZBNKLOAD
008500            ALTERNATE KEY IS BTX-REC-ALTKEY1 WITH DUPLICATES      ZBNKLOAD
008600            FILE STATUS  IS WS-BNKTXN-NDX-STATUS.                 ZBNKLOAD
008700                                                                  ZBNKLOAD
008800     SELECT BNKHELP-NDX                                           ZBNKLOAD
008900            ASSIGN       TO NDXHELP                               ZBNKLOAD
009000            ORGANIZATION IS INDEXED                               ZBNKLOAD
009100            ACCESS MODE  IS RANDOM                                ZBNKLOAD
009200            RECORD KEY   IS HLP-KEY                               ZBNKLOAD
009300            FILE STATUS  IS WS-BNKHELP-NDX-STATUS.                ZBNKLOAD
009400                                                                  ZBNKLOAD
009500 DATA DIVISION.                                                   ZBNKLOAD
009600 FILE SECTION.                                                    ZBNKLOAD
009700 FD  BNKACC-SEQ                                                   ZBNKLOAD
009800     RECORDING MODE IS F                                          ZBNKLOAD
009900     RECORD CONTAINS 200 CHARACTERS.                              ZBNKLOAD
010000 01  BNKACC-SEQ-REC                          PIC X(200).          ZBNKLOAD
010100                                                                  ZBNKLOAD
010200 FD  BNKATYPE-SEQ                                                 ZBNKLOAD
010300     RECORDING MODE IS F                                          ZBNKLOAD
010400     RECORD CONTAINS 100 CHARACTERS.                              ZBNKLOAD
010500 01  BNKATYPE-SEQ-REC                        PIC X(100).          ZBNKLOAD
010600                                                                  ZBNKLOAD
010700 FD  BNKCUST-SEQ                                                  ZBNKLOAD
010800     RECORDING MODE IS F                                          ZBNKLOAD
010900     RECORD CONTAINS 250 CHARACTERS.                              ZBNKLOAD
011000 01  BNKCUST-SEQ-REC                         PIC X(250).          ZBNKLOAD
011100                                                                  ZBNKLOAD
011200 FD  BNKTXN-SEQ                                                   ZBNKLOAD
011300     RECORDING MODE IS F                                          ZBNKLOAD
011400     RECORD CONTAINS 400 CHARACTERS.                              ZBNKLOAD
011500 01  BNKTXN-SEQ-REC                          PIC X(400).          ZBNKLOAD
011600                                                                  ZBNKLOAD
011700 FD  BNKHELP-SEQ                                                  ZBNKLOAD
011800     RECORDING MODE IS F                                          ZBNKLOAD
011900     RECORD CONTAINS 83 CHARACTERS.                               ZBNKLOAD
012000 01  BNKHELP-SEQ-REC                         PIC X(83).           ZBNKLOAD
012100                                                                  ZBNKLOAD
012200 FD  BNKACC-NDX.                                                  ZBNKLOAD
012300 01  BNKACC-REC.                                                  ZBNKLOAD
012400 COPY CBANKVAC.                                                   ZBNKLOAD
012500                                                                  ZBNKLOAD
012600 FD  BNKCUST-NDX.                                                 ZBNKLOAD
012700 01  BNKCUST-REC.                                                 ZBNKLOAD
012800 COPY CBANKVCS.                                                   ZBNKLOAD
012900                                                                  ZBNKLOAD
013000 FD  BNKATYPE-NDX.                                                ZBNKLOAD
013100 01  BNKATYPE-REC.                                                ZBNKLOAD
013200 COPY CBANKVAT.                                                   ZBNKLOAD
013300                                                                  ZBNKLOAD
013400 FD  BNKTXN-NDX.                                                  ZBNKLOAD
013500 01  BNKTXN-REC.                                                  ZBNKLOAD
013600 COPY CBANKVTX.                                                   ZBNKLOAD
013700                                                                  ZBNKLOAD
013800 FD  BNKHELP-NDX.                                                 ZBNKLOAD
013900 01  BNKHELP-REC.                                                 ZBNKLOAD
014000 COPY CHELPVSM.                                                   ZBNKLOAD
014100                                                                  ZBNKLOAD
014200 WORKING-STORAGE SECTION.                                         ZBNKLOAD
014300 COPY CTIMERD.                                                    ZBNKLOAD
014400                                                                  ZBNKLOAD
014500 01  WS-MISC-STORAGE.                                             ZBNKLOAD
014600   05  WS-PROGRAM-ID                         PIC X(8)             ZBNKLOAD
014700       VALUE 'ZBNKLOAD'.                                          ZBNKLOAD
014800   05  WS-BNKACC-SEQ-STATUS                  PIC X(2).            ZBNKLOAD
014900   05  WS-BNKATYPE-SEQ-STATUS                PIC X(2).            ZBNKLOAD
015000   05  WS-BNKCUST-SEQ-STATUS                 PIC X(2).            ZBNKLOAD
015100   05  WS-BNKTXN-SEQ-STATUS                  PIC X(2).            ZBNKLOAD
015200   05  WS-BNKHELP-SEQ-STATUS                 PIC X(2).            ZBNKLOAD
015300   05  WS-BNKACC-NDX-STATUS                  PIC X(2).            ZBNKLOAD
015400   05  WS-BNKATYPE-NDX-STATUS                PIC X(2).            ZBNKLOAD
015500   05  WS-BNKCUST-NDX-STATUS                 PIC X(2).            ZBNKLOAD
015600   05  WS-BNKTXN-NDX-STATUS                  PIC X(2).            ZBNKLOAD
015700   05  WS-BNKHELP-NDX-STATUS                 PIC X(2).            ZBNKLOAD
015800                                                                  ZBNKLOAD
015900   05  WS-IO-STATUS.                                              ZBNKLOAD
016000     10  WS-IO-STAT1                         PIC X(1).            ZBNKLOAD
016100     10  WS-IO-STAT2                         PIC X(1).            ZBNKLOAD
016200                                                                  ZBNKLOAD
016300   05  WS-TWO-BYTES.                                              ZBNKLOAD
016400     10  WS-TWO-BYTES-LEFT                   PIC X(1).            ZBNKLOAD
016500     10  WS-TWO-BYTES-RIGHT                  PIC X(1).            ZBNKLOAD
016600   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES                  ZBNKLOAD
016700                                             PIC 9(1) COMP.       ZBNKLOAD
016800                                                                  ZBNKLOAD
016900   05  WS-RECORD-COUNTER1                    PIC 9(5)             ZBNKLOAD
017000       VALUE ZERO.                                                ZBNKLOAD
017100   05  WS-RECORD-COUNTER2                    PIC 9(5)             ZBNKLOAD
017200       VALUE ZERO.                                                ZBNKLOAD
017300                                                                  ZBNKLOAD
017400   05  WS-OPEN-ERROR                         PIC 9(3).            ZBNKLOAD
017500     88  OPEN-OK                             VALUE 0.             ZBNKLOAD
017600                                                                  ZBNKLOAD
017700   05  WS-FILE                               PIC X(16).           ZBNKLOAD
017800                                                                  ZBNKLOAD
017900                                                                  ZBNKLOAD
018000   05  WS-LAST-PID                           PIC X(5)             ZBNKLOAD
018100       VALUE LOW-VALUES.                                          ZBNKLOAD
018200                                                                  ZBNKLOAD
018300 01  WS-CONSOLE-MESSAGE                      PIC X(60).           ZBNKLOAD
018400                                                                  ZBNKLOAD
018500 PROCEDURE DIVISION.                                              ZBNKLOAD
018600     DISPLAY "sTARTED".                                           ZBNKLOAD
018700     PERFORM RUN-TIME.                                            ZBNKLOAD
018800     SET OPEN-OK TO TRUE.                                         ZBNKLOAD
018900                                                                  ZBNKLOAD
019000     OPEN INPUT BNKACC-SEQ.                                       ZBNKLOAD
019100     MOVE WS-BNKACC-SEQ-STATUS TO WS-IO-STATUS.                   ZBNKLOAD
019200     MOVE 'BNKACC-SEQ' TO WS-FILE.                                ZBNKLOAD
019300     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
019400             CHECK-OPEN-EXIT.                                     ZBNKLOAD
019500                                                                  ZBNKLOAD
019600     OPEN INPUT BNKATYPE-SEQ.                                     ZBNKLOAD
019700     MOVE WS-BNKATYPE-SEQ-STATUS TO WS-IO-STATUS.                 ZBNKLOAD
019800     MOVE 'BNKATYPE-SEQ' TO WS-FILE.                              ZBNKLOAD
019900     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
020000             CHECK-OPEN-EXIT.                                     ZBNKLOAD
020100                                                                  ZBNKLOAD
020200     OPEN INPUT BNKCUST-SEQ.                                      ZBNKLOAD
020300     MOVE WS-BNKCUST-SEQ-STATUS TO WS-IO-STATUS.                  ZBNKLOAD
020400     MOVE 'BNKCUST-SEQ' TO WS-FILE.                               ZBNKLOAD
020500     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
020600             CHECK-OPEN-EXIT.                                     ZBNKLOAD
020700                                                                  ZBNKLOAD
020800     OPEN INPUT BNKTXN-SEQ.                                       ZBNKLOAD
020900     MOVE WS-BNKTXN-SEQ-STATUS TO WS-IO-STATUS.                   ZBNKLOAD
021000     MOVE 'BNKTXN-SEQ' TO WS-FILE.                                ZBNKLOAD
021100     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
021200             CHECK-OPEN-EXIT.                                     ZBNKLOAD
021300                                                                  ZBNKLOAD
021400     OPEN INPUT BNKHELP-SEQ.                                      ZBNKLOAD
021500     MOVE WS-BNKHELP-SEQ-STATUS TO WS-IO-STATUS.                  ZBNKLOAD
021600     MOVE 'BNKHELP-SEQ' TO WS-FILE.                               ZBNKLOAD
021700     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
021800             CHECK-OPEN-EXIT.                                     ZBNKLOAD
021900                                                                  ZBNKLOAD
022000     OPEN OUTPUT BNKACC-NDX.                                      ZBNKLOAD
022100     MOVE WS-BNKACC-NDX-STATUS TO WS-IO-STATUS.                   ZBNKLOAD
022200     MOVE 'BNKACC-NDX' TO WS-FILE.                                ZBNKLOAD
022300     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
022400             CHECK-OPEN-EXIT.                                     ZBNKLOAD
022500                                                                  ZBNKLOAD
022600     OPEN OUTPUT BNKATYPE-NDX.                                    ZBNKLOAD
022700     MOVE WS-BNKATYPE-NDX-STATUS TO WS-IO-STATUS.                 ZBNKLOAD
022800     MOVE 'BNKATYPE-NDX' TO WS-FILE.                              ZBNKLOAD
022900     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
023000             CHECK-OPEN-EXIT.                                     ZBNKLOAD
023100                                                                  ZBNKLOAD
023200     OPEN OUTPUT BNKCUST-NDX.                                     ZBNKLOAD
023300     MOVE WS-BNKCUST-NDX-STATUS TO WS-IO-STATUS.                  ZBNKLOAD
023400     MOVE 'BNKCUST-NDX' TO WS-FILE.                               ZBNKLOAD
023500     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
023600             CHECK-OPEN-EXIT.                                     ZBNKLOAD
023700                                                                  ZBNKLOAD
023800     OPEN OUTPUT BNKTXN-NDX.                                      ZBNKLOAD
023900     MOVE WS-BNKTXN-NDX-STATUS TO WS-IO-STATUS.                   ZBNKLOAD
024000     MOVE 'BNKTXN-NDX' TO WS-FILE.                                ZBNKLOAD
024100     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
024200             CHECK-OPEN-EXIT.                                     ZBNKLOAD
024300                                                                  ZBNKLOAD
024400     OPEN OUTPUT BNKHELP-NDX.                                     ZBNKLOAD
024500     MOVE WS-BNKHELP-NDX-STATUS TO WS-IO-STATUS.                  ZBNKLOAD
024600     MOVE 'BNKHELP-NDX' TO WS-FILE.                               ZBNKLOAD
024700     PERFORM CHECK-OPEN THRU                                      ZBNKLOAD
024800             CHECK-OPEN-EXIT.                                     ZBNKLOAD
024900                                                                  ZBNKLOAD
025000     IF WS-OPEN-ERROR IS NOT EQUAL TO ZERO                        ZBNKLOAD
025100        MOVE 'Aborting...' TO WS-CONSOLE-MESSAGE                  ZBNKLOAD
025200        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKLOAD
025300        GO TO QUICK-EXIT                                          ZBNKLOAD
025400     END-IF.                                                      ZBNKLOAD
025500                                                                  ZBNKLOAD
025600 BNKACC-PROCESS.                                                  ZBNKLOAD
025700     MOVE 0 TO WS-RECORD-COUNTER1.                                ZBNKLOAD
025800 BNKACC-LOOP.                                                     ZBNKLOAD
025900     READ BNKACC-SEQ INTO BNKACC-REC                              ZBNKLOAD
026000       AT END                                                     ZBNKLOAD
026100         GO TO BNKACC-ENDED.                                      ZBNKLOAD
026200     WRITE BNKACC-REC.                                            ZBNKLOAD
026300     ADD 1 TO WS-RECORD-COUNTER1.                                 ZBNKLOAD
026400     GO TO BNKACC-LOOP.                                           ZBNKLOAD
026500 BNKACC-ENDED.                                                    ZBNKLOAD
026600     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBNKLOAD
026700     STRING WS-RECORD-COUNTER1 DELIMITED BY SIZE                  ZBNKLOAD
026800            ' processed from BNKACC' DELIMITED BY SIZE            ZBNKLOAD
026900       INTO WS-CONSOLE-MESSAGE.                                   ZBNKLOAD
027000     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKLOAD
027100                                                                  ZBNKLOAD
027200 BNKATYPE-PROCESS.                                                ZBNKLOAD
027300     MOVE 0 TO WS-RECORD-COUNTER1.                                ZBNKLOAD
027400 BNKATYPE-LOOP.                                                   ZBNKLOAD
027500     READ BNKATYPE-SEQ INTO BNKATYPE-REC                          ZBNKLOAD
027600       AT END                                                     ZBNKLOAD
027700         GO TO BNKATYPE-ENDED.                                    ZBNKLOAD
027800     WRITE BNKATYPE-REC.                                          ZBNKLOAD
027900     ADD 1 TO WS-RECORD-COUNTER1.                                 ZBNKLOAD
028000     GO TO BNKATYPE-LOOP.                                         ZBNKLOAD
028100 BNKATYPE-ENDED.                                                  ZBNKLOAD
028200     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBNKLOAD
028300     STRING WS-RECORD-COUNTER1 DELIMITED BY SIZE                  ZBNKLOAD
028400            ' processed from BNKATYPE' DELIMITED BY SIZE          ZBNKLOAD
028500       INTO WS-CONSOLE-MESSAGE.                                   ZBNKLOAD
028600     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKLOAD
028700                                                                  ZBNKLOAD
028800 BNKCUST-PROCESS.                                                 ZBNKLOAD
028900     MOVE 0 TO WS-RECORD-COUNTER1.                                ZBNKLOAD
029000 BNKCUST-LOOP.                                                    ZBNKLOAD
029100     READ BNKCUST-SEQ INTO BNKCUST-REC                            ZBNKLOAD
029200       AT END                                                     ZBNKLOAD
029300         GO TO BNKCUST-ENDED.                                     ZBNKLOAD
029400     WRITE BNKCUST-REC.                                           ZBNKLOAD
029500     ADD 1 TO WS-RECORD-COUNTER1.                                 ZBNKLOAD
029600     GO TO BNKCUST-LOOP.                                          ZBNKLOAD
029700 BNKCUST-ENDED.                                                   ZBNKLOAD
029800     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBNKLOAD
029900     STRING WS-RECORD-COUNTER1 DELIMITED BY SIZE                  ZBNKLOAD
030000            ' processed from BNKCUST' DELIMITED BY SIZE           ZBNKLOAD
030100       INTO WS-CONSOLE-MESSAGE.                                   ZBNKLOAD
030200     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKLOAD
030300                                                                  ZBNKLOAD
030400 BNKTXN-PROCESS.                                                  ZBNKLOAD
030500     MOVE 0 TO WS-RECORD-COUNTER1.                                ZBNKLOAD
030600 BNKTXN-LOOP.                                                     ZBNKLOAD
030700     READ BNKTXN-SEQ INTO BNKTXN-REC                              ZBNKLOAD
030800       AT END                                                     ZBNKLOAD
030900         GO TO BNKTXN-ENDED.                                      ZBNKLOAD
031000     WRITE BNKTXN-REC.                                            ZBNKLOAD
031100     ADD 1 TO WS-RECORD-COUNTER1.                                 ZBNKLOAD
031200     GO TO BNKTXN-LOOP.                                           ZBNKLOAD
031300 BNKTXN-ENDED.                                                    ZBNKLOAD
031400     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBNKLOAD
031500     STRING WS-RECORD-COUNTER1 DELIMITED BY SIZE                  ZBNKLOAD
031600            ' processed from BNKTXN' DELIMITED BY SIZE            ZBNKLOAD
031700       INTO WS-CONSOLE-MESSAGE.                                   ZBNKLOAD
031800     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKLOAD
031900                                                                  ZBNKLOAD
032000 BNKHELP-PROCESS.                                                 ZBNKLOAD
032100     MOVE 0 TO WS-RECORD-COUNTER1.                                ZBNKLOAD
032200 BNKHELP-LOOP.                                                    ZBNKLOAD
032300     READ BNKHELP-SEQ INTO BNKHELP-REC                            ZBNKLOAD
032400       AT END                                                     ZBNKLOAD
032500         GO TO BNKHELP-ENDED.                                     ZBNKLOAD
032600     WRITE BNKHELP-REC.                                           ZBNKLOAD
032700     ADD 1 TO WS-RECORD-COUNTER1.                                 ZBNKLOAD
032800     GO TO BNKHELP-LOOP.                                          ZBNKLOAD
032900 BNKHELP-ENDED.                                                   ZBNKLOAD
033000     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBNKLOAD
033100     STRING WS-RECORD-COUNTER1 DELIMITED BY SIZE                  ZBNKLOAD
033200            ' processed from BNKHELP' DELIMITED BY SIZE           ZBNKLOAD
033300       INTO WS-CONSOLE-MESSAGE.                                   ZBNKLOAD
033400     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKLOAD
033500                                                                  ZBNKLOAD
033600     CLOSE BNKACC-SEQ.                                            ZBNKLOAD
033700     CLOSE BNKATYPE-SEQ.                                          ZBNKLOAD
033800     CLOSE BNKCUST-SEQ.                                           ZBNKLOAD
033900     CLOSE BNKTXN-SEQ.                                            ZBNKLOAD
034000     CLOSE BNKHELP-SEQ.                                           ZBNKLOAD
034100     CLOSE BNKACC-NDX.                                            ZBNKLOAD
034200     CLOSE BNKATYPE-NDX.                                          ZBNKLOAD
034300     CLOSE BNKCUST-NDX.                                           ZBNKLOAD
034400     CLOSE BNKTXN-NDX.                                            ZBNKLOAD
034500     CLOSE BNKHELP-NDX.                                           ZBNKLOAD
034600                                                                  ZBNKLOAD
034700                                                                  ZBNKLOAD
034800     PERFORM RUN-TIME.                                            ZBNKLOAD
034900                                                                  ZBNKLOAD
035000     MOVE 0 TO RETURN-CODE.                                       ZBNKLOAD
035100 QUICK-EXIT.                                                      ZBNKLOAD
035200     GOBACK.                                                      ZBNKLOAD
035300                                                                  ZBNKLOAD
035400***************************************************************** ZBNKLOAD
035500* Check file open OK                                            * ZBNKLOAD
035600***************************************************************** ZBNKLOAD
035700 CHECK-OPEN.                                                      ZBNKLOAD
035800     IF WS-IO-STATUS IS EQUAL TO '00'                             ZBNKLOAD
035900        MOVE SPACES TO WS-CONSOLE-MESSAGE                         ZBNKLOAD
036000        STRING WS-FILE DELIMITED BY ' '                           ZBNKLOAD
036100               ' opened ok' DELIMITED BY SIZE                     ZBNKLOAD
036200          INTO WS-CONSOLE-MESSAGE                                 ZBNKLOAD
036300        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKLOAD
036400     ELSE                                                         ZBNKLOAD
036500        MOVE SPACES TO WS-CONSOLE-MESSAGE                         ZBNKLOAD
036600        STRING WS-FILE DELIMITED BY ' '                           ZBNKLOAD
036700               ' opened ok' DELIMITED BY SIZE                     ZBNKLOAD
036800          INTO WS-CONSOLE-MESSAGE                                 ZBNKLOAD
036900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKLOAD
037000        PERFORM DISPLAY-IO-STATUS                                 ZBNKLOAD
037100        ADD 1 TO WS-OPEN-ERROR                                    ZBNKLOAD
037200     END-IF.                                                      ZBNKLOAD
037300 CHECK-OPEN-EXIT.                                                 ZBNKLOAD
037400     EXIT.                                                        ZBNKLOAD
037500                                                                  ZBNKLOAD
037600                                                                  ZBNKLOAD
037700***************************************************************** ZBNKLOAD
037800* Display the file status bytes. This routine will display as   * ZBNKLOAD
037900* two digits if the full two byte file status is numeric. If    * ZBNKLOAD
038000* second byte is non-numeric then it will be treated as a       * ZBNKLOAD
038100* binary number.                                                * ZBNKLOAD
038200***************************************************************** ZBNKLOAD
038300 DISPLAY-IO-STATUS.                                               ZBNKLOAD
038400     IF WS-IO-STATUS NUMERIC                                      ZBNKLOAD
038500        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKLOAD
038600        STRING 'File status -' DELIMITED BY SIZE                  ZBNKLOAD
038700               WS-IO-STATUS DELIMITED BY SIZE                     ZBNKLOAD
038800          INTO WS-CONSOLE-MESSAGE                                 ZBNKLOAD
038900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKLOAD
039000     ELSE                                                         ZBNKLOAD
039100        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY     ZBNKLOAD
039200        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT                    ZBNKLOAD
039300        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKLOAD
039400        STRING 'File status -' DELIMITED BY SIZE                  ZBNKLOAD
039500               WS-IO-STAT1 DELIMITED BY SIZE                      ZBNKLOAD
039600               '/' DELIMITED BY SIZE                              ZBNKLOAD
039700               WS-TWO-BYTES DELIMITED BY SIZE                     ZBNKLOAD
039800          INTO WS-CONSOLE-MESSAGE                                 ZBNKLOAD
039900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKLOAD
040000     END-IF.                                                      ZBNKLOAD
040100                                                                  ZBNKLOAD
040200***************************************************************** ZBNKLOAD
040300* 'ABORT' the program.                                          * ZBNKLOAD
040400* Post a message to the console and issue a STOP RUN            * ZBNKLOAD
040500***************************************************************** ZBNKLOAD
040600 ABORT-PROGRAM.                                                   ZBNKLOAD
040700     IF WS-CONSOLE-MESSAGE NOT = SPACES                           ZBNKLOAD
040800        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKLOAD
040900     END-IF.                                                      ZBNKLOAD
041000     MOVE 'Program is abending...'  TO WS-CONSOLE-MESSAGE.        ZBNKLOAD
041100     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKLOAD
041200     MOVE 16 TO RETURN-CODE.                                      ZBNKLOAD
041300     STOP RUN.                                                    ZBNKLOAD
041400                                                                  ZBNKLOAD
041500***************************************************************** ZBNKLOAD
041600* Display CONSOLE messages...                                   * ZBNKLOAD
041700***************************************************************** ZBNKLOAD
041800 DISPLAY-CONSOLE-MESSAGE.                                         ZBNKLOAD
041900     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE.              ZBNKLOAD
042000*    DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE               ZBNKLOAD
042100*      UPON CONSOLE.                                              ZBNKLOAD
042200     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       ZBNKLOAD
042300                                                                  ZBNKLOAD
042400 COPY CTIMERP.                                                    ZBNKLOAD
042500                                                                  ZBNKLOAD
042600* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      ZBNKLOAD
