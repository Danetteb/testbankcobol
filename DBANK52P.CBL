000100***************************************************************** DBANK52P
000200*                                                               * DBANK52P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK52P
000400*   This demonstration program is provided for use by users     * DBANK52P
000500*   of Micro Focus products and may be used, modified and       * DBANK52P
000600*   distributed as part of your application provided that       * DBANK52P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK52P
000800*   in this material.                                           * DBANK52P
000900*                                                               * DBANK52P
001000***************************************************************** DBANK52P
001100                                                                  DBANK52P
001200***************************************************************** DBANK52P
001300* Program:     DBANK52P.CBL                                     * DBANK52P
001400* Function:    Sequential read of bank data for batch job       * DBANK52P
001500*              VSAM version                                     * DBANK52P
001600***************************************************************** DBANK52P
001700                                                                  DBANK52P
001800 IDENTIFICATION DIVISION.                                         DBANK52P
001900 PROGRAM-ID.                                                      DBANK52P
002000     DBANK52P.                                                    DBANK52P
002100 DATE-WRITTEN.                                                    DBANK52P
002200     September 2002.                                              DBANK52P
002300 DATE-COMPILED.                                                   DBANK52P
002400     Today.                                                       DBANK52P
002500                                                                  DBANK52P
002600 ENVIRONMENT DIVISION.                                            DBANK52P
002700                                                                  DBANK52P
002800 INPUT-OUTPUT   SECTION.                                          DBANK52P
002900   FILE-CONTROL.                                                  DBANK52P
003000     SELECT BNKTXN-FILE                                           DBANK52P
003100            ASSIGN       TO BNKTXN                                DBANK52P
003200            ORGANIZATION IS INDEXED                               DBANK52P
003300            ACCESS MODE  IS SEQUENTIAL                            DBANK52P
003400            RECORD KEY   IS BTX-REC-TIMESTAMP                     DBANK52P
003500            ALTERNATE KEY IS BTX-REC-ALTKEY1 WITH DUPLICATES      DBANK52P
003600            FILE STATUS  IS WS-BNKTXN-STATUS.                     DBANK52P
003700                                                                  DBANK52P
003800 DATA DIVISION.                                                   DBANK52P
003900                                                                  DBANK52P
004000 FILE SECTION.                                                    DBANK52P
004100 FD  BNKTXN-FILE.                                                 DBANK52P
004200 01  BNKTXN-REC.                                                  DBANK52P
004300 COPY CBANKVTX.                                                   DBANK52P
004400                                                                  DBANK52P
004500 WORKING-STORAGE SECTION.                                         DBANK52P
004600 01  WS-MISC-STORAGE.                                             DBANK52P
004700   05  WS-PROGRAM-ID                         PIC X(8)             DBANK52P
004800       VALUE 'DBANK52P'.                                          DBANK52P
004900   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK52P
005000   05  WS-SUB1                               PIC S9(4) COMP.      DBANK52P
005100                                                                  DBANK52P
005200   05  WS-BNKTXN-STATUS.                                          DBANK52P
005300     10  WS-BNKTXN-STAT1                     PIC X(1).            DBANK52P
005400     10  WS-BNKTXN-STAT2                     PIC X(1).            DBANK52P
005500                                                                  DBANK52P
005600 01  WS-COMMAREA.                                                 DBANK52P
005700 COPY CIOFUNCS.                                                   DBANK52P
005800 COPY CBANKD51.                                                   DBANK52P
005900 COPY CBANKD52.                                                   DBANK52P
006000                                                                  DBANK52P
006100 COPY CBANKTXD.                                                   DBANK52P
006200                                                                  DBANK52P
006300 LINKAGE SECTION.                                                 DBANK52P
006400 01  DFHCOMMAREA.                                                 DBANK52P
006500   05  LK-COMMAREA                           PIC X(1)             DBANK52P
006600         OCCURS 1 TO 4096 TIMES                                   DBANK52P
006700           DEPENDING ON WS-COMMAREA-LENGTH.                       DBANK52P
006800                                                                  DBANK52P
006900 PROCEDURE DIVISION USING DFHCOMMAREA.                            DBANK52P
007000***************************************************************** DBANK52P
007100* Move the passed data to our area                              * DBANK52P
007200***************************************************************** DBANK52P
007300     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK52P
007400     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK52P
007500                                                                  DBANK52P
007600***************************************************************** DBANK52P
007700* Initialize our output area                                    * DBANK52P
007800***************************************************************** DBANK52P
007900     MOVE SPACES TO CD52O-DATA.                                   DBANK52P
008000                                                                  DBANK52P
008100***************************************************************** DBANK52P
008200* Check what is required                                        * DBANK52P
008300***************************************************************** DBANK52P
008400     EVALUATE TRUE                                                DBANK52P
008500       WHEN IO-REQUEST-FUNCTION-OPEN                              DBANK52P
008600        PERFORM OPEN-FILE THRU                                    DBANK52P
008700                OPEN-FILE-EXIT                                    DBANK52P
008800       WHEN IO-REQUEST-FUNCTION-READ                              DBANK52P
008900        PERFORM READ-FILE THRU                                    DBANK52P
009000                READ-FILE-EXIT                                    DBANK52P
009100       WHEN IO-REQUEST-FUNCTION-CLOSE                             DBANK52P
009200        PERFORM CLOSE-FILE THRU                                   DBANK52P
009300                CLOSE-FILE-EXIT                                   DBANK52P
009400       WHEN OTHER                                                 DBANK52P
009500        SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK52P
009600     END-EVALUATE.                                                DBANK52P
009700                                                                  DBANK52P
009800***************************************************************** DBANK52P
009900* Move the result back to the callers area                      * DBANK52P
010000***************************************************************** DBANK52P
010100     MOVE WS-COMMAREA TO DFHCOMMAREA  (1:WS-COMMAREA-LENGTH).     DBANK52P
010200                                                                  DBANK52P
010300***************************************************************** DBANK52P
010400* Return to our caller                                          * DBANK52P
010500***************************************************************** DBANK52P
010600     GOBACK.                                                      DBANK52P
010700                                                                  DBANK52P
010800                                                                  DBANK52P
010900***************************************************************** DBANK52P
011000* Open the file so we can read TXN sequentially                 * DBANK52P
011100***************************************************************** DBANK52P
011200 OPEN-FILE.                                                       DBANK52P
011300     OPEN INPUT BNKTXN-FILE.                                      DBANK52P
011400     IF WS-BNKTXN-STATUS = '00'                                   DBANK52P
011500        SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK52P
011600     ELSE                                                         DBANK52P
011700        SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK52P
011800     END-IF.                                                      DBANK52P
011900 OPEN-FILE-EXIT.                                                  DBANK52P
012000     EXIT.                                                        DBANK52P
012100                                                                  DBANK52P
012200***************************************************************** DBANK52P
012300* Read sequentially through the transaction file                * DBANK52P
012400***************************************************************** DBANK52P
012500 READ-FILE.                                                       DBANK52P
012600     READ BNKTXN-FILE.                                            DBANK52P
012700* Was read ok?                                                    DBANK52P
012800     IF WS-BNKTXN-STATUS IS EQUAL TO '00'                         DBANK52P
012900        SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK52P
013000     END-IF.                                                      DBANK52P
013100* Was read at end-of-file?                                        DBANK52P
013200     IF WS-BNKTXN-STATUS IS EQUAL TO '10'                         DBANK52P
013300        SET IO-REQUEST-STATUS-EOF TO TRUE                         DBANK52P
013400     END-IF.                                                      DBANK52P
013500     IF WS-BNKTXN-STATUS IS NOT EQUAL TO '00' AND                 DBANK52P
013600        WS-BNKTXN-STATUS IS NOT EQUAL TO '10'                     DBANK52P
013700        SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK52P
013800     END-IF.                                                      DBANK52P
013900     IF WS-BNKTXN-STATUS IS EQUAL TO '00'                         DBANK52P
014000        IF BTX-REC-TYPE IS EQUAL TO '1' AND                       DBANK52P
014100           (BTX-REC-PID IS EQUAL TO CD52I-PID OR                  DBANK52P
014200            CD52-REQUESTED-ALL)                                   DBANK52P
014300           MOVE BTX-REC-PID TO CD52O-PID                          DBANK52P
014400           MOVE BTX-REC-ACCNO TO CD52O-ACC-NO                     DBANK52P
014500           MOVE BTX-REC-TIMESTAMP TO CD52O-TIMESTAMP              DBANK52P
014600           MOVE BTX-REC-AMOUNT TO CD52O-AMOUNT                    DBANK52P
014700           MOVE BTX-REC-DATA-OLD TO TXN-DATA-OLD                  DBANK52P
014800           MOVE TXN-T1-OLD-DESC TO CD52O-DESC                     DBANK52P
014900        ELSE                                                      DBANK52P
015000           GO TO READ-FILE                                        DBANK52P
015100        END-IF                                                    DBANK52P
015200     END-IF.                                                      DBANK52P
015300 READ-FILE-EXIT.                                                  DBANK52P
015400     EXIT.                                                        DBANK52P
015500                                                                  DBANK52P
015600***************************************************************** DBANK52P
015700* Close the file                                                * DBANK52P
015800***************************************************************** DBANK52P
015900 CLOSE-FILE.                                                      DBANK52P
016000     CLOSE BNKTXN-FILE.                                           DBANK52P
016100     IF WS-BNKTXN-STATUS = '00'                                   DBANK52P
016200        SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK52P
016300     ELSE                                                         DBANK52P
016400       SET IO-REQUEST-STATUS-ERROR TO TRUE                        DBANK52P
016500     END-IF.                                                      DBANK52P
016600 CLOSE-FILE-EXIT.                                                 DBANK52P
016700     EXIT.                                                        DBANK52P
016800                                                                  DBANK52P
016900* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK52P
