000100***************************************************************** DBANK05P
000200*                                                               * DBANK05P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK05P
000400*   This demonstration program is provided for use by users     * DBANK05P
000500*   of Micro Focus products and may be used, modified and       * DBANK05P
000600*   distributed as part of your application provided that       * DBANK05P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK05P
000800*   in this material.                                           * DBANK05P
000900*                                                               * DBANK05P
001000***************************************************************** DBANK05P
001100                                                                  DBANK05P
001200***************************************************************** DBANK05P
001300* Program:     DBANK05P.CBL                                     * DBANK05P
001400* Function:    Obtain list of transactions for an account       * DBANK05P
001500*              VSAM Version                                     * DBANK05P
001600***************************************************************** DBANK05P
001700                                                                  DBANK05P
001800 IDENTIFICATION DIVISION.                                         DBANK05P
001900 PROGRAM-ID.                                                      DBANK05P
002000     DBANK05P.                                                    DBANK05P
002100 DATE-WRITTEN.                                                    DBANK05P
002200     September 2002.                                              DBANK05P
002300 DATE-COMPILED.                                                   DBANK05P
002400     Today.                                                       DBANK05P
002500                                                                  DBANK05P
002600 ENVIRONMENT DIVISION.                                            DBANK05P
002700                                                                  DBANK05P
002800 DATA DIVISION.                                                   DBANK05P
002900                                                                  DBANK05P
003000 WORKING-STORAGE SECTION.                                         DBANK05P
003100 01  WS-MISC-STORAGE.                                             DBANK05P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK05P
003300       VALUE 'DBANK05P'.                                          DBANK05P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK05P
003500   05  WS-SUB1                               PIC S9(4) COMP.      DBANK05P
003600   05  WS-SUB2                               PIC S9(4) COMP.      DBANK05P
003700   05  WS-RESP                               PIC S9(8) COMP.      DBANK05P
003800   05  WS-WORK1                              PIC S9(15) COMP-3.   DBANK05P
003900   05  WS-WORK2                              PIC S9(15) COMP-3.   DBANK05P
004000   05  WS-BNKTXN-AIX1-RID                    PIC X(35).           DBANK05P
004100   05  WS-BNKTXN-AIX1-RID-R REDEFINES WS-BNKTXN-AIX1-RID.         DBANK05P
004200     10  WS-BNKTXN-AIX1-RID-ACC              PIC X(9).            DBANK05P
004300     10  WS-BNKTXN-AIX1-RID-STAMP.                                DBANK05P
004400       15  WS-BNKTXN-AIX1-RID-DATE           PIC X(10).           DBANK05P
004500       15  WS-BNKTXN-AIX1-RID-DOT1           PIC X(1).            DBANK05P
004600       15  WS-BNKTXN-AIX1-RID-TIME           PIC X(15).           DBANK05P
004700       15  WS-BNKTXN-AIX1-RID-TIME-R REDEFINES                    DBANK05P
004800             WS-BNKTXN-AIX1-RID-TIME.                             DBANK05P
004900         20  WS-BNKTXN-AIX1-RID-HH           PIC 9(2).            DBANK05P
005000         20  WS-BNKTXN-AIX1-RID-DOT2         PIC X(1).            DBANK05P
005100         20  WS-BNKTXN-AIX1-RID-MM           PIC 9(2).            DBANK05P
005200         20  WS-BNKTXN-AIX1-RID-DOT3         PIC X(1).            DBANK05P
005300         20  WS-BNKTXN-AIX1-RID-SS           PIC 9(2).            DBANK05P
005400         20  WS-BNKTXN-AIX1-RID-DOT4         PIC X(1).            DBANK05P
005500         20  WS-BNKTXN-AIX1-RID-DEC          PIC 9(6).            DBANK05P
005600                                                                  DBANK05P
005700 01  WS-BNKTXN-REC.                                               DBANK05P
005800 COPY CBANKVTX.                                                   DBANK05P
005900                                                                  DBANK05P
006000 COPY CBANKTXD.                                                   DBANK05P
006100                                                                  DBANK05P
006200 01  WS-TWOS-COMP.                                                DBANK05P
006300   05  WS-TWOS-COMP-REQ                      PIC X(1).            DBANK05P
006400     88  WS-TWOS-COMP-REQ-YES                VALUE 'Y'.           DBANK05P
006500     88  WS-TWOS-COMP-REQ-NO                 VALUE 'N'.           DBANK05P
006600   05  WS-TWOS-COMP-LEN                      PIC S9(4) COMP.      DBANK05P
006700   05  WS-TWOS-COMP-INPUT                    PIC X(256).          DBANK05P
006800   05  WS-TWOS-COMP-OUTPUT                   PIC X(256).          DBANK05P
006900                                                                  DBANK05P
007000 01  WS-COMMAREA.                                                 DBANK05P
007100 COPY CBANKD05.                                                   DBANK05P
007200                                                                  DBANK05P
007300 COPY CABENDD.                                                    DBANK05P
007400                                                                  DBANK05P
007500 LINKAGE SECTION.                                                 DBANK05P
007600 01  DFHCOMMAREA.                                                 DBANK05P
007700   05  LK-COMMAREA                           PIC X(1)             DBANK05P
007800       OCCURS 1 TO 4096 TIMES                                     DBANK05P
007900         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK05P
008000                                                                  DBANK05P
008100 COPY CENTRY.                                                     DBANK05P
008200***************************************************************** DBANK05P
008300* Move the passed data to our area                              * DBANK05P
008400***************************************************************** DBANK05P
008500     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK05P
008600     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK05P
008700                                                                  DBANK05P
008800***************************************************************** DBANK05P
008900* Initialize our output area                                    * DBANK05P
009000***************************************************************** DBANK05P
009100     MOVE SPACES TO CD05O-DATA.                                   DBANK05P
009200                                                                  DBANK05P
009300***************************************************************** DBANK05P
009400* Setup the start position for the browse                       * DBANK05P
009500***************************************************************** DBANK05P
009600     MOVE CD05I-ACC TO WS-BNKTXN-AIX1-RID-ACC.                    DBANK05P
009700     MOVE CD05I-START-ID TO WS-BNKTXN-AIX1-RID-STAMP.             DBANK05P
009800* We can't do a GT or LT, only GTEQ, as we can with DL1 or SQL.   DBANK05P
009900* Thus we will 'fix' the search key by adjusting the time stamp   DBANK05P
010000* up by 1 if we are going GT.                                     DBANK05P
010100* We will convert the time part the timestamp (hh:mm:ss.dddddd)   DBANK05P
010200* to a single nunber be the number of milli-sec. We then add or   DBANK05P
010300* subtract 1 as appropriate and then convert it back. This should DBANK05P
010400* work for any time that doesen't show as 23:59:59.999999 as this DBANK05P
010500* will cause a change in the date.                                DBANK05P
010600     IF CD05-START-HIGH                                           DBANK05P
010700        COMPUTE WS-WORK1 =                                        DBANK05P
010800                (WS-BNKTXN-AIX1-RID-HH * 60 * 60 * 1000000) +     DBANK05P
010900                (WS-BNKTXN-AIX1-RID-MM * 60 * 1000000) +          DBANK05P
011000                (WS-BNKTXN-AIX1-RID-SS * 1000000) +               DBANK05P
011100                WS-BNKTXN-AIX1-RID-DEC                            DBANK05P
011200        ADD 1 TO WS-WORK1                                         DBANK05P
011300        DIVIDE 1000000 INTO WS-WORK1                              DBANK05P
011400          GIVING WS-WORK2                                         DBANK05P
011500            REMAINDER WS-BNKTXN-AIX1-RID-DEC                      DBANK05P
011600        MOVE WS-WORK2 TO WS-WORK1                                 DBANK05P
011700        DIVIDE 60 INTO WS-WORK1                                   DBANK05P
011800          GIVING WS-WORK2                                         DBANK05P
011900            REMAINDER WS-BNKTXN-AIX1-RID-SS                       DBANK05P
012000        MOVE WS-WORK2 TO WS-WORK1                                 DBANK05P
012100        DIVIDE 60 INTO WS-WORK1                                   DBANK05P
012200          GIVING WS-WORK2                                         DBANK05P
012300            REMAINDER WS-BNKTXN-AIX1-RID-MM                       DBANK05P
012400        MOVE WS-WORK2 TO WS-WORK1                                 DBANK05P
012500        MOVE WS-WORK1 TO WS-BNKTXN-AIX1-RID-HH                    DBANK05P
012600     END-IF.                                                      DBANK05P
012700     EXEC CICS STARTBR FILE('BNKTXN1')                            DBANK05P
012800                       RIDFLD(WS-BNKTXN-AIX1-RID)                 DBANK05P
012900                       GTEQ                                       DBANK05P
013000     END-EXEC.                                                    DBANK05P
013100                                                                  DBANK05P
013200     MOVE 0 TO WS-SUB1.                                           DBANK05P
013300                                                                  DBANK05P
013400***************************************************************** DBANK05P
013500* Now attempt to get the requested records                      * DBANK05P
013600***************************************************************** DBANK05P
013700 TRANSACTION-FETCH-LOOP.                                          DBANK05P
013800     ADD 1 TO WS-SUB1.                                            DBANK05P
013900     IF WS-SUB1 IS GREATER THAN 9                                 DBANK05P
014000        SET CD05-IS-MORE-DATA TO TRUE                             DBANK05P
014100        GO TO TRANSACTION-FETCH-LOOP-EXIT                         DBANK05P
014200     END-IF.                                                      DBANK05P
014300     IF CD05-START-EQUAL OR                                       DBANK05P
014400        CD05-START-HIGH                                           DBANK05P
014500        EXEC CICS READNEXT FILE('BNKTXN1')                        DBANK05P
014600                           INTO(WS-BNKTXN-REC)                    DBANK05P
014700                           LENGTH(LENGTH OF WS-BNKTXN-REC)        DBANK05P
014800                           RIDFLD(WS-BNKTXN-AIX1-RID)             DBANK05P
014900                           RESP(WS-RESP)                          DBANK05P
015000        END-EXEC                                                  DBANK05P
015100     END-IF.                                                      DBANK05P
015200* If we are reading 'low' then we need to read backwards. This is DBANK05P
015300* OK except we want the record prior to the on with the provided  DBANK05P
015400* key so we throw away the 1st record.                            DBANK05P
015500     IF CD05-START-LOW                                            DBANK05P
015600        EXEC CICS READPREV FILE('BNKTXN1')                        DBANK05P
015700                           INTO(WS-BNKTXN-REC)                    DBANK05P
015800                           LENGTH(LENGTH OF WS-BNKTXN-REC)        DBANK05P
015900                           RIDFLD(WS-BNKTXN-AIX1-RID)             DBANK05P
016000                           RESP(WS-RESP)                          DBANK05P
016100        END-EXEC                                                  DBANK05P
016200        IF WS-SUB1 IS EQUAL TO 1                                  DBANK05P
016300           EXEC CICS READPREV FILE('BNKTXN1')                     DBANK05P
016400                              INTO(WS-BNKTXN-REC)                 DBANK05P
016500                              LENGTH(LENGTH OF WS-BNKTXN-REC)     DBANK05P
016600                              RIDFLD(WS-BNKTXN-AIX1-RID)          DBANK05P
016700                              RESP(WS-RESP)                       DBANK05P
016800           END-EXEC                                               DBANK05P
016900        END-IF                                                    DBANK05P
017000     END-IF.                                                      DBANK05P
017100                                                                  DBANK05P
017200***************************************************************** DBANK05P
017300* Did we get the record OK                                      * DBANK05P
017400***************************************************************** DBANK05P
017500     IF CD05-START-LOW                                            DBANK05P
017600        IF WS-SUB1 IS GREATER THAN 8                              DBANK05P
017700           MOVE WS-SUB1 TO WS-SUB2                                DBANK05P
017800        ELSE                                                      DBANK05P
017900           SUBTRACT WS-SUB1 FROM 9 GIVING WS-SUB2                 DBANK05P
018000        END-IF                                                    DBANK05P
018100     ELSE                                                         DBANK05P
018200        MOVE WS-SUB1 TO WS-SUB2                                   DBANK05P
018300     END-IF.                                                      DBANK05P
018400     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DBANK05P
018500        IF CD05I-ACC IS EQUAL TO BTX-REC-ACCNO                    DBANK05P
018600           SET CD05-IS-DATA TO TRUE                               DBANK05P
018700           MOVE BTX-REC-TIMESTAMP TO CD05O-ID (WS-SUB2)           DBANK05P
018800           MOVE BTX-REC-AMOUNT TO CD05O-AMT-N (WS-SUB2)           DBANK05P
018900           MOVE BTX-REC-DATA-OLD TO TXN-T1-OLD                    DBANK05P
019000           MOVE TXN-T1-OLD-DESC TO CD05O-DESC (WS-SUB2)           DBANK05P
019100           GO TO TRANSACTION-FETCH-LOOP                           DBANK05P
019200        ELSE                                                      DBANK05P
019300           SET CD05-NO-MORE-DATA TO TRUE                          DBANK05P
019400           GO TO TRANSACTION-FETCH-LOOP-EXIT                      DBANK05P
019500        END-IF                                                    DBANK05P
019600     ELSE                                                         DBANK05P
019700        SET CD05-NO-MORE-DATA TO TRUE                             DBANK05P
019800        GO TO TRANSACTION-FETCH-LOOP-EXIT                         DBANK05P
019900     END-IF.                                                      DBANK05P
020000                                                                  DBANK05P
020100 TRANSACTION-FETCH-LOOP-EXIT.                                     DBANK05P
020200     EXEC CICS ENDBR FILE('BNKTXN1')                              DBANK05P
020300     END-EXEC.                                                    DBANK05P
020400                                                                  DBANK05P
020500***************************************************************** DBANK05P
020600* Move the result back to the callers area                      * DBANK05P
020700***************************************************************** DBANK05P
020800     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK05P
020900                                                                  DBANK05P
021000***************************************************************** DBANK05P
021100* Return to our caller                                          * DBANK05P
021200***************************************************************** DBANK05P
021300 COPY CRETURN.                                                    DBANK05P
021400                                                                  DBANK05P
021500* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK05P
