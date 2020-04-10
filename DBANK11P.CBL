000100***************************************************************** DBANK11P
000200*                                                               * DBANK11P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK11P
000400*   This demonstration program is provided for use by users     * DBANK11P
000500*   of Micro Focus products and may be used, modified and       * DBANK11P
000600*   distributed as part of your application provided that       * DBANK11P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK11P
000800*   in this material.                                           * DBANK11P
000900*                                                               * DBANK11P
001000***************************************************************** DBANK11P
001100                                                                  DBANK11P
001200***************************************************************** DBANK11P
001300* Program:     DBANK11P.CBL                                     * DBANK11P
001400* Function:    Obtain Bank Account Details (Extended)           * DBANK11P
001500*              VSAM Version                                     * DBANK11P
001600***************************************************************** DBANK11P
001700                                                                  DBANK11P
001800 IDENTIFICATION DIVISION.                                         DBANK11P
001900 PROGRAM-ID.                                                      DBANK11P
002000     DBANK11P.                                                    DBANK11P
002100 DATE-WRITTEN.                                                    DBANK11P
002200     September 2002.                                              DBANK11P
002300 DATE-COMPILED.                                                   DBANK11P
002400     Today.                                                       DBANK11P
002500                                                                  DBANK11P
002600 ENVIRONMENT DIVISION.                                            DBANK11P
002700                                                                  DBANK11P
002800 DATA DIVISION.                                                   DBANK11P
002900                                                                  DBANK11P
003000 WORKING-STORAGE SECTION.                                         DBANK11P
003100 01  WS-MISC-STORAGE.                                             DBANK11P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK11P
003300       VALUE 'DBANK11P'.                                          DBANK11P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK11P
003500   05  WS-RESP                               PIC S9(8) COMP.      DBANK11P
003600   05  WS-BNKACC-RID                         PIC X(9).            DBANK11P
003700   05  WS-BNKTXN-AIX1-RID                    PIC X(35).           DBANK11P
003800   05  WS-BNKTXN-AIX1-RID-R REDEFINES WS-BNKTXN-AIX1-RID.         DBANK11P
003900     10  WS-BNKTXN-AIX1-RID-ACC              PIC X(9).            DBANK11P
004000     10  WS-BNKTXN-AIX1-RID-STAMP.                                DBANK11P
004100       15  WS-BNKTXN-AIX1-RID-DATE           PIC X(10).           DBANK11P
004200       15  WS-BNKTXN-AIX1-RID-DOT1           PIC X(1).            DBANK11P
004300       15  WS-BNKTXN-AIX1-RID-TIME           PIC X(15).           DBANK11P
004400       15  WS-BNKTXN-AIX1-RID-TIME-R REDEFINES                    DBANK11P
004500             WS-BNKTXN-AIX1-RID-TIME.                             DBANK11P
004600         20  WS-BNKTXN-AIX1-RID-HH           PIC 9(2).            DBANK11P
004700         20  WS-BNKTXN-AIX1-RID-DOT2         PIC X(1).            DBANK11P
004800         20  WS-BNKTXN-AIX1-RID-MM           PIC 9(2).            DBANK11P
004900         20  WS-BNKTXN-AIX1-RID-DOT3         PIC X(1).            DBANK11P
005000         20  WS-BNKTXN-AIX1-RID-SS           PIC 9(2).            DBANK11P
005100         20  WS-BNKTXN-AIX1-RID-DOT4         PIC X(1).            DBANK11P
005200         20  WS-BNKTXN-AIX1-RID-DEC          PIC 9(6).            DBANK11P
005300   05  WS-TRANS-COUNT                        PIC S9(10) COMP-3.   DBANK11P
005400   05  WS-TRANS-EDIT                         PIC Z(6)9.           DBANK11P
005500   05  WS-TRANS-EDIT-X REDEFINES WS-TRANS-EDIT                    DBANK11P
005600                                             PIC X(7).            DBANK11P
005700                                                                  DBANK11P
005800 COPY CTSTAMPD.                                                   DBANK11P
005900                                                                  DBANK11P
006000 01  WS-BNKACC-REC.                                               DBANK11P
006100 COPY CBANKVAC.                                                   DBANK11P
006200                                                                  DBANK11P
006300 01  WS-BNKTXN-REC.                                               DBANK11P
006400 COPY CBANKVTX.                                                   DBANK11P
006500                                                                  DBANK11P
006600 01  WS-COMMAREA.                                                 DBANK11P
006700 COPY CBANKD11.                                                   DBANK11P
006800                                                                  DBANK11P
006900 COPY CABENDD.                                                    DBANK11P
007000                                                                  DBANK11P
007100 LINKAGE SECTION.                                                 DBANK11P
007200 01  DFHCOMMAREA.                                                 DBANK11P
007300   05  LK-COMMAREA                           PIC X(1)             DBANK11P
007400       OCCURS 1 TO 6144 TIMES                                     DBANK11P
007500         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK11P
007600                                                                  DBANK11P
007700 COPY CENTRY.                                                     DBANK11P
007800***************************************************************** DBANK11P
007900* Move the passed data to our area                              * DBANK11P
008000***************************************************************** DBANK11P
008100     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK11P
008200     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK11P
008300                                                                  DBANK11P
008400***************************************************************** DBANK11P
008500* Initialize our output area                                    * DBANK11P
008600***************************************************************** DBANK11P
008700     MOVE SPACES TO CD11O-DATA.                                   DBANK11P
008800                                                                  DBANK11P
008900***************************************************************** DBANK11P
009000* Now attempt to get the requested record                       * DBANK11P
009100***************************************************************** DBANK11P
009200     MOVE CD11I-ACCNO TO WS-BNKACC-RID.                           DBANK11P
009300     EXEC CICS READ FILE('BNKACC')                                DBANK11P
009400                    INTO(WS-BNKACC-REC)                           DBANK11P
009500                    LENGTH(LENGTH OF WS-BNKACC-REC)               DBANK11P
009600                    RIDFLD(WS-BNKACC-RID)                         DBANK11P
009700                    RESP(WS-RESP)                                 DBANK11P
009800     END-EXEC.                                                    DBANK11P
009900                                                                  DBANK11P
010000***************************************************************** DBANK11P
010100* Did we get the record OK                                      * DBANK11P
010200***************************************************************** DBANK11P
010300     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK11P
010400        MOVE SPACES TO CD11O-ACCNO                                DBANK11P
010500        GO TO FINISH                                              DBANK11P
010600     END-IF.                                                      DBANK11P
010700                                                                  DBANK11P
010800***************************************************************** DBANK11P
010900* We got the record OK                                          * DBANK11P
011000***************************************************************** DBANK11P
011100     MOVE BAC-REC-ACCNO TO CD11O-ACCNO.                           DBANK11P
011200     MOVE ALL '?' TO CD11O-DESC.                                  DBANK11P
011300     MOVE BAC-REC-BALANCE TO CD11O-BAL-N.                         DBANK11P
011400     MOVE BAC-REC-LAST-STMT-DTE TO CD11O-DTE.                     DBANK11P
011500     MOVE 'No' TO CD11O-TRANS.                                    DBANK11P
011600     MOVE BAC-REC-ATM-ENABLED TO CD11O-ATM-ENABLED.               DBANK11P
011700     MOVE BAC-REC-ATM-DAY-LIMIT TO CD11O-ATM-LIM-N.               DBANK11P
011800     MOVE BAC-REC-ATM-DAY-DTE TO CD11O-ATM-LDTE.                  DBANK11P
011900     MOVE BAC-REC-ATM-DAY-AMT TO CD11O-ATM-LAMT-N.                DBANK11P
012000     MOVE BAC-REC-RP1-DAY TO CD11O-RP1DAY.                        DBANK11P
012100     MOVE BAC-REC-RP1-AMOUNT TO CD11O-RP1AMT-N.                   DBANK11P
012200     MOVE BAC-REC-RP1-PID TO CD11O-RP1PID.                        DBANK11P
012300     MOVE BAC-REC-RP1-ACCNO TO CD11O-RP1ACC.                      DBANK11P
012400     MOVE BAC-REC-RP1-LAST-PAY TO CD11O-RP1DTE.                   DBANK11P
012500     MOVE BAC-REC-RP2-DAY TO CD11O-RP2DAY.                        DBANK11P
012600     MOVE BAC-REC-RP2-AMOUNT TO CD11O-RP2AMT-N.                   DBANK11P
012700     MOVE BAC-REC-RP2-PID TO CD11O-RP2PID.                        DBANK11P
012800     MOVE BAC-REC-RP2-ACCNO TO CD11O-RP2ACC.                      DBANK11P
012900     MOVE BAC-REC-RP2-LAST-PAY TO CD11O-RP2DTE.                   DBANK11P
013000     MOVE BAC-REC-RP3-DAY TO CD11O-RP3DAY.                        DBANK11P
013100     MOVE BAC-REC-RP3-AMOUNT TO CD11O-RP3AMT-N.                   DBANK11P
013200     MOVE BAC-REC-RP3-PID TO CD11O-RP3PID.                        DBANK11P
013300     MOVE BAC-REC-RP3-ACCNO TO CD11O-RP3ACC.                      DBANK11P
013400     MOVE BAC-REC-RP3-LAST-PAY TO CD11O-RP3DTE.                   DBANK11P
013500                                                                  DBANK11P
013600***************************************************************** DBANK11P
013700* Check for transactions                                        * DBANK11P
013800***************************************************************** DBANK11P
013900 BROWSE-START.                                                    DBANK11P
014000     MOVE 0 TO WS-TRANS-COUNT.                                    DBANK11P
014100     MOVE LOW-VALUES TO WS-BNKTXN-AIX1-RID.                       DBANK11P
014200     MOVE CD11I-ACCNO TO WS-BNKTXN-AIX1-RID-ACC.                  DBANK11P
014300     EXEC CICS STARTBR FILE('BNKTXN1')                            DBANK11P
014400                       RIDFLD(WS-BNKTXN-AIX1-RID)                 DBANK11P
014500                       GTEQ                                       DBANK11P
014600     END-EXEC.                                                    DBANK11P
014700     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK11P
014800        GO TO BROWSE-STOP                                         DBANK11P
014900     END-IF.                                                      DBANK11P
015000 BROWSE-LOOP.                                                     DBANK11P
015100     EXEC CICS READNEXT FILE('BNKTXN1')                           DBANK11P
015200                        INTO(WS-BNKTXN-REC)                       DBANK11P
015300                        LENGTH(LENGTH OF WS-BNKTXN-REC)           DBANK11P
015400                        RIDFLD(WS-BNKTXN-AIX1-RID)                DBANK11P
015500                        RESP(WS-RESP)                             DBANK11P
015600     END-EXEC.                                                    DBANK11P
015700     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DBANK11P
015800        IF CD11I-ACCNO IS EQUAL TO BTX-REC-ACCNO                  DBANK11P
015900           ADD 1 TO WS-TRANS-COUNT                                DBANK11P
016000           GO TO BROWSE-LOOP                                      DBANK11P
016100        END-IF                                                    DBANK11P
016200     END-IF.                                                      DBANK11P
016300                                                                  DBANK11P
016400 BROWSE-STOP.                                                     DBANK11P
016500     EXEC CICS ENDBR FILE('BNKTXN1')                              DBANK11P
016600     END-EXEC.                                                    DBANK11P
016700                                                                  DBANK11P
016800     IF WS-TRANS-COUNT IS EQUAL TO 0                              DBANK11P
016900        MOVE 'No' TO CD11O-TRANS                                  DBANK11P
017000     ELSE                                                         DBANK11P
017100        MOVE WS-TRANS-COUNT TO WS-TRANS-EDIT                      DBANK11P
017200        PERFORM TRANS-LEFT-JUST                                   DBANK11P
017300        MOVE WS-TRANS-EDIT-X TO CD11O-TRANS                       DBANK11P
017400     END-IF.                                                      DBANK11P
017500                                                                  DBANK11P
017600***************************************************************** DBANK11P
017700* Move the result back to the callers area                      * DBANK11P
017800***************************************************************** DBANK11P
017900     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK11P
018000                                                                  DBANK11P
018100***************************************************************** DBANK11P
018200* Return to our caller                                          * DBANK11P
018300***************************************************************** DBANK11P
018400 FINISH.                                                          DBANK11P
018500 COPY CRETURN.                                                    DBANK11P
018600                                                                  DBANK11P
018700 TRANS-LEFT-JUST.                                                 DBANK11P
018800     IF WS-TRANS-EDIT-X(1:1) IS EQUAL TO SPACE                    DBANK11P
018900        MOVE WS-TRANS-EDIT-X(2:LENGTH OF WS-TRANS-EDIT-X - 1)     DBANK11P
019000          TO WS-TRANS-EDIT-X(1:LENGTH OF WS-TRANS-EDIT-X - 1)     DBANK11P
019100        MOVE SPACE                                                DBANK11P
019200          TO WS-TRANS-EDIT-X(LENGTH OF WS-TRANS-EDIT-X:1)         DBANK11P
019300        GO TO TRANS-LEFT-JUST.                                    DBANK11P
019400                                                                  DBANK11P
019500* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK11P
