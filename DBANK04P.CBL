000100***************************************************************** DBANK04P
000200*                                                               * DBANK04P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK04P
000400*   This demonstration program is provided for use by users     * DBANK04P
000500*   of Micro Focus products and may be used, modified and       * DBANK04P
000600*   distributed as part of your application provided that       * DBANK04P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK04P
000800*   in this material.                                           * DBANK04P
000900*                                                               * DBANK04P
001000***************************************************************** DBANK04P
001100                                                                  DBANK04P
001200***************************************************************** DBANK04P
001300* Program:     DBANK04P.CBL                                     * DBANK04P
001400* Function:    Update acount balances                           * DBANK04P
001500*              VSAM Version                                     * DBANK04P
001600***************************************************************** DBANK04P
001700                                                                  DBANK04P
001800 IDENTIFICATION DIVISION.                                         DBANK04P
001900 PROGRAM-ID.                                                      DBANK04P
002000     DBANK04P.                                                    DBANK04P
002100 DATE-WRITTEN.                                                    DBANK04P
002200     September 2002.                                              DBANK04P
002300 DATE-COMPILED.                                                   DBANK04P
002400     Today.                                                       DBANK04P
002500                                                                  DBANK04P
002600 ENVIRONMENT DIVISION.                                            DBANK04P
002700                                                                  DBANK04P
002800 DATA DIVISION.                                                   DBANK04P
002900                                                                  DBANK04P
003000 WORKING-STORAGE SECTION.                                         DBANK04P
003100 01  WS-MISC-STORAGE.                                             DBANK04P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK04P
003300       VALUE 'DBANK04P'.                                          DBANK04P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK04P
003500   05  WS-READ-TOKEN-FROM                    PIC S9(8) COMP.      DBANK04P
003600   05  WS-READ-TOKEN-TO                      PIC S9(8) COMP.      DBANK04P
003700   05  WS-RESP                               PIC S9(8) COMP.      DBANK04P
003800   05  WS-BNKACC-FROM-RID                    PIC X(9).            DBANK04P
003900   05  WS-BNKACC-TO-RID                      PIC X(9).            DBANK04P
004000                                                                  DBANK04P
004100 COPY CTSTAMPD.                                                   DBANK04P
004200                                                                  DBANK04P
004300 01  WS-BNKACC-FROM-REC.                                          DBANK04P
004400 COPY CBANKVAC.                                                   DBANK04P
004500                                                                  DBANK04P
004600 01  WS-BNKACC-TO-REC.                                            DBANK04P
004700 COPY CBANKVAC.                                                   DBANK04P
004800                                                                  DBANK04P
004900 01  WS-COMMAREA.                                                 DBANK04P
005000 COPY CBANKD04.                                                   DBANK04P
005100                                                                  DBANK04P
005200 COPY CABENDD.                                                    DBANK04P
005300                                                                  DBANK04P
005400 LINKAGE SECTION.                                                 DBANK04P
005500 01  DFHCOMMAREA.                                                 DBANK04P
005600   05  LK-COMMAREA                           PIC X(1)             DBANK04P
005700       OCCURS 1 TO 4096 TIMES                                     DBANK04P
005800         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK04P
005900                                                                  DBANK04P
006000 COPY CENTRY.                                                     DBANK04P
006100***************************************************************** DBANK04P
006200* Move the passed data to our area                              * DBANK04P
006300***************************************************************** DBANK04P
006400     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK04P
006500     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK04P
006600                                                                  DBANK04P
006700***************************************************************** DBANK04P
006800* Initialize our output area                                    * DBANK04P
006900***************************************************************** DBANK04P
007000     MOVE SPACES TO CD04O-DATA.                                   DBANK04P
007100     SET CD04O-UPDATE-FAIL TO TRUE.                               DBANK04P
007200     MOVE '0001-01-01-00.000.00.00000' TO CD04O-TIMESTAMP.        DBANK04P
007300                                                                  DBANK04P
007400***************************************************************** DBANK04P
007500* Try to the the 'from' account to check the balance            * DBANK04P
007600***************************************************************** DBANK04P
007700     MOVE CD04I-FROM-ACC TO WS-BNKACC-FROM-RID.                   DBANK04P
007800     EXEC CICS READ FILE('BNKACC')                                DBANK04P
007900                    UPDATE                                        DBANK04P
008000                    INTO(WS-BNKACC-FROM-REC)                      DBANK04P
008100                    LENGTH(LENGTH OF WS-BNKACC-FROM-REC)          DBANK04P
008200                    RIDFLD(WS-BNKACC-FROM-RID)                    DBANK04P
008300                    TOKEN(WS-READ-TOKEN-FROM)                     DBANK04P
008400                    RESP(WS-RESP)                                 DBANK04P
008500     END-EXEC.                                                    DBANK04P
008600                                                                  DBANK04P
008700***************************************************************** DBANK04P
008800* Did we get the record OK                                      * DBANK04P
008900***************************************************************** DBANK04P
009000     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK04P
009100        MOVE 'Unable to read FROM account details'                DBANK04P
009200          TO CD04O-MSG                                            DBANK04P
009300        GO TO DBANK04P-EXIT                                       DBANK04P
009400     END-IF.                                                      DBANK04P
009500     IF CD04I-FROM-OLD-BAL IS NOT EQUAL TO                        DBANK04P
009600          BAC-REC-BALANCE IN WS-BNKACC-FROM-REC                   DBANK04P
009700        MOVE 'FROM account balance has changed'                   DBANK04P
009800          TO CD04O-MSG                                            DBANK04P
009900        GO TO DBANK04P-EXIT                                       DBANK04P
010000     END-IF.                                                      DBANK04P
010100                                                                  DBANK04P
010200***************************************************************** DBANK04P
010300* Try to the the 'to' account to check the balance              * DBANK04P
010400***************************************************************** DBANK04P
010500     MOVE CD04I-TO-ACC TO WS-BNKACC-TO-RID.                       DBANK04P
010600     EXEC CICS READ FILE('BNKACC')                                DBANK04P
010700                    UPDATE                                        DBANK04P
010800                    INTO(WS-BNKACC-TO-REC)                        DBANK04P
010900                    LENGTH(LENGTH OF WS-BNKACC-TO-REC)            DBANK04P
011000                    RIDFLD(WS-BNKACC-TO-RID)                      DBANK04P
011100                    TOKEN(WS-READ-TOKEN-TO)                       DBANK04P
011200                    RESP(WS-RESP)                                 DBANK04P
011300     END-EXEC.                                                    DBANK04P
011400                                                                  DBANK04P
011500***************************************************************** DBANK04P
011600* Did we get the record OK                                      * DBANK04P
011700***************************************************************** DBANK04P
011800     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK04P
011900        MOVE 'Unable to read TO account details'                  DBANK04P
012000          TO CD04O-MSG                                            DBANK04P
012100        GO TO DBANK04P-EXIT                                       DBANK04P
012200     END-IF.                                                      DBANK04P
012300     IF CD04I-TO-OLD-BAL IS NOT EQUAL TO                          DBANK04P
012400          BAC-REC-BALANCE IN WS-BNKACC-TO-REC                     DBANK04P
012500        MOVE 'TO account balance has changed'                     DBANK04P
012600          TO CD04O-MSG                                            DBANK04P
012700        GO TO DBANK04P-EXIT                                       DBANK04P
012800     END-IF.                                                      DBANK04P
012900                                                                  DBANK04P
013000***************************************************************** DBANK04P
013100* Try to update the records                                     * DBANK04P
013200***************************************************************** DBANK04P
013300     MOVE CD04I-FROM-NEW-BAL                                      DBANK04P
013400       TO BAC-REC-BALANCE IN WS-BNKACC-FROM-REC.                  DBANK04P
013500     EXEC CICS REWRITE FILE('BNKACC')                             DBANK04P
013600                       FROM(WS-BNKACC-FROM-REC)                   DBANK04P
013700                       LENGTH(LENGTH OF WS-BNKACC-FROM-REC)       DBANK04P
013800                       TOKEN(WS-READ-TOKEN-FROM)                  DBANK04P
013900                       RESP(WS-RESP)                              DBANK04P
014000     END-EXEC.                                                    DBANK04P
014100     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK04P
014200        MOVE 'Unable to rewrite FROM account details'             DBANK04P
014300          TO CD04O-MSG                                            DBANK04P
014400        GO TO DBANK04P-EXIT                                       DBANK04P
014500     END-IF.                                                      DBANK04P
014600                                                                  DBANK04P
014700     MOVE CD04I-TO-NEW-BAL                                        DBANK04P
014800       TO BAC-REC-BALANCE IN WS-BNKACC-TO-REC.                    DBANK04P
014900     EXEC CICS REWRITE FILE('BNKACC')                             DBANK04P
015000                       FROM(WS-BNKACC-TO-REC)                     DBANK04P
015100                       LENGTH(LENGTH OF WS-BNKACC-TO-REC)         DBANK04P
015200                       TOKEN(WS-READ-TOKEN-TO)                    DBANK04P
015300                       RESP(WS-RESP)                              DBANK04P
015400     END-EXEC.                                                    DBANK04P
015500     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK04P
015600        MOVE 'Unable to rewrite TO account details'               DBANK04P
015700          TO CD04O-MSG                                            DBANK04P
015800        GO TO DBANK04P-EXIT                                       DBANK04P
015900     END-IF.                                                      DBANK04P
016000                                                                  DBANK04P
016100***************************************************************** DBANK04P
016200* If we got this far then the accounts should have been updated * DBANK04P
016300***************************************************************** DBANK04P
016400* Simulate SQL TIMESTAMP function                                 DBANK04P
016500 COPY CTSTAMPP.                                                   DBANK04P
016600     MOVE WS-TIMESTAMP TO CD04O-TIMESTAMP.                        DBANK04P
016700     SET CD04O-UPDATE-OK TO TRUE.                                 DBANK04P
016800                                                                  DBANK04P
016900 DBANK04P-EXIT.                                                   DBANK04P
017000***************************************************************** DBANK04P
017100* Move the result back to the callers area                      * DBANK04P
017200***************************************************************** DBANK04P
017300     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK04P
017400                                                                  DBANK04P
017500***************************************************************** DBANK04P
017600* Return to our caller                                          * DBANK04P
017700***************************************************************** DBANK04P
017800 COPY CRETURN.                                                    DBANK04P
017900                                                                  DBANK04P
018000* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK04P
