000100***************************************************************** BBANK35P
000200*                                                               * BBANK35P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * BBANK35P
000400*   This demonstration program is provided for use by users     * BBANK35P
000500*   of Micro Focus products and may be used, modified and       * BBANK35P
000600*   distributed as part of your application provided that       * BBANK35P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK35P
000800*   in this material.                                           * BBANK35P
000900*                                                               * BBANK35P
001000***************************************************************** BBANK35P
001100                                                                  BBANK35P
001200***************************************************************** BBANK35P
001300* Program:     BBANK35P.CBL                                     * BBANK35P
001400* Layer:       Business logic                                   * BBANK35P
001500* Function:    Populate details of specific account             * BBANK35P
001600***************************************************************** BBANK35P
001700                                                                  BBANK35P
001800 IDENTIFICATION DIVISION.                                         BBANK35P
001900 PROGRAM-ID.                                                      BBANK35P
002000     BBANK35P.                                                    BBANK35P
002100 DATE-WRITTEN.                                                    BBANK35P
002200     September 2002.                                              BBANK35P
002300 DATE-COMPILED.                                                   BBANK35P
002400     Today.                                                       BBANK35P
002500                                                                  BBANK35P
002600 ENVIRONMENT DIVISION.                                            BBANK35P
002700                                                                  BBANK35P
002800 DATA DIVISION.                                                   BBANK35P
002900 WORKING-STORAGE SECTION.                                         BBANK35P
003000 01  WS-MISC-STORAGE.                                             BBANK35P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK35P
003200       VALUE 'BBANK35P'.                                          BBANK35P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK35P
003400     88  INPUT-OK                            VALUE '0'.           BBANK35P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK35P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK35P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK35P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK35P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK35P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK35P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK35P
004200     88  PFK-VALID                           VALUE '0'.           BBANK35P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK35P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK35P
004500   05  WS-EDIT-AMT-3                         PIC ZZ9.             BBANK35P
004600   05  WS-EDIT-AMT-5-2                       PIC ZZ,ZZ9.99-.      BBANK35P
004700   05  WS-EDIT-AMT-7-2                       PIC Z,ZZZ,ZZ9.99-.   BBANK35P
004800   05  WS-DYNAMIC-PGM                        PIC X(8)             BBANK35P
004900       VALUE 'UNKNOWN'.                                           BBANK35P
005000   05  WS-SUB1                               PIC S9(4) COMP.      BBANK35P
005100   05  WS-SUB-LIMIT                          PIC S9(4) COMP.      BBANK35P
005200                                                                  BBANK35P
005300 01  WS-TIME-DATE-WORK-AREA.                                      BBANK35P
005400 COPY CDATED.                                                     BBANK35P
005500                                                                  BBANK35P
005600 01  WS-BANK-DATA.                                                BBANK35P
005700 COPY CBANKDAT.                                                   BBANK35P
005800                                                                  BBANK35P
005900 01  WS-HELP-DATA.                                                BBANK35P
006000 COPY CHELPD01.                                                   BBANK35P
006100                                                                  BBANK35P
006200 01  WS-TXN-LIST.                                                 BBANK35P
006300 COPY CBANKD11.                                                   BBANK35P
006400                                                                  BBANK35P
006500 COPY CABENDD.                                                    BBANK35P
006600                                                                  BBANK35P
006700 LINKAGE SECTION.                                                 BBANK35P
006800 01  DFHCOMMAREA.                                                 BBANK35P
006900   05  LK-COMMAREA                           PIC X(6144).         BBANK35P
007000                                                                  BBANK35P
007100 COPY CENTRY.                                                     BBANK35P
007200***************************************************************** BBANK35P
007300* Make ourselves re-entrant                                     * BBANK35P
007400***************************************************************** BBANK35P
007500     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK35P
007600                                                                  BBANK35P
007700***************************************************************** BBANK35P
007800* Move the passed area to our area                              * BBANK35P
007900***************************************************************** BBANK35P
008000     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK35P
008100                                                                  BBANK35P
008200***************************************************************** BBANK35P
008300* Ensure error message is cleared                               * BBANK35P
008400***************************************************************** BBANK35P
008500     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK35P
008600                                                                  BBANK35P
008700***************************************************************** BBANK35P
008800* This is the main process                                      * BBANK35P
008900***************************************************************** BBANK35P
009000                                                                  BBANK35P
009100***************************************************************** BBANK35P
009200* Save the passed return flag and then turn it off              * BBANK35P
009300***************************************************************** BBANK35P
009400     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     BBANK35P
009500     SET BANK-RETURN-FLAG-OFF TO TRUE.                            BBANK35P
009600                                                                  BBANK35P
009700***************************************************************** BBANK35P
009800* Check the AID to see if its valid at this point               * BBANK35P
009900***************************************************************** BBANK35P
010000     SET PFK-INVALID TO TRUE.                                     BBANK35P
010100     IF BANK-AID-ENTER OR                                         BBANK35P
010200        BANK-AID-PFK03 OR                                         BBANK35P
010300        BANK-AID-PFK04                                            BBANK35P
010400        SET PFK-VALID TO TRUE                                     BBANK35P
010500     END-IF.                                                      BBANK35P
010600     IF BANK-AID-PFK01 AND                                        BBANK35P
010700        BANK-HELP-INACTIVE                                        BBANK35P
010800        SET BANK-HELP-ACTIVE TO TRUE                              BBANK35P
010900        SET PFK-VALID TO TRUE                                     BBANK35P
011000     END-IF.                                                      BBANK35P
011100     IF PFK-INVALID                                               BBANK35P
011200        SET BANK-AID-ENTER TO TRUE                                BBANK35P
011300     END-IF.                                                      BBANK35P
011400                                                                  BBANK35P
011500***************************************************************** BBANK35P
011600* Check the AID to see if we have to quit                       * BBANK35P
011700***************************************************************** BBANK35P
011800     IF BANK-AID-PFK03                                            BBANK35P
011900        MOVE 'BBANK35P' TO BANK-LAST-PROG                         BBANK35P
012000        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK35P
012100        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK35P
012200        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK35P
012300        GO TO COMMON-RETURN                                       BBANK35P
012400     END-IF.                                                      BBANK35P
012500                                                                  BBANK35P
012600***************************************************************** BBANK35P
012700* Check the to see if user needs or has been using help         * BBANK35P
012800***************************************************************** BBANK35P
012900     IF BANK-HELP-ACTIVE                                          BBANK35P
013000        IF BANK-AID-PFK04                                         BBANK35P
013100           SET BANK-HELP-INACTIVE TO TRUE                         BBANK35P
013200           MOVE 00 TO BANK-HELP-SCREEN                            BBANK35P
013300           MOVE 'BBANK35P' TO BANK-LAST-PROG                      BBANK35P
013400           MOVE 'BBANK35P' TO BANK-NEXT-PROG                      BBANK35P
013500           MOVE 'MBANK35' TO BANK-LAST-MAPSET                     BBANK35P
013600           MOVE 'HELP35A' TO BANK-LAST-MAP                        BBANK35P
013700           MOVE 'MBANK35' TO BANK-NEXT-MAPSET                     BBANK35P
013800           MOVE 'BANK35A' TO BANK-NEXT-MAP                        BBANK35P
013900           GO TO COMMON-RETURN                                    BBANK35P
014000        ELSE                                                      BBANK35P
014100           MOVE 01 TO BANK-HELP-SCREEN                            BBANK35P
014200           MOVE 'BBANK35P' TO BANK-LAST-PROG                      BBANK35P
014300           MOVE 'BBANK35P' TO BANK-NEXT-PROG                      BBANK35P
014400           MOVE 'MBANK35' TO BANK-LAST-MAPSET                     BBANK35P
014500           MOVE 'BANK35A' TO BANK-LAST-MAP                        BBANK35P
014600           MOVE 'MBANK35' TO BANK-NEXT-MAPSET                     BBANK35P
014700           MOVE 'HELP35A' TO BANK-NEXT-MAP                        BBANK35P
014800           MOVE 'BANK35' TO HELP01I-SCRN                          BBANK35P
014900           COPY CHELPX01.                                         BBANK35P
015000           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK35P
015100           GO TO COMMON-RETURN                                    BBANK35P
015200     END-IF.                                                      BBANK35P
015300                                                                  BBANK35P
015400***************************************************************** BBANK35P
015500* Check the AID to see if we have to return to previous screen  * BBANK35P
015600***************************************************************** BBANK35P
015700     IF BANK-AID-PFK04                                            BBANK35P
015800        MOVE 'BBANK35P' TO BANK-LAST-PROG                         BBANK35P
015900        MOVE 'BBANK30P' TO BANK-NEXT-PROG                         BBANK35P
016000        MOVE 'MBANK30' TO BANK-NEXT-MAPSET                        BBANK35P
016100        MOVE 'BANK30A' TO BANK-NEXT-MAP                           BBANK35P
016200        SET BANK-AID-ENTER TO TRUE                                BBANK35P
016300        GO TO COMMON-RETURN                                       BBANK35P
016400     END-IF.                                                      BBANK35P
016500                                                                  BBANK35P
016600* Check if we have set the screen up before or is this 1st time   BBANK35P
016700     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK35'                BBANK35P
016800        MOVE 'BBANK35P' TO BANK-LAST-PROG                         BBANK35P
016900        MOVE 'BBANK35P' TO BANK-NEXT-PROG                         BBANK35P
017000        MOVE 'MBANK35' TO BANK-LAST-MAPSET                        BBANK35P
017100        MOVE 'BANK35A' TO BANK-LAST-MAP                           BBANK35P
017200        MOVE 'MBANK35' TO BANK-NEXT-MAPSET                        BBANK35P
017300        MOVE 'BANK35A' TO BANK-NEXT-MAP                           BBANK35P
017400        SET BANK-PAGING-OFF TO TRUE                               BBANK35P
017500        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK35P
017600                POPULATE-SCREEN-DATA-EXIT                         BBANK35P
017700        GO TO COMMON-RETURN                                       BBANK35P
017800     END-IF.                                                      BBANK35P
017900                                                                  BBANK35P
018000                                                                  BBANK35P
018100* Check if we have set the screen up before or is this 1st time   BBANK35P
018200     IF BANK-LAST-MAPSET IS EQUAL TO 'MBANK35'                    BBANK35P
018300        MOVE 'BBANK35P' TO BANK-LAST-PROG                         BBANK35P
018400        MOVE 'BBANK35P' TO BANK-NEXT-PROG                         BBANK35P
018500        MOVE 'MBANK35' TO BANK-LAST-MAPSET                        BBANK35P
018600        MOVE 'BANK35A' TO BANK-LAST-MAP                           BBANK35P
018700        MOVE 'MBANK35' TO BANK-NEXT-MAPSET                        BBANK35P
018800        MOVE 'BANK35A' TO BANK-NEXT-MAP                           BBANK35P
018900        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK35P
019000                POPULATE-SCREEN-DATA-EXIT                         BBANK35P
019100        GO TO COMMON-RETURN                                       BBANK35P
019200     END-IF.                                                      BBANK35P
019300                                                                  BBANK35P
019400***************************************************************** BBANK35P
019500* If we get this far then we have an error in our logic as we   * BBANK35P
019600* don't know where to go next.                                  * BBANK35P
019700***************************************************************** BBANK35P
019800     IF NOT BANK-ENV-CICS                                         BBANK35P
019900        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK35P
020000        MOVE '0001' TO ABEND-CODE                                 BBANK35P
020100        MOVE SPACES TO ABEND-REASON                               BBANK35P
020200        COPY CABENDPO.                                            BBANK35P
020300     END-IF.                                                      BBANK35P
020400     GOBACK.                                                      BBANK35P
020500                                                                  BBANK35P
020600 COMMON-RETURN.                                                   BBANK35P
020700     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK35P
020800 COPY CRETURN.                                                    BBANK35P
020900                                                                  BBANK35P
021000 POPULATE-SCREEN-DATA.                                            BBANK35P
021100     MOVE SPACES TO BANK-SCR35-ATM-FIELDS.                        BBANK35P
021200     MOVE SPACES TO BANK-SCR35-RP-FIELDS.                         BBANK35P
021300     MOVE SPACES TO CD11-DATA.                                    BBANK35P
021400* Set criteria for search to populate screen                      BBANK35P
021500     MOVE BANK-SCR35-ACC TO CD11I-ACCNO.                          BBANK35P
021600* Now go get the data                                             BBANK35P
021700 COPY CBANKX11.                                                   BBANK35P
021800     IF CD11O-ACCNO IS NOT EQUAL TO SPACES                        BBANK35P
021900        MOVE CD11O-BAL TO BANK-SCR35-BAL                          BBANK35P
022000        IF CD11O-BAL IS EQUAL TO SPACES                           BBANK35P
022100           MOVE CD11O-BAL TO BANK-SCR35-BAL                       BBANK35P
022200        ELSE                                                      BBANK35P
022300           MOVE CD11O-BAL-N TO WS-EDIT-AMT-7-2                    BBANK35P
022400           MOVE WS-EDIT-AMT-7-2 TO BANK-SCR35-BAL                 BBANK35P
022500        END-IF                                                    BBANK35P
022600        IF CD11O-DTE IS EQUAL TO SPACES                           BBANK35P
022700           MOVE CD11O-DTE TO BANK-SCR35-DTE                       BBANK35P
022800        ELSE                                                      BBANK35P
022900           MOVE CD11O-DTE TO DDI-DATA                             BBANK35P
023000           SET DDI-ISO TO TRUE                                    BBANK35P
023100           SET DDO-DD-MMM-YYYY TO TRUE                            BBANK35P
023200           PERFORM CALL-DATECONV THRU                             BBANK35P
023300                  CALL-DATECONV-EXIT                              BBANK35P
023400           MOVE DDO-DATA TO BANK-SCR35-DTE                        BBANK35P
023500        END-IF                                                    BBANK35P
023600        MOVE CD11O-ATM-ENABLED TO BANK-SCR35-ATM-ENABLED          BBANK35P
023700        IF CD11O-ATM-LIM IS EQUAL TO SPACES                       BBANK35P
023800           MOVE CD11O-ATM-LIM TO BANK-SCR35-ATM-LIM               BBANK35P
023900        ELSE                                                      BBANK35P
024000           MOVE CD11O-ATM-LIM-N TO WS-EDIT-AMT-3                  BBANK35P
024100           MOVE WS-EDIT-AMT-3 TO BANK-SCR35-ATM-LIM               BBANK35P
024200        END-IF                                                    BBANK35P
024300        IF CD11O-ATM-LDTE IS EQUAL TO SPACES                      BBANK35P
024400           MOVE CD11O-ATM-LDTE TO BANK-SCR35-DTE                  BBANK35P
024500        ELSE                                                      BBANK35P
024600           MOVE CD11O-ATM-LDTE TO DDI-DATA                        BBANK35P
024700           SET DDI-ISO TO TRUE                                    BBANK35P
024800           SET DDO-DD-MMM-YYYY TO TRUE                            BBANK35P
024900           PERFORM CALL-DATECONV THRU                             BBANK35P
025000                  CALL-DATECONV-EXIT                              BBANK35P
025100           MOVE DDO-DATA TO BANK-SCR35-ATM-LDTE                   BBANK35P
025200        END-IF                                                    BBANK35P
025300        IF CD11O-ATM-LAMT IS EQUAL TO SPACES                      BBANK35P
025400           MOVE CD11O-ATM-LAMT TO BANK-SCR35-ATM-LAMT             BBANK35P
025500        ELSE                                                      BBANK35P
025600           MOVE CD11O-ATM-LAMT-N TO WS-EDIT-AMT-3                 BBANK35P
025700           MOVE WS-EDIT-AMT-3 TO BANK-SCR35-ATM-LAMT              BBANK35P
025800        END-IF                                                    BBANK35P
025900        MOVE CD11O-RP1DAY TO BANK-SCR35-RP1DAY                    BBANK35P
026000        IF CD11O-RP1AMT IS EQUAL TO SPACES                        BBANK35P
026100           MOVE CD11O-RP1AMT TO BANK-SCR35-RP1AMT                 BBANK35P
026200        ELSE                                                      BBANK35P
026300           MOVE CD11O-RP1AMT-N TO WS-EDIT-AMT-5-2                 BBANK35P
026400           MOVE WS-EDIT-AMT-5-2 TO BANK-SCR35-RP1AMT              BBANK35P
026500        END-IF                                                    BBANK35P
026600        MOVE CD11O-RP1PID TO BANK-SCR35-RP1PID                    BBANK35P
026700        MOVE CD11O-RP1ACC TO BANK-SCR35-RP1ACC                    BBANK35P
026800        IF CD11O-RP1DTE IS EQUAL TO SPACES                        BBANK35P
026900           MOVE CD11O-RP1DTE TO BANK-SCR35-RP1DTE                 BBANK35P
027000        ELSE                                                      BBANK35P
027100           MOVE CD11O-RP1DTE TO DDI-DATA                          BBANK35P
027200           SET DDI-ISO TO TRUE                                    BBANK35P
027300           SET DDO-DD-MMM-YYYY TO TRUE                            BBANK35P
027400           PERFORM CALL-DATECONV THRU                             BBANK35P
027500                  CALL-DATECONV-EXIT                              BBANK35P
027600           MOVE DDO-DATA TO BANK-SCR35-RP1DTE                     BBANK35P
027700        END-IF                                                    BBANK35P
027800        MOVE CD11O-RP2DAY TO BANK-SCR35-RP2DAY                    BBANK35P
027900        IF CD11O-RP2AMT IS EQUAL TO SPACES                        BBANK35P
028000           MOVE CD11O-RP2AMT TO BANK-SCR35-RP2AMT                 BBANK35P
028100        ELSE                                                      BBANK35P
028200           MOVE CD11O-RP2AMT-N TO WS-EDIT-AMT-5-2                 BBANK35P
028300           MOVE WS-EDIT-AMT-5-2 TO BANK-SCR35-RP2AMT              BBANK35P
028400        END-IF                                                    BBANK35P
028500        MOVE CD11O-RP2PID TO BANK-SCR35-RP2PID                    BBANK35P
028600        MOVE CD11O-RP2ACC TO BANK-SCR35-RP2ACC                    BBANK35P
028700        IF CD11O-RP2DTE IS EQUAL TO SPACES                        BBANK35P
028800           MOVE CD11O-RP2DTE TO BANK-SCR35-RP2DTE                 BBANK35P
028900        ELSE                                                      BBANK35P
029000           MOVE CD11O-RP2DTE TO DDI-DATA                          BBANK35P
029100           SET DDI-ISO TO TRUE                                    BBANK35P
029200           SET DDO-DD-MMM-YYYY TO TRUE                            BBANK35P
029300           PERFORM CALL-DATECONV THRU                             BBANK35P
029400                  CALL-DATECONV-EXIT                              BBANK35P
029500           MOVE DDO-DATA TO BANK-SCR35-RP2DTE                     BBANK35P
029600        END-IF                                                    BBANK35P
029700        MOVE CD11O-RP3DAY TO BANK-SCR35-RP3DAY                    BBANK35P
029800        MOVE CD11O-RP3AMT TO BANK-SCR35-RP3AMT                    BBANK35P
029900        IF CD11O-RP3AMT IS EQUAL TO SPACES                        BBANK35P
030000           MOVE CD11O-RP3AMT TO BANK-SCR35-RP3AMT                 BBANK35P
030100        ELSE                                                      BBANK35P
030200           MOVE CD11O-RP3AMT-N TO WS-EDIT-AMT-5-2                 BBANK35P
030300           MOVE WS-EDIT-AMT-5-2 TO BANK-SCR35-RP3AMT              BBANK35P
030400        END-IF                                                    BBANK35P
030500        MOVE CD11O-RP3PID TO BANK-SCR35-RP3PID                    BBANK35P
030600        MOVE CD11O-RP3ACC TO BANK-SCR35-RP3ACC                    BBANK35P
030700        IF CD11O-RP3DTE IS EQUAL TO SPACES                        BBANK35P
030800           MOVE CD11O-RP3DTE TO BANK-SCR35-RP3DTE                 BBANK35P
030900        ELSE                                                      BBANK35P
031000           MOVE CD11O-RP3DTE TO DDI-DATA                          BBANK35P
031100           SET DDI-ISO TO TRUE                                    BBANK35P
031200           SET DDO-DD-MMM-YYYY TO TRUE                            BBANK35P
031300           PERFORM CALL-DATECONV THRU                             BBANK35P
031400                  CALL-DATECONV-EXIT                              BBANK35P
031500           MOVE DDO-DATA TO BANK-SCR35-RP3DTE                     BBANK35P
031600        END-IF                                                    BBANK35P
031700     END-IF.                                                      BBANK35P
031800                                                                  BBANK35P
031900 POPULATE-SCREEN-DATA-EXIT.                                       BBANK35P
032000     EXIT.                                                        BBANK35P
032100                                                                  BBANK35P
032200***************************************************************** BBANK35P
032300* Call common routine to perform date conversions               * BBANK35P
032400***************************************************************** BBANK35P
032500 CALL-DATECONV.                                                   BBANK35P
032600     MOVE BANK-ENV TO DD-ENV.                                     BBANK35P
032700     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           BBANK35P
032800     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            BBANK35P
032900 CALL-DATECONV-EXIT.                                              BBANK35P
033000     EXIT.                                                        BBANK35P
033100                                                                  BBANK35P
033200* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        BBANK35P
