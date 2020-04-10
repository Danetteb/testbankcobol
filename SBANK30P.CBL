000100***************************************************************** SBANK30P
000200*                                                               * SBANK30P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK30P
000400*   This demonstration program is provided for use by users     * SBANK30P
000500*   of Micro Focus products and may be used, modified and       * SBANK30P
000600*   distributed as part of your application provided that       * SBANK30P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK30P
000800*   in this material.                                           * SBANK30P
000900*                                                               * SBANK30P
001000***************************************************************** SBANK30P
001100                                                                  SBANK30P
001200***************************************************************** SBANK30P
001300* Program:     SBANK30P.CBL (CICS Version)                      * SBANK30P
001400* Layer:       Screen handling                                  * SBANK30P
001500* Function:    Display account balances                         * SBANK30P
001600***************************************************************** SBANK30P
001700                                                                  SBANK30P
001800 IDENTIFICATION DIVISION.                                         SBANK30P
001900 PROGRAM-ID.                                                      SBANK30P
002000     SBANK30P.                                                    SBANK30P
002100 DATE-WRITTEN.                                                    SBANK30P
002200     September 2002.                                              SBANK30P
002300 DATE-COMPILED.                                                   SBANK30P
002400     Today.                                                       SBANK30P
002500                                                                  SBANK30P
002600 ENVIRONMENT DIVISION.                                            SBANK30P
002700                                                                  SBANK30P
002800 DATA DIVISION.                                                   SBANK30P
002900 WORKING-STORAGE SECTION.                                         SBANK30P
003000 01  WS-MISC-STORAGE.                                             SBANK30P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK30P
003200       VALUE 'SBANK30P'.                                          SBANK30P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK30P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK30P
003500       VALUE SPACES.                                              SBANK30P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK30P
003700       VALUE 'UNKNOWN'.                                           SBANK30P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK30P
003900   05  WS-WORK1                              PIC X(1).            SBANK30P
004000   05  WS-SUB1                               PIC S9(4) COMP.      SBANK30P
004100                                                                  SBANK30P
004200 01  MAPAREA                                 PIC X(2048).         SBANK30P
004300 COPY MBANK30.                                                    SBANK30P
004400                                                                  SBANK30P
004500 01  WS-TIME-DATE-WORK-AREA.                                      SBANK30P
004600 COPY CDATED.                                                     SBANK30P
004700                                                                  SBANK30P
004800 01  WS-BANK-DATA-AREAS.                                          SBANK30P
004900   05  WS-BANK-DATA.                                              SBANK30P
005000 COPY CBANKDAT.                                                   SBANK30P
005100   05  WS-BANK-EXT-DATA.                                          SBANK30P
005200 COPY CBANKEXT.                                                   SBANK30P
005300                                                                  SBANK30P
005400 COPY CSCRNHDD.                                                   SBANK30P
005500                                                                  SBANK30P
005600 COPY DFHAID.                                                     SBANK30P
005700                                                                  SBANK30P
005800 COPY DFHBMSCA.                                                   SBANK30P
005900                                                                  SBANK30P
006000 COPY CABENDD.                                                    SBANK30P
006100                                                                  SBANK30P
006200 LINKAGE SECTION.                                                 SBANK30P
006300 01  DFHCOMMAREA.                                                 SBANK30P
006400   05  FILLER                                PIC X(7068).         SBANK30P
006500                                                                  SBANK30P
006600 PROCEDURE DIVISION.                                              SBANK30P
006700***************************************************************** SBANK30P
006800* Write entry to log to show we have been invoked               * SBANK30P
006900***************************************************************** SBANK30P
007000     COPY CTRACE.                                                 SBANK30P
007100                                                                  SBANK30P
007200***************************************************************** SBANK30P
007300* Store our transaction-id                                      * SBANK30P
007400***************************************************************** SBANK30P
007500     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK30P
007600                                                                  SBANK30P
007700***************************************************************** SBANK30P
007800* Store passed data or abend if there wasn't any                * SBANK30P
007900***************************************************************** SBANK30P
008000     IF EIBCALEN IS EQUAL TO 0                                    SBANK30P
008100        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK30P
008200        MOVE '0001' TO ABEND-CODE                                 SBANK30P
008300        MOVE SPACES TO ABEND-REASON                               SBANK30P
008400        COPY CABENDPO.                                            SBANK30P
008500     ELSE                                                         SBANK30P
008600        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK30P
008700        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK30P
008800        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK30P
008900     END-IF.                                                      SBANK30P
009000                                                                  SBANK30P
009100***************************************************************** SBANK30P
009200* This is the main process                                      * SBANK30P
009300***************************************************************** SBANK30P
009400                                                                  SBANK30P
009500***************************************************************** SBANK30P
009600* Determine what we have to do (read from or send to screen)    * SBANK30P
009700***************************************************************** SBANK30P
009800     MOVE LOW-VALUE TO MAPAREA.                                   SBANK30P
009900     EVALUATE TRUE                                                SBANK30P
010000       WHEN BANK-MAP-FUNCTION-GET                                 SBANK30P
010100         PERFORM SCREEN30-READ THRU                               SBANK30P
010200                 SCREEN30-READ-EXIT                               SBANK30P
010300       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK30P
010400         PERFORM SCREEN30-BUILD-AND-SEND THRU                     SBANK30P
010500                 SCREEN30-BUILD-AND-SEND-EXIT                     SBANK30P
010600       WHEN OTHER                                                 SBANK30P
010700         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK30P
010800         MOVE '0002' TO ABEND-CODE                                SBANK30P
010900         MOVE SPACES TO ABEND-REASON                              SBANK30P
011000         COPY CABENDPO.                                           SBANK30P
011100     END-EVALUATE.                                                SBANK30P
011200                                                                  SBANK30P
011300* Call the appropriate routine to handle the business logic       SBANK30P
011400     IF BANK-MAP-FUNCTION-GET                                     SBANK30P
011500        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK30P
011600                       COMMAREA(WS-BANK-DATA)                     SBANK30P
011700                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK30P
011800        END-EXEC                                                  SBANK30P
011900     END-IF.                                                      SBANK30P
012000                                                                  SBANK30P
012100***************************************************************** SBANK30P
012200* Now we have to have finished and can return to our invoker.   * SBANK30P
012300***************************************************************** SBANK30P
012400* Now return to CICS                                              SBANK30P
012500     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK30P
012600          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK30P
012700     EXEC CICS                                                    SBANK30P
012800          RETURN                                                  SBANK30P
012900     END-EXEC.                                                    SBANK30P
013000     GOBACK.                                                      SBANK30P
013100                                                                  SBANK30P
013200***************************************************************** SBANK30P
013300* Screen processing for MBANK30                                 * SBANK30P
013400*---------------------------------------------------------------* SBANK30P
013500* Retrieve data from screen and format it                       * SBANK30P
013600***************************************************************** SBANK30P
013700 SCREEN30-READ.                                                   SBANK30P
013800     MOVE 'BBANK30P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK30P
013900     IF BANK-AID-CLEAR                                            SBANK30P
014000        SET BANK-AID-PFK03 TO TRUE                                SBANK30P
014100        GO TO SCREEN30-READ-EXIT                                  SBANK30P
014200     END-IF.                                                      SBANK30P
014300     IF BANK-ENV-CICS                                             SBANK30P
014400        GO TO SCREEN30-READ-CICS                                  SBANK30P
014500     ELSE                                                         SBANK30P
014600        GO TO SCREEN30-READ-INET                                  SBANK30P
014700     END-IF.                                                      SBANK30P
014800                                                                  SBANK30P
014900 SCREEN30-READ-CICS.                                              SBANK30P
015000     IF BANK-HELP-INACTIVE                                        SBANK30P
015100        EXEC CICS RECEIVE MAP('BANK30A')                          SBANK30P
015200                          MAPSET('MBANK30')                       SBANK30P
015300        END-EXEC                                                  SBANK30P
015400     ELSE                                                         SBANK30P
015500        EXEC CICS RECEIVE MAP('HELP30A')                          SBANK30P
015600                          MAPSET('MBANK30')                       SBANK30P
015700        END-EXEC                                                  SBANK30P
015800        GO TO SCREEN30-READ-EXIT                                  SBANK30P
015900     END-IF.                                                      SBANK30P
016000                                                                  SBANK30P
016100     IF DET1L IN BANK30AI IS EQUAL TO 0                           SBANK30P
016200           MOVE LOW-VALUES TO BANK-SCR30-DET1                     SBANK30P
016300     ELSE                                                         SBANK30P
016400        MOVE DET1I IN BANK30AI TO BANK-SCR30-DET1                 SBANK30P
016500        IF BANK-SCR30-DET1 IS EQUAL TO SPACES OR                  SBANK30P
016600           BANK-SCR30-DET1 IS EQUAL TO ALL '_'                    SBANK30P
016700           MOVE LOW-VALUES TO BANK-SCR30-DET1                     SBANK30P
016800     END-IF.                                                      SBANK30P
016900                                                                  SBANK30P
017000     IF TXS1L IN BANK30AI IS EQUAL TO 0                           SBANK30P
017100           MOVE LOW-VALUES TO BANK-SCR30-TXS1                     SBANK30P
017200     ELSE                                                         SBANK30P
017300        MOVE TXS1I IN BANK30AI TO BANK-SCR30-TXS1                 SBANK30P
017400        IF BANK-SCR30-TXS1 IS EQUAL TO SPACES OR                  SBANK30P
017500           BANK-SCR30-TXS1 IS EQUAL TO ALL '_'                    SBANK30P
017600           MOVE LOW-VALUES TO BANK-SCR30-TXS1                     SBANK30P
017700     END-IF.                                                      SBANK30P
017800                                                                  SBANK30P
017900     IF DET2L IN BANK30AI IS EQUAL TO 0                           SBANK30P
018000           MOVE LOW-VALUES TO BANK-SCR30-DET2                     SBANK30P
018100     ELSE                                                         SBANK30P
018200        MOVE DET2I IN BANK30AI TO BANK-SCR30-DET2                 SBANK30P
018300        IF BANK-SCR30-DET2 IS EQUAL TO SPACES OR                  SBANK30P
018400           BANK-SCR30-DET2 IS EQUAL TO ALL '_'                    SBANK30P
018500           MOVE LOW-VALUES TO BANK-SCR30-DET2                     SBANK30P
018600     END-IF.                                                      SBANK30P
018700                                                                  SBANK30P
018800     IF TXS2L IN BANK30AI IS EQUAL TO 0                           SBANK30P
018900           MOVE LOW-VALUES TO BANK-SCR30-TXS2                     SBANK30P
019000     ELSE                                                         SBANK30P
019100        MOVE TXS2I IN BANK30AI TO BANK-SCR30-TXS2                 SBANK30P
019200        IF BANK-SCR30-TXS2 IS EQUAL TO SPACES OR                  SBANK30P
019300           BANK-SCR30-TXS2 IS EQUAL TO ALL '_'                    SBANK30P
019400           MOVE LOW-VALUES TO BANK-SCR30-TXS2                     SBANK30P
019500     END-IF.                                                      SBANK30P
019600                                                                  SBANK30P
019700                                                                  SBANK30P
019800     IF DET3L IN BANK30AI IS EQUAL TO 0                           SBANK30P
019900           MOVE LOW-VALUES TO BANK-SCR30-DET3                     SBANK30P
020000     ELSE                                                         SBANK30P
020100        MOVE DET3I IN BANK30AI TO BANK-SCR30-DET3                 SBANK30P
020200        IF BANK-SCR30-DET3 IS EQUAL TO SPACES OR                  SBANK30P
020300           BANK-SCR30-DET3 IS EQUAL TO ALL '_'                    SBANK30P
020400           MOVE LOW-VALUES TO BANK-SCR30-DET3                     SBANK30P
020500     END-IF.                                                      SBANK30P
020600                                                                  SBANK30P
020700     IF TXS3L IN BANK30AI IS EQUAL TO 0                           SBANK30P
020800           MOVE LOW-VALUES TO BANK-SCR30-TXS3                     SBANK30P
020900     ELSE                                                         SBANK30P
021000        MOVE TXS3I IN BANK30AI TO BANK-SCR30-TXS3                 SBANK30P
021100        IF BANK-SCR30-TXS3 IS EQUAL TO SPACES OR                  SBANK30P
021200           BANK-SCR30-TXS3 IS EQUAL TO ALL '_'                    SBANK30P
021300           MOVE LOW-VALUES TO BANK-SCR30-TXS3                     SBANK30P
021400     END-IF.                                                      SBANK30P
021500                                                                  SBANK30P
021600     IF DET4L IN BANK30AI IS EQUAL TO 0                           SBANK30P
021700           MOVE LOW-VALUES TO BANK-SCR30-DET4                     SBANK30P
021800     ELSE                                                         SBANK30P
021900        MOVE DET4I IN BANK30AI TO BANK-SCR30-DET4                 SBANK30P
022000        IF BANK-SCR30-DET4 IS EQUAL TO SPACES OR                  SBANK30P
022100           BANK-SCR30-DET4 IS EQUAL TO ALL '_'                    SBANK30P
022200           MOVE LOW-VALUES TO BANK-SCR30-DET4                     SBANK30P
022300     END-IF.                                                      SBANK30P
022400                                                                  SBANK30P
022500     IF TXS4L IN BANK30AI IS EQUAL TO 0                           SBANK30P
022600           MOVE LOW-VALUES TO BANK-SCR30-TXS4                     SBANK30P
022700     ELSE                                                         SBANK30P
022800        MOVE TXS4I IN BANK30AI TO BANK-SCR30-TXS4                 SBANK30P
022900        IF BANK-SCR30-TXS4 IS EQUAL TO SPACES OR                  SBANK30P
023000           BANK-SCR30-TXS4 IS EQUAL TO ALL '_'                    SBANK30P
023100           MOVE LOW-VALUES TO BANK-SCR30-TXS4                     SBANK30P
023200     END-IF.                                                      SBANK30P
023300                                                                  SBANK30P
023400     IF DET5L IN BANK30AI IS EQUAL TO 0                           SBANK30P
023500           MOVE LOW-VALUES TO BANK-SCR30-DET5                     SBANK30P
023600     ELSE                                                         SBANK30P
023700        MOVE DET5I IN BANK30AI TO BANK-SCR30-DET5                 SBANK30P
023800        IF BANK-SCR30-DET5 IS EQUAL TO SPACES OR                  SBANK30P
023900           BANK-SCR30-DET5 IS EQUAL TO ALL '_'                    SBANK30P
024000           MOVE LOW-VALUES TO BANK-SCR30-DET5                     SBANK30P
024100     END-IF.                                                      SBANK30P
024200                                                                  SBANK30P
024300     IF TXS5L IN BANK30AI IS EQUAL TO 0                           SBANK30P
024400           MOVE LOW-VALUES TO BANK-SCR30-TXS5                     SBANK30P
024500     ELSE                                                         SBANK30P
024600        MOVE TXS5I IN BANK30AI TO BANK-SCR30-TXS5                 SBANK30P
024700        IF BANK-SCR30-TXS5 IS EQUAL TO SPACES OR                  SBANK30P
024800           BANK-SCR30-TXS5 IS EQUAL TO ALL '_'                    SBANK30P
024900           MOVE LOW-VALUES TO BANK-SCR30-TXS5                     SBANK30P
025000     END-IF.                                                      SBANK30P
025100                                                                  SBANK30P
025200     GO TO SCREEN30-READ-EXIT.                                    SBANK30P
025300                                                                  SBANK30P
025400 SCREEN30-READ-INET.                                              SBANK30P
025500     MOVE EXT-IP30-DET1 TO BANK-SCR30-DET1.                       SBANK30P
025600     MOVE EXT-IP30-TXS1 TO BANK-SCR30-TXS1.                       SBANK30P
025700     MOVE EXT-IP30-DET2 TO BANK-SCR30-DET2.                       SBANK30P
025800     MOVE EXT-IP30-TXS2 TO BANK-SCR30-TXS2.                       SBANK30P
025900     MOVE EXT-IP30-DET3 TO BANK-SCR30-DET3.                       SBANK30P
026000     MOVE EXT-IP30-TXS3 TO BANK-SCR30-TXS3.                       SBANK30P
026100     MOVE EXT-IP30-DET4 TO BANK-SCR30-DET4.                       SBANK30P
026200     MOVE EXT-IP30-TXS4 TO BANK-SCR30-TXS4.                       SBANK30P
026300     MOVE EXT-IP30-DET5 TO BANK-SCR30-DET5.                       SBANK30P
026400     MOVE EXT-IP30-TXS5 TO BANK-SCR30-TXS5.                       SBANK30P
026500     GO TO SCREEN30-READ-EXIT.                                    SBANK30P
026600                                                                  SBANK30P
026700 SCREEN30-READ-EXIT.                                              SBANK30P
026800     EXIT.                                                        SBANK30P
026900                                                                  SBANK30P
027000***************************************************************** SBANK30P
027100* Screen processing for SCREEN30 (BANK30/HELP30)                * SBANK30P
027200*---------------------------------------------------------------* SBANK30P
027300* Build the output screen and send it                           * SBANK30P
027400***************************************************************** SBANK30P
027500 SCREEN30-BUILD-AND-SEND.                                         SBANK30P
027600* Clear map area, get date & time and move to the map             SBANK30P
027700     MOVE LOW-VALUES TO BANK30AO.                                 SBANK30P
027800     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK30P
027900     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK30P
028000     SET DDI-YYDDD TO TRUE.                                       SBANK30P
028100     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK30P
028200     PERFORM CALL-DATECONV THRU                                   SBANK30P
028300             CALL-DATECONV-EXIT.                                  SBANK30P
028400* Ensure the last map fields are correct                          SBANK30P
028500     IF BANK-HELP-ACTIVE                                          SBANK30P
028600        MOVE 'MBANK30' TO BANK-LAST-MAPSET                        SBANK30P
028700        MOVE 'HELP30A' TO BANK-LAST-MAP                           SBANK30P
028800     ELSE                                                         SBANK30P
028900        MOVE 'MBANK30' TO BANK-LAST-MAPSET                        SBANK30P
029000        MOVE 'BANK30A' TO BANK-LAST-MAP                           SBANK30P
029100     END-IF.                                                      SBANK30P
029200     IF BANK-ENV-CICS                                             SBANK30P
029300        GO TO SCREEN30-BUILD-AND-SEND-CICS                        SBANK30P
029400     ELSE                                                         SBANK30P
029500        GO TO SCREEN30-BUILD-AND-SEND-INET                        SBANK30P
029600     END-IF.                                                      SBANK30P
029700                                                                  SBANK30P
029800 SCREEN30-BUILD-AND-SEND-CICS.                                    SBANK30P
029900     IF BANK-LAST-MAP IS EQUAL TO 'BANK30A'                       SBANK30P
030000        GO TO BANK30-BUILD-AND-SEND-CICS                          SBANK30P
030100     END-IF.                                                      SBANK30P
030200     IF BANK-LAST-MAP IS EQUAL TO 'HELP30A'                       SBANK30P
030300        GO TO HELP30-BUILD-AND-SEND-CICS                          SBANK30P
030400     END-IF.                                                      SBANK30P
030500     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK30P
030600     MOVE '0003' TO ABEND-CODE                                    SBANK30P
030700     MOVE SPACES TO ABEND-REASON                                  SBANK30P
030800     COPY CABENDPO.                                               SBANK30P
030900     GOBACK.                                                      SBANK30P
031000                                                                  SBANK30P
031100 BANK30-BUILD-AND-SEND-CICS.                                      SBANK30P
031200     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK30AO==.        SBANK30P
031300     MOVE WS-TRAN-ID TO TRANO IN BANK30AO.                        SBANK30P
031400     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK30AO.                    SBANK30P
031500     MOVE DDO-DATA TO DATEO IN BANK30AO.                          SBANK30P
031600* Move in any error message                                       SBANK30P
031700     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK30AO.                  SBANK30P
031800* Move in screen specific fields                                  SBANK30P
031900     MOVE BANK-USERID TO USERIDO IN BANK30AO.                     SBANK30P
032000     MOVE BANK-USERID-NAME TO USERNMO IN BANK30AO.                SBANK30P
032100                                                                  SBANK30P
032200     IF BANK-SCR30-ACC1 IS NOT EQUAL TO SPACES                    SBANK30P
032300        MOVE -1 TO DET1L IN BANK30AI                              SBANK30P
032400        MOVE BANK-SCR30-DET1 TO DET1O IN BANK30AO                 SBANK30P
032500        IF BANK-SCR30-TXN1 IS EQUAL TO '*'                        SBANK30P
032600           MOVE -1 TO TXS1L IN BANK30AI                           SBANK30P
032700        ELSE                                                      SBANK30P
032800           MOVE DFHBMASK TO TXS1A IN BANK30AI                     SBANK30P
032900           MOVE SPACE TO TXS1O IN BANK30AO                        SBANK30P
033000        END-IF                                                    SBANK30P
033100     ELSE                                                         SBANK30P
033200        MOVE DFHBMASK TO DET1A IN BANK30AI                        SBANK30P
033300        MOVE SPACE TO DET1O IN BANK30AO                           SBANK30P
033400        MOVE DFHBMASK TO TXS1A IN BANK30AI                        SBANK30P
033500        MOVE SPACE TO TXS1O IN BANK30AO                           SBANK30P
033600     END-IF.                                                      SBANK30P
033700     MOVE BANK-SCR30-ACC1 TO ACC1O IN BANK30AO.                   SBANK30P
033800     MOVE BANK-SCR30-DSC1 TO DSC1O IN BANK30AO.                   SBANK30P
033900     MOVE BANK-SCR30-BAL1 TO BAL1O IN BANK30AO.                   SBANK30P
034000     MOVE BANK-SCR30-SRV1 TO SRV1O IN BANK30AO.                   SBANK30P
034100     MOVE BANK-SCR30-DTE1 TO DTE1O IN BANK30AO.                   SBANK30P
034200     MOVE BANK-SCR30-TXN1 TO TXN1O IN BANK30AO.                   SBANK30P
034300                                                                  SBANK30P
034400     IF BANK-SCR30-ACC2 IS NOT EQUAL TO SPACES                    SBANK30P
034500        MOVE -1 TO DET2L IN BANK30AI                              SBANK30P
034600        MOVE BANK-SCR30-DET2 TO DET2O IN BANK30AO                 SBANK30P
034700        IF BANK-SCR30-TXN2 IS EQUAL TO '*'                        SBANK30P
034800           MOVE -1 TO TXS2L IN BANK30AI                           SBANK30P
034900        ELSE                                                      SBANK30P
035000           MOVE DFHBMASK TO TXS2A IN BANK30AI                     SBANK30P
035100           MOVE SPACE TO TXS2O IN BANK30AO                        SBANK30P
035200        END-IF                                                    SBANK30P
035300     ELSE                                                         SBANK30P
035400        MOVE DFHBMASK TO DET2A IN BANK30AI                        SBANK30P
035500        MOVE SPACE TO DET2O IN BANK30AO                           SBANK30P
035600        MOVE DFHBMASK TO TXS2A IN BANK30AI                        SBANK30P
035700        MOVE SPACE TO TXS2O IN BANK30AO                           SBANK30P
035800     END-IF.                                                      SBANK30P
035900     MOVE BANK-SCR30-ACC2 TO ACC2O IN BANK30AO.                   SBANK30P
036000     MOVE BANK-SCR30-DSC2 TO DSC2O IN BANK30AO.                   SBANK30P
036100     MOVE BANK-SCR30-BAL2 TO BAL2O IN BANK30AO.                   SBANK30P
036200     MOVE BANK-SCR30-SRV2 TO SRV2O IN BANK30AO.                   SBANK30P
036300     MOVE BANK-SCR30-DTE2 TO DTE2O IN BANK30AO.                   SBANK30P
036400     MOVE BANK-SCR30-TXN2 TO TXN2O IN BANK30AO.                   SBANK30P
036500                                                                  SBANK30P
036600     IF BANK-SCR30-ACC3 IS NOT EQUAL TO SPACES                    SBANK30P
036700        MOVE -1 TO DET3L IN BANK30AI                              SBANK30P
036800        MOVE BANK-SCR30-DET3 TO DET3O IN BANK30AO                 SBANK30P
036900        IF BANK-SCR30-TXN3 IS EQUAL TO '*'                        SBANK30P
037000           MOVE -1 TO TXS3L IN BANK30AI                           SBANK30P
037100        ELSE                                                      SBANK30P
037200           MOVE DFHBMASK TO TXS3A IN BANK30AI                     SBANK30P
037300           MOVE SPACE TO TXS3O IN BANK30AO                        SBANK30P
037400        END-IF                                                    SBANK30P
037500     ELSE                                                         SBANK30P
037600        MOVE DFHBMASK TO DET3A IN BANK30AI                        SBANK30P
037700        MOVE SPACE TO DET3O IN BANK30AO                           SBANK30P
037800        MOVE DFHBMASK TO TXS3A IN BANK30AI                        SBANK30P
037900        MOVE SPACE TO TXS3O IN BANK30AO                           SBANK30P
038000     END-IF.                                                      SBANK30P
038100     MOVE BANK-SCR30-ACC3 TO ACC3O IN BANK30AO.                   SBANK30P
038200     MOVE BANK-SCR30-DSC3 TO DSC3O IN BANK30AO.                   SBANK30P
038300     MOVE BANK-SCR30-BAL3 TO BAL3O IN BANK30AO.                   SBANK30P
038400     MOVE BANK-SCR30-SRV3 TO SRV3O IN BANK30AO.                   SBANK30P
038500     MOVE BANK-SCR30-DTE3 TO DTE3O IN BANK30AO.                   SBANK30P
038600     MOVE BANK-SCR30-TXN3 TO TXN3O IN BANK30AO.                   SBANK30P
038700                                                                  SBANK30P
038800     IF BANK-SCR30-ACC4 IS NOT EQUAL TO SPACES                    SBANK30P
038900        MOVE -1 TO DET4L IN BANK30AI                              SBANK30P
039000        MOVE BANK-SCR30-DET4 TO DET4O IN BANK30AO                 SBANK30P
039100        IF BANK-SCR30-TXN4 IS EQUAL TO '*'                        SBANK30P
039200           MOVE -1 TO TXS4L IN BANK30AI                           SBANK30P
039300        ELSE                                                      SBANK30P
039400           MOVE DFHBMASK TO TXS4A IN BANK30AI                     SBANK30P
039500           MOVE SPACE TO TXS4O IN BANK30AO                        SBANK30P
039600        END-IF                                                    SBANK30P
039700     ELSE                                                         SBANK30P
039800        MOVE DFHBMASK TO DET4A IN BANK30AI                        SBANK30P
039900        MOVE SPACE TO DET4O IN BANK30AO                           SBANK30P
040000        MOVE DFHBMASK TO TXS4A IN BANK30AI                        SBANK30P
040100        MOVE SPACE TO TXS4O IN BANK30AO                           SBANK30P
040200     END-IF.                                                      SBANK30P
040300     MOVE BANK-SCR30-ACC4 TO ACC4O IN BANK30AO.                   SBANK30P
040400     MOVE BANK-SCR30-DSC4 TO DSC4O IN BANK30AO.                   SBANK30P
040500     MOVE BANK-SCR30-BAL4 TO BAL4O IN BANK30AO.                   SBANK30P
040600     MOVE BANK-SCR30-SRV4 TO SRV4O IN BANK30AO.                   SBANK30P
040700     MOVE BANK-SCR30-DTE4 TO DTE4O IN BANK30AO.                   SBANK30P
040800     MOVE BANK-SCR30-TXN4 TO TXN4O IN BANK30AO.                   SBANK30P
040900                                                                  SBANK30P
041000     IF BANK-SCR30-ACC5 IS NOT EQUAL TO SPACES                    SBANK30P
041100        MOVE -1 TO DET5L IN BANK30AI                              SBANK30P
041200        MOVE BANK-SCR30-DET5 TO DET5O IN BANK30AO                 SBANK30P
041300        IF BANK-SCR30-TXN5 IS EQUAL TO '*'                        SBANK30P
041400           MOVE -1 TO TXS5L IN BANK30AI                           SBANK30P
041500        ELSE                                                      SBANK30P
041600           MOVE DFHBMASK TO TXS5A IN BANK30AI                     SBANK30P
041700           MOVE SPACE TO TXS5O IN BANK30AO                        SBANK30P
041800        END-IF                                                    SBANK30P
041900     ELSE                                                         SBANK30P
042000        MOVE DFHBMASK TO DET5A IN BANK30AI                        SBANK30P
042100        MOVE SPACE TO DET5O IN BANK30AO                           SBANK30P
042200        MOVE DFHBMASK TO TXS5A IN BANK30AI                        SBANK30P
042300        MOVE SPACE TO TXS5O IN BANK30AO                           SBANK30P
042400     END-IF.                                                      SBANK30P
042500     MOVE BANK-SCR30-ACC5 TO ACC5O IN BANK30AO.                   SBANK30P
042600     MOVE BANK-SCR30-DSC5 TO DSC5O IN BANK30AO.                   SBANK30P
042700     MOVE BANK-SCR30-BAL5 TO BAL5O IN BANK30AO.                   SBANK30P
042800     MOVE BANK-SCR30-SRV5 TO SRV5O IN BANK30AO.                   SBANK30P
042900     MOVE BANK-SCR30-DTE5 TO DTE5O IN BANK30AO.                   SBANK30P
043000     MOVE BANK-SCR30-TXN5 TO TXN5O IN BANK30AO.                   SBANK30P
043100     MOVE BANK-SCR30-SRVMSG TO SRVMSGO IN BANK30AO.               SBANK30P
043200* Turn colour off if required                                     SBANK30P
043300     IF COLOUR-OFF                                                SBANK30P
043400        MOVE DFHGREEN TO TXT01C IN BANK30AO                       SBANK30P
043500        MOVE DFHGREEN TO SCRNC IN BANK30AO                        SBANK30P
043600        MOVE DFHGREEN TO HEAD1C IN BANK30AO                       SBANK30P
043700        MOVE DFHGREEN TO DATEC IN BANK30AO                        SBANK30P
043800        MOVE DFHGREEN TO TXT02C IN BANK30AO                       SBANK30P
043900        MOVE DFHGREEN TO TRANC IN BANK30AO                        SBANK30P
044000        MOVE DFHGREEN TO HEAD2C IN BANK30AO                       SBANK30P
044100        MOVE DFHGREEN TO TIMEC IN BANK30AO                        SBANK30P
044200        MOVE DFHGREEN TO TXT03C IN BANK30AO                       SBANK30P
044300        MOVE DFHGREEN TO USERIDC IN BANK30AO                      SBANK30P
044400        MOVE DFHGREEN TO TXT04C IN BANK30AO                       SBANK30P
044500        MOVE DFHGREEN TO USERNMC IN BANK30AO                      SBANK30P
044600        MOVE DFHGREEN TO TXT05C IN BANK30AO                       SBANK30P
044700        MOVE DFHGREEN TO TXT06C IN BANK30AO                       SBANK30P
044800        MOVE DFHGREEN TO TXT07C IN BANK30AO                       SBANK30P
044900        MOVE DFHGREEN TO TXT08C IN BANK30AO                       SBANK30P
045000        MOVE DFHGREEN TO TXT09C IN BANK30AO                       SBANK30P
045100        MOVE DFHGREEN TO TXT10C IN BANK30AO                       SBANK30P
045200        MOVE DFHGREEN TO TXT11C IN BANK30AO                       SBANK30P
045300        MOVE DFHGREEN TO TXT12C IN BANK30AO                       SBANK30P
045400        MOVE DFHGREEN TO TXT13C IN BANK30AO                       SBANK30P
045500        MOVE DFHGREEN TO TXT14C IN BANK30AO                       SBANK30P
045600        MOVE DFHGREEN TO TXT15C IN BANK30AO                       SBANK30P
045700        MOVE DFHGREEN TO DET1C IN BANK30AO                        SBANK30P
045800        MOVE DFHGREEN TO ACC1C IN BANK30AO                        SBANK30P
045900        MOVE DFHGREEN TO DSC1C IN BANK30AO                        SBANK30P
046000        MOVE DFHGREEN TO BAL1C IN BANK30AO                        SBANK30P
046100        MOVE DFHGREEN TO SRV1C IN BANK30AO                        SBANK30P
046200        MOVE DFHGREEN TO DTE1C IN BANK30AO                        SBANK30P
046300        MOVE DFHGREEN TO TXN1C IN BANK30AO                        SBANK30P
046400        MOVE DFHGREEN TO TXS1C IN BANK30AO                        SBANK30P
046500        MOVE DFHGREEN TO DET2C IN BANK30AO                        SBANK30P
046600        MOVE DFHGREEN TO ACC2C IN BANK30AO                        SBANK30P
046700        MOVE DFHGREEN TO DSC2C IN BANK30AO                        SBANK30P
046800        MOVE DFHGREEN TO BAL2C IN BANK30AO                        SBANK30P
046900        MOVE DFHGREEN TO SRV2C IN BANK30AO                        SBANK30P
047000        MOVE DFHGREEN TO DTE2C IN BANK30AO                        SBANK30P
047100        MOVE DFHGREEN TO TXN2C IN BANK30AO                        SBANK30P
047200        MOVE DFHGREEN TO TXS2C IN BANK30AO                        SBANK30P
047300        MOVE DFHGREEN TO ACC3C IN BANK30AO                        SBANK30P
047400        MOVE DFHGREEN TO DSC3C IN BANK30AO                        SBANK30P
047500        MOVE DFHGREEN TO BAL3C IN BANK30AO                        SBANK30P
047600        MOVE DFHGREEN TO SRV3C IN BANK30AO                        SBANK30P
047700        MOVE DFHGREEN TO DTE3C IN BANK30AO                        SBANK30P
047800        MOVE DFHGREEN TO TXN3C IN BANK30AO                        SBANK30P
047900        MOVE DFHGREEN TO TXS3C IN BANK30AO                        SBANK30P
048000        MOVE DFHGREEN TO DET4C IN BANK30AO                        SBANK30P
048100        MOVE DFHGREEN TO ACC4C IN BANK30AO                        SBANK30P
048200        MOVE DFHGREEN TO DSC4C IN BANK30AO                        SBANK30P
048300        MOVE DFHGREEN TO BAL4C IN BANK30AO                        SBANK30P
048400        MOVE DFHGREEN TO SRV4C IN BANK30AO                        SBANK30P
048500        MOVE DFHGREEN TO DTE4C IN BANK30AO                        SBANK30P
048600        MOVE DFHGREEN TO TXN4C IN BANK30AO                        SBANK30P
048700        MOVE DFHGREEN TO TXS4C IN BANK30AO                        SBANK30P
048800        MOVE DFHGREEN TO DET5C IN BANK30AO                        SBANK30P
048900        MOVE DFHGREEN TO ACC5C IN BANK30AO                        SBANK30P
049000        MOVE DFHGREEN TO DSC5C IN BANK30AO                        SBANK30P
049100        MOVE DFHGREEN TO BAL5C IN BANK30AO                        SBANK30P
049200        MOVE DFHGREEN TO SRV5C IN BANK30AO                        SBANK30P
049300        MOVE DFHGREEN TO DTE5C IN BANK30AO                        SBANK30P
049400        MOVE DFHGREEN TO TXN5C IN BANK30AO                        SBANK30P
049500        MOVE DFHGREEN TO TXS5C IN BANK30AO                        SBANK30P
049600        MOVE DFHGREEN TO SRVMSGC IN BANK30AO                      SBANK30P
049700        MOVE DFHGREEN TO ERRMSGC IN BANK30AO                      SBANK30P
049800        MOVE DFHGREEN TO TXT16C IN BANK30AO                       SBANK30P
049900     END-IF.                                                      SBANK30P
050000                                                                  SBANK30P
050100     EXEC CICS SEND MAP('BANK30A')                                SBANK30P
050200                    MAPSET('MBANK30')                             SBANK30P
050300                    ERASE                                         SBANK30P
050400                    FREEKB                                        SBANK30P
050500     END-EXEC.                                                    SBANK30P
050600     GO TO SCREEN30-BUILD-AND-SEND-EXIT.                          SBANK30P
050700                                                                  SBANK30P
050800 HELP30-BUILD-AND-SEND-CICS.                                      SBANK30P
050900     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK30P
051000                             ==<<SCRN>>== BY ==HELP30AO==.        SBANK30P
051100                                                                  SBANK30P
051200     EXEC CICS SEND MAP('HELP30A')                                SBANK30P
051300                    MAPSET('MBANK30')                             SBANK30P
051400                    ERASE                                         SBANK30P
051500                    FREEKB                                        SBANK30P
051600     END-EXEC.                                                    SBANK30P
051700     GO TO SCREEN30-BUILD-AND-SEND-EXIT.                          SBANK30P
051800                                                                  SBANK30P
051900 SCREEN30-BUILD-AND-SEND-INET.                                    SBANK30P
052000     MOVE SPACES TO EXT-OP-DATA.                                  SBANK30P
052100     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK30P
052200     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK30P
052300     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK30P
052400     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK30P
052500     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK30P
052600     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK30P
052700* Move in screen name                                             SBANK30P
052800     MOVE 'BANK30' TO EXT-OP-SCREEN.                              SBANK30P
052900* Move in userid and any error message                            SBANK30P
053000     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK30P
053100     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK30P
053200     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK30P
053300* Move in screen specific fields                                  SBANK30P
053400     MOVE BANK-SCR30-DET1 TO EXT-OP30-DET1.                       SBANK30P
053500     MOVE BANK-SCR30-ACC1 TO EXT-OP30-ACC1.                       SBANK30P
053600     MOVE BANK-SCR30-DSC1 TO EXT-OP30-DSC1.                       SBANK30P
053700     MOVE BANK-SCR30-BAL1 TO EXT-OP30-BAL1.                       SBANK30P
053800     MOVE BANK-SCR30-SRV1 TO EXT-OP30-SRV1.                       SBANK30P
053900     MOVE BANK-SCR30-DTE1 TO EXT-OP30-DTE1.                       SBANK30P
054000     MOVE BANK-SCR30-TXN1 TO EXT-OP30-TXN1.                       SBANK30P
054100     MOVE BANK-SCR30-TXS1 TO EXT-OP30-TXS1.                       SBANK30P
054200     MOVE BANK-SCR30-DET2 TO EXT-OP30-DET2.                       SBANK30P
054300     MOVE BANK-SCR30-ACC2 TO EXT-OP30-ACC2.                       SBANK30P
054400     MOVE BANK-SCR30-DSC2 TO EXT-OP30-DSC2.                       SBANK30P
054500     MOVE BANK-SCR30-BAL2 TO EXT-OP30-BAL2.                       SBANK30P
054600     MOVE BANK-SCR30-SRV2 TO EXT-OP30-SRV2.                       SBANK30P
054700     MOVE BANK-SCR30-DTE2 TO EXT-OP30-DTE2.                       SBANK30P
054800     MOVE BANK-SCR30-TXN2 TO EXT-OP30-TXN2.                       SBANK30P
054900     MOVE BANK-SCR30-TXS2 TO EXT-OP30-TXS2.                       SBANK30P
055000     MOVE BANK-SCR30-DET3 TO EXT-OP30-DET3.                       SBANK30P
055100     MOVE BANK-SCR30-ACC3 TO EXT-OP30-ACC3.                       SBANK30P
055200     MOVE BANK-SCR30-DSC3 TO EXT-OP30-DSC3.                       SBANK30P
055300     MOVE BANK-SCR30-BAL3 TO EXT-OP30-BAL3.                       SBANK30P
055400     MOVE BANK-SCR30-SRV3 TO EXT-OP30-SRV3.                       SBANK30P
055500     MOVE BANK-SCR30-DTE3 TO EXT-OP30-DTE3.                       SBANK30P
055600     MOVE BANK-SCR30-TXN3 TO EXT-OP30-TXN3.                       SBANK30P
055700     MOVE BANK-SCR30-TXS3 TO EXT-OP30-TXS3.                       SBANK30P
055800     MOVE BANK-SCR30-DET4 TO EXT-OP30-DET4.                       SBANK30P
055900     MOVE BANK-SCR30-ACC4 TO EXT-OP30-ACC4.                       SBANK30P
056000     MOVE BANK-SCR30-DSC4 TO EXT-OP30-DSC4.                       SBANK30P
056100     MOVE BANK-SCR30-BAL4 TO EXT-OP30-BAL4.                       SBANK30P
056200     MOVE BANK-SCR30-SRV4 TO EXT-OP30-SRV4.                       SBANK30P
056300     MOVE BANK-SCR30-DTE4 TO EXT-OP30-DTE4.                       SBANK30P
056400     MOVE BANK-SCR30-TXN4 TO EXT-OP30-TXN4.                       SBANK30P
056500     MOVE BANK-SCR30-TXS4 TO EXT-OP30-TXS4.                       SBANK30P
056600     MOVE BANK-SCR30-DET5 TO EXT-OP30-DET5.                       SBANK30P
056700     MOVE BANK-SCR30-ACC5 TO EXT-OP30-ACC5.                       SBANK30P
056800     MOVE BANK-SCR30-DSC5 TO EXT-OP30-DSC5.                       SBANK30P
056900     MOVE BANK-SCR30-BAL5 TO EXT-OP30-BAL5.                       SBANK30P
057000     MOVE BANK-SCR30-SRV5 TO EXT-OP30-SRV5.                       SBANK30P
057100     MOVE BANK-SCR30-DTE5 TO EXT-OP30-DTE5.                       SBANK30P
057200     MOVE BANK-SCR30-TXN5 TO EXT-OP30-TXN5.                       SBANK30P
057300     MOVE BANK-SCR30-TXS5 TO EXT-OP30-TXS5.                       SBANK30P
057400                                                                  SBANK30P
057500 SCREEN30-BUILD-AND-SEND-EXIT.                                    SBANK30P
057600     EXIT.                                                        SBANK30P
057700                                                                  SBANK30P
057800***************************************************************** SBANK30P
057900* Call common routine to perform date conversions               * SBANK30P
058000***************************************************************** SBANK30P
058100 CALL-DATECONV.                                                   SBANK30P
058200     MOVE BANK-ENV TO DD-ENV.                                     SBANK30P
058300     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK30P
058400     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK30P
058500 CALL-DATECONV-EXIT.                                              SBANK30P
058600     EXIT.                                                        SBANK30P
058700                                                                  SBANK30P
058800* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK30P
