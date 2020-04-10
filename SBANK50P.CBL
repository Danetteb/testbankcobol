000100***************************************************************** SBANK50P
000200*                                                               * SBANK50P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK50P
000400*   This demonstration program is provided for use by users     * SBANK50P
000500*   of Micro Focus products and may be used, modified and       * SBANK50P
000600*   distributed as part of your application provided that       * SBANK50P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK50P
000800*   in this material.                                           * SBANK50P
000900*                                                               * SBANK50P
001000***************************************************************** SBANK50P
001100                                                                  SBANK50P
001200***************************************************************** SBANK50P
001300* Program:     SBANK50P.CBL (CICS Version)                      * SBANK50P
001400* Layer:       Screen handling                                  * SBANK50P
001500* Function:    Transfer funds between accounts                  * SBANK50P
001600***************************************************************** SBANK50P
001700                                                                  SBANK50P
001800 IDENTIFICATION DIVISION.                                         SBANK50P
001900 PROGRAM-ID.                                                      SBANK50P
002000     SBANK50P.                                                    SBANK50P
002100 DATE-WRITTEN.                                                    SBANK50P
002200     September 2002.                                              SBANK50P
002300 DATE-COMPILED.                                                   SBANK50P
002400     Today.                                                       SBANK50P
002500                                                                  SBANK50P
002600 ENVIRONMENT DIVISION.                                            SBANK50P
002700                                                                  SBANK50P
002800 DATA DIVISION.                                                   SBANK50P
002900 WORKING-STORAGE SECTION.                                         SBANK50P
003000 01  WS-MISC-STORAGE.                                             SBANK50P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK50P
003200       VALUE 'SBANK50P'.                                          SBANK50P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK50P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK50P
003500       VALUE SPACES.                                              SBANK50P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK50P
003700       VALUE 'UNKNOWN'.                                           SBANK50P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK50P
003900   05  WS-WORK1                              PIC X(1).            SBANK50P
004000   05  WS-SUB1                               PIC S9(4) COMP.      SBANK50P
004100                                                                  SBANK50P
004200 01  MAPAREA                                 PIC X(2048).         SBANK50P
004300 COPY MBANK50.                                                    SBANK50P
004400                                                                  SBANK50P
004500 01  WS-TIME-DATE-WORK-AREA.                                      SBANK50P
004600 COPY CDATED.                                                     SBANK50P
004700                                                                  SBANK50P
004800 01  WS-BANK-DATA-AREAS.                                          SBANK50P
004900   05  WS-BANK-DATA.                                              SBANK50P
005000 COPY CBANKDAT.                                                   SBANK50P
005100   05  WS-BANK-EXT-DATA.                                          SBANK50P
005200 COPY CBANKEXT.                                                   SBANK50P
005300                                                                  SBANK50P
005400 COPY CSCRNHDD.                                                   SBANK50P
005500                                                                  SBANK50P
005600 COPY DFHAID.                                                     SBANK50P
005700                                                                  SBANK50P
005800 COPY DFHBMSCA.                                                   SBANK50P
005900                                                                  SBANK50P
006000 COPY CABENDD.                                                    SBANK50P
006100                                                                  SBANK50P
006200 LINKAGE SECTION.                                                 SBANK50P
006300 01  DFHCOMMAREA.                                                 SBANK50P
006400   05  FILLER                                PIC X(7168).         SBANK50P
006500                                                                  SBANK50P
006600 PROCEDURE DIVISION.                                              SBANK50P
006700***************************************************************** SBANK50P
006800* Write entry to log to show we have been invoked               * SBANK50P
006900***************************************************************** SBANK50P
007000     COPY CTRACE.                                                 SBANK50P
007100                                                                  SBANK50P
007200***************************************************************** SBANK50P
007300* Store our transaction-id                                      * SBANK50P
007400***************************************************************** SBANK50P
007500     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK50P
007600                                                                  SBANK50P
007700***************************************************************** SBANK50P
007800* Store passed data or abend if there wasn't any                * SBANK50P
007900***************************************************************** SBANK50P
008000     IF EIBCALEN IS EQUAL TO 0                                    SBANK50P
008100        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK50P
008200        MOVE '0001' TO ABEND-CODE                                 SBANK50P
008300        MOVE SPACES TO ABEND-REASON                               SBANK50P
008400        COPY CABENDPO.                                            SBANK50P
008500     ELSE                                                         SBANK50P
008600        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK50P
008700        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK50P
008800        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK50P
008900     END-IF.                                                      SBANK50P
009000                                                                  SBANK50P
009100***************************************************************** SBANK50P
009200* This is the main process                                      * SBANK50P
009300***************************************************************** SBANK50P
009400                                                                  SBANK50P
009500***************************************************************** SBANK50P
009600* Determine what we have to do (read from or send to screen)    * SBANK50P
009700***************************************************************** SBANK50P
009800     MOVE LOW-VALUE TO MAPAREA.                                   SBANK50P
009900     EVALUATE TRUE                                                SBANK50P
010000       WHEN BANK-MAP-FUNCTION-GET                                 SBANK50P
010100         PERFORM SCREEN50-READ THRU                               SBANK50P
010200                 SCREEN50-READ-EXIT                               SBANK50P
010300       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK50P
010400         PERFORM SCREEN50-BUILD-AND-SEND THRU                     SBANK50P
010500                 SCREEN50-BUILD-AND-SEND-EXIT                     SBANK50P
010600       WHEN OTHER                                                 SBANK50P
010700         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK50P
010800         MOVE '0002' TO ABEND-CODE                                SBANK50P
010900         MOVE SPACES TO ABEND-REASON                              SBANK50P
011000         COPY CABENDPO.                                           SBANK50P
011100     END-EVALUATE.                                                SBANK50P
011200                                                                  SBANK50P
011300* Call the appropriate routine to handle the business logic       SBANK50P
011400     IF BANK-MAP-FUNCTION-GET                                     SBANK50P
011500        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK50P
011600                       COMMAREA(WS-BANK-DATA)                     SBANK50P
011700                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK50P
011800        END-EXEC                                                  SBANK50P
011900     END-IF.                                                      SBANK50P
012000                                                                  SBANK50P
012100***************************************************************** SBANK50P
012200* Now we have to have finished and can return to our invoker.   * SBANK50P
012300***************************************************************** SBANK50P
012400* Now return to CICS                                              SBANK50P
012500     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK50P
012600          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK50P
012700     EXEC CICS                                                    SBANK50P
012800          RETURN                                                  SBANK50P
012900     END-EXEC.                                                    SBANK50P
013000     GOBACK.                                                      SBANK50P
013100                                                                  SBANK50P
013200***************************************************************** SBANK50P
013300* Screen processing for MBANK50                                 * SBANK50P
013400*---------------------------------------------------------------* SBANK50P
013500* Retrieve data from screen and format it                       * SBANK50P
013600***************************************************************** SBANK50P
013700 SCREEN50-READ.                                                   SBANK50P
013800     MOVE 'BBANK50P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK50P
013900     IF BANK-AID-CLEAR                                            SBANK50P
014000        SET BANK-AID-PFK03 TO TRUE                                SBANK50P
014100        GO TO SCREEN50-READ-EXIT                                  SBANK50P
014200     END-IF.                                                      SBANK50P
014300     IF BANK-ENV-CICS                                             SBANK50P
014400        GO TO SCREEN50-READ-CICS                                  SBANK50P
014500     ELSE                                                         SBANK50P
014600        GO TO SCREEN50-READ-INET                                  SBANK50P
014700     END-IF.                                                      SBANK50P
014800                                                                  SBANK50P
014900 SCREEN50-READ-CICS.                                              SBANK50P
015000     IF BANK-HELP-INACTIVE                                        SBANK50P
015100        EXEC CICS RECEIVE MAP('BANK50A')                          SBANK50P
015200                          MAPSET('MBANK50')                       SBANK50P
015300        END-EXEC                                                  SBANK50P
015400     ELSE                                                         SBANK50P
015500        EXEC CICS RECEIVE MAP('HELP50A')                          SBANK50P
015600                          MAPSET('MBANK50')                       SBANK50P
015700        END-EXEC                                                  SBANK50P
015800        GO TO SCREEN50-READ-EXIT                                  SBANK50P
015900     END-IF.                                                      SBANK50P
016000                                                                  SBANK50P
016100     IF XFERAMTL IN BANK50AI IS EQUAL TO 0                        SBANK50P
016200           MOVE LOW-VALUES TO BANK-SCR50-XFER                     SBANK50P
016300     ELSE                                                         SBANK50P
016400        MOVE XFERAMTI IN BANK50AI TO BANK-SCR50-XFER              SBANK50P
016500        IF BANK-SCR50-XFER IS EQUAL TO SPACES                     SBANK50P
016600           MOVE LOW-VALUES TO BANK-SCR50-XFER                     SBANK50P
016700     END-IF.                                                      SBANK50P
016800                                                                  SBANK50P
016900     IF FROM1L IN BANK50AI IS EQUAL TO 0                          SBANK50P
017000           MOVE LOW-VALUES TO BANK-SCR50-FRM1                     SBANK50P
017100     ELSE                                                         SBANK50P
017200        MOVE FROM1I IN BANK50AI TO BANK-SCR50-FRM1                SBANK50P
017300        IF BANK-SCR50-FRM1 IS EQUAL TO SPACES OR                  SBANK50P
017400           BANK-SCR50-FRM1 IS EQUAL TO ALL '_'                    SBANK50P
017500           MOVE LOW-VALUES TO BANK-SCR50-FRM1                     SBANK50P
017600     END-IF.                                                      SBANK50P
017700                                                                  SBANK50P
017800     IF FROM2L IN BANK50AI IS EQUAL TO 0                          SBANK50P
017900           MOVE LOW-VALUES TO BANK-SCR50-FRM2                     SBANK50P
018000     ELSE                                                         SBANK50P
018100        MOVE FROM2I IN BANK50AI TO BANK-SCR50-FRM2                SBANK50P
018200        IF BANK-SCR50-FRM2 IS EQUAL TO SPACES OR                  SBANK50P
018300           BANK-SCR50-FRM2 IS EQUAL TO ALL '_'                    SBANK50P
018400           MOVE LOW-VALUES TO BANK-SCR50-FRM2                     SBANK50P
018500     END-IF.                                                      SBANK50P
018600                                                                  SBANK50P
018700     IF FROM3L IN BANK50AI IS EQUAL TO 0                          SBANK50P
018800           MOVE LOW-VALUES TO BANK-SCR50-FRM3                     SBANK50P
018900     ELSE                                                         SBANK50P
019000        MOVE FROM3I IN BANK50AI TO BANK-SCR50-FRM3                SBANK50P
019100        IF BANK-SCR50-FRM3 IS EQUAL TO SPACES OR                  SBANK50P
019200           BANK-SCR50-FRM3 IS EQUAL TO ALL '_'                    SBANK50P
019300           MOVE LOW-VALUES TO BANK-SCR50-FRM3                     SBANK50P
019400     END-IF.                                                      SBANK50P
019500                                                                  SBANK50P
019600     IF FROM4L IN BANK50AI IS EQUAL TO 0                          SBANK50P
019700           MOVE LOW-VALUES TO BANK-SCR50-FRM4                     SBANK50P
019800     ELSE                                                         SBANK50P
019900        MOVE FROM4I IN BANK50AI TO BANK-SCR50-FRM4                SBANK50P
020000        IF BANK-SCR50-FRM4 IS EQUAL TO SPACES OR                  SBANK50P
020100           BANK-SCR50-FRM4 IS EQUAL TO ALL '_'                    SBANK50P
020200           MOVE LOW-VALUES TO BANK-SCR50-FRM4                     SBANK50P
020300     END-IF.                                                      SBANK50P
020400                                                                  SBANK50P
020500     IF FROM5L IN BANK50AI IS EQUAL TO 0                          SBANK50P
020600           MOVE LOW-VALUES TO BANK-SCR50-FRM5                     SBANK50P
020700     ELSE                                                         SBANK50P
020800        MOVE FROM5I IN BANK50AI TO BANK-SCR50-FRM5                SBANK50P
020900        IF BANK-SCR50-FRM5 IS EQUAL TO SPACES OR                  SBANK50P
021000           BANK-SCR50-FRM5 IS EQUAL TO ALL '_'                    SBANK50P
021100           MOVE LOW-VALUES TO BANK-SCR50-FRM5                     SBANK50P
021200     END-IF.                                                      SBANK50P
021300                                                                  SBANK50P
021400     IF TO1L IN BANK50AI IS EQUAL TO 0                            SBANK50P
021500           MOVE LOW-VALUES TO BANK-SCR50-TO1                      SBANK50P
021600     ELSE                                                         SBANK50P
021700        MOVE TO1I IN BANK50AI TO BANK-SCR50-TO1                   SBANK50P
021800        IF BANK-SCR50-TO1 IS EQUAL TO SPACES OR                   SBANK50P
021900           BANK-SCR50-TO1 IS EQUAL TO ALL '_'                     SBANK50P
022000           MOVE LOW-VALUES TO BANK-SCR50-TO1                      SBANK50P
022100     END-IF.                                                      SBANK50P
022200                                                                  SBANK50P
022300     IF TO2L IN BANK50AI IS EQUAL TO 0                            SBANK50P
022400           MOVE LOW-VALUES TO BANK-SCR50-TO2                      SBANK50P
022500     ELSE                                                         SBANK50P
022600        MOVE TO2I IN BANK50AI TO BANK-SCR50-TO2                   SBANK50P
022700        IF BANK-SCR50-TO2 IS EQUAL TO SPACES OR                   SBANK50P
022800           BANK-SCR50-TO2 IS EQUAL TO ALL '_'                     SBANK50P
022900           MOVE LOW-VALUES TO BANK-SCR50-TO2                      SBANK50P
023000     END-IF.                                                      SBANK50P
023100                                                                  SBANK50P
023200     IF TO3L IN BANK50AI IS EQUAL TO 0                            SBANK50P
023300           MOVE LOW-VALUES TO BANK-SCR50-TO3                      SBANK50P
023400     ELSE                                                         SBANK50P
023500        MOVE TO3I IN BANK50AI TO BANK-SCR50-TO3                   SBANK50P
023600        IF BANK-SCR50-TO3 IS EQUAL TO SPACES OR                   SBANK50P
023700           BANK-SCR50-TO3 IS EQUAL TO ALL '_'                     SBANK50P
023800           MOVE LOW-VALUES TO BANK-SCR50-TO3                      SBANK50P
023900     END-IF.                                                      SBANK50P
024000                                                                  SBANK50P
024100     IF TO4L IN BANK50AI IS EQUAL TO 0                            SBANK50P
024200           MOVE LOW-VALUES TO BANK-SCR50-TO4                      SBANK50P
024300     ELSE                                                         SBANK50P
024400        MOVE TO4I IN BANK50AI TO BANK-SCR50-TO4                   SBANK50P
024500        IF BANK-SCR50-TO4 IS EQUAL TO SPACES OR                   SBANK50P
024600           BANK-SCR50-TO4 IS EQUAL TO ALL '_'                     SBANK50P
024700           MOVE LOW-VALUES TO BANK-SCR50-TO4                      SBANK50P
024800     END-IF.                                                      SBANK50P
024900                                                                  SBANK50P
025000     IF TO5L IN BANK50AI IS EQUAL TO 0                            SBANK50P
025100           MOVE LOW-VALUES TO BANK-SCR50-TO5                      SBANK50P
025200     ELSE                                                         SBANK50P
025300        MOVE TO5I IN BANK50AI TO BANK-SCR50-TO5                   SBANK50P
025400        IF BANK-SCR50-TO5 IS EQUAL TO SPACES OR                   SBANK50P
025500           BANK-SCR50-TO5 IS EQUAL TO ALL '_'                     SBANK50P
025600           MOVE LOW-VALUES TO BANK-SCR50-TO5                      SBANK50P
025700     END-IF.                                                      SBANK50P
025800                                                                  SBANK50P
025900     GO TO SCREEN50-READ-EXIT.                                    SBANK50P
026000                                                                  SBANK50P
026100 SCREEN50-READ-INET.                                              SBANK50P
026200     MOVE EXT-IP50-XFER TO BANK-SCR50-XFER.                       SBANK50P
026300     MOVE EXT-IP50-FRM1 TO BANK-SCR50-FRM1.                       SBANK50P
026400     MOVE EXT-IP50-TO1 TO BANK-SCR50-TO1.                         SBANK50P
026500     MOVE EXT-IP50-FRM2 TO BANK-SCR50-FRM2.                       SBANK50P
026600     MOVE EXT-IP50-TO2 TO BANK-SCR50-TO2.                         SBANK50P
026700     MOVE EXT-IP50-FRM3 TO BANK-SCR50-FRM3.                       SBANK50P
026800     MOVE EXT-IP50-TO3 TO BANK-SCR50-TO3.                         SBANK50P
026900     MOVE EXT-IP50-FRM4 TO BANK-SCR50-FRM4.                       SBANK50P
027000     MOVE EXT-IP50-TO4 TO BANK-SCR50-TO4.                         SBANK50P
027100     MOVE EXT-IP50-FRM5 TO BANK-SCR50-FRM5.                       SBANK50P
027200     MOVE EXT-IP50-TO5 TO BANK-SCR50-TO5.                         SBANK50P
027300     GO TO SCREEN50-READ-EXIT.                                    SBANK50P
027400                                                                  SBANK50P
027500 SCREEN50-READ-EXIT.                                              SBANK50P
027600     EXIT.                                                        SBANK50P
027700                                                                  SBANK50P
027800***************************************************************** SBANK50P
027900* Screen processing for SCREEN50 (BANK50/HELP50)                * SBANK50P
028000*---------------------------------------------------------------* SBANK50P
028100* Build the output screen and send it                           * SBANK50P
028200***************************************************************** SBANK50P
028300 SCREEN50-BUILD-AND-SEND.                                         SBANK50P
028400* Clear map area, get date & time and move to the map             SBANK50P
028500     MOVE LOW-VALUES TO BANK50AO.                                 SBANK50P
028600     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK50P
028700     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK50P
028800     SET DDI-YYDDD TO TRUE.                                       SBANK50P
028900     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK50P
029000     PERFORM CALL-DATECONV THRU                                   SBANK50P
029100             CALL-DATECONV-EXIT.                                  SBANK50P
029200* Ensure the last map fields are correct                          SBANK50P
029300     IF BANK-HELP-ACTIVE                                          SBANK50P
029400        MOVE 'MBANK50' TO BANK-LAST-MAPSET                        SBANK50P
029500        MOVE 'HELP50A' TO BANK-LAST-MAP                           SBANK50P
029600     ELSE                                                         SBANK50P
029700        MOVE 'MBANK50' TO BANK-LAST-MAPSET                        SBANK50P
029800        MOVE 'BANK50A' TO BANK-LAST-MAP                           SBANK50P
029900     END-IF.                                                      SBANK50P
030000     IF BANK-ENV-CICS                                             SBANK50P
030100        GO TO SCREEN50-BUILD-AND-SEND-CICS                        SBANK50P
030200     ELSE                                                         SBANK50P
030300        GO TO SCREEN50-BUILD-AND-SEND-INET                        SBANK50P
030400     END-IF.                                                      SBANK50P
030500                                                                  SBANK50P
030600 SCREEN50-BUILD-AND-SEND-CICS.                                    SBANK50P
030700     IF BANK-LAST-MAP IS EQUAL TO 'BANK50A'                       SBANK50P
030800        GO TO BANK50-BUILD-AND-SEND-CICS                          SBANK50P
030900     END-IF.                                                      SBANK50P
031000     IF BANK-LAST-MAP IS EQUAL TO 'HELP50A'                       SBANK50P
031100        GO TO HELP50-BUILD-AND-SEND-CICS                          SBANK50P
031200     END-IF.                                                      SBANK50P
031300     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK50P
031400     MOVE '0003' TO ABEND-CODE                                    SBANK50P
031500     MOVE SPACES TO ABEND-REASON                                  SBANK50P
031600     COPY CABENDPO.                                               SBANK50P
031700     GOBACK.                                                      SBANK50P
031800                                                                  SBANK50P
031900 BANK50-BUILD-AND-SEND-CICS.                                      SBANK50P
032000     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK50AO==.        SBANK50P
032100     MOVE WS-TRAN-ID TO TRANO IN BANK50AO.                        SBANK50P
032200     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK50AO.                    SBANK50P
032300     MOVE DDO-DATA TO DATEO IN BANK50AO.                          SBANK50P
032400* Move in any error message                                       SBANK50P
032500     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK50AO.                  SBANK50P
032600* Move in screen specific fields                                  SBANK50P
032700     MOVE BANK-USERID TO USERIDO IN BANK50AO.                     SBANK50P
032800     MOVE BANK-USERID-NAME TO USERNMO IN BANK50AO.                SBANK50P
032900                                                                  SBANK50P
033000     MOVE BANK-SCR50-XFER TO XFERAMTO IN BANK50AO.                SBANK50P
033100     IF BANK-SCR50-ACC1 IS NOT EQUAL TO SPACES                    SBANK50P
033200        MOVE -1 TO FROM1L IN BANK50AI                             SBANK50P
033300        MOVE -1 TO TO1L IN BANK50AI                               SBANK50P
033400        MOVE BANK-SCR50-FRM1 TO FROM1O IN BANK50AO                SBANK50P
033500        MOVE BANK-SCR50-TO1  TO TO1O  IN BANK50AO                 SBANK50P
033600        MOVE BANK-SCR50-ACC1 TO ACC1O IN BANK50AO                 SBANK50P
033700        MOVE BANK-SCR50-DSC1 TO DSC1O IN BANK50AO                 SBANK50P
033800        MOVE BANK-SCR50-BAL1 TO BAL1O IN BANK50AO                 SBANK50P
033900     ELSE                                                         SBANK50P
034000        MOVE DFHBMASK TO FROM1A IN BANK50AI                       SBANK50P
034100        MOVE DFHBMASK TO TO1A IN BANK50AI                         SBANK50P
034200        MOVE SPACES TO FROM1O IN BANK50AO                         SBANK50P
034300        MOVE SPACES TO TO1O IN BANK50AO                           SBANK50P
034400        MOVE SPACES TO ACC1O IN BANK50AO                          SBANK50P
034500        MOVE SPACES TO DSC1O IN BANK50AO                          SBANK50P
034600        MOVE SPACES TO BAL1O IN BANK50AO                          SBANK50P
034700     END-IF.                                                      SBANK50P
034800     IF BANK-SCR50-ACC2 IS NOT EQUAL TO SPACES                    SBANK50P
034900        MOVE -1 TO FROM2L IN BANK50AI                             SBANK50P
035000        MOVE -1 TO TO2L IN BANK50AI                               SBANK50P
035100        MOVE BANK-SCR50-FRM2 TO FROM2O IN BANK50AO                SBANK50P
035200        MOVE BANK-SCR50-TO2  TO TO2O  IN BANK50AO                 SBANK50P
035300        MOVE BANK-SCR50-ACC2 TO ACC2O IN BANK50AO                 SBANK50P
035400        MOVE BANK-SCR50-DSC2 TO DSC2O IN BANK50AO                 SBANK50P
035500        MOVE BANK-SCR50-BAL2 TO BAL2O IN BANK50AO                 SBANK50P
035600     ELSE                                                         SBANK50P
035700        MOVE DFHBMASK TO FROM2A IN BANK50AI                       SBANK50P
035800        MOVE DFHBMASK TO TO2A IN BANK50AI                         SBANK50P
035900        MOVE SPACE TO FROM2O IN BANK50AO                          SBANK50P
036000        MOVE SPACE TO TO2O IN BANK50AO                            SBANK50P
036100        MOVE SPACES TO ACC2O IN BANK50AO                          SBANK50P
036200        MOVE SPACES TO DSC2O IN BANK50AO                          SBANK50P
036300        MOVE SPACES TO BAL2O IN BANK50AO                          SBANK50P
036400     END-IF.                                                      SBANK50P
036500     IF BANK-SCR50-ACC3 IS NOT EQUAL TO SPACES                    SBANK50P
036600        MOVE -1 TO FROM3L IN BANK50AI                             SBANK50P
036700        MOVE -1 TO TO3L IN BANK50AI                               SBANK50P
036800        MOVE BANK-SCR50-FRM3 TO FROM3O IN BANK50AO                SBANK50P
036900        MOVE BANK-SCR50-TO3  TO TO3O  IN BANK50AO                 SBANK50P
037000        MOVE BANK-SCR50-ACC3 TO ACC3O IN BANK50AO                 SBANK50P
037100        MOVE BANK-SCR50-DSC3 TO DSC3O IN BANK50AO                 SBANK50P
037200        MOVE BANK-SCR50-BAL3 TO BAL3O IN BANK50AO                 SBANK50P
037300     ELSE                                                         SBANK50P
037400        MOVE DFHBMASK TO FROM3A IN BANK50AI                       SBANK50P
037500        MOVE DFHBMASK TO TO3A IN BANK50AI                         SBANK50P
037600        MOVE SPACE TO FROM3O IN BANK50AO                          SBANK50P
037700        MOVE SPACE TO TO3O IN BANK50AO                            SBANK50P
037800        MOVE SPACES TO ACC3O IN BANK50AO                          SBANK50P
037900        MOVE SPACES TO DSC3O IN BANK50AO                          SBANK50P
038000        MOVE SPACES TO BAL3O IN BANK50AO                          SBANK50P
038100     END-IF.                                                      SBANK50P
038200     IF BANK-SCR50-ACC4 IS NOT EQUAL TO SPACES                    SBANK50P
038300        MOVE -1 TO FROM4L IN BANK50AI                             SBANK50P
038400        MOVE -1 TO TO4L IN BANK50AI                               SBANK50P
038500        MOVE BANK-SCR50-FRM4 TO FROM4O IN BANK50AO                SBANK50P
038600        MOVE BANK-SCR50-TO4  TO TO4O  IN BANK50AO                 SBANK50P
038700        MOVE BANK-SCR50-ACC4 TO ACC4O IN BANK50AO                 SBANK50P
038800        MOVE BANK-SCR50-DSC4 TO DSC4O IN BANK50AO                 SBANK50P
038900        MOVE BANK-SCR50-BAL4 TO BAL4O IN BANK50AO                 SBANK50P
039000     ELSE                                                         SBANK50P
039100        MOVE DFHBMASK TO FROM4A IN BANK50AI                       SBANK50P
039200        MOVE DFHBMASK TO TO4A IN BANK50AI                         SBANK50P
039300        MOVE SPACE TO FROM4O IN BANK50AO                          SBANK50P
039400        MOVE SPACE TO TO4O IN BANK50AO                            SBANK50P
039500        MOVE SPACES TO ACC4O IN BANK50AO                          SBANK50P
039600        MOVE SPACES TO DSC4O IN BANK50AO                          SBANK50P
039700        MOVE SPACES TO BAL4O IN BANK50AO                          SBANK50P
039800     END-IF.                                                      SBANK50P
039900     IF BANK-SCR50-ACC5 IS NOT EQUAL TO SPACES                    SBANK50P
040000        MOVE -1 TO FROM5L IN BANK50AI                             SBANK50P
040100        MOVE -1 TO TO5L IN BANK50AI                               SBANK50P
040200        MOVE BANK-SCR50-FRM5 TO FROM5O IN BANK50AO                SBANK50P
040300        MOVE BANK-SCR50-TO5  TO TO5O  IN BANK50AO                 SBANK50P
040400        MOVE BANK-SCR50-ACC5 TO ACC5O IN BANK50AO                 SBANK50P
040500        MOVE BANK-SCR50-DSC5 TO DSC5O IN BANK50AO                 SBANK50P
040600        MOVE BANK-SCR50-BAL5 TO BAL5O IN BANK50AO                 SBANK50P
040700     ELSE                                                         SBANK50P
040800        MOVE DFHBMASK TO FROM5A IN BANK50AI                       SBANK50P
040900        MOVE DFHBMASK TO TO5A IN BANK50AI                         SBANK50P
041000        MOVE SPACE TO FROM5O IN BANK50AO                          SBANK50P
041100        MOVE SPACE TO TO5O IN BANK50AO                            SBANK50P
041200        MOVE SPACES TO ACC5O IN BANK50AO                          SBANK50P
041300        MOVE SPACES TO DSC5O IN BANK50AO                          SBANK50P
041400        MOVE SPACES TO BAL5O IN BANK50AO                          SBANK50P
041500     END-IF.                                                      SBANK50P
041600* Turn colour off if required                                     SBANK50P
041700     IF COLOUR-OFF                                                SBANK50P
041800        MOVE DFHGREEN TO TXT01C IN BANK50AO                       SBANK50P
041900        MOVE DFHGREEN TO SCRNC IN BANK50AO                        SBANK50P
042000        MOVE DFHGREEN TO HEAD1C IN BANK50AO                       SBANK50P
042100        MOVE DFHGREEN TO DATEC IN BANK50AO                        SBANK50P
042200        MOVE DFHGREEN TO TXT02C IN BANK50AO                       SBANK50P
042300        MOVE DFHGREEN TO TRANC IN BANK50AO                        SBANK50P
042400        MOVE DFHGREEN TO HEAD2C IN BANK50AO                       SBANK50P
042500        MOVE DFHGREEN TO TIMEC IN BANK50AO                        SBANK50P
042600        MOVE DFHGREEN TO TXT03C IN BANK50AO                       SBANK50P
042700        MOVE DFHGREEN TO USERIDC IN BANK50AO                      SBANK50P
042800        MOVE DFHGREEN TO TXT04C IN BANK50AO                       SBANK50P
042900        MOVE DFHGREEN TO USERNMC IN BANK50AO                      SBANK50P
043000        MOVE DFHGREEN TO TXT05C IN BANK50AO                       SBANK50P
043100        MOVE DFHGREEN TO TXT06C IN BANK50AO                       SBANK50P
043200        MOVE DFHGREEN TO XFERAMTC IN BANK50AO                     SBANK50P
043300        MOVE DFHGREEN TO TXT07C IN BANK50AO                       SBANK50P
043400        MOVE DFHGREEN TO TXT08C IN BANK50AO                       SBANK50P
043500        MOVE DFHGREEN TO TXT09C IN BANK50AO                       SBANK50P
043600        MOVE DFHGREEN TO TXT10C IN BANK50AO                       SBANK50P
043700        MOVE DFHGREEN TO FROM1C IN BANK50AO                       SBANK50P
043800        MOVE DFHGREEN TO TO1C IN BANK50AO                         SBANK50P
043900        MOVE DFHGREEN TO ACC1C IN BANK50AO                        SBANK50P
044000        MOVE DFHGREEN TO DSC1C IN BANK50AO                        SBANK50P
044100        MOVE DFHGREEN TO BAL1C IN BANK50AO                        SBANK50P
044200        MOVE DFHGREEN TO FROM2C IN BANK50AO                       SBANK50P
044300        MOVE DFHGREEN TO TO2C IN BANK50AO                         SBANK50P
044400        MOVE DFHGREEN TO ACC2C IN BANK50AO                        SBANK50P
044500        MOVE DFHGREEN TO DSC2C IN BANK50AO                        SBANK50P
044600        MOVE DFHGREEN TO BAL2C IN BANK50AO                        SBANK50P
044700        MOVE DFHGREEN TO FROM3C IN BANK50AO                       SBANK50P
044800        MOVE DFHGREEN TO TO3C IN BANK50AO                         SBANK50P
044900        MOVE DFHGREEN TO ACC3C IN BANK50AO                        SBANK50P
045000        MOVE DFHGREEN TO DSC3C IN BANK50AO                        SBANK50P
045100        MOVE DFHGREEN TO BAL3C IN BANK50AO                        SBANK50P
045200        MOVE DFHGREEN TO FROM4C IN BANK50AO                       SBANK50P
045300        MOVE DFHGREEN TO TO4C IN BANK50AO                         SBANK50P
045400        MOVE DFHGREEN TO ACC4C IN BANK50AO                        SBANK50P
045500        MOVE DFHGREEN TO DSC4C IN BANK50AO                        SBANK50P
045600        MOVE DFHGREEN TO BAL4C IN BANK50AO                        SBANK50P
045700        MOVE DFHGREEN TO FROM5C IN BANK50AO                       SBANK50P
045800        MOVE DFHGREEN TO TO5C IN BANK50AO                         SBANK50P
045900        MOVE DFHGREEN TO ACC5C IN BANK50AO                        SBANK50P
046000        MOVE DFHGREEN TO DSC5C IN BANK50AO                        SBANK50P
046100        MOVE DFHGREEN TO BAL5C IN BANK50AO                        SBANK50P
046200        MOVE DFHGREEN TO ERRMSGC IN BANK50AO                      SBANK50P
046300        MOVE DFHGREEN TO TXT11C IN BANK50AO                       SBANK50P
046400     END-IF.                                                      SBANK50P
046500                                                                  SBANK50P
046600     EXEC CICS SEND MAP('BANK50A')                                SBANK50P
046700                    MAPSET('MBANK50')                             SBANK50P
046800                    ERASE                                         SBANK50P
046900                    FREEKB                                        SBANK50P
047000     END-EXEC.                                                    SBANK50P
047100     GO TO SCREEN50-BUILD-AND-SEND-EXIT.                          SBANK50P
047200                                                                  SBANK50P
047300 HELP50-BUILD-AND-SEND-CICS.                                      SBANK50P
047400     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK50P
047500                             ==<<SCRN>>== BY ==HELP50AO==.        SBANK50P
047600                                                                  SBANK50P
047700     EXEC CICS SEND MAP('HELP50A')                                SBANK50P
047800                    MAPSET('MBANK50')                             SBANK50P
047900                    ERASE                                         SBANK50P
048000                    FREEKB                                        SBANK50P
048100     END-EXEC.                                                    SBANK50P
048200     GO TO SCREEN50-BUILD-AND-SEND-EXIT.                          SBANK50P
048300                                                                  SBANK50P
048400 SCREEN50-BUILD-AND-SEND-INET.                                    SBANK50P
048500     MOVE SPACES TO EXT-OP-DATA.                                  SBANK50P
048600     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK50P
048700     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK50P
048800     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK50P
048900     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK50P
049000     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK50P
049100     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK50P
049200* Move in screen name                                             SBANK50P
049300     MOVE 'BANK50' TO EXT-OP-SCREEN.                              SBANK50P
049400* Move in any error message                                       SBANK50P
049500     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK50P
049600     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK50P
049700     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK50P
049800* Move in screen specific fields                                  SBANK50P
049900     MOVE BANK-SCR50-XFER TO EXT-OP50-XFER.                       SBANK50P
050000     MOVE BANK-SCR50-FRM1 TO EXT-OP50-FRM1.                       SBANK50P
050100     MOVE BANK-SCR50-TO1  TO EXT-OP50-TO1.                        SBANK50P
050200     MOVE BANK-SCR50-ACC1 TO EXT-OP50-ACC1.                       SBANK50P
050300     MOVE BANK-SCR50-DSC1 TO EXT-OP50-DSC1.                       SBANK50P
050400     MOVE BANK-SCR50-BAL1 TO EXT-OP50-BAL1.                       SBANK50P
050500     MOVE BANK-SCR50-FRM2 TO EXT-OP50-FRM2.                       SBANK50P
050600     MOVE BANK-SCR50-TO2  TO EXT-OP50-TO2.                        SBANK50P
050700     MOVE BANK-SCR50-ACC2 TO EXT-OP50-ACC2.                       SBANK50P
050800     MOVE BANK-SCR50-DSC2 TO EXT-OP50-DSC2.                       SBANK50P
050900     MOVE BANK-SCR50-BAL2 TO EXT-OP50-BAL2.                       SBANK50P
051000     MOVE BANK-SCR50-FRM3 TO EXT-OP50-FRM3.                       SBANK50P
051100     MOVE BANK-SCR50-TO3  TO EXT-OP50-TO3.                        SBANK50P
051200     MOVE BANK-SCR50-ACC3 TO EXT-OP50-ACC3.                       SBANK50P
051300     MOVE BANK-SCR50-DSC3 TO EXT-OP50-DSC3.                       SBANK50P
051400     MOVE BANK-SCR50-BAL3 TO EXT-OP50-BAL3.                       SBANK50P
051500     MOVE BANK-SCR50-FRM4 TO EXT-OP50-FRM4.                       SBANK50P
051600     MOVE BANK-SCR50-TO4  TO EXT-OP50-TO4.                        SBANK50P
051700     MOVE BANK-SCR50-ACC4 TO EXT-OP50-ACC4.                       SBANK50P
051800     MOVE BANK-SCR50-DSC4 TO EXT-OP50-DSC4.                       SBANK50P
051900     MOVE BANK-SCR50-BAL4 TO EXT-OP50-BAL4.                       SBANK50P
052000     MOVE BANK-SCR50-FRM5 TO EXT-OP50-FRM5.                       SBANK50P
052100     MOVE BANK-SCR50-TO5  TO EXT-OP50-TO5.                        SBANK50P
052200     MOVE BANK-SCR50-ACC5 TO EXT-OP50-ACC5.                       SBANK50P
052300     MOVE BANK-SCR50-DSC5 TO EXT-OP50-DSC5.                       SBANK50P
052400     MOVE BANK-SCR50-BAL5 TO EXT-OP50-BAL5.                       SBANK50P
052500                                                                  SBANK50P
052600 SCREEN50-BUILD-AND-SEND-EXIT.                                    SBANK50P
052700     EXIT.                                                        SBANK50P
052800                                                                  SBANK50P
052900***************************************************************** SBANK50P
053000* Call common routine to perform date conversions               * SBANK50P
053100***************************************************************** SBANK50P
053200 CALL-DATECONV.                                                   SBANK50P
053300     MOVE BANK-ENV TO DD-ENV.                                     SBANK50P
053400     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK50P
053500     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK50P
053600 CALL-DATECONV-EXIT.                                              SBANK50P
053700     EXIT.                                                        SBANK50P
053800                                                                  SBANK50P
053900* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK50P
