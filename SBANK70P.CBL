000100***************************************************************** SBANK70P
000200*                                                               * SBANK70P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK70P
000400*   This demonstration program is provided for use by users     * SBANK70P
000500*   of Micro Focus products and may be used, modified and       * SBANK70P
000600*   distributed as part of your application provided that       * SBANK70P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK70P
000800*   in this material.                                           * SBANK70P
000900*                                                               * SBANK70P
001000***************************************************************** SBANK70P
001100                                                                  SBANK70P
001200***************************************************************** SBANK70P
001300* Program:     SBANK70P.CBL (CICS Version)                      * SBANK70P
001400* Layer:       Screen handling                                  * SBANK70P
001500* Function:    Calculate cost of loan                           * SBANK70P
001600***************************************************************** SBANK70P
001700                                                                  SBANK70P
001800 IDENTIFICATION DIVISION.                                         SBANK70P
001900 PROGRAM-ID.                                                      SBANK70P
002000     SBANK70P.                                                    SBANK70P
002100 DATE-WRITTEN.                                                    SBANK70P
002200     September 2002.                                              SBANK70P
002300 DATE-COMPILED.                                                   SBANK70P
002400     Today.                                                       SBANK70P
002500                                                                  SBANK70P
002600 ENVIRONMENT DIVISION.                                            SBANK70P
002700                                                                  SBANK70P
002800 DATA DIVISION.                                                   SBANK70P
002900 WORKING-STORAGE SECTION.                                         SBANK70P
003000 01  WS-MISC-STORAGE.                                             SBANK70P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK70P
003200       VALUE 'SBANK70P'.                                          SBANK70P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK70P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK70P
003500       VALUE SPACES.                                              SBANK70P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK70P
003700       VALUE 'UNKNOWN'.                                           SBANK70P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK70P
003900   05  WS-WORK1                              PIC X(1).            SBANK70P
004000   05  WS-SUB1                               PIC S9(4) COMP.      SBANK70P
004100                                                                  SBANK70P
004200 01  MAPAREA                                 PIC X(2048).         SBANK70P
004300 COPY MBANK70.                                                    SBANK70P
004400                                                                  SBANK70P
004500 01  WS-TIME-DATE-WORK-AREA.                                      SBANK70P
004600 COPY CDATED.                                                     SBANK70P
004700                                                                  SBANK70P
004800 01  WS-BANK-DATA-AREAS.                                          SBANK70P
004900   05  WS-BANK-DATA.                                              SBANK70P
005000 COPY CBANKDAT.                                                   SBANK70P
005100   05  WS-BANK-EXT-DATA.                                          SBANK70P
005200 COPY CBANKEXT.                                                   SBANK70P
005300                                                                  SBANK70P
005400 COPY CSCRNHDD.                                                   SBANK70P
005500                                                                  SBANK70P
005600 COPY DFHAID.                                                     SBANK70P
005700                                                                  SBANK70P
005800 COPY DFHBMSCA.                                                   SBANK70P
005900                                                                  SBANK70P
006000 COPY CABENDD.                                                    SBANK70P
006100                                                                  SBANK70P
006200 LINKAGE SECTION.                                                 SBANK70P
006300 01  DFHCOMMAREA.                                                 SBANK70P
006400   05  FILLER                                PIC X(7168).         SBANK70P
006500                                                                  SBANK70P
006600 PROCEDURE DIVISION.                                              SBANK70P
006700***************************************************************** SBANK70P
006800* Write entry to log to show we have been invoked               * SBANK70P
006900***************************************************************** SBANK70P
007000     COPY CTRACE.                                                 SBANK70P
007100                                                                  SBANK70P
007200***************************************************************** SBANK70P
007300* Store our transaction-id                                      * SBANK70P
007400***************************************************************** SBANK70P
007500     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK70P
007600                                                                  SBANK70P
007700***************************************************************** SBANK70P
007800* Store passed data or abend if there wasn't any                * SBANK70P
007900***************************************************************** SBANK70P
008000     IF EIBCALEN IS EQUAL TO 0                                    SBANK70P
008100        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK70P
008200        MOVE '0001' TO ABEND-CODE                                 SBANK70P
008300        MOVE SPACES TO ABEND-REASON                               SBANK70P
008400        COPY CABENDPO.                                            SBANK70P
008500     ELSE                                                         SBANK70P
008600        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK70P
008700        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK70P
008800        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK70P
008900     END-IF.                                                      SBANK70P
009000                                                                  SBANK70P
009100***************************************************************** SBANK70P
009200* This is the main process                                      * SBANK70P
009300***************************************************************** SBANK70P
009400                                                                  SBANK70P
009500***************************************************************** SBANK70P
009600* Determine what we have to do (read from or send to screen)    * SBANK70P
009700***************************************************************** SBANK70P
009800     MOVE LOW-VALUE TO MAPAREA.                                   SBANK70P
009900     EVALUATE TRUE                                                SBANK70P
010000       WHEN BANK-MAP-FUNCTION-GET                                 SBANK70P
010100         PERFORM SCREEN70-READ THRU                               SBANK70P
010200                 SCREEN70-READ-EXIT                               SBANK70P
010300       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK70P
010400         PERFORM SCREEN70-BUILD-AND-SEND THRU                     SBANK70P
010500                 SCREEN70-BUILD-AND-SEND-EXIT                     SBANK70P
010600       WHEN OTHER                                                 SBANK70P
010700         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK70P
010800         MOVE '0002' TO ABEND-CODE                                SBANK70P
010900         MOVE SPACES TO ABEND-REASON                              SBANK70P
011000         COPY CABENDPO.                                           SBANK70P
011100     END-EVALUATE.                                                SBANK70P
011200                                                                  SBANK70P
011300* Call the appropriate routine to handle the business logic       SBANK70P
011400     IF BANK-MAP-FUNCTION-GET                                     SBANK70P
011500        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK70P
011600                       COMMAREA(WS-BANK-DATA)                     SBANK70P
011700                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK70P
011800        END-EXEC                                                  SBANK70P
011900     END-IF.                                                      SBANK70P
012000                                                                  SBANK70P
012100***************************************************************** SBANK70P
012200* Now we have to have finished and can return to our invoker.   * SBANK70P
012300***************************************************************** SBANK70P
012400* Now return to CICS                                              SBANK70P
012500     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK70P
012600          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK70P
012700     EXEC CICS                                                    SBANK70P
012800          RETURN                                                  SBANK70P
012900     END-EXEC.                                                    SBANK70P
013000     GOBACK.                                                      SBANK70P
013100                                                                  SBANK70P
013200***************************************************************** SBANK70P
013300* Screen processing for MBANK70                                 * SBANK70P
013400*---------------------------------------------------------------* SBANK70P
013500* Retrieve data from screen and format it                       * SBANK70P
013600***************************************************************** SBANK70P
013700 SCREEN70-READ.                                                   SBANK70P
013800     MOVE 'BBANK70P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK70P
013900     IF BANK-AID-CLEAR                                            SBANK70P
014000        SET BANK-AID-PFK03 TO TRUE                                SBANK70P
014100        GO TO SCREEN70-READ-EXIT                                  SBANK70P
014200     END-IF.                                                      SBANK70P
014300     IF BANK-ENV-CICS                                             SBANK70P
014400        GO TO SCREEN70-READ-CICS                                  SBANK70P
014500     ELSE                                                         SBANK70P
014600        GO TO SCREEN70-READ-INET                                  SBANK70P
014700     END-IF.                                                      SBANK70P
014800                                                                  SBANK70P
014900 SCREEN70-READ-CICS.                                              SBANK70P
015000     IF BANK-HELP-INACTIVE                                        SBANK70P
015100        EXEC CICS RECEIVE MAP('BANK70A')                          SBANK70P
015200                          MAPSET('MBANK70')                       SBANK70P
015300        END-EXEC                                                  SBANK70P
015400     ELSE                                                         SBANK70P
015500        EXEC CICS RECEIVE MAP('HELP70A')                          SBANK70P
015600                          MAPSET('MBANK70')                       SBANK70P
015700        END-EXEC                                                  SBANK70P
015800        GO TO SCREEN70-READ-EXIT                                  SBANK70P
015900     END-IF.                                                      SBANK70P
016000                                                                  SBANK70P
016100     IF AMOUNTL IN BANK70AI IS EQUAL TO 0                         SBANK70P
016200           MOVE LOW-VALUES TO BANK-SCR70-AMOUNT                   SBANK70P
016300     ELSE                                                         SBANK70P
016400        MOVE AMOUNTI IN BANK70AI TO BANK-SCR70-AMOUNT             SBANK70P
016500        IF BANK-SCR70-AMOUNT IS EQUAL TO SPACES                   SBANK70P
016600           MOVE LOW-VALUES TO BANK-SCR70-AMOUNT                   SBANK70P
016700     END-IF.                                                      SBANK70P
016800                                                                  SBANK70P
016900     IF RATEL IN BANK70AI IS EQUAL TO 0                           SBANK70P
017000           MOVE LOW-VALUES TO BANK-SCR70-RATE                     SBANK70P
017100     ELSE                                                         SBANK70P
017200        MOVE RATEI IN BANK70AI TO BANK-SCR70-RATE                 SBANK70P
017300        IF BANK-SCR70-RATE IS EQUAL TO SPACES                     SBANK70P
017400           MOVE LOW-VALUES TO BANK-SCR70-RATE                     SBANK70P
017500     END-IF.                                                      SBANK70P
017600                                                                  SBANK70P
017700     IF TERML IN BANK70AI IS EQUAL TO 0                           SBANK70P
017800           MOVE LOW-VALUES TO BANK-SCR70-TERM                     SBANK70P
017900     ELSE                                                         SBANK70P
018000        MOVE TERMI IN BANK70AI TO BANK-SCR70-TERM                 SBANK70P
018100        IF BANK-SCR70-TERM IS EQUAL TO SPACES                     SBANK70P
018200           MOVE LOW-VALUES TO BANK-SCR70-TERM                     SBANK70P
018300     END-IF.                                                      SBANK70P
018400                                                                  SBANK70P
018500     GO TO SCREEN70-READ-EXIT.                                    SBANK70P
018600                                                                  SBANK70P
018700 SCREEN70-READ-INET.                                              SBANK70P
018800     MOVE EXT-IP70-AMOUNT TO BANK-SCR70-AMOUNT.                   SBANK70P
018900     MOVE EXT-IP70-RATE TO BANK-SCR70-RATE.                       SBANK70P
019000     MOVE EXT-IP70-TERM TO BANK-SCR70-TERM.                       SBANK70P
019100     GO TO SCREEN70-READ-EXIT.                                    SBANK70P
019200                                                                  SBANK70P
019300 SCREEN70-READ-EXIT.                                              SBANK70P
019400     EXIT.                                                        SBANK70P
019500                                                                  SBANK70P
019600***************************************************************** SBANK70P
019700* Screen processing for SCREEN70 (BANK70/HELP70)                * SBANK70P
019800*---------------------------------------------------------------* SBANK70P
019900* Build the output screen and send it                           * SBANK70P
020000***************************************************************** SBANK70P
020100 SCREEN70-BUILD-AND-SEND.                                         SBANK70P
020200* Clear map area, get date & time and move to the map             SBANK70P
020300     MOVE LOW-VALUES TO BANK70AO.                                 SBANK70P
020400     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK70P
020500     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK70P
020600     SET DDI-YYDDD TO TRUE.                                       SBANK70P
020700     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK70P
020800     PERFORM CALL-DATECONV THRU                                   SBANK70P
020900             CALL-DATECONV-EXIT.                                  SBANK70P
021000* Ensure the last map fields are correct                          SBANK70P
021100     IF BANK-HELP-ACTIVE                                          SBANK70P
021200        MOVE 'MBANK70' TO BANK-LAST-MAPSET                        SBANK70P
021300        MOVE 'HELP70A' TO BANK-LAST-MAP                           SBANK70P
021400     ELSE                                                         SBANK70P
021500        MOVE 'MBANK70' TO BANK-LAST-MAPSET                        SBANK70P
021600        MOVE 'BANK70A' TO BANK-LAST-MAP                           SBANK70P
021700     END-IF.                                                      SBANK70P
021800     IF BANK-ENV-CICS                                             SBANK70P
021900        GO TO SCREEN70-BUILD-AND-SEND-CICS                        SBANK70P
022000     ELSE                                                         SBANK70P
022100        GO TO SCREEN70-BUILD-AND-SEND-INET                        SBANK70P
022200     END-IF.                                                      SBANK70P
022300                                                                  SBANK70P
022400 SCREEN70-BUILD-AND-SEND-CICS.                                    SBANK70P
022500     IF BANK-LAST-MAP IS EQUAL TO 'BANK70A'                       SBANK70P
022600        GO TO BANK70-BUILD-AND-SEND-CICS                          SBANK70P
022700     END-IF.                                                      SBANK70P
022800     IF BANK-LAST-MAP IS EQUAL TO 'HELP70A'                       SBANK70P
022900        GO TO HELP70-BUILD-AND-SEND-CICS                          SBANK70P
023000     END-IF.                                                      SBANK70P
023100     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK70P
023200     MOVE '0003' TO ABEND-CODE                                    SBANK70P
023300     MOVE SPACES TO ABEND-REASON                                  SBANK70P
023400     COPY CABENDPO.                                               SBANK70P
023500     GOBACK.                                                      SBANK70P
023600                                                                  SBANK70P
023700 BANK70-BUILD-AND-SEND-CICS.                                      SBANK70P
023800     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK70AO==.        SBANK70P
023900     MOVE WS-TRAN-ID TO TRANO IN BANK70AO.                        SBANK70P
024000     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK70AO.                    SBANK70P
024100     MOVE DDO-DATA TO DATEO IN BANK70AO.                          SBANK70P
024200* Move in any error message                                       SBANK70P
024300     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK70AO.                  SBANK70P
024400* Move in screen specific fields                                  SBANK70P
024500     MOVE BANK-SCR70-AMOUNT TO AMOUNTO IN BANK70AO.               SBANK70P
024600     MOVE BANK-SCR70-RATE TO RATEO IN BANK70AO.                   SBANK70P
024700     MOVE BANK-SCR70-TERM TO TERMO IN BANK70AO.                   SBANK70P
024800     MOVE BANK-SCR70-PAYMENT TO PAYMENTO IN BANK70AO.             SBANK70P
024900* Turn colour off if required                                     SBANK70P
025000     IF COLOUR-OFF                                                SBANK70P
025100        MOVE DFHGREEN TO TXT01C IN BANK70AO                       SBANK70P
025200        MOVE DFHGREEN TO SCRNC IN BANK70AO                        SBANK70P
025300        MOVE DFHGREEN TO HEAD1C IN BANK70AO                       SBANK70P
025400        MOVE DFHGREEN TO DATEC IN BANK70AO                        SBANK70P
025500        MOVE DFHGREEN TO TXT02C IN BANK70AO                       SBANK70P
025600        MOVE DFHGREEN TO TRANC IN BANK70AO                        SBANK70P
025700        MOVE DFHGREEN TO HEAD2C IN BANK70AO                       SBANK70P
025800        MOVE DFHGREEN TO TIMEC IN BANK70AO                        SBANK70P
025900        MOVE DFHGREEN TO TXT03C IN BANK70AO                       SBANK70P
026000        MOVE DFHGREEN TO TXT04C IN BANK70AO                       SBANK70P
026100        MOVE DFHGREEN TO TXT05C IN BANK70AO                       SBANK70P
026200        MOVE DFHGREEN TO AMOUNTC IN BANK70AO                      SBANK70P
026300        MOVE DFHGREEN TO TXT06C IN BANK70AO                       SBANK70P
026400        MOVE DFHGREEN TO RATEC IN BANK70AO                        SBANK70P
026500        MOVE DFHGREEN TO TXT07C IN BANK70AO                       SBANK70P
026600        MOVE DFHGREEN TO TERMC IN BANK70AO                        SBANK70P
026700        MOVE DFHGREEN TO TXT08C IN BANK70AO                       SBANK70P
026800        MOVE DFHGREEN TO PAYMENTC IN BANK70AO                     SBANK70P
026900        MOVE DFHGREEN TO ERRMSGC IN BANK70AO                      SBANK70P
027000        MOVE DFHGREEN TO TXT10C IN BANK70AO                       SBANK70P
027100     END-IF.                                                      SBANK70P
027200* Hide line if no payment                                         SBANK70P
027300     IF BANK-SCR70-PAYMENT IS EQUAL TO SPACES                     SBANK70P
027400        MOVE SPACES TO TXT08O IN BANK70AO                         SBANK70P
027500        MOVE SPACES TO PAYMENTO IN BANK70AO                       SBANK70P
027600     END-IF.                                                      SBANK70P
027700     EXEC CICS SEND MAP('BANK70A')                                SBANK70P
027800                    MAPSET('MBANK70')                             SBANK70P
027900                    ERASE                                         SBANK70P
028000                    FREEKB                                        SBANK70P
028100     END-EXEC.                                                    SBANK70P
028200                                                                  SBANK70P
028300     GO TO SCREEN70-BUILD-AND-SEND-EXIT.                          SBANK70P
028400                                                                  SBANK70P
028500 HELP70-BUILD-AND-SEND-CICS.                                      SBANK70P
028600     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK70P
028700                             ==<<SCRN>>== BY ==HELP70AO==.        SBANK70P
028800                                                                  SBANK70P
028900     EXEC CICS SEND MAP('HELP70A')                                SBANK70P
029000                    MAPSET('MBANK70')                             SBANK70P
029100                    ERASE                                         SBANK70P
029200                    FREEKB                                        SBANK70P
029300     END-EXEC.                                                    SBANK70P
029400     GO TO SCREEN70-BUILD-AND-SEND-EXIT.                          SBANK70P
029500                                                                  SBANK70P
029600 SCREEN70-BUILD-AND-SEND-INET.                                    SBANK70P
029700     MOVE SPACES TO EXT-OP-DATA.                                  SBANK70P
029800     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK70P
029900     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK70P
030000     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK70P
030100     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK70P
030200     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK70P
030300     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK70P
030400* Move in screen name                                             SBANK70P
030500     MOVE 'BANK70' TO EXT-OP-SCREEN.                              SBANK70P
030600* Move in any error message                                       SBANK70P
030700     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK70P
030800     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK70P
030900     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK70P
031000* Move in screen specific fields                                  SBANK70P
031100     MOVE BANK-SCR70-AMOUNT TO EXT-OP70-AMOUNT.                   SBANK70P
031200     MOVE BANK-SCR70-RATE TO EXT-OP70-RATE.                       SBANK70P
031300     MOVE BANK-SCR70-TERM TO EXT-OP70-TERM.                       SBANK70P
031400     MOVE BANK-SCR70-PAYMENT TO EXT-OP70-PAYMENT.                 SBANK70P
031500                                                                  SBANK70P
031600 SCREEN70-BUILD-AND-SEND-EXIT.                                    SBANK70P
031700     EXIT.                                                        SBANK70P
031800                                                                  SBANK70P
031900***************************************************************** SBANK70P
032000* Call common routine to perform date conversions               * SBANK70P
032100***************************************************************** SBANK70P
032200 CALL-DATECONV.                                                   SBANK70P
032300     MOVE BANK-ENV TO DD-ENV.                                     SBANK70P
032400     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK70P
032500     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK70P
032600 CALL-DATECONV-EXIT.                                              SBANK70P
032700     EXIT.                                                        SBANK70P
032800                                                                  SBANK70P
032900* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK70P
