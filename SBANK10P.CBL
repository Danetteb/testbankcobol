
000100***************************************************************** SBANK10P
000200*                                                               * SBANK10P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK10P
000400*   This demonstration program is provided for use by users     * SBANK10P
000500*   of Micro Focus products and may be used, modified and       * SBANK10P
000600*   distributed as part of your application provided that       * SBANK10P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK10P
000800*   in this material.                                           * SBANK10P
000900*                                                               * SBANK10P
001000***************************************************************** SBANK10P
001100                                                                  SBANK10P
001200***************************************************************** SBANK10P
001300* Program:     SBANK10P.CBL (CICS Version)                      * SBANK10P
001400* Layer:       Screen handling                                  * SBANK10P
001500* Function:    Signon to system to identify user                * SBANK10P
001600***************************************************************** SBANK10P
001700                                                                  SBANK10P
001800 IDENTIFICATION DIVISION.                                         SBANK10P
001900 PROGRAM-ID.                                                      SBANK10P
002000     SBANK10P.                                                    SBANK10P
002100 DATE-WRITTEN.                                                    SBANK10P
002200     September 2002.                                              SBANK10P
002300 DATE-COMPILED.                                                   SBANK10P
002400     Today.                                                       SBANK10P
002500                                                                  SBANK10P
002600 ENVIRONMENT DIVISION.                                            SBANK10P
002700                                                                  SBANK10P
002800 DATA DIVISION.                                                   SBANK10P
002900 WORKING-STORAGE SECTION.                                         SBANK10P
003000 01  WS-MISC-STORAGE.                                             SBANK10P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK10P
003200       VALUE 'SBANK10P'.                                          SBANK10P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK10P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK10P
003500       VALUE SPACES.                                              SBANK10P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK10P
003700       VALUE 'UNKNOWN'.                                           SBANK10P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK10P
003900                                                                  SBANK10P
004000 01  MAPAREA                                 PIC X(2048).         SBANK10P
004100 COPY MBANK10.                                                    SBANK10P
004200                                                                  SBANK10P
004300 01  WS-TIME-DATE-WORK-AREA.                                      SBANK10P
004400 COPY CDATED.                                                     SBANK10P
004500                                                                  SBANK10P
004600 01  WS-BANK-DATA-AREAS.                                          SBANK10P
004700   05  WS-BANK-DATA.                                              SBANK10P
004800 COPY CBANKDAT.                                                   SBANK10P
004900   05  WS-BANK-EXT-DATA.                                          SBANK10P
005000 COPY CBANKEXT.                                                   SBANK10P
005100                                                                  SBANK10P
005200 COPY CSCRNHDD.                                                   SBANK10P
005300                                                                  SBANK10P
005400 COPY DFHAID.                                                     SBANK10P
005500                                                                  SBANK10P
005600 COPY DFHBMSCA.                                                   SBANK10P
005700                                                                  SBANK10P
005800 COPY CABENDD.                                                    SBANK10P
005900                                                                  SBANK10P
006000 LINKAGE SECTION.                                                 SBANK10P
006100 01  DFHCOMMAREA.                                                 SBANK10P
006200   05  FILLER                                PIC X(7128).         SBANK10P
006300                                                                  SBANK10P
006400 PROCEDURE DIVISION.                                              SBANK10P
006500***************************************************************** SBANK10P
006600* Write entry to log to show we have been invoked               * SBANK10P
006700***************************************************************** SBANK10P
006800     COPY CTRACE.                                                 SBANK10P
006900                                                                  SBANK10P
007000***************************************************************** SBANK10P
007100* Store our transaction-id                                      * SBANK10P
007200***************************************************************** SBANK10P
007300     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK10P
007400                                                                  SBANK10P
007500***************************************************************** SBANK10P
007600* Store passed data or abend if there wasn't any                * SBANK10P
007700***************************************************************** SBANK10P
007800     IF EIBCALEN IS EQUAL TO 0                                    SBANK10P
007900        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK10P
008000        MOVE '0001' TO ABEND-CODE                                 SBANK10P
008100        MOVE SPACES TO ABEND-REASON                               SBANK10P
008200        COPY CABENDPO.                                            SBANK10P
008300     ELSE                                                         SBANK10P
008400        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK10P
008500        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK10P
008600        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK10P
008700     END-IF.                                                      SBANK10P
008800                                                                  SBANK10P
008900***************************************************************** SBANK10P
009000* This is the main process                                      * SBANK10P
009100***************************************************************** SBANK10P
009200                                                                  SBANK10P
009300***************************************************************** SBANK10P
009400* Determine what we have to do (read from or send to screen)    * SBANK10P
009500***************************************************************** SBANK10P
009600     MOVE LOW-VALUE TO MAPAREA.                                   SBANK10P
009700     EVALUATE TRUE                                                SBANK10P
009800       WHEN BANK-MAP-FUNCTION-GET                                 SBANK10P
009900         PERFORM SCREEN10-READ THRU                               SBANK10P
010000                 SCREEN10-READ-EXIT                               SBANK10P
010100       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK10P
010200         PERFORM SCREEN10-BUILD-AND-SEND THRU                     SBANK10P
010300                 SCREEN10-BUILD-AND-SEND-EXIT                     SBANK10P
010400       WHEN OTHER                                                 SBANK10P
010500         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK10P
010600         MOVE '0002' TO ABEND-CODE                                SBANK10P
010700         MOVE SPACES TO ABEND-REASON                              SBANK10P
010800         COPY CABENDPO.                                           SBANK10P
010900     END-EVALUATE.                                                SBANK10P
011000                                                                  SBANK10P
011100* Call the appropriate routine to handle the business logic       SBANK10P
011200     IF BANK-MAP-FUNCTION-GET                                     SBANK10P
011300        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK10P
011400                       COMMAREA(WS-BANK-DATA)                     SBANK10P
011500                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK10P
011600        END-EXEC                                                  SBANK10P
011700     END-IF.                                                      SBANK10P
011800                                                                  SBANK10P
011900***************************************************************** SBANK10P
012000* Now we have to have finished and can return to our invoker.   * SBANK10P
012100***************************************************************** SBANK10P
012200* Now return to CICS                                              SBANK10P
012300     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK10P
012400          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK10P
012500     EXEC CICS                                                    SBANK10P
012600          RETURN                                                  SBANK10P
012700     END-EXEC.                                                    SBANK10P
012800     GOBACK.                                                      SBANK10P
012900                                                                  SBANK10P
013000***************************************************************** SBANK10P
013100* Screen processing for MBANK10                                 * SBANK10P
013200*---------------------------------------------------------------* SBANK10P
013300* Retrieve data from screen and format it                       * SBANK10P
013400***************************************************************** SBANK10P
013500 SCREEN10-READ.                                                   SBANK10P
013600     MOVE 'BBANK10P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK10P
013700     IF BANK-AID-CLEAR                                            SBANK10P
013800        SET BANK-AID-PFK03 TO TRUE                                SBANK10P
013900        GO TO SCREEN10-READ-EXIT                                  SBANK10P
014000     END-IF.                                                      SBANK10P
014100     IF BANK-LAST-MAPSET IS EQUAL TO SPACES                       SBANK10P
014200        GO TO SCREEN10-READ-EXIT                                  SBANK10P
014300     END-IF.                                                      SBANK10P
014400     IF BANK-ENV-CICS                                             SBANK10P
014500        GO TO SCREEN10-READ-CICS                                  SBANK10P
014600     ELSE                                                         SBANK10P
014700        GO TO SCREEN10-READ-INET                                  SBANK10P
014800     END-IF.                                                      SBANK10P
014900                                                                  SBANK10P
015000 SCREEN10-READ-CICS.                                              SBANK10P
015100     IF BANK-HELP-INACTIVE                                        SBANK10P
015200        EXEC CICS RECEIVE MAP('BANK10A')                          SBANK10P
015300                          MAPSET('MBANK10')                       SBANK10P
015400        END-EXEC                                                  SBANK10P
015500     ELSE                                                         SBANK10P
015600        EXEC CICS RECEIVE MAP('HELP10A')                          SBANK10P
015700                          MAPSET('MBANK10')                       SBANK10P
015800        END-EXEC                                                  SBANK10P
015900        GO TO SCREEN10-READ-EXIT                                  SBANK10P
016000     END-IF.                                                      SBANK10P
016100                                                                  SBANK10P
016200     IF USERIDL IN BANK10AI IS EQUAL TO 0                         SBANK10P
016300           MOVE LOW-VALUES TO BANK-SIGNON-ID                      SBANK10P
016400     ELSE                                                         SBANK10P
016500        MOVE USERIDI IN BANK10AI                                  SBANK10P
016600          TO BANK-SIGNON-ID (1:USERIDL IN BANK10AI)               SBANK10P
016700     END-IF.                                                      SBANK10P
016800                                                                  SBANK10P
016900     IF PSWDL IN BANK10AI IS EQUAL TO 0                           SBANK10P
017000        MOVE LOW-VALUES TO BANK-PSWD                              SBANK10P
017100     ELSE                                                         SBANK10P
017200        MOVE PSWDI IN BANK10AI                                    SBANK10P
017300          TO BANK-PSWD (1:PSWDL IN BANK10AI)                      SBANK10P
017400     END-IF.                                                      SBANK10P
017500                                                                  SBANK10P
017600     GO TO SCREEN10-READ-EXIT.                                    SBANK10P
017700                                                                  SBANK10P
017800 SCREEN10-READ-INET.                                              SBANK10P
017900     MOVE EXT-IP10-USERID TO BANK-SIGNON-ID.                      SBANK10P
018000     MOVE EXT-IP10-PSWD TO BANK-PSWD.                             SBANK10P
018100     GO TO SCREEN10-READ-EXIT.                                    SBANK10P
018200                                                                  SBANK10P
018300 SCREEN10-READ-EXIT.                                              SBANK10P
018400     EXIT.                                                        SBANK10P
018500                                                                  SBANK10P
018600***************************************************************** SBANK10P
018700* Screen processing for SCREEN10 (BANK10/HELP10)                * SBANK10P
018800*---------------------------------------------------------------* SBANK10P
018900* Build the output screen and send it                           * SBANK10P
019000***************************************************************** SBANK10P
019100 SCREEN10-BUILD-AND-SEND.                                         SBANK10P
019200* Clear map area, get date & time and move to the map             SBANK10P
019300     MOVE LOW-VALUES TO BANK10AO.                                 SBANK10P
019400     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK10P
019500     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK10P
019600     SET DDI-YYDDD TO TRUE.                                       SBANK10P
019700     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK10P
019800     PERFORM CALL-DATECONV THRU                                   SBANK10P
019900             CALL-DATECONV-EXIT.                                  SBANK10P
020000* Ensure the last map fields are correct                          SBANK10P
020100     IF BANK-HELP-ACTIVE                                          SBANK10P
020200        MOVE 'MBANK10' TO BANK-LAST-MAPSET                        SBANK10P
020300        MOVE 'HELP10A' TO BANK-LAST-MAP                           SBANK10P
020400     ELSE                                                         SBANK10P
020500        MOVE 'MBANK10' TO BANK-LAST-MAPSET                        SBANK10P
020600        MOVE 'BANK10A' TO BANK-LAST-MAP                           SBANK10P
020700     END-IF.                                                      SBANK10P
020800     IF BANK-ENV-CICS                                             SBANK10P
020900        GO TO SCREEN10-BUILD-AND-SEND-CICS                        SBANK10P
021000     ELSE                                                         SBANK10P
021100        GO TO SCREEN10-BUILD-AND-SEND-INET                        SBANK10P
021200     END-IF.                                                      SBANK10P
021300                                                                  SBANK10P
021400 SCREEN10-BUILD-AND-SEND-CICS.                                    SBANK10P
021500     IF BANK-LAST-MAP IS EQUAL TO 'BANK10A'                       SBANK10P
021600        GO TO BANK10-BUILD-AND-SEND-CICS                          SBANK10P
021700     END-IF.                                                      SBANK10P
021800     IF BANK-LAST-MAP IS EQUAL TO 'HELP10A'                       SBANK10P
021900        GO TO HELP10-BUILD-AND-SEND-CICS                          SBANK10P
022000     END-IF.                                                      SBANK10P
022100     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK10P
022200     MOVE '0003' TO ABEND-CODE                                    SBANK10P
022300     MOVE SPACES TO ABEND-REASON                                  SBANK10P
022400     COPY CABENDPO.                                               SBANK10P
022500     GOBACK.                                                      SBANK10P
022600                                                                  SBANK10P
022700 BANK10-BUILD-AND-SEND-CICS.                                      SBANK10P
022800     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK10AO==.        SBANK10P
022900     MOVE WS-TRAN-ID TO TRANO IN BANK10AO.                        SBANK10P
023000     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK10AO.                    SBANK10P
023100     MOVE DDO-DATA TO DATEO IN BANK10AO.                          SBANK10P
023200* Move in any error message                                       SBANK10P
023300     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK10AO.                  SBANK10P
023400* Move in screen specific fields                                  SBANK10P
023500     MOVE -1 TO USERIDL IN BANK10AI.                              SBANK10P
023600     MOVE BANK-SIGNON-ID TO USERIDO IN BANK10AO.                  SBANK10P
023700     MOVE -1 TO PSWDL IN BANK10AI.                                SBANK10P
023800     MOVE BANK-PSWD TO PSWDO IN BANK10AO.                         SBANK10P
023900* Turn colour off if required                                     SBANK10P
024000     IF COLOUR-OFF                                                SBANK10P
024100        MOVE DFHGREEN TO TXT01C IN BANK10AO                       SBANK10P
024200        MOVE DFHGREEN TO SCRNC IN BANK10AO                        SBANK10P
024300        MOVE DFHGREEN TO HEAD1C IN BANK10AO                       SBANK10P
024400        MOVE DFHGREEN TO DATEC IN BANK10AO                        SBANK10P
024500        MOVE DFHGREEN TO TXT02C IN BANK10AO                       SBANK10P
024600        MOVE DFHGREEN TO TRANC IN BANK10AO                        SBANK10P
024700        MOVE DFHGREEN TO HEAD2C IN BANK10AO                       SBANK10P
024800        MOVE DFHGREEN TO TIMEC IN BANK10AO                        SBANK10P
024900        MOVE DFHGREEN TO TXT03C IN BANK10AO                       SBANK10P
025000        MOVE DFHGREEN TO TXT04C IN BANK10AO                       SBANK10P
025100        MOVE DFHGREEN TO TXT05C IN BANK10AO                       SBANK10P
025200        MOVE DFHGREEN TO TXT06C IN BANK10AO                       SBANK10P
025300        MOVE DFHGREEN TO USERIDC IN BANK10AO                      SBANK10P
025400        MOVE DFHGREEN TO TXT07C IN BANK10AO                       SBANK10P
025500        MOVE DFHGREEN TO PSWDC IN BANK10AO                        SBANK10P
025600        MOVE DFHGREEN TO ERRMSGC IN BANK10AO                      SBANK10P
025700        MOVE DFHGREEN TO TXT08C IN BANK10AO                       SBANK10P
025800     END-IF.                                                      SBANK10P
025900                                                                  SBANK10P
026000     EXEC CICS SEND MAP('BANK10A')                                SBANK10P
026100                    MAPSET('MBANK10')                             SBANK10P
026200                    ERASE                                         SBANK10P
026300                    FREEKB                                        SBANK10P
026400     END-EXEC.                                                    SBANK10P
026500     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          SBANK10P
026600                                                                  SBANK10P
026700 HELP10-BUILD-AND-SEND-CICS.                                      SBANK10P
026800     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK10P
026900                             ==<<SCRN>>== BY ==HELP10AO==.        SBANK10P
027000                                                                  SBANK10P
027100     EXEC CICS SEND MAP('HELP10A')                                SBANK10P
027200                    MAPSET('MBANK10')                             SBANK10P
027300                    ERASE                                         SBANK10P
027400                    FREEKB                                        SBANK10P
027500     END-EXEC.                                                    SBANK10P
027600     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          SBANK10P
027700                                                                  SBANK10P
027800                                                                  SBANK10P
027900 SCREEN10-BUILD-AND-SEND-INET.                                    SBANK10P
028000     MOVE SPACES TO EXT-OP-DATA.                                  SBANK10P
028100     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK10P
028200     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK10P
028300     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK10P
028400     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK10P
028500     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK10P
028600     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK10P
028700* Move in screen name                                             SBANK10P
028800     MOVE 'BANK10' TO EXT-OP-SCREEN.                              SBANK10P
028900* Move in userid and any error message                            SBANK10P
029000     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK10P
029100     MOVE BANK-SIGNON-ID TO EXT-OP-USERID.                        SBANK10P
029200     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK10P
029300* Move in screen specific fields                                  SBANK10P
029400     MOVE BANK-PSWD TO EXT-OP10-PSWD.                             SBANK10P
029500     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          SBANK10P
029600                                                                  SBANK10P
029700 SCREEN10-BUILD-AND-SEND-EXIT.                                    SBANK10P
029800     EXIT.                                                        SBANK10P
029900                                                                  SBANK10P
030000***************************************************************** SBANK10P
030100* Call common routine to perform date conversions               * SBANK10P
030200***************************************************************** SBANK10P
030300 CALL-DATECONV.                                                   SBANK10P
030400     MOVE BANK-ENV TO DD-ENV.                                     SBANK10P
030500     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK10P
030600     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK10P
030700 CALL-DATECONV-EXIT.                                              SBANK10P
030800     EXIT.                                                        SBANK10P
030900                                                                  SBANK10P
031000* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK10P
