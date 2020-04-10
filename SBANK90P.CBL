000100***************************************************************** SBANK90P
000200*                                                               * SBANK90P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK90P
000400*   This demonstration program is provided for use by users     * SBANK90P
000500*   of Micro Focus products and may be used, modified and       * SBANK90P
000600*   distributed as part of your application provided that       * SBANK90P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK90P
000800*   in this material.                                           * SBANK90P
000900*                                                               * SBANK90P
001000***************************************************************** SBANK90P
001100                                                                  SBANK90P
001200***************************************************************** SBANK90P
001300* Program:     SBANK90P.CBL (CICS Version)                      * SBANK90P
001400* Layer:       Screen handling                                  * SBANK90P
001500* Function:    Display request for informtion                   * SBANK90P
001600***************************************************************** SBANK90P
001700                                                                  SBANK90P
001800 IDENTIFICATION DIVISION.                                         SBANK90P
001900 PROGRAM-ID.                                                      SBANK90P
002000     SBANK90P.                                                    SBANK90P
002100 DATE-WRITTEN.                                                    SBANK90P
002200     September 2002.                                              SBANK90P
002300 DATE-COMPILED.                                                   SBANK90P
002400     Today.                                                       SBANK90P
002500                                                                  SBANK90P
002600 ENVIRONMENT DIVISION.                                            SBANK90P
002700                                                                  SBANK90P
002800 DATA DIVISION.                                                   SBANK90P
002900 WORKING-STORAGE SECTION.                                         SBANK90P
003000 01  WS-MISC-STORAGE.                                             SBANK90P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK90P
003200       VALUE 'SBANK90P'.                                          SBANK90P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK90P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK90P
003500       VALUE SPACES.                                              SBANK90P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK90P
003700       VALUE 'UNKNOWN'.                                           SBANK90P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK90P
003900                                                                  SBANK90P
004000 01  MAPAREA                                 PIC X(2048).         SBANK90P
004100 COPY MBANK90.                                                    SBANK90P
004200                                                                  SBANK90P
004300 01  WS-TIME-DATE-WORK-AREA.                                      SBANK90P
004400 COPY CDATED.                                                     SBANK90P
004500                                                                  SBANK90P
004600 01  WS-BANK-DATA-AREAS.                                          SBANK90P
004700   05  WS-BANK-DATA.                                              SBANK90P
004800 COPY CBANKDAT.                                                   SBANK90P
004900   05  WS-BANK-EXT-DATA.                                          SBANK90P
005000 COPY CBANKEXT.                                                   SBANK90P
005100                                                                  SBANK90P
005200 COPY CSCRNHDD.                                                   SBANK90P
005300                                                                  SBANK90P
005400 COPY DFHAID.                                                     SBANK90P
005500                                                                  SBANK90P
005600 COPY DFHBMSCA.                                                   SBANK90P
005700                                                                  SBANK90P
005800 COPY CABENDD.                                                    SBANK90P
005900                                                                  SBANK90P
006000 LINKAGE SECTION.                                                 SBANK90P
006100 01  DFHCOMMAREA.                                                 SBANK90P
006200   05  FILLER                                PIC X(7168).         SBANK90P
006300                                                                  SBANK90P
006400 PROCEDURE DIVISION.                                              SBANK90P
006500***************************************************************** SBANK90P
006600* Write entry to log to show we have been invoked               * SBANK90P
006700***************************************************************** SBANK90P
006800     COPY CTRACE.                                                 SBANK90P
006900                                                                  SBANK90P
007000***************************************************************** SBANK90P
007100* Store our transaction-id                                      * SBANK90P
007200***************************************************************** SBANK90P
007300     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK90P
007400                                                                  SBANK90P
007500***************************************************************** SBANK90P
007600* Store passed data or abend if there wasn't any                * SBANK90P
007700***************************************************************** SBANK90P
007800     IF EIBCALEN IS EQUAL TO 0                                    SBANK90P
007900        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK90P
008000        MOVE '0001' TO ABEND-CODE                                 SBANK90P
008100        MOVE SPACES TO ABEND-REASON                               SBANK90P
008200        COPY CABENDPO.                                            SBANK90P
008300     ELSE                                                         SBANK90P
008400        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK90P
008500        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK90P
008600        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK90P
008700     END-IF.                                                      SBANK90P
008800                                                                  SBANK90P
008900***************************************************************** SBANK90P
009000* This is the main process                                      * SBANK90P
009100***************************************************************** SBANK90P
009200                                                                  SBANK90P
009300***************************************************************** SBANK90P
009400* Determine what we have to do (read from or send to screen)    * SBANK90P
009500***************************************************************** SBANK90P
009600     MOVE LOW-VALUE TO MAPAREA.                                   SBANK90P
009700     EVALUATE TRUE                                                SBANK90P
009800       WHEN BANK-MAP-FUNCTION-GET                                 SBANK90P
009900         PERFORM SCREEN90-READ THRU                               SBANK90P
010000                 SCREEN90-READ-EXIT                               SBANK90P
010100       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK90P
010200         PERFORM SCREEN90-BUILD-AND-SEND THRU                     SBANK90P
010300                 SCREEN90-BUILD-AND-SEND-EXIT                     SBANK90P
010400       WHEN OTHER                                                 SBANK90P
010500         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK90P
010600         MOVE '0001' TO ABEND-CODE                                SBANK90P
010700         MOVE SPACES TO ABEND-REASON                              SBANK90P
010800         COPY CABENDPO.                                           SBANK90P
010900     END-EVALUATE.                                                SBANK90P
011000                                                                  SBANK90P
011100* Call the appropriate routine to handle the business logic       SBANK90P
011200     IF BANK-MAP-FUNCTION-GET                                     SBANK90P
011300        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK90P
011400                       COMMAREA(WS-BANK-DATA)                     SBANK90P
011500                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK90P
011600        END-EXEC                                                  SBANK90P
011700     END-IF.                                                      SBANK90P
011800                                                                  SBANK90P
011900***************************************************************** SBANK90P
012000* Now we have to have finished and can return to our invoker.   * SBANK90P
012100***************************************************************** SBANK90P
012200* Now return to CICS                                              SBANK90P
012300     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK90P
012400          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK90P
012500     EXEC CICS                                                    SBANK90P
012600          RETURN                                                  SBANK90P
012700     END-EXEC.                                                    SBANK90P
012800     GOBACK.                                                      SBANK90P
012900                                                                  SBANK90P
013000***************************************************************** SBANK90P
013100* Screen processing for MBANK90                                 * SBANK90P
013200*---------------------------------------------------------------* SBANK90P
013300* Retrieve data from screen and format it                       * SBANK90P
013400***************************************************************** SBANK90P
013500 SCREEN90-READ.                                                   SBANK90P
013600     MOVE 'BBANK90P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK90P
013700     IF BANK-AID-CLEAR                                            SBANK90P
013800        SET BANK-AID-PFK03 TO TRUE                                SBANK90P
013900        GO TO SCREEN90-READ-EXIT                                  SBANK90P
014000     END-IF.                                                      SBANK90P
014100     IF BANK-ENV-CICS                                             SBANK90P
014200        GO TO SCREEN90-READ-CICS                                  SBANK90P
014300     ELSE                                                         SBANK90P
014400        GO TO SCREEN90-READ-INET                                  SBANK90P
014500     END-IF.                                                      SBANK90P
014600                                                                  SBANK90P
014700 SCREEN90-READ-CICS.                                              SBANK90P
014800     IF BANK-HELP-INACTIVE                                        SBANK90P
014900        EXEC CICS RECEIVE MAP('BANK90A')                          SBANK90P
015000                          MAPSET('MBANK90')                       SBANK90P
015100        END-EXEC                                                  SBANK90P
015200     ELSE                                                         SBANK90P
015300        EXEC CICS RECEIVE MAP('HELP90A')                          SBANK90P
015400                          MAPSET('MBANK90')                       SBANK90P
015500        END-EXEC                                                  SBANK90P
015600        GO TO SCREEN90-READ-EXIT                                  SBANK90P
015700     END-IF.                                                      SBANK90P
015800                                                                  SBANK90P
015900     GO TO SCREEN90-READ-EXIT.                                    SBANK90P
016000                                                                  SBANK90P
016100 SCREEN90-READ-INET.                                              SBANK90P
016200     GO TO SCREEN90-READ-EXIT.                                    SBANK90P
016300                                                                  SBANK90P
016400 SCREEN90-READ-EXIT.                                              SBANK90P
016500     EXIT.                                                        SBANK90P
016600                                                                  SBANK90P
016700***************************************************************** SBANK90P
016800* Screen processing for SCREEN90 (BANK90/HELP90)                * SBANK90P
016900*---------------------------------------------------------------* SBANK90P
017000* Build the output screen and send it                           * SBANK90P
017100***************************************************************** SBANK90P
017200 SCREEN90-BUILD-AND-SEND.                                         SBANK90P
017300* Clear map area, get date & time and move to the map             SBANK90P
017400     MOVE LOW-VALUES TO BANK90AO.                                 SBANK90P
017500     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK90P
017600     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK90P
017700     SET DDI-YYDDD TO TRUE.                                       SBANK90P
017800     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK90P
017900     PERFORM CALL-DATECONV THRU                                   SBANK90P
018000             CALL-DATECONV-EXIT.                                  SBANK90P
018100* Ensure the last map fields are correct                          SBANK90P
018200     IF BANK-HELP-ACTIVE                                          SBANK90P
018300        MOVE 'MHELP90' TO BANK-LAST-MAPSET                        SBANK90P
018400        MOVE 'HELP90A' TO BANK-LAST-MAP                           SBANK90P
018500     ELSE                                                         SBANK90P
018600        MOVE 'MBANK90' TO BANK-LAST-MAPSET                        SBANK90P
018700        MOVE 'BANK90A' TO BANK-LAST-MAP                           SBANK90P
018800     END-IF.                                                      SBANK90P
018900     IF BANK-ENV-CICS                                             SBANK90P
019000        GO TO SCREEN90-BUILD-AND-SEND-CICS                        SBANK90P
019100     ELSE                                                         SBANK90P
019200        GO TO SCREEN90-BUILD-AND-SEND-INET                        SBANK90P
019300     END-IF.                                                      SBANK90P
019400                                                                  SBANK90P
019500 SCREEN90-BUILD-AND-SEND-CICS.                                    SBANK90P
019600     IF BANK-LAST-MAP IS EQUAL TO 'BANK90A'                       SBANK90P
019700        GO TO BANK90-BUILD-AND-SEND-CICS                          SBANK90P
019800     END-IF.                                                      SBANK90P
019900     IF BANK-LAST-MAP IS EQUAL TO 'HELP90A'                       SBANK90P
020000        GO TO HELP90-BUILD-AND-SEND-CICS                          SBANK90P
020100     END-IF.                                                      SBANK90P
020200     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK90P
020300     MOVE '0003' TO ABEND-CODE                                    SBANK90P
020400     MOVE SPACES TO ABEND-REASON                                  SBANK90P
020500     COPY CABENDPO.                                               SBANK90P
020600     GOBACK.                                                      SBANK90P
020700                                                                  SBANK90P
020800 BANK90-BUILD-AND-SEND-CICS.                                      SBANK90P
020900     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK90AO==.        SBANK90P
021000     MOVE WS-TRAN-ID TO TRANO IN BANK90AO.                        SBANK90P
021100     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK90AO.                    SBANK90P
021200     MOVE DDO-DATA TO DATEO IN BANK90AO.                          SBANK90P
021300* Move in any error message                                       SBANK90P
021400     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK90AO.                  SBANK90P
021500* Move in screen specific fields                                  SBANK90P
021600* Move in screen specific fields                                  SBANK90P
021700        MOVE BANK-SCR90-LINE (01) TO INF01O IN BANK90AO.          SBANK90P
021800        MOVE BANK-SCR90-LINE (02) TO INF02O IN BANK90AO.          SBANK90P
021900        MOVE BANK-SCR90-LINE (03) TO INF03O IN BANK90AO.          SBANK90P
022000        MOVE BANK-SCR90-LINE (04) TO INF04O IN BANK90AO.          SBANK90P
022100        MOVE BANK-SCR90-LINE (05) TO INF05O IN BANK90AO.          SBANK90P
022200        MOVE BANK-SCR90-LINE (06) TO INF06O IN BANK90AO.          SBANK90P
022300        MOVE BANK-SCR90-LINE (07) TO INF07O IN BANK90AO.          SBANK90P
022400        MOVE BANK-SCR90-LINE (08) TO INF08O IN BANK90AO.          SBANK90P
022500        MOVE BANK-SCR90-LINE (09) TO INF09O IN BANK90AO.          SBANK90P
022600        MOVE BANK-SCR90-LINE (10) TO INF10O IN BANK90AO.          SBANK90P
022700        MOVE BANK-SCR90-LINE (11) TO INF11O IN BANK90AO.          SBANK90P
022800        MOVE BANK-SCR90-LINE (12) TO INF12O IN BANK90AO.          SBANK90P
022900        MOVE BANK-SCR90-LINE (13) TO INF13O IN BANK90AO.          SBANK90P
023000        MOVE BANK-SCR90-LINE (14) TO INF14O IN BANK90AO.          SBANK90P
023100        MOVE BANK-SCR90-LINE (15) TO INF15O IN BANK90AO.          SBANK90P
023200        MOVE BANK-SCR90-LINE (16) TO INF16O IN BANK90AO.          SBANK90P
023300        MOVE BANK-SCR90-LINE (17) TO INF17O IN BANK90AO.          SBANK90P
023400        MOVE BANK-SCR90-LINE (18) TO INF18O IN BANK90AO.          SBANK90P
023500        MOVE BANK-SCR90-LINE (19) TO INF19O IN BANK90AO.          SBANK90P
023600* Turn colour off if required                                     SBANK90P
023700     IF COLOUR-OFF                                                SBANK90P
023800        MOVE DFHGREEN TO TXT01C IN BANK90AO                       SBANK90P
023900        MOVE DFHGREEN TO SCRNC IN BANK90AO                        SBANK90P
024000        MOVE DFHGREEN TO HEAD1C IN BANK90AO                       SBANK90P
024100        MOVE DFHGREEN TO DATEC IN BANK90AO                        SBANK90P
024200        MOVE DFHGREEN TO TXT02C IN BANK90AO                       SBANK90P
024300        MOVE DFHGREEN TO TRANC IN BANK90AO                        SBANK90P
024400        MOVE DFHGREEN TO HEAD2C IN BANK90AO                       SBANK90P
024500        MOVE DFHGREEN TO TIMEC IN BANK90AO                        SBANK90P
024600        MOVE DFHGREEN TO INF01C IN BANK90AO                       SBANK90P
024700        MOVE DFHGREEN TO INF02C IN BANK90AO                       SBANK90P
024800        MOVE DFHGREEN TO INF03C IN BANK90AO                       SBANK90P
024900        MOVE DFHGREEN TO INF04C IN BANK90AO                       SBANK90P
025000        MOVE DFHGREEN TO INF05C IN BANK90AO                       SBANK90P
025100        MOVE DFHGREEN TO INF06C IN BANK90AO                       SBANK90P
025200        MOVE DFHGREEN TO INF07C IN BANK90AO                       SBANK90P
025300        MOVE DFHGREEN TO INF08C IN BANK90AO                       SBANK90P
025400        MOVE DFHGREEN TO INF09C IN BANK90AO                       SBANK90P
025500        MOVE DFHGREEN TO INF10C IN BANK90AO                       SBANK90P
025600        MOVE DFHGREEN TO INF11C IN BANK90AO                       SBANK90P
025700        MOVE DFHGREEN TO INF12C IN BANK90AO                       SBANK90P
025800        MOVE DFHGREEN TO INF13C IN BANK90AO                       SBANK90P
025900        MOVE DFHGREEN TO INF14C IN BANK90AO                       SBANK90P
026000        MOVE DFHGREEN TO INF15C IN BANK90AO                       SBANK90P
026100        MOVE DFHGREEN TO INF16C IN BANK90AO                       SBANK90P
026200        MOVE DFHGREEN TO INF17C IN BANK90AO                       SBANK90P
026300        MOVE DFHGREEN TO INF18C IN BANK90AO                       SBANK90P
026400        MOVE DFHGREEN TO INF19C IN BANK90AO                       SBANK90P
026500        MOVE DFHGREEN TO TXT03C IN BANK90AO                       SBANK90P
026600        MOVE DFHGREEN TO ERRMSGC IN BANK90AO                      SBANK90P
026700     END-IF.                                                      SBANK90P
026800                                                                  SBANK90P
026900     EXEC CICS SEND MAP('BANK90A')                                SBANK90P
027000                    MAPSET('MBANK90')                             SBANK90P
027100                    ERASE                                         SBANK90P
027200                    FREEKB                                        SBANK90P
027300     END-EXEC.                                                    SBANK90P
027400     GO TO SCREEN90-BUILD-AND-SEND-EXIT.                          SBANK90P
027500                                                                  SBANK90P
027600 HELP90-BUILD-AND-SEND-CICS.                                      SBANK90P
027700     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK90P
027800                             ==<<SCRN>>== BY ==HELP90AO==.        SBANK90P
027900                                                                  SBANK90P
028000     EXEC CICS SEND MAP('HELP90A')                                SBANK90P
028100                    MAPSET('MBANK90')                             SBANK90P
028200                    ERASE                                         SBANK90P
028300                    FREEKB                                        SBANK90P
028400     END-EXEC.                                                    SBANK90P
028500     GO TO SCREEN90-BUILD-AND-SEND-EXIT.                          SBANK90P
028600                                                                  SBANK90P
028700 SCREEN90-BUILD-AND-SEND-INET.                                    SBANK90P
028800     MOVE SPACES TO EXT-OP-DATA.                                  SBANK90P
028900     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK90P
029000     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK90P
029100     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK90P
029200     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK90P
029300     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK90P
029400     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK90P
029500* Move in screen name                                             SBANK90P
029600     MOVE 'BANK90' TO EXT-OP-SCREEN.                              SBANK90P
029700* Move in userid and any error message                            SBANK90P
029800     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK90P
029900     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK90P
030000     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK90P
030100* Move in screen specific fields                                  SBANK90P
030200*    MOVE BANK-SCR20-SEL5TX TO EXT-OP20-SEL5TX.                   SBANK90P
030300     GO TO SCREEN90-BUILD-AND-SEND-EXIT.                          SBANK90P
030400                                                                  SBANK90P
030500 SCREEN90-BUILD-AND-SEND-EXIT.                                    SBANK90P
030600     EXIT.                                                        SBANK90P
030700                                                                  SBANK90P
030800***************************************************************** SBANK90P
030900* Call common routine to perform date conversions               * SBANK90P
031000***************************************************************** SBANK90P
031100 CALL-DATECONV.                                                   SBANK90P
031200     MOVE BANK-ENV TO DD-ENV.                                     SBANK90P
031300     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK90P
031400     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK90P
031500 CALL-DATECONV-EXIT.                                              SBANK90P
031600     EXIT.                                                        SBANK90P
031700                                                                  SBANK90P
031800* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK90P
