000100***************************************************************** SBANK80P
000200*                                                               * SBANK80P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK80P
000400*   This demonstration program is provided for use by users     * SBANK80P
000500*   of Micro Focus products and may be used, modified and       * SBANK80P
000600*   distributed as part of your application provided that       * SBANK80P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK80P
000800*   in this material.                                           * SBANK80P
000900*                                                               * SBANK80P
001000***************************************************************** SBANK80P
001100                                                                  SBANK80P
001200***************************************************************** SBANK80P
001300* Program:     SBANK80P.CBL (CICS Version)                      * SBANK80P
001400* Layer:       Screen handling                                  * SBANK80P
001500* Function:    Request printing of statements                   * SBANK80P
001600***************************************************************** SBANK80P
001700                                                                  SBANK80P
001800 IDENTIFICATION DIVISION.                                         SBANK80P
001900 PROGRAM-ID.                                                      SBANK80P
002000     SBANK80P.                                                    SBANK80P
002100 DATE-WRITTEN.                                                    SBANK80P
002200     September 2002.                                              SBANK80P
002300 DATE-COMPILED.                                                   SBANK80P
002400     Today.                                                       SBANK80P
002500                                                                  SBANK80P
002600 ENVIRONMENT DIVISION.                                            SBANK80P
002700                                                                  SBANK80P
002800 DATA DIVISION.                                                   SBANK80P
002900 WORKING-STORAGE SECTION.                                         SBANK80P
003000 01  WS-MISC-STORAGE.                                             SBANK80P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK80P
003200       VALUE 'SBANK80P'.                                          SBANK80P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK80P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK80P
003500       VALUE SPACES.                                              SBANK80P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK80P
003700       VALUE 'UNKNOWN'.                                           SBANK80P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK80P
003900   05  WS-WORK1                              PIC X(1).            SBANK80P
004000   05  WS-SUB1                               PIC S9(4) COMP.      SBANK80P
004100                                                                  SBANK80P
004200 01  MAPAREA                                 PIC X(2048).         SBANK80P
004300 COPY MBANK80.                                                    SBANK80P
004400                                                                  SBANK80P
004500 01  WS-TIME-DATE-WORK-AREA.                                      SBANK80P
004600 COPY CDATED.                                                     SBANK80P
004700                                                                  SBANK80P
004800 01  WS-BANK-DATA-AREAS.                                          SBANK80P
004900   05  WS-BANK-DATA.                                              SBANK80P
005000 COPY CBANKDAT.                                                   SBANK80P
005100   05  WS-BANK-EXT-DATA.                                          SBANK80P
005200 COPY CBANKEXT.                                                   SBANK80P
005300                                                                  SBANK80P
005400 COPY CSCRNHDD.                                                   SBANK80P
005500                                                                  SBANK80P
005600 COPY DFHAID.                                                     SBANK80P
005700                                                                  SBANK80P
005800 COPY DFHBMSCA.                                                   SBANK80P
005900                                                                  SBANK80P
006000 COPY CABENDD.                                                    SBANK80P
006100                                                                  SBANK80P
006200 LINKAGE SECTION.                                                 SBANK80P
006300 01  DFHCOMMAREA.                                                 SBANK80P
006400   05  FILLER                                PIC X(7168).         SBANK80P
006500                                                                  SBANK80P
006600 PROCEDURE DIVISION.                                              SBANK80P
006700***************************************************************** SBANK80P
006800* Write entry to log to show we have been invoked               * SBANK80P
006900***************************************************************** SBANK80P
007000     COPY CTRACE.                                                 SBANK80P
007100                                                                  SBANK80P
007200***************************************************************** SBANK80P
007300* Store our transaction-id                                      * SBANK80P
007400***************************************************************** SBANK80P
007500     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK80P
007600                                                                  SBANK80P
007700***************************************************************** SBANK80P
007800* Store passed data or abend if there wasn't any                * SBANK80P
007900***************************************************************** SBANK80P
008000     IF EIBCALEN IS EQUAL TO 0                                    SBANK80P
008100        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK80P
008200        MOVE '0001' TO ABEND-CODE                                 SBANK80P
008300        MOVE SPACES TO ABEND-REASON                               SBANK80P
008400        COPY CABENDPO.                                            SBANK80P
008500     ELSE                                                         SBANK80P
008600        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK80P
008700        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK80P
008800        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK80P
008900     END-IF.                                                      SBANK80P
009000                                                                  SBANK80P
009100***************************************************************** SBANK80P
009200* This is the main process                                      * SBANK80P
009300***************************************************************** SBANK80P
009400                                                                  SBANK80P
009500***************************************************************** SBANK80P
009600* Determine what we have to do (read from or send to screen)    * SBANK80P
009700***************************************************************** SBANK80P
009800     MOVE LOW-VALUE TO MAPAREA.                                   SBANK80P
009900     EVALUATE TRUE                                                SBANK80P
010000       WHEN BANK-MAP-FUNCTION-GET                                 SBANK80P
010100         PERFORM SCREEN80-READ THRU                               SBANK80P
010200                 SCREEN80-READ-EXIT                               SBANK80P
010300       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK80P
010400         PERFORM SCREEN80-BUILD-AND-SEND THRU                     SBANK80P
010500                 SCREEN80-BUILD-AND-SEND-EXIT                     SBANK80P
010600       WHEN OTHER                                                 SBANK80P
010700         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK80P
010800         MOVE '0002' TO ABEND-CODE                                SBANK80P
010900         MOVE SPACES TO ABEND-REASON                              SBANK80P
011000         COPY CABENDPO.                                           SBANK80P
011100     END-EVALUATE.                                                SBANK80P
011200                                                                  SBANK80P
011300* Call the appropriate routine to handle the business logic       SBANK80P
011400     IF BANK-MAP-FUNCTION-GET                                     SBANK80P
011500        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK80P
011600                       COMMAREA(WS-BANK-DATA)                     SBANK80P
011700                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK80P
011800        END-EXEC                                                  SBANK80P
011900     END-IF.                                                      SBANK80P
012000                                                                  SBANK80P
012100***************************************************************** SBANK80P
012200* Now we have to have finished and can return to our invoker.   * SBANK80P
012300***************************************************************** SBANK80P
012400* Now return to CICS                                              SBANK80P
012500     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK80P
012600          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK80P
012700     EXEC CICS                                                    SBANK80P
012800          RETURN                                                  SBANK80P
012900     END-EXEC.                                                    SBANK80P
013000     GOBACK.                                                      SBANK80P
013100                                                                  SBANK80P
013200***************************************************************** SBANK80P
013300* Screen processing for MBANK80                                 * SBANK80P
013400*---------------------------------------------------------------* SBANK80P
013500* Retrieve data from screen and format it                       * SBANK80P
013600***************************************************************** SBANK80P
013700 SCREEN80-READ.                                                   SBANK80P
013800     MOVE 'BBANK80P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK80P
013900     IF BANK-AID-CLEAR                                            SBANK80P
014000        SET BANK-AID-PFK03 TO TRUE                                SBANK80P
014100        GO TO SCREEN80-READ-EXIT                                  SBANK80P
014200     END-IF.                                                      SBANK80P
014300     IF BANK-ENV-CICS                                             SBANK80P
014400        GO TO SCREEN80-READ-CICS                                  SBANK80P
014500     ELSE                                                         SBANK80P
014600        GO TO SCREEN80-READ-INET                                  SBANK80P
014700     END-IF.                                                      SBANK80P
014800                                                                  SBANK80P
014900 SCREEN80-READ-CICS.                                              SBANK80P
015000     IF BANK-HELP-INACTIVE                                        SBANK80P
015100        EXEC CICS RECEIVE MAP('BANK80A')                          SBANK80P
015200                          MAPSET('MBANK80')                       SBANK80P
015300        END-EXEC                                                  SBANK80P
015400     ELSE                                                         SBANK80P
015500        EXEC CICS RECEIVE MAP('HELP80A')                          SBANK80P
015600                          MAPSET('MBANK80')                       SBANK80P
015700        END-EXEC                                                  SBANK80P
015800        GO TO SCREEN80-READ-EXIT                                  SBANK80P
015900     END-IF.                                                      SBANK80P
016000                                                                  SBANK80P
016100     IF OPT1L IN BANK80AI IS EQUAL TO 0                           SBANK80P
016200        MOVE LOW-VALUES TO BANK-SCR80-OPT1                        SBANK80P
016300     ELSE                                                         SBANK80P
016400        MOVE OPT1I IN BANK80AI TO BANK-SCR80-OPT1                 SBANK80P
016500        IF BANK-SCR80-OPT1 IS EQUAL TO SPACES OR                  SBANK80P
016600           BANK-SCR80-OPT1 IS EQUAL TO '_'                        SBANK80P
016700           MOVE LOW-VALUES TO BANK-SCR80-OPT1                     SBANK80P
016800        END-IF                                                    SBANK80P
016900     END-IF.                                                      SBANK80P
017000                                                                  SBANK80P
017100     IF OPT2L IN BANK80AI IS EQUAL TO 0                           SBANK80P
017200        MOVE LOW-VALUES TO BANK-SCR80-OPT2                        SBANK80P
017300     ELSE                                                         SBANK80P
017400        MOVE OPT2I IN BANK80AI TO BANK-SCR80-OPT2                 SBANK80P
017500        IF BANK-SCR80-OPT2 IS EQUAL TO SPACES OR                  SBANK80P
017600           BANK-SCR80-OPT2 IS EQUAL TO '_'                        SBANK80P
017700           MOVE LOW-VALUES TO BANK-SCR80-OPT2                     SBANK80P
017800        END-IF                                                    SBANK80P
017900     END-IF.                                                      SBANK80P
018000                                                                  SBANK80P
018100     GO TO SCREEN80-READ-EXIT.                                    SBANK80P
018200                                                                  SBANK80P
018300 SCREEN80-READ-INET.                                              SBANK80P
018400     MOVE EXT-IP80-OPT1 TO BANK-SCR80-OPT1.                       SBANK80P
018500     MOVE EXT-IP80-OPT2 TO BANK-SCR80-OPT2.                       SBANK80P
018600     IF BANK-AID-PFK10                                            SBANK80P
018700        SET PRINT-CONFIRM TO TRUE                                 SBANK80P
018800     END-IF.                                                      SBANK80P
018900     GO TO SCREEN80-READ-EXIT.                                    SBANK80P
019000                                                                  SBANK80P
019100 SCREEN80-READ-EXIT.                                              SBANK80P
019200     EXIT.                                                        SBANK80P
019300                                                                  SBANK80P
019400***************************************************************** SBANK80P
019500* Screen processing for SCREEN80 (BANK80/HELP80)                * SBANK80P
019600*---------------------------------------------------------------* SBANK80P
019700* Build the output screen and send it                           * SBANK80P
019800***************************************************************** SBANK80P
019900 SCREEN80-BUILD-AND-SEND.                                         SBANK80P
020000* Clear map area, get date & time and move to the map             SBANK80P
020100     MOVE LOW-VALUES TO BANK80AO.                                 SBANK80P
020200     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK80P
020300     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK80P
020400     SET DDI-YYDDD TO TRUE.                                       SBANK80P
020500     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK80P
020600     PERFORM CALL-DATECONV THRU                                   SBANK80P
020700             CALL-DATECONV-EXIT.                                  SBANK80P
020800* Ensure the last map fields are correct                          SBANK80P
020900     IF BANK-HELP-ACTIVE                                          SBANK80P
021000        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        SBANK80P
021100        MOVE 'HELP80A' TO BANK-LAST-MAP                           SBANK80P
021200     ELSE                                                         SBANK80P
021300        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        SBANK80P
021400        MOVE 'BANK80A' TO BANK-LAST-MAP                           SBANK80P
021500     END-IF.                                                      SBANK80P
021600     IF BANK-ENV-CICS                                             SBANK80P
021700        GO TO SCREEN80-BUILD-AND-SEND-CICS                        SBANK80P
021800     ELSE                                                         SBANK80P
021900        GO TO SCREEN80-BUILD-AND-SEND-INET                        SBANK80P
022000     END-IF.                                                      SBANK80P
022100                                                                  SBANK80P
022200 SCREEN80-BUILD-AND-SEND-CICS.                                    SBANK80P
022300     IF BANK-LAST-MAP IS EQUAL TO 'BANK80A'                       SBANK80P
022400        GO TO BANK80-BUILD-AND-SEND-CICS                          SBANK80P
022500     END-IF.                                                      SBANK80P
022600     IF BANK-LAST-MAP IS EQUAL TO 'HELP80A'                       SBANK80P
022700        GO TO HELP80-BUILD-AND-SEND-CICS                          SBANK80P
022800     END-IF.                                                      SBANK80P
022900     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK80P
023000     MOVE '0003' TO ABEND-CODE                                    SBANK80P
023100     MOVE SPACES TO ABEND-REASON                                  SBANK80P
023200     COPY CABENDPO.                                               SBANK80P
023300     GOBACK.                                                      SBANK80P
023400                                                                  SBANK80P
023500 BANK80-BUILD-AND-SEND-CICS.                                      SBANK80P
023600     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK80AO==.        SBANK80P
023700     MOVE WS-TRAN-ID TO TRANO IN BANK80AO.                        SBANK80P
023800     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK80AO.                    SBANK80P
023900     MOVE DDO-DATA TO DATEO IN BANK80AO.                          SBANK80P
024000* Move in any error message                                       SBANK80P
024100     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK80AO.                  SBANK80P
024200* Move in screen specific fields                                  SBANK80P
024300     MOVE BANK-SCR80-CONTACT-ID TO USERIDO IN BANK80AO.           SBANK80P
024400     MOVE BANK-SCR80-CONTACT-NAME TO USERNMO IN BANK80AO.         SBANK80P
024500                                                                  SBANK80P
024600     MOVE BANK-SCR80-ADDR1 TO MADDR1O IN BANK80AO.                SBANK80P
024700     MOVE BANK-SCR80-ADDR2 TO MADDR2O IN BANK80AO.                SBANK80P
024800     MOVE BANK-SCR80-STATE TO MSTATEO IN BANK80AO.                SBANK80P
024900     MOVE BANK-SCR80-CNTRY TO MCNTRYO IN BANK80AO.                SBANK80P
025000     MOVE BANK-SCR80-PSTCDE TO MPSTCDEO IN BANK80AO.              SBANK80P
025100     MOVE BANK-SCR80-EMAIL TO MEMAILO IN BANK80AO.                SBANK80P
025200                                                                  SBANK80P
025300     IF BANK-SCR80-EMAIL IS NOT EQUAL TO SPACES                   SBANK80P
025400        MOVE -1 TO OPT1L IN BANK80AI                              SBANK80P
025500        IF BANK-SCR80-OPT1 IS EQUAL TO LOW-VALUES                 SBANK80P
025600           MOVE ALL '_' TO OPT1O IN BANK80AO                      SBANK80P
025700        ELSE                                                      SBANK80P
025800           MOVE BANK-SCR80-OPT1 TO OPT1O IN BANK80AO              SBANK80P
025900        END-IF                                                    SBANK80P
026000        MOVE -1 TO OPT2L IN BANK80AI                              SBANK80P
026100        IF BANK-SCR80-OPT2 IS EQUAL TO LOW-VALUES                 SBANK80P
026200           MOVE ALL '_' TO OPT2O IN BANK80AO                      SBANK80P
026300        ELSE                                                      SBANK80P
026400           MOVE BANK-SCR80-OPT2 TO OPT2O IN BANK80AO              SBANK80P
026500        END-IF                                                    SBANK80P
026600        IF BANK-SCR80-OPT2 IS EQUAL TO HIGH-VALUES                SBANK80P
026700           MOVE LOW-VALUES TO BANK-SCR80-OPT2                     SBANK80P
026800           MOVE ALL '_' TO OPT2O IN BANK80AO                      SBANK80P
026900           MOVE DFHBMPRF TO OPT2A IN BANK80AI                     SBANK80P
027000        END-IF                                                    SBANK80P
027100     ELSE                                                         SBANK80P
027200        MOVE -1 TO TXT01L IN BANK80AI                             SBANK80P
027300        MOVE '(not available)' to MEMAILO IN BANK80AO             SBANK80P
027400        MOVE SPACES TO TXT07O IN BANK80AO                         SBANK80P
027500        MOVE SPACES TO TXT08O IN BANK80AO                         SBANK80P
027600        MOVE SPACES TO OPT1O IN BANK80AO                          SBANK80P
027700        MOVE SPACES TO TXT09O IN BANK80AO                         SBANK80P
027800        MOVE SPACES TO OPT2O IN BANK80AO                          SBANK80P
027900        MOVE DFHBMPRF TO OPT1A IN BANK80AI                        SBANK80P
028000        MOVE DFHBMPRF TO OPT2A IN BANK80AI                        SBANK80P
028100     END-IF.                                                      SBANK80P
028200                                                                  SBANK80P
028300* Turn colour off if required                                     SBANK80P
028400     IF COLOUR-OFF                                                SBANK80P
028500        MOVE DFHGREEN TO TXT01C IN BANK80AO                       SBANK80P
028600        MOVE DFHGREEN TO SCRNC IN BANK80AO                        SBANK80P
028700        MOVE DFHGREEN TO HEAD1C IN BANK80AO                       SBANK80P
028800        MOVE DFHGREEN TO DATEC IN BANK80AO                        SBANK80P
028900        MOVE DFHGREEN TO TXT02C IN BANK80AO                       SBANK80P
029000        MOVE DFHGREEN TO TRANC IN BANK80AO                        SBANK80P
029100        MOVE DFHGREEN TO HEAD2C IN BANK80AO                       SBANK80P
029200        MOVE DFHGREEN TO TIMEC IN BANK80AO                        SBANK80P
029300        MOVE DFHGREEN TO TXT03C IN BANK80AO                       SBANK80P
029400        MOVE DFHGREEN TO USERIDC IN BANK80AO                      SBANK80P
029500        MOVE DFHGREEN TO TXT04C IN BANK80AO                       SBANK80P
029600        MOVE DFHGREEN TO USERNMC IN BANK80AO                      SBANK80P
029700        MOVE DFHGREEN TO TXT05C IN BANK80AO                       SBANK80P
029800        MOVE DFHGREEN TO MADDR1C IN BANK80AO                      SBANK80P
029900        MOVE DFHGREEN TO MADDR2C IN BANK80AO                      SBANK80P
030000        MOVE DFHGREEN TO MSTATEC IN BANK80AO                      SBANK80P
030100        MOVE DFHGREEN TO MCNTRYC IN BANK80AO                      SBANK80P
030200        MOVE DFHGREEN TO MPSTCDEC IN BANK80AO                     SBANK80P
030300        MOVE DFHGREEN TO TXT06C IN BANK80AO                       SBANK80P
030400        MOVE DFHGREEN TO MEMAILC IN BANK80AO                      SBANK80P
030500        MOVE DFHGREEN TO TXT07C IN BANK80AO                       SBANK80P
030600        MOVE DFHGREEN TO TXT08C IN BANK80AO                       SBANK80P
030700        MOVE DFHGREEN TO OPT1C IN BANK80AO                        SBANK80P
030800        MOVE DFHGREEN TO TXT09C IN BANK80AO                       SBANK80P
030900        MOVE DFHGREEN TO OPT2C IN BANK80AO                        SBANK80P
031000        MOVE DFHGREEN TO ERRMSGC IN BANK80AO                      SBANK80P
031100        MOVE DFHGREEN TO TXT17C IN BANK80AO                       SBANK80P
031200     END-IF.                                                      SBANK80P
031300                                                                  SBANK80P
031400     EXEC CICS SEND MAP('BANK80A')                                SBANK80P
031500                    MAPSET('MBANK80')                             SBANK80P
031600                    CURSOR                                        SBANK80P
031700                    ERASE                                         SBANK80P
031800                    FREEKB                                        SBANK80P
031900     END-EXEC.                                                    SBANK80P
032000     GO TO SCREEN80-BUILD-AND-SEND-EXIT.                          SBANK80P
032100                                                                  SBANK80P
032200 HELP80-BUILD-AND-SEND-CICS.                                      SBANK80P
032300     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK80P
032400                             ==<<SCRN>>== BY ==HELP80AO==.        SBANK80P
032500                                                                  SBANK80P
032600     EXEC CICS SEND MAP('HELP80A')                                SBANK80P
032700                    MAPSET('MBANK80')                             SBANK80P
032800                    ERASE                                         SBANK80P
032900                    FREEKB                                        SBANK80P
033000     END-EXEC.                                                    SBANK80P
033100     GO TO SCREEN80-BUILD-AND-SEND-EXIT.                          SBANK80P
033200                                                                  SBANK80P
033300 SCREEN80-BUILD-AND-SEND-INET.                                    SBANK80P
033400     MOVE SPACES TO EXT-OP-DATA.                                  SBANK80P
033500     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK80P
033600     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK80P
033700     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK80P
033800     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK80P
033900     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK80P
034000     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK80P
034100* Move in screen name                                             SBANK80P
034200     MOVE 'BANK80' TO EXT-OP-SCREEN.                              SBANK80P
034300* Move in userid and any error message                            SBANK80P
034400     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK80P
034500     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK80P
034600     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK80P
034700* Move in screen specific fields                                  SBANK80P
034800     MOVE BANK-SCR80-ADDR1 TO EXT-OP80-ADDR1.                     SBANK80P
034900     MOVE BANK-SCR80-ADDR2 TO EXT-OP80-ADDR2.                     SBANK80P
035000     MOVE BANK-SCR80-STATE TO EXT-OP80-STATE.                     SBANK80P
035100     MOVE BANK-SCR80-CNTRY TO EXT-OP80-CNTRY.                     SBANK80P
035200     MOVE BANK-SCR80-PSTCDE TO EXT-OP80-PSTCDE.                   SBANK80P
035300     MOVE BANK-SCR80-EMAIL TO EXT-OP80-EMAIL.                     SBANK80P
035400     MOVE BANK-SCR80-OPT1 TO EXT-OP80-OPT1.                       SBANK80P
035500     MOVE BANK-SCR80-OPT2 TO EXT-OP80-OPT2.                       SBANK80P
035600                                                                  SBANK80P
035700 SCREEN80-BUILD-AND-SEND-EXIT.                                    SBANK80P
035800     EXIT.                                                        SBANK80P
035900                                                                  SBANK80P
036000***************************************************************** SBANK80P
036100* Call common routine to perform date conversions               * SBANK80P
036200***************************************************************** SBANK80P
036300 CALL-DATECONV.                                                   SBANK80P
036400     MOVE BANK-ENV TO DD-ENV.                                     SBANK80P
036500     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK80P
036600     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK80P
036700 CALL-DATECONV-EXIT.                                              SBANK80P
036800     EXIT.                                                        SBANK80P
036900                                                                  SBANK80P
037000* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK80P
