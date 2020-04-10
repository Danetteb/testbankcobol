000100***************************************************************** SBANK35P
000200*                                                               * SBANK35P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK35P
000400*   This demonstration program is provided for use by users     * SBANK35P
000500*   of Micro Focus products and may be used, modified and       * SBANK35P
000600*   distributed as part of your application provided that       * SBANK35P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK35P
000800*   in this material.                                           * SBANK35P
000900*                                                               * SBANK35P
001000***************************************************************** SBANK35P
001100                                                                  SBANK35P
001200***************************************************************** SBANK35P
001300* Program:     SBANK35P.CBL (CICS Version)                      * SBANK35P
001400* Layer:       Screen handling                                  * SBANK35P
001500* Function:    Display account details (extended)               * SBANK35P
001600***************************************************************** SBANK35P
001700                                                                  SBANK35P
001800 IDENTIFICATION DIVISION.                                         SBANK35P
001900 PROGRAM-ID.                                                      SBANK35P
002000     SBANK35P.                                                    SBANK35P
002100 DATE-WRITTEN.                                                    SBANK35P
002200     September 2002.                                              SBANK35P
002300 DATE-COMPILED.                                                   SBANK35P
002400     Today.                                                       SBANK35P
002500                                                                  SBANK35P
002600 ENVIRONMENT DIVISION.                                            SBANK35P
002700                                                                  SBANK35P
002800 DATA DIVISION.                                                   SBANK35P
002900 WORKING-STORAGE SECTION.                                         SBANK35P
003000 01  WS-MISC-STORAGE.                                             SBANK35P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK35P
003200       VALUE 'SBANK35P'.                                          SBANK35P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK35P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK35P
003500       VALUE SPACES.                                              SBANK35P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK35P
003700       VALUE 'UNKNOWN'.                                           SBANK35P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK35P
003900   05  WS-WORK1                              PIC X(1).            SBANK35P
004000                                                                  SBANK35P
004100 01  MAPAREA                                 PIC X(2048).         SBANK35P
004200 COPY MBANK35.                                                    SBANK35P
004300                                                                  SBANK35P
004400 01  WS-TIME-DATE-WORK-AREA.                                      SBANK35P
004500 COPY CDATED.                                                     SBANK35P
004600                                                                  SBANK35P
004700 01  WS-BANK-DATA-AREAS.                                          SBANK35P
004800   05  WS-BANK-DATA.                                              SBANK35P
004900 COPY CBANKDAT.                                                   SBANK35P
005000   05  WS-BANK-EXT-DATA.                                          SBANK35P
005100 COPY CBANKEXT.                                                   SBANK35P
005200                                                                  SBANK35P
005300 COPY CSCRNHDD.                                                   SBANK35P
005400                                                                  SBANK35P
005500 COPY DFHAID.                                                     SBANK35P
005600                                                                  SBANK35P
005700 COPY DFHBMSCA.                                                   SBANK35P
005800                                                                  SBANK35P
005900 COPY CABENDD.                                                    SBANK35P
006000                                                                  SBANK35P
006100 LINKAGE SECTION.                                                 SBANK35P
006200 01  DFHCOMMAREA.                                                 SBANK35P
006300   05  FILLER                                 PIC X(7168).        SBANK35P
006400                                                                  SBANK35P
006500 PROCEDURE DIVISION.                                              SBANK35P
006600***************************************************************** SBANK35P
006700* Write entry to log to show we have been invoked               * SBANK35P
006800***************************************************************** SBANK35P
006900     COPY CTRACE.                                                 SBANK35P
007000                                                                  SBANK35P
007100***************************************************************** SBANK35P
007200* Store our transaction-id                                      * SBANK35P
007300***************************************************************** SBANK35P
007400     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK35P
007500                                                                  SBANK35P
007600***************************************************************** SBANK35P
007700* Store passed data or abend if there wasn't any                * SBANK35P
007800***************************************************************** SBANK35P
007900     IF EIBCALEN IS EQUAL TO 0                                    SBANK35P
008000        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK35P
008100        MOVE '0001' TO ABEND-CODE                                 SBANK35P
008200        MOVE SPACES TO ABEND-REASON                               SBANK35P
008300        COPY CABENDPO.                                            SBANK35P
008400     ELSE                                                         SBANK35P
008500        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK35P
008600        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK35P
008700        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK35P
008800     END-IF.                                                      SBANK35P
008900                                                                  SBANK35P
009000***************************************************************** SBANK35P
009100* This is the main process                                      * SBANK35P
009200***************************************************************** SBANK35P
009300                                                                  SBANK35P
009400***************************************************************** SBANK35P
009500* Determine what we have to do (read from or send to screen)    * SBANK35P
009600***************************************************************** SBANK35P
009700     MOVE LOW-VALUE TO MAPAREA.                                   SBANK35P
009800     EVALUATE TRUE                                                SBANK35P
009900       WHEN BANK-MAP-FUNCTION-GET                                 SBANK35P
010000         PERFORM SCREEN35-READ THRU                               SBANK35P
010100                 SCREEN35-READ-EXIT                               SBANK35P
010200       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK35P
010300         PERFORM SCREEN35-BUILD-AND-SEND THRU                     SBANK35P
010400                 SCREEN35-BUILD-AND-SEND-EXIT                     SBANK35P
010500       WHEN OTHER                                                 SBANK35P
010600         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK35P
010700         MOVE '0002' TO ABEND-CODE                                SBANK35P
010800         MOVE SPACES TO ABEND-REASON                              SBANK35P
010900         COPY CABENDPO.                                           SBANK35P
011000     END-EVALUATE.                                                SBANK35P
011100                                                                  SBANK35P
011200* Call the appropriate routine to handle the business logic       SBANK35P
011300     IF BANK-MAP-FUNCTION-GET                                     SBANK35P
011400        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK35P
011500                       COMMAREA(WS-BANK-DATA)                     SBANK35P
011600                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK35P
011700        END-EXEC                                                  SBANK35P
011800     END-IF.                                                      SBANK35P
011900                                                                  SBANK35P
012000***************************************************************** SBANK35P
012100* Now we have to have finished and can return to our invoker.   * SBANK35P
012200***************************************************************** SBANK35P
012300* Now return to CICS                                              SBANK35P
012400     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK35P
012500          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK35P
012600     EXEC CICS                                                    SBANK35P
012700          RETURN                                                  SBANK35P
012800     END-EXEC.                                                    SBANK35P
012900     GOBACK.                                                      SBANK35P
013000                                                                  SBANK35P
013100***************************************************************** SBANK35P
013200* Screen processing for MBANK35                                 * SBANK35P
013300*---------------------------------------------------------------* SBANK35P
013400* Retrieve data from screen and format it                       * SBANK35P
013500***************************************************************** SBANK35P
013600 SCREEN35-READ.                                                   SBANK35P
013700     MOVE 'BBANK35P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK35P
013800     IF BANK-AID-CLEAR                                            SBANK35P
013900        SET BANK-AID-PFK03 TO TRUE                                SBANK35P
014000        GO TO SCREEN35-READ-EXIT                                  SBANK35P
014100     END-IF.                                                      SBANK35P
014200     IF BANK-ENV-CICS                                             SBANK35P
014300        GO TO SCREEN35-READ-CICS                                  SBANK35P
014400     ELSE                                                         SBANK35P
014500        GO TO SCREEN35-READ-INET                                  SBANK35P
014600     END-IF.                                                      SBANK35P
014700                                                                  SBANK35P
014800 SCREEN35-READ-CICS.                                              SBANK35P
014900     IF BANK-HELP-INACTIVE                                        SBANK35P
015000        EXEC CICS RECEIVE MAP('BANK35A')                          SBANK35P
015100                          MAPSET('MBANK35')                       SBANK35P
015200        END-EXEC                                                  SBANK35P
015300     ELSE                                                         SBANK35P
015400        EXEC CICS RECEIVE MAP('HELP35A')                          SBANK35P
015500                          MAPSET('MBANK35')                       SBANK35P
015600        END-EXEC                                                  SBANK35P
015700        GO TO SCREEN35-READ-EXIT                                  SBANK35P
015800     END-IF.                                                      SBANK35P
015900                                                                  SBANK35P
016000     GO TO SCREEN35-READ-EXIT.                                    SBANK35P
016100                                                                  SBANK35P
016200 SCREEN35-READ-INET.                                              SBANK35P
016300     GO TO SCREEN35-READ-EXIT.                                    SBANK35P
016400                                                                  SBANK35P
016500 SCREEN35-READ-EXIT.                                              SBANK35P
016600     EXIT.                                                        SBANK35P
016700                                                                  SBANK35P
016800***************************************************************** SBANK35P
016900* Screen processing for SCREEN35 (BANK35/HELP35)                * SBANK35P
017000*---------------------------------------------------------------* SBANK35P
017100* Build the output screen and send it                           * SBANK35P
017200***************************************************************** SBANK35P
017300 SCREEN35-BUILD-AND-SEND.                                         SBANK35P
017400* Clear map area, get date & time and move to the map             SBANK35P
017500     MOVE LOW-VALUES TO BANK35AO.                                 SBANK35P
017600     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK35P
017700     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK35P
017800     SET DDI-YYDDD TO TRUE.                                       SBANK35P
017900     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK35P
018000     PERFORM CALL-DATECONV THRU                                   SBANK35P
018100             CALL-DATECONV-EXIT.                                  SBANK35P
018200* Ensure the last map fields are correct                          SBANK35P
018300     IF BANK-HELP-ACTIVE                                          SBANK35P
018400        MOVE 'MBANK35' TO BANK-LAST-MAPSET                        SBANK35P
018500        MOVE 'HELP35A' TO BANK-LAST-MAP                           SBANK35P
018600     ELSE                                                         SBANK35P
018700        MOVE 'MBANK35' TO BANK-LAST-MAPSET                        SBANK35P
018800        MOVE 'BANK35A' TO BANK-LAST-MAP                           SBANK35P
018900     END-IF.                                                      SBANK35P
019000     IF BANK-ENV-CICS                                             SBANK35P
019100        GO TO SCREEN35-BUILD-AND-SEND-CICS                        SBANK35P
019200     ELSE                                                         SBANK35P
019300        GO TO SCREEN35-BUILD-AND-SEND-INET                        SBANK35P
019400     END-IF.                                                      SBANK35P
019500                                                                  SBANK35P
019600 SCREEN35-BUILD-AND-SEND-CICS.                                    SBANK35P
019700     IF BANK-LAST-MAP IS EQUAL TO 'BANK35A'                       SBANK35P
019800        GO TO BANK35-BUILD-AND-SEND-CICS                          SBANK35P
019900     END-IF.                                                      SBANK35P
020000     IF BANK-LAST-MAP IS EQUAL TO 'HELP35A'                       SBANK35P
020100        GO TO HELP35-BUILD-AND-SEND-CICS                          SBANK35P
020200     END-IF.                                                      SBANK35P
020300     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK35P
020400     MOVE '0003' TO ABEND-CODE                                    SBANK35P
020500     MOVE SPACES TO ABEND-REASON                                  SBANK35P
020600     COPY CABENDPO.                                               SBANK35P
020700     GOBACK.                                                      SBANK35P
020800                                                                  SBANK35P
020900 BANK35-BUILD-AND-SEND-CICS.                                      SBANK35P
021000     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK35AO==.        SBANK35P
021100     MOVE WS-TRAN-ID TO TRANO IN BANK35AO.                        SBANK35P
021200     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK35AO.                    SBANK35P
021300     MOVE DDO-DATA TO DATEO IN BANK35AO.                          SBANK35P
021400* Move in any error message                                       SBANK35P
021500     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK35AO.                  SBANK35P
021600* Move in screen specific fields                                  SBANK35P
021700     MOVE BANK-USERID TO USERIDO IN BANK35AO.                     SBANK35P
021800     MOVE BANK-USERID-NAME TO USERNMO IN BANK35AO.                SBANK35P
021900                                                                  SBANK35P
022000     MOVE BANK-SCR35-ACC TO ACCNOO IN BANK35AO.                   SBANK35P
022100     MOVE BANK-SCR35-DSC TO ACCTYPEO IN BANK35AO.                 SBANK35P
022200                                                                  SBANK35P
022300     MOVE BANK-SCR35-BAL TO BALO IN BANK35AO.                     SBANK35P
022400     MOVE BANK-SCR35-DTE TO DTEO IN BANK35AO.                     SBANK35P
022500     IF BANK-SCR35-ATM-ENABLED IS EQUAL TO 'Y'                    SBANK35P
022600        MOVE 'Yes' TO ATMVISO IN BANK35AO                         SBANK35P
022700        MOVE BANK-SCR35-ATM-LIM TO ATMLIMO IN BANK35AO            SBANK35P
022800        MOVE BANK-SCR35-ATM-LDTE TO ATMLDTEO IN BANK35AO          SBANK35P
022900        MOVE BANK-SCR35-ATM-LAMT TO ATMLAMTO IN BANK35AO          SBANK35P
023000     ELSE                                                         SBANK35P
023100        MOVE 'No' TO ATMVISO IN BANK35AO                          SBANK35P
023200        MOVE SPACES TO TXT10O IN BANK35AO                         SBANK35P
023300        MOVE SPACES TO ATMLIMO IN BANK35AO                        SBANK35P
023400        MOVE SPACES TO TXT11O IN BANK35AO                         SBANK35P
023500        MOVE SPACES TO ATMLDTEO IN BANK35AO                       SBANK35P
023600        MOVE SPACES TO TXT12O IN BANK35AO                         SBANK35P
023700        MOVE SPACES TO ATMLAMTO IN BANK35AO                       SBANK35P
023800     END-IF.                                                      SBANK35P
023900     IF BANK-SCR35-RP1ACC IS NOT EQUAL TO SPACES                  SBANK35P
024000        MOVE BANK-SCR35-RP1DAY TO RP1DAYO IN BANK35AO             SBANK35P
024100        MOVE BANK-SCR35-RP1AMT TO RP1AMTO IN BANK35AO             SBANK35P
024200        MOVE BANK-SCR35-RP1PID TO RP1PIDO IN BANK35AO             SBANK35P
024300        MOVE '/' TO RP1SEPO IN BANK35AO                           SBANK35P
024400        MOVE BANK-SCR35-RP1ACC TO RP1ACCO IN BANK35AO             SBANK35P
024500        MOVE BANK-SCR35-RP1DTE TO RP1DTEO IN BANK35AO             SBANK35P
024600     ELSE                                                         SBANK35P
024700        MOVE SPACES TO RP1DAYO IN BANK35AO                        SBANK35P
024800        MOVE SPACES TO RP1AMTO IN BANK35AO                        SBANK35P
024900        MOVE SPACES TO RP1PIDO IN BANK35AO                        SBANK35P
025000        MOVE SPACES TO RP1SEPO IN BANK35AO                        SBANK35P
025100        MOVE SPACES TO RP1ACCO IN BANK35AO                        SBANK35P
025200        MOVE SPACES TO RP1DTEO IN BANK35AO                        SBANK35P
025300     END-IF.                                                      SBANK35P
025400     IF BANK-SCR35-RP2ACC IS NOT EQUAL TO SPACES                  SBANK35P
025500        MOVE BANK-SCR35-RP2DAY TO RP2DAYO IN BANK35AO             SBANK35P
025600        MOVE BANK-SCR35-RP2AMT TO RP2AMTO IN BANK35AO             SBANK35P
025700        MOVE BANK-SCR35-RP2PID TO RP2PIDO IN BANK35AO             SBANK35P
025800        MOVE '/' TO RP2SEPO IN BANK35AO                           SBANK35P
025900        MOVE BANK-SCR35-RP2ACC TO RP2ACCO IN BANK35AO             SBANK35P
026000        MOVE BANK-SCR35-RP2DTE TO RP2DTEO IN BANK35AO             SBANK35P
026100     ELSE                                                         SBANK35P
026200        MOVE SPACES TO RP2DAYO IN BANK35AO                        SBANK35P
026300        MOVE SPACES TO RP2AMTO IN BANK35AO                        SBANK35P
026400        MOVE SPACES TO RP2PIDO IN BANK35AO                        SBANK35P
026500        MOVE SPACES TO RP2SEPO IN BANK35AO                        SBANK35P
026600        MOVE SPACES TO RP2ACCO IN BANK35AO                        SBANK35P
026700        MOVE SPACES TO RP2DTEO IN BANK35AO                        SBANK35P
026800     END-IF.                                                      SBANK35P
026900     IF BANK-SCR35-RP3ACC IS NOT EQUAL TO SPACES                  SBANK35P
027000        MOVE BANK-SCR35-RP3DAY TO RP3DAYO IN BANK35AO             SBANK35P
027100        MOVE BANK-SCR35-RP3AMT TO RP3AMTO IN BANK35AO             SBANK35P
027200        MOVE BANK-SCR35-RP3PID TO RP3PIDO IN BANK35AO             SBANK35P
027300        MOVE '/' TO RP3SEPO IN BANK35AO                           SBANK35P
027400        MOVE BANK-SCR35-RP3ACC TO RP3ACCO IN BANK35AO             SBANK35P
027500        MOVE BANK-SCR35-RP3DTE TO RP3DTEO IN BANK35AO             SBANK35P
027600     ELSE                                                         SBANK35P
027700        MOVE SPACES TO RP3DAYO IN BANK35AO                        SBANK35P
027800        MOVE SPACES TO RP3AMTO IN BANK35AO                        SBANK35P
027900        MOVE SPACES TO RP3PIDO IN BANK35AO                        SBANK35P
028000        MOVE SPACES TO RP3SEPO IN BANK35AO                        SBANK35P
028100        MOVE SPACES TO RP3ACCO IN BANK35AO                        SBANK35P
028200        MOVE SPACES TO RP3DTEO IN BANK35AO                        SBANK35P
028300     END-IF.                                                      SBANK35P
028400* Turn colour off if requ8red                                     SBANK35P
028500     IF COLOUR-OFF                                                SBANK35P
028600        MOVE DFHGREEN TO TXT01C IN BANK35AO                       SBANK35P
028700        MOVE DFHGREEN TO SCRNC IN BANK35AO                        SBANK35P
028800        MOVE DFHGREEN TO HEAD1C IN BANK35AO                       SBANK35P
028900        MOVE DFHGREEN TO DATEC IN BANK35AO                        SBANK35P
029000        MOVE DFHGREEN TO TXT02C IN BANK35AO                       SBANK35P
029100        MOVE DFHGREEN TO TRANC IN BANK35AO                        SBANK35P
029200        MOVE DFHGREEN TO HEAD2C IN BANK35AO                       SBANK35P
029300        MOVE DFHGREEN TO TIMEC IN BANK35AO                        SBANK35P
029400        MOVE DFHGREEN TO TXT03C IN BANK35AO                       SBANK35P
029500        MOVE DFHGREEN TO USERIDC IN BANK35AO                      SBANK35P
029600        MOVE DFHGREEN TO TXT13C IN BANK35AO                       SBANK35P
029700        MOVE DFHGREEN TO TXT04C IN BANK35AO                       SBANK35P
029800        MOVE DFHGREEN TO USERNMC IN BANK35AO                      SBANK35P
029900        MOVE DFHGREEN TO TXT05C IN BANK35AO                       SBANK35P
030000        MOVE DFHGREEN TO ACCNOC IN BANK35AO                       SBANK35P
030100        MOVE DFHGREEN TO ACCTYPEC IN BANK35AO                     SBANK35P
030200        MOVE DFHGREEN TO TXT06C IN BANK35AO                       SBANK35P
030300        MOVE DFHGREEN TO BALC IN BANK35AO                         SBANK35P
030400        MOVE DFHGREEN TO TXT07C IN BANK35AO                       SBANK35P
030500        MOVE DFHGREEN TO DTEC IN BANK35AO                         SBANK35P
030600        MOVE DFHGREEN TO TXT08C IN BANK35AO                       SBANK35P
030700        MOVE DFHGREEN TO TXT09C IN BANK35AO                       SBANK35P
030800        MOVE DFHGREEN TO ATMVISC IN BANK35AO                      SBANK35P
030900        MOVE DFHGREEN TO TXT10C IN BANK35AO                       SBANK35P
031000        MOVE DFHGREEN TO ATMLIMC IN BANK35AO                      SBANK35P
031100        MOVE DFHGREEN TO TXT11C IN BANK35AO                       SBANK35P
031200        MOVE DFHGREEN TO ATMLDTEC IN BANK35AO                     SBANK35P
031300        MOVE DFHGREEN TO TXT12C IN BANK35AO                       SBANK35P
031400        MOVE DFHGREEN TO ATMLAMTC IN BANK35AO                     SBANK35P
031500        MOVE DFHGREEN TO TXT13C IN BANK35AO                       SBANK35P
031600        MOVE DFHGREEN TO TXT14C IN BANK35AO                       SBANK35P
031700        MOVE DFHGREEN TO TXT15C IN BANK35AO                       SBANK35P
031800        MOVE DFHGREEN TO TXT16C IN BANK35AO                       SBANK35P
031900        MOVE DFHGREEN TO TXT17C IN BANK35AO                       SBANK35P
032000        MOVE DFHGREEN TO RP1DAYC IN BANK35AO                      SBANK35P
032100        MOVE DFHGREEN TO RP1AMTC IN BANK35AO                      SBANK35P
032200        MOVE DFHGREEN TO RP1PIDC IN BANK35AO                      SBANK35P
032300        MOVE DFHGREEN TO RP1SEPC IN BANK35AO                      SBANK35P
032400        MOVE DFHGREEN TO RP1ACCC IN BANK35AO                      SBANK35P
032500        MOVE DFHGREEN TO RP1DTEC IN BANK35AO                      SBANK35P
032600        MOVE DFHGREEN TO RP2DAYC IN BANK35AO                      SBANK35P
032700        MOVE DFHGREEN TO RP2AMTC IN BANK35AO                      SBANK35P
032800        MOVE DFHGREEN TO RP2PIDC IN BANK35AO                      SBANK35P
032900        MOVE DFHGREEN TO RP2SEPC IN BANK35AO                      SBANK35P
033000        MOVE DFHGREEN TO RP2ACCC IN BANK35AO                      SBANK35P
033100        MOVE DFHGREEN TO RP2DTEC IN BANK35AO                      SBANK35P
033200        MOVE DFHGREEN TO RP3DAYC IN BANK35AO                      SBANK35P
033300        MOVE DFHGREEN TO RP3AMTC IN BANK35AO                      SBANK35P
033400        MOVE DFHGREEN TO RP3PIDC IN BANK35AO                      SBANK35P
033500        MOVE DFHGREEN TO RP3SEPC IN BANK35AO                      SBANK35P
033600        MOVE DFHGREEN TO RP3ACCC IN BANK35AO                      SBANK35P
033700        MOVE DFHGREEN TO RP3DTEC IN BANK35AO                      SBANK35P
033800        MOVE DFHGREEN TO ERRMSGC IN BANK35AO                      SBANK35P
033900        MOVE DFHGREEN TO TXT18C IN BANK35AO                       SBANK35P
034000     END-IF.                                                      SBANK35P
034100                                                                  SBANK35P
034200     EXEC CICS SEND MAP('BANK35A')                                SBANK35P
034300                    MAPSET('MBANK35')                             SBANK35P
034400                    ERASE                                         SBANK35P
034500                    FREEKB                                        SBANK35P
034600     END-EXEC.                                                    SBANK35P
034700     GO TO SCREEN35-BUILD-AND-SEND-EXIT.                          SBANK35P
034800                                                                  SBANK35P
034900 HELP35-BUILD-AND-SEND-CICS.                                      SBANK35P
035000     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK35P
035100                             ==<<SCRN>>== BY ==HELP35AO==.        SBANK35P
035200                                                                  SBANK35P
035300     EXEC CICS SEND MAP('HELP35A')                                SBANK35P
035400                    MAPSET('MBANK35')                             SBANK35P
035500                    ERASE                                         SBANK35P
035600                    FREEKB                                        SBANK35P
035700     END-EXEC.                                                    SBANK35P
035800     GO TO SCREEN35-BUILD-AND-SEND-EXIT.                          SBANK35P
035900                                                                  SBANK35P
036000 SCREEN35-BUILD-AND-SEND-INET.                                    SBANK35P
036100     MOVE SPACES TO EXT-OP-DATA.                                  SBANK35P
036200     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK35P
036300     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK35P
036400     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK35P
036500     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK35P
036600     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK35P
036700     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK35P
036800* Move in screen name                                             SBANK35P
036900     MOVE 'BANK35' TO EXT-OP-SCREEN.                              SBANK35P
037000* Move in userid and any error message                            SBANK35P
037100     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK35P
037200     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK35P
037300     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK35P
037400* Move in screen specific fields                                  SBANK35P
037500     MOVE BANK-SCR35-ACC TO EXT-OP35-ACCNO.                       SBANK35P
037600     MOVE BANK-SCR35-DSC TO EXT-OP35-ACCTYPE.                     SBANK35P
037700     MOVE BANK-SCR35-BAL TO EXT-OP35-BALANCE.                     SBANK35P
037800     MOVE BANK-SCR35-DTE TO EXT-OP35-STMT-DATE.                   SBANK35P
037900     MOVE BANK-SCR35-ATM-ENABLED TO EXT-OP35-ATM-VIS.             SBANK35P
038000     MOVE BANK-SCR35-ATM-LIM TO EXT-OP35-ATM-LIM                  SBANK35P
038100     MOVE BANK-SCR35-ATM-LDTE TO EXT-OP35-ATM-LDTE.               SBANK35P
038200     MOVE BANK-SCR35-ATM-LAMT TO EXT-OP35-ATM-LAMT.               SBANK35P
038300     IF BANK-SCR35-RP1ACC IS NOT EQUAL TO SPACES                  SBANK35P
038400        MOVE BANK-SCR35-RP1DAY TO EXT-OP35-RP-DAY(1)              SBANK35P
038500        MOVE BANK-SCR35-RP1AMT TO EXT-OP35-RP-AMT(1)              SBANK35P
038600        MOVE BANK-SCR35-RP1PID TO EXT-OP35-RP-PID(1)              SBANK35P
038700        MOVE BANK-SCR35-RP1ACC TO EXT-OP35-RP-ACC(1)              SBANK35P
038800        MOVE BANK-SCR35-RP1DTE TO EXT-OP35-RP-DTE(1)              SBANK35P
038900     ELSE                                                         SBANK35P
039000        MOVE SPACES TO EXT-OP35-RP-DETAILS(1)                     SBANK35P
039100     END-IF.                                                      SBANK35P
039200     IF BANK-SCR35-RP2ACC IS NOT EQUAL TO SPACES                  SBANK35P
039300        MOVE BANK-SCR35-RP2DAY TO EXT-OP35-RP-DAY(2)              SBANK35P
039400        MOVE BANK-SCR35-RP2AMT TO EXT-OP35-RP-AMT(2)              SBANK35P
039500        MOVE BANK-SCR35-RP2PID TO EXT-OP35-RP-PID(2)              SBANK35P
039600        MOVE BANK-SCR35-RP2ACC TO EXT-OP35-RP-ACC(2)              SBANK35P
039700        MOVE BANK-SCR35-RP2DTE TO EXT-OP35-RP-DTE(2)              SBANK35P
039800     ELSE                                                         SBANK35P
039900        MOVE SPACES TO EXT-OP35-RP-DETAILS(2)                     SBANK35P
040000     END-IF.                                                      SBANK35P
040100     IF BANK-SCR35-RP3ACC IS NOT EQUAL TO SPACES                  SBANK35P
040200        MOVE BANK-SCR35-RP3DAY TO EXT-OP35-RP-DAY(3)              SBANK35P
040300        MOVE BANK-SCR35-RP3AMT TO EXT-OP35-RP-AMT(3)              SBANK35P
040400        MOVE BANK-SCR35-RP3PID TO EXT-OP35-RP-PID(3)              SBANK35P
040500        MOVE BANK-SCR35-RP3ACC TO EXT-OP35-RP-ACC(3)              SBANK35P
040600        MOVE BANK-SCR35-RP3DTE TO EXT-OP35-RP-DTE(3)              SBANK35P
040700     ELSE                                                         SBANK35P
040800        MOVE SPACES TO EXT-OP35-RP-DETAILS(3)                     SBANK35P
040900     END-IF.                                                      SBANK35P
041000                                                                  SBANK35P
041100 SCREEN35-BUILD-AND-SEND-EXIT.                                    SBANK35P
041200     EXIT.                                                        SBANK35P
041300                                                                  SBANK35P
041400***************************************************************** SBANK35P
041500* Call common routine to perform date conversions               * SBANK35P
041600***************************************************************** SBANK35P
041700 CALL-DATECONV.                                                   SBANK35P
041800     MOVE BANK-ENV TO DD-ENV.                                     SBANK35P
041900     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK35P
042000     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK35P
042100 CALL-DATECONV-EXIT.                                              SBANK35P
042200     EXIT.                                                        SBANK35P
042300                                                                  SBANK35P
042400* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK35P
