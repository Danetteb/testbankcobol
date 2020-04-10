000100***************************************************************** SBANK60P
000200*                                                               * SBANK60P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK60P
000400*   This demonstration program is provided for use by users     * SBANK60P
000500*   of Micro Focus products and may be used, modified and       * SBANK60P
000600*   distributed as part of your application provided that       * SBANK60P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK60P
000800*   in this material.                                           * SBANK60P
000900*                                                               * SBANK60P
001000***************************************************************** SBANK60P
001100                                                                  SBANK60P
001200***************************************************************** SBANK60P
001300* Program:     SBANK60P.CBL (CICS Version)                      * SBANK60P
001400* Layer:       Screen handling                                  * SBANK60P
001500* Function:    Update address & telno                           * SBANK60P
001600***************************************************************** SBANK60P
001700                                                                  SBANK60P
001800 IDENTIFICATION DIVISION.                                         SBANK60P
001900 PROGRAM-ID.                                                      SBANK60P
002000     SBANK60P.                                                    SBANK60P
002100 DATE-WRITTEN.                                                    SBANK60P
002200     September 2002.                                              SBANK60P
002300 DATE-COMPILED.                                                   SBANK60P
002400     Today.                                                       SBANK60P
002500                                                                  SBANK60P
002600 ENVIRONMENT DIVISION.                                            SBANK60P
002700                                                                  SBANK60P
002800 DATA DIVISION.                                                   SBANK60P
002900 WORKING-STORAGE SECTION.                                         SBANK60P
003000 01  WS-MISC-STORAGE.                                             SBANK60P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK60P
003200       VALUE 'SBANK60P'.                                          SBANK60P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK60P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK60P
003500       VALUE SPACES.                                              SBANK60P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK60P
003700       VALUE 'UNKNOWN'.                                           SBANK60P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK60P
003900   05  WS-WORK1                              PIC X(1).            SBANK60P
004000   05  WS-SUB1                               PIC S9(4) COMP.      SBANK60P
004100                                                                  SBANK60P
004200 01  MAPAREA                                 PIC X(2048).         SBANK60P
004300 COPY MBANK60.                                                    SBANK60P
004400                                                                  SBANK60P
004500 01  WS-TIME-DATE-WORK-AREA.                                      SBANK60P
004600 COPY CDATED.                                                     SBANK60P
004700                                                                  SBANK60P
004800 01  WS-BANK-DATA-AREAS.                                          SBANK60P
004900   05  WS-BANK-DATA.                                              SBANK60P
005000 COPY CBANKDAT.                                                   SBANK60P
005100   05  WS-BANK-EXT-DATA.                                          SBANK60P
005200 COPY CBANKEXT.                                                   SBANK60P
005300                                                                  SBANK60P
005400 COPY CSCRNHDD.                                                   SBANK60P
005500                                                                  SBANK60P
005600 COPY DFHAID.                                                     SBANK60P
005700                                                                  SBANK60P
005800 COPY DFHBMSCA.                                                   SBANK60P
005900                                                                  SBANK60P
006000 COPY CABENDD.                                                    SBANK60P
006100                                                                  SBANK60P
006200 LINKAGE SECTION.                                                 SBANK60P
006300 01  DFHCOMMAREA.                                                 SBANK60P
006400   05  FILLER                                PIC X(7168).         SBANK60P
006500                                                                  SBANK60P
006600 PROCEDURE DIVISION.                                              SBANK60P
006700***************************************************************** SBANK60P
006800* Write entry to log to show we have been invoked               * SBANK60P
006900***************************************************************** SBANK60P
007000     COPY CTRACE.                                                 SBANK60P
007100                                                                  SBANK60P
007200***************************************************************** SBANK60P
007300* Store our transaction-id                                      * SBANK60P
007400***************************************************************** SBANK60P
007500     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK60P
007600                                                                  SBANK60P
007700***************************************************************** SBANK60P
007800* Store passed data or abend if there wasn't any                * SBANK60P
007900***************************************************************** SBANK60P
008000     IF EIBCALEN IS EQUAL TO 0                                    SBANK60P
008100        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK60P
008200        MOVE '0001' TO ABEND-CODE                                 SBANK60P
008300         MOVE SPACES TO ABEND-REASON                              SBANK60P
008400        COPY CABENDPO.                                            SBANK60P
008500     ELSE                                                         SBANK60P
008600        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK60P
008700        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK60P
008800        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK60P
008900     END-IF.                                                      SBANK60P
009000                                                                  SBANK60P
009100***************************************************************** SBANK60P
009200* This is the main process                                      * SBANK60P
009300***************************************************************** SBANK60P
009400                                                                  SBANK60P
009500***************************************************************** SBANK60P
009600* Determine what we have to do (read from or send to screen)    * SBANK60P
009700***************************************************************** SBANK60P
009800     MOVE LOW-VALUE TO MAPAREA.                                   SBANK60P
009900     EVALUATE TRUE                                                SBANK60P
010000       WHEN BANK-MAP-FUNCTION-GET                                 SBANK60P
010100         PERFORM SCREEN60-READ THRU                               SBANK60P
010200                 SCREEN60-READ-EXIT                               SBANK60P
010300       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK60P
010400         PERFORM SCREEN60-BUILD-AND-SEND THRU                     SBANK60P
010500                 SCREEN60-BUILD-AND-SEND-EXIT                     SBANK60P
010600       WHEN OTHER                                                 SBANK60P
010700         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK60P
010800         MOVE '0002' TO ABEND-CODE                                SBANK60P
010900         MOVE SPACES TO ABEND-REASON                              SBANK60P
011000         COPY CABENDPO.                                           SBANK60P
011100     END-EVALUATE.                                                SBANK60P
011200                                                                  SBANK60P
011300* Call the appropriate routine to handle the business logic       SBANK60P
011400     IF BANK-MAP-FUNCTION-GET                                     SBANK60P
011500        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK60P
011600                       COMMAREA(WS-BANK-DATA)                     SBANK60P
011700                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK60P
011800        END-EXEC                                                  SBANK60P
011900     END-IF.                                                      SBANK60P
012000                                                                  SBANK60P
012100***************************************************************** SBANK60P
012200* Now we have to have finished and can return to our invoker.   * SBANK60P
012300***************************************************************** SBANK60P
012400* Now return to CICS                                              SBANK60P
012500     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK60P
012600          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK60P
012700     EXEC CICS                                                    SBANK60P
012800          RETURN                                                  SBANK60P
012900     END-EXEC.                                                    SBANK60P
013000     GOBACK.                                                      SBANK60P
013100                                                                  SBANK60P
013200***************************************************************** SBANK60P
013300* Screen processing for MBANK60                                 * SBANK60P
013400*---------------------------------------------------------------* SBANK60P
013500* Retrieve data from screen and format it                       * SBANK60P
013600***************************************************************** SBANK60P
013700 SCREEN60-READ.                                                   SBANK60P
013800     MOVE 'BBANK60P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK60P
013900     IF BANK-AID-CLEAR                                            SBANK60P
014000        SET BANK-AID-PFK03 TO TRUE                                SBANK60P
014100        GO TO SCREEN60-READ-EXIT                                  SBANK60P
014200     END-IF.                                                      SBANK60P
014300     IF BANK-ENV-CICS                                             SBANK60P
014400        GO TO SCREEN60-READ-CICS                                  SBANK60P
014500     ELSE                                                         SBANK60P
014600        GO TO SCREEN60-READ-INET                                  SBANK60P
014700     END-IF.                                                      SBANK60P
014800                                                                  SBANK60P
014900 SCREEN60-READ-CICS.                                              SBANK60P
015000     IF BANK-HELP-INACTIVE                                        SBANK60P
015100        EXEC CICS RECEIVE MAP('BANK60A')                          SBANK60P
015200                          MAPSET('MBANK60')                       SBANK60P
015300        END-EXEC                                                  SBANK60P
015400     ELSE                                                         SBANK60P
015500        EXEC CICS RECEIVE MAP('HELP60A')                          SBANK60P
015600                          MAPSET('MBANK60')                       SBANK60P
015700        END-EXEC                                                  SBANK60P
015800        GO TO SCREEN60-READ-EXIT                                  SBANK60P
015900     END-IF.                                                      SBANK60P
016000                                                                  SBANK60P
016100     IF NADDR1L IN BANK60AI IS EQUAL TO 0                         SBANK60P
016200        MOVE SPACES TO NADDR1I IN BANK60AI                        SBANK60P
016300     END-IF.                                                      SBANK60P
016400                                                                  SBANK60P
016500     IF NADDR2L IN BANK60AI IS EQUAL TO 0                         SBANK60P
016600        MOVE SPACES TO NADDR2I IN BANK60AI                        SBANK60P
016700     END-IF.                                                      SBANK60P
016800                                                                  SBANK60P
016900     IF NSTATEL IN BANK60AI IS EQUAL TO 0                         SBANK60P
017000        MOVE SPACES TO NSTATEI IN BANK60AI                        SBANK60P
017100     END-IF.                                                      SBANK60P
017200                                                                  SBANK60P
017300     IF NCNTRYL IN BANK60AI IS EQUAL TO 0                         SBANK60P
017400        MOVE SPACES TO NCNTRYI IN BANK60AI                        SBANK60P
017500     END-IF.                                                      SBANK60P
017600                                                                  SBANK60P
017700     IF NPSTCDEL IN BANK60AI IS EQUAL TO 0                        SBANK60P
017800        MOVE SPACES TO NPSTCDEI IN BANK60AI                       SBANK60P
017900     END-IF.                                                      SBANK60P
018000                                                                  SBANK60P
018100     IF NTELNOL IN BANK60AI IS EQUAL TO 0                         SBANK60P
018200        MOVE SPACES TO NTELNOI IN BANK60AI                        SBANK60P
018300     END-IF.                                                      SBANK60P
018400                                                                  SBANK60P
018500     IF NEMAILL IN BANK60AI IS EQUAL TO 0                         SBANK60P
018600        MOVE SPACES TO NEMAILI IN BANK60AI                        SBANK60P
018700     END-IF.                                                      SBANK60P
018800                                                                  SBANK60P
018900     IF NSMAILL IN BANK60AI IS EQUAL TO 0                         SBANK60P
019000        MOVE SPACES TO NSMAILI IN BANK60AI                        SBANK60P
019100     END-IF.                                                      SBANK60P
019200                                                                  SBANK60P
019300     IF NSEMAILL IN BANK60AI IS EQUAL TO 0                        SBANK60P
019400        MOVE SPACES TO NSEMAILI IN BANK60AI                       SBANK60P
019500     END-IF.                                                      SBANK60P
019600                                                                  SBANK60P
019700     MOVE NADDR1I IN BANK60AI TO BANK-SCR60-NEW-ADDR1.            SBANK60P
019800     MOVE NADDR2I IN BANK60AI TO BANK-SCR60-NEW-ADDR2.            SBANK60P
019900     MOVE NSTATEI IN BANK60AI TO BANK-SCR60-NEW-STATE.            SBANK60P
020000     MOVE NCNTRYI IN BANK60AI TO BANK-SCR60-NEW-CNTRY.            SBANK60P
020100     MOVE NPSTCDEI IN BANK60AI TO BANK-SCR60-NEW-PSTCDE.          SBANK60P
020200     MOVE NTELNOI IN BANK60AI TO BANK-SCR60-NEW-TELNO.            SBANK60P
020300     MOVE NEMAILI IN BANK60AI TO BANK-SCR60-NEW-EMAIL.            SBANK60P
020400     MOVE NSMAILI IN BANK60AI TO BANK-SCR60-NEW-SEND-MAIL.        SBANK60P
020500     MOVE NSEMAILI IN BANK60AI TO BANK-SCR60-NEW-SEND-EMAIL.      SBANK60P
020600                                                                  SBANK60P
020700     GO TO SCREEN60-READ-EXIT.                                    SBANK60P
020800                                                                  SBANK60P
020900 SCREEN60-READ-INET.                                              SBANK60P
021000     MOVE EXT-IP60-NADDR1 TO BANK-SCR60-NEW-ADDR1.                SBANK60P
021100     MOVE EXT-IP60-NADDR2 TO BANK-SCR60-NEW-ADDR2.                SBANK60P
021200     MOVE EXT-IP60-NSTATE TO BANK-SCR60-NEW-STATE.                SBANK60P
021300     MOVE EXT-IP60-NCNTRY TO BANK-SCR60-NEW-CNTRY.                SBANK60P
021400     MOVE EXT-IP60-NPSTCDE TO BANK-SCR60-NEW-PSTCDE.              SBANK60P
021500     MOVE EXT-IP60-NTELNO TO BANK-SCR60-NEW-TELNO.                SBANK60P
021600     MOVE EXT-IP60-NEMAIL TO BANK-SCR60-NEW-EMAIL.                SBANK60P
021700     MOVE EXT-IP60-NSMAIL TO BANK-SCR60-NEW-SEND-MAIL.            SBANK60P
021800     MOVE EXT-IP60-NSEMAIL TO BANK-SCR60-NEW-SEND-EMAIL.          SBANK60P
021900     GO TO SCREEN60-READ-EXIT.                                    SBANK60P
022000                                                                  SBANK60P
022100 SCREEN60-READ-EXIT.                                              SBANK60P
022200     EXIT.                                                        SBANK60P
022300                                                                  SBANK60P
022400***************************************************************** SBANK60P
022500* Screen processing for SCREEN60 (BANK60/HELP60)                * SBANK60P
022600*---------------------------------------------------------------* SBANK60P
022700* Build the output screen and send it                           * SBANK60P
022800***************************************************************** SBANK60P
022900 SCREEN60-BUILD-AND-SEND.                                         SBANK60P
023000* Clear map area, get date & time and move to the map             SBANK60P
023100     MOVE LOW-VALUES TO BANK60AO.                                 SBANK60P
023200     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK60P
023300     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK60P
023400     SET DDI-YYDDD TO TRUE.                                       SBANK60P
023500     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK60P
023600     PERFORM CALL-DATECONV THRU                                   SBANK60P
023700             CALL-DATECONV-EXIT.                                  SBANK60P
023800* Ensure the last map fields are correct                          SBANK60P
023900     IF BANK-HELP-ACTIVE                                          SBANK60P
024000        MOVE 'MBANK60' TO BANK-LAST-MAPSET                        SBANK60P
024100        MOVE 'HELP60A' TO BANK-LAST-MAP                           SBANK60P
024200     ELSE                                                         SBANK60P
024300        MOVE 'MBANK60' TO BANK-LAST-MAPSET                        SBANK60P
024400        MOVE 'BANK60A' TO BANK-LAST-MAP                           SBANK60P
024500     END-IF.                                                      SBANK60P
024600     IF BANK-ENV-CICS                                             SBANK60P
024700        GO TO SCREEN60-BUILD-AND-SEND-CICS                        SBANK60P
024800     ELSE                                                         SBANK60P
024900        GO TO SCREEN60-BUILD-AND-SEND-INET                        SBANK60P
025000     END-IF.                                                      SBANK60P
025100                                                                  SBANK60P
025200 SCREEN60-BUILD-AND-SEND-CICS.                                    SBANK60P
025300     IF BANK-LAST-MAP IS EQUAL TO 'BANK60A'                       SBANK60P
025400        GO TO BANK60-BUILD-AND-SEND-CICS                          SBANK60P
025500     END-IF.                                                      SBANK60P
025600     IF BANK-LAST-MAP IS EQUAL TO 'HELP60A'                       SBANK60P
025700        GO TO HELP60-BUILD-AND-SEND-CICS                          SBANK60P
025800     END-IF.                                                      SBANK60P
025900     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK60P
026000     MOVE '0003' TO ABEND-CODE                                    SBANK60P
026100     MOVE SPACES TO ABEND-REASON                                  SBANK60P
026200     COPY CABENDPO.                                               SBANK60P
026300     GOBACK.                                                      SBANK60P
026400                                                                  SBANK60P
026500 BANK60-BUILD-AND-SEND-CICS.                                      SBANK60P
026600     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK60AO==.        SBANK60P
026700     MOVE WS-TRAN-ID TO TRANO IN BANK60AO.                        SBANK60P
026800     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK60AO.                    SBANK60P
026900     MOVE DDO-DATA TO DATEO IN BANK60AO.                          SBANK60P
027000* Move in any error message                                       SBANK60P
027100     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK60AO.                  SBANK60P
027200* Move in screen specific fields                                  SBANK60P
027300     MOVE BANK-SCR60-CONTACT-ID TO USERIDO IN BANK60AO.           SBANK60P
027400     MOVE BANK-SCR60-CONTACT-NAME TO USERNMO IN BANK60AO.         SBANK60P
027500                                                                  SBANK60P
027600     MOVE BANK-SCR60-OLD-ADDR1 TO OADDR1O IN BANK60AO.            SBANK60P
027700     MOVE BANK-SCR60-OLD-ADDR2 TO OADDR2O IN BANK60AO.            SBANK60P
027800     MOVE BANK-SCR60-OLD-STATE TO OSTATEO IN BANK60AO.            SBANK60P
027900     MOVE BANK-SCR60-OLD-CNTRY TO OCNTRYO IN BANK60AO.            SBANK60P
028000     MOVE BANK-SCR60-OLD-PSTCDE TO OPSTCDEO IN BANK60AO.          SBANK60P
028100     MOVE BANK-SCR60-OLD-TELNO TO OTELNOO IN BANK60AO.            SBANK60P
028200     MOVE BANK-SCR60-OLD-EMAIL TO OEMAILO IN BANK60AO.            SBANK60P
028300     MOVE BANK-SCR60-OLD-SEND-MAIL TO OSMAILO IN BANK60AO.        SBANK60P
028400     MOVE BANK-SCR60-OLD-SEND-EMAIL TO OSEMAILO IN BANK60AO.      SBANK60P
028500     MOVE BANK-SCR60-NEW-ADDR1 TO NADDR1O IN BANK60AO.            SBANK60P
028600     MOVE BANK-SCR60-NEW-ADDR2 TO NADDR2O IN BANK60AO.            SBANK60P
028700     MOVE BANK-SCR60-NEW-STATE TO NSTATEO IN BANK60AO.            SBANK60P
028800     MOVE BANK-SCR60-NEW-CNTRY TO NCNTRYO IN BANK60AO.            SBANK60P
028900     MOVE BANK-SCR60-NEW-PSTCDE TO NPSTCDEO IN BANK60AO.          SBANK60P
029000     MOVE BANK-SCR60-NEW-TELNO TO NTELNOO IN BANK60AO.            SBANK60P
029100     MOVE BANK-SCR60-NEW-EMAIL TO NEMAILO IN BANK60AO.            SBANK60P
029200     MOVE BANK-SCR60-NEW-SEND-MAIL TO NSMAILO IN BANK60AO.        SBANK60P
029300     MOVE BANK-SCR60-NEW-SEND-EMAIL TO NSEMAILO IN BANK60AO.      SBANK60P
029400     IF ADDR-CHANGE-VERIFY                                        SBANK60P
029500        MOVE DFHBMPRF TO NADDR1A IN BANK60AI                      SBANK60P
029600        MOVE DFHBMPRF TO NADDR2A IN BANK60AI                      SBANK60P
029700        MOVE DFHBMPRF TO NSTATEA IN BANK60AI                      SBANK60P
029800        MOVE DFHBMPRF TO NCNTRYA IN BANK60AI                      SBANK60P
029900        MOVE DFHBMPRF TO NADDR1A IN BANK60AI                      SBANK60P
030000        MOVE DFHBMPRF TO NPSTCDEA IN BANK60AI                     SBANK60P
030100        MOVE DFHBMPRF TO NTELNOA IN BANK60AI                      SBANK60P
030200        MOVE DFHBMPRF TO NEMAILA IN BANK60AI                      SBANK60P
030300        MOVE DFHBMPRF TO NSMAILA IN BANK60AI                      SBANK60P
030400        MOVE DFHBMPRF TO NSEMAILA IN BANK60AI                     SBANK60P
030500     END-IF.                                                      SBANK60P
030600* Turn colour off if required                                     SBANK60P
030700     IF COLOUR-OFF                                                SBANK60P
030800        MOVE DFHGREEN TO TXT01C IN BANK60AO                       SBANK60P
030900        MOVE DFHGREEN TO SCRNC IN BANK60AO                        SBANK60P
031000        MOVE DFHGREEN TO HEAD1C IN BANK60AO                       SBANK60P
031100        MOVE DFHGREEN TO DATEC IN BANK60AO                        SBANK60P
031200        MOVE DFHGREEN TO TXT02C IN BANK60AO                       SBANK60P
031300        MOVE DFHGREEN TO TRANC IN BANK60AO                        SBANK60P
031400        MOVE DFHGREEN TO HEAD2C IN BANK60AO                       SBANK60P
031500        MOVE DFHGREEN TO TIMEC IN BANK60AO                        SBANK60P
031600        MOVE DFHGREEN TO TXT03C IN BANK60AO                       SBANK60P
031700        MOVE DFHGREEN TO USERIDC IN BANK60AO                      SBANK60P
031800        MOVE DFHGREEN TO TXT04C IN BANK60AO                       SBANK60P
031900        MOVE DFHGREEN TO USERNMC IN BANK60AO                      SBANK60P
032000        MOVE DFHGREEN TO TXT05C IN BANK60AO                       SBANK60P
032100        MOVE DFHGREEN TO TXT06C IN BANK60AO                       SBANK60P
032200        MOVE DFHGREEN TO TXT07C IN BANK60AO                       SBANK60P
032300        MOVE DFHGREEN TO NADDR1C IN BANK60AO                      SBANK60P
032400        MOVE DFHGREEN TO OADDR1C IN BANK60AO                      SBANK60P
032500        MOVE DFHGREEN TO NADDR2C IN BANK60AO                      SBANK60P
032600        MOVE DFHGREEN TO OADDR2C IN BANK60AO                      SBANK60P
032700        MOVE DFHGREEN TO TXT08C IN BANK60AO                       SBANK60P
032800        MOVE DFHGREEN TO NSTATEC IN BANK60AO                      SBANK60P
032900        MOVE DFHGREEN TO OSTATEC IN BANK60AO                      SBANK60P
033000        MOVE DFHGREEN TO TXT09C IN BANK60AO                       SBANK60P
033100        MOVE DFHGREEN TO NCNTRYC IN BANK60AO                      SBANK60P
033200        MOVE DFHGREEN TO OCNTRYC IN BANK60AO                      SBANK60P
033300        MOVE DFHGREEN TO TXT10C IN BANK60AO                       SBANK60P
033400        MOVE DFHGREEN TO NPSTCDEC IN BANK60AO                     SBANK60P
033500        MOVE DFHGREEN TO OPSTCDEC IN BANK60AO                     SBANK60P
033600        MOVE DFHGREEN TO TXT11C IN BANK60AO                       SBANK60P
033700        MOVE DFHGREEN TO NTELNOC IN BANK60AO                      SBANK60P
033800        MOVE DFHGREEN TO OTELNOC IN BANK60AO                      SBANK60P
033900        MOVE DFHGREEN TO TXT12C IN BANK60AO                       SBANK60P
034000        MOVE DFHGREEN TO NEMAILC IN BANK60AO                      SBANK60P
034100        MOVE DFHGREEN TO OEMAILC IN BANK60AO                      SBANK60P
034200        MOVE DFHGREEN TO TXT13C IN BANK60AO                       SBANK60P
034300        MOVE DFHGREEN TO TXT14C IN BANK60AO                       SBANK60P
034400        MOVE DFHGREEN TO NSMAILC IN BANK60AO                      SBANK60P
034500        MOVE DFHGREEN TO TXT15C IN BANK60AO                       SBANK60P
034600        MOVE DFHGREEN TO NSEMAILC IN BANK60AO                     SBANK60P
034700        MOVE DFHGREEN TO TXT16C IN BANK60AO                       SBANK60P
034800        MOVE DFHGREEN TO OSMAILC IN BANK60AO                      SBANK60P
034900        MOVE DFHGREEN TO OSEMAILC IN BANK60AO                     SBANK60P
035000        MOVE DFHGREEN TO ERRMSGC IN BANK60AO                      SBANK60P
035100        MOVE DFHGREEN TO TXT17C IN BANK60AO                       SBANK60P
035200     END-IF.                                                      SBANK60P
035300                                                                  SBANK60P
035400     EXEC CICS SEND MAP('BANK60A')                                SBANK60P
035500                    MAPSET('MBANK60')                             SBANK60P
035600                    ERASE                                         SBANK60P
035700                    FREEKB                                        SBANK60P
035800     END-EXEC.                                                    SBANK60P
035900     GO TO SCREEN60-BUILD-AND-SEND-EXIT.                          SBANK60P
036000                                                                  SBANK60P
036100 HELP60-BUILD-AND-SEND-CICS.                                      SBANK60P
036200     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK60P
036300                             ==<<SCRN>>== BY ==HELP60AO==.        SBANK60P
036400                                                                  SBANK60P
036500     EXEC CICS SEND MAP('HELP60A')                                SBANK60P
036600                    MAPSET('MBANK60')                             SBANK60P
036700                    ERASE                                         SBANK60P
036800                    FREEKB                                        SBANK60P
036900     END-EXEC.                                                    SBANK60P
037000     GO TO SCREEN60-BUILD-AND-SEND-EXIT.                          SBANK60P
037100                                                                  SBANK60P
037200 SCREEN60-BUILD-AND-SEND-INET.                                    SBANK60P
037300     MOVE SPACES TO EXT-OP-DATA.                                  SBANK60P
037400     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK60P
037500     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK60P
037600     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK60P
037700     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK60P
037800     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK60P
037900     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK60P
038000* Move in screen name                                             SBANK60P
038100     MOVE 'BANK60' TO EXT-OP-SCREEN.                              SBANK60P
038200* Move in userid and any error message                            SBANK60P
038300     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK60P
038400     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK60P
038500     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK60P
038600* Move in screen specific fields                                  SBANK60P
038700     MOVE BANK-SCR60-OLD-ADDR1 TO EXT-OP60-OADDR1.                SBANK60P
038800     MOVE BANK-SCR60-OLD-ADDR2 TO EXT-OP60-OADDR2.                SBANK60P
038900     MOVE BANK-SCR60-OLD-STATE TO EXT-OP60-OSTATE.                SBANK60P
039000     MOVE BANK-SCR60-OLD-CNTRY TO EXT-OP60-OCNTRY.                SBANK60P
039100     MOVE BANK-SCR60-OLD-PSTCDE TO EXT-OP60-OPSTCDE.              SBANK60P
039200     MOVE BANK-SCR60-OLD-TELNO TO EXT-OP60-OTELNO.                SBANK60P
039300     MOVE BANK-SCR60-NEW-ADDR1 TO EXT-OP60-NADDR1.                SBANK60P
039400     MOVE BANK-SCR60-NEW-ADDR2 TO EXT-OP60-NADDR2.                SBANK60P
039500     MOVE BANK-SCR60-NEW-STATE TO EXT-OP60-NSTATE.                SBANK60P
039600     MOVE BANK-SCR60-NEW-CNTRY TO EXT-OP60-NCNTRY.                SBANK60P
039700     MOVE BANK-SCR60-NEW-PSTCDE TO EXT-OP60-NPSTCDE.              SBANK60P
039800     MOVE BANK-SCR60-NEW-TELNO TO EXT-OP60-NTELNO.                SBANK60P
039900     MOVE BANK-SCR60-NEW-EMAIL TO EXT-OP60-NEMAIL.                SBANK60P
040000     MOVE BANK-SCR60-NEW-SEND-MAIL TO EXT-OP60-NSMAIL.            SBANK60P
040100     MOVE BANK-SCR60-NEW-SEND-EMAIL TO EXT-OP60-NSEMAIL.          SBANK60P
040200                                                                  SBANK60P
040300 SCREEN60-BUILD-AND-SEND-EXIT.                                    SBANK60P
040400     EXIT.                                                        SBANK60P
040500                                                                  SBANK60P
040600***************************************************************** SBANK60P
040700* Call common routine to perform date conversions               * SBANK60P
040800***************************************************************** SBANK60P
040900 CALL-DATECONV.                                                   SBANK60P
041000     MOVE BANK-ENV TO DD-ENV.                                     SBANK60P
041100     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK60P
041200     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK60P
041300 CALL-DATECONV-EXIT.                                              SBANK60P
041400     EXIT.                                                        SBANK60P
041500                                                                  SBANK60P
041600* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK60P
