000100***************************************************************** SBANK40P
000200*                                                               * SBANK40P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK40P
000400*   This demonstration program is provided for use by users     * SBANK40P
000500*   of Micro Focus products and may be used, modified and       * SBANK40P
000600*   distributed as part of your application provided that       * SBANK40P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK40P
000800*   in this material.                                           * SBANK40P
000900*                                                               * SBANK40P
001000***************************************************************** SBANK40P
001100                                                                  SBANK40P
001200***************************************************************** SBANK40P
001300* Program:     SBANK40P.CBL (CICS Version)                      * SBANK40P
001400* Layer:       Screen handling                                  * SBANK40P
001500* Function:    Display transaction details                      * SBANK40P
001600***************************************************************** SBANK40P
001700                                                                  SBANK40P
001800 IDENTIFICATION DIVISION.                                         SBANK40P
001900 PROGRAM-ID.                                                      SBANK40P
002000     SBANK40P.                                                    SBANK40P
002100 DATE-WRITTEN.                                                    SBANK40P
002200     September 2002.                                              SBANK40P
002300 DATE-COMPILED.                                                   SBANK40P
002400     Today.                                                       SBANK40P
002500                                                                  SBANK40P
002600 ENVIRONMENT DIVISION.                                            SBANK40P
002700                                                                  SBANK40P
002800 DATA DIVISION.                                                   SBANK40P
002900 WORKING-STORAGE SECTION.                                         SBANK40P
003000 01  WS-MISC-STORAGE.                                             SBANK40P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK40P
003200       VALUE 'SBANK40P'.                                          SBANK40P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK40P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK40P
003500       VALUE SPACES.                                              SBANK40P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK40P
003700       VALUE 'UNKNOWN'.                                           SBANK40P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK40P
003900   05  WS-WORK1                              PIC X(1).            SBANK40P
004000   05  WS-SUB1                               PIC S9(4) COMP.      SBANK40P
004100                                                                  SBANK40P
004200 01  MAPAREA                                 PIC X(2048).         SBANK40P
004300 COPY MBANK40.                                                    SBANK40P
004400                                                                  SBANK40P
004500 01  WS-TIME-DATE-WORK-AREA.                                      SBANK40P
004600 COPY CDATED.                                                     SBANK40P
004700                                                                  SBANK40P
004800 01  WS-BANK-DATA-AREAS.                                          SBANK40P
004900   05  WS-BANK-DATA.                                              SBANK40P
005000 COPY CBANKDAT.                                                   SBANK40P
005100   05  WS-BANK-EXT-DATA.                                          SBANK40P
005200 COPY CBANKEXT.                                                   SBANK40P
005300                                                                  SBANK40P
005400 COPY CSCRNHDD.                                                   SBANK40P
005500                                                                  SBANK40P
005600 COPY DFHAID.                                                     SBANK40P
005700                                                                  SBANK40P
005800 COPY DFHBMSCA.                                                   SBANK40P
005900                                                                  SBANK40P
006000 COPY CABENDD.                                                    SBANK40P
006100                                                                  SBANK40P
006200 LINKAGE SECTION.                                                 SBANK40P
006300 01  DFHCOMMAREA.                                                 SBANK40P
006400   05  FILLER                                 PIC X(7168).        SBANK40P
006500                                                                  SBANK40P
006600 PROCEDURE DIVISION.                                              SBANK40P
006700***************************************************************** SBANK40P
006800* Write entry to log to show we have been invoked               * SBANK40P
006900***************************************************************** SBANK40P
007000     COPY CTRACE.                                                 SBANK40P
007100                                                                  SBANK40P
007200***************************************************************** SBANK40P
007300* Store our transaction-id                                      * SBANK40P
007400***************************************************************** SBANK40P
007500     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK40P
007600                                                                  SBANK40P
007700***************************************************************** SBANK40P
007800* Store passed data or abend if there wasn't any                * SBANK40P
007900***************************************************************** SBANK40P
008000     IF EIBCALEN IS EQUAL TO 0                                    SBANK40P
008100        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK40P
008200        MOVE '0001' TO ABEND-CODE                                 SBANK40P
008300        MOVE SPACES TO ABEND-REASON                               SBANK40P
008400        COPY CABENDPO.                                            SBANK40P
008500     ELSE                                                         SBANK40P
008600        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK40P
008700        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK40P
008800        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK40P
008900     END-IF.                                                      SBANK40P
009000                                                                  SBANK40P
009100***************************************************************** SBANK40P
009200* This is the main process                                      * SBANK40P
009300***************************************************************** SBANK40P
009400                                                                  SBANK40P
009500***************************************************************** SBANK40P
009600* Determine what we have to do (read from or send to screen)    * SBANK40P
009700***************************************************************** SBANK40P
009800     MOVE LOW-VALUE TO MAPAREA.                                   SBANK40P
009900     EVALUATE TRUE                                                SBANK40P
010000       WHEN BANK-MAP-FUNCTION-GET                                 SBANK40P
010100         PERFORM SCREEN40-READ THRU                               SBANK40P
010200                 SCREEN40-READ-EXIT                               SBANK40P
010300       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK40P
010400         PERFORM SCREEN40-BUILD-AND-SEND THRU                     SBANK40P
010500                 SCREEN40-BUILD-AND-SEND-EXIT                     SBANK40P
010600       WHEN OTHER                                                 SBANK40P
010700         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK40P
010800         MOVE '0002' TO ABEND-CODE                                SBANK40P
010900         MOVE SPACES TO ABEND-REASON                              SBANK40P
011000         COPY CABENDPO.                                           SBANK40P
011100     END-EVALUATE.                                                SBANK40P
011200                                                                  SBANK40P
011300* Call the appropriate routine to handle the business logic       SBANK40P
011400     IF BANK-MAP-FUNCTION-GET                                     SBANK40P
011500        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK40P
011600                       COMMAREA(WS-BANK-DATA)                     SBANK40P
011700                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK40P
011800        END-EXEC                                                  SBANK40P
011900     END-IF.                                                      SBANK40P
012000                                                                  SBANK40P
012100***************************************************************** SBANK40P
012200* Now we have to have finished and can return to our invoker.   * SBANK40P
012300***************************************************************** SBANK40P
012400* Now return to CICS                                              SBANK40P
012500     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK40P
012600          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK40P
012700     EXEC CICS                                                    SBANK40P
012800          RETURN                                                  SBANK40P
012900     END-EXEC.                                                    SBANK40P
013000     GOBACK.                                                      SBANK40P
013100                                                                  SBANK40P
013200***************************************************************** SBANK40P
013300* Screen processing for MBANK40                                 * SBANK40P
013400*---------------------------------------------------------------* SBANK40P
013500* Retrieve data from screen and format it                       * SBANK40P
013600***************************************************************** SBANK40P
013700 SCREEN40-READ.                                                   SBANK40P
013800     MOVE 'BBANK40P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK40P
013900     IF BANK-AID-CLEAR                                            SBANK40P
014000        SET BANK-AID-PFK03 TO TRUE                                SBANK40P
014100        GO TO SCREEN40-READ-EXIT                                  SBANK40P
014200     END-IF.                                                      SBANK40P
014300     IF BANK-ENV-CICS                                             SBANK40P
014400        GO TO SCREEN40-READ-CICS                                  SBANK40P
014500     ELSE                                                         SBANK40P
014600        GO TO SCREEN40-READ-INET                                  SBANK40P
014700     END-IF.                                                      SBANK40P
014800                                                                  SBANK40P
014900 SCREEN40-READ-CICS.                                              SBANK40P
015000     IF BANK-HELP-INACTIVE                                        SBANK40P
015100        EXEC CICS RECEIVE MAP('BANK40A')                          SBANK40P
015200                          MAPSET('MBANK40')                       SBANK40P
015300        END-EXEC                                                  SBANK40P
015400     ELSE                                                         SBANK40P
015500        EXEC CICS RECEIVE MAP('HELP40A')                          SBANK40P
015600                          MAPSET('MBANK40')                       SBANK40P
015700        END-EXEC                                                  SBANK40P
015800        GO TO SCREEN40-READ-EXIT                                  SBANK40P
015900     END-IF.                                                      SBANK40P
016000                                                                  SBANK40P
016100     GO TO SCREEN40-READ-EXIT.                                    SBANK40P
016200                                                                  SBANK40P
016300 SCREEN40-READ-INET.                                              SBANK40P
016400     GO TO SCREEN40-READ-EXIT.                                    SBANK40P
016500                                                                  SBANK40P
016600 SCREEN40-READ-EXIT.                                              SBANK40P
016700     EXIT.                                                        SBANK40P
016800                                                                  SBANK40P
016900***************************************************************** SBANK40P
017000* Screen processing for SCREEN40 (BANK40/HELP40)                * SBANK40P
017100*---------------------------------------------------------------* SBANK40P
017200* Build the output screen and send it                           * SBANK40P
017300***************************************************************** SBANK40P
017400 SCREEN40-BUILD-AND-SEND.                                         SBANK40P
017500* Clear map area, get date & time and move to the map             SBANK40P
017600     MOVE LOW-VALUES TO BANK40AO.                                 SBANK40P
017700     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK40P
017800     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK40P
017900     SET DDI-YYDDD TO TRUE.                                       SBANK40P
018000     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK40P
018100     PERFORM CALL-DATECONV THRU                                   SBANK40P
018200             CALL-DATECONV-EXIT.                                  SBANK40P
018300* Ensure the last map fields are correct                          SBANK40P
018400     IF BANK-HELP-ACTIVE                                          SBANK40P
018500        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        SBANK40P
018600        MOVE 'HELP40A' TO BANK-LAST-MAP                           SBANK40P
018700     ELSE                                                         SBANK40P
018800        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        SBANK40P
018900        MOVE 'BANK40A' TO BANK-LAST-MAP                           SBANK40P
019000     END-IF.                                                      SBANK40P
019100     IF BANK-ENV-CICS                                             SBANK40P
019200        GO TO SCREEN40-BUILD-AND-SEND-CICS                        SBANK40P
019300     ELSE                                                         SBANK40P
019400        GO TO SCREEN40-BUILD-AND-SEND-INET                        SBANK40P
019500     END-IF.                                                      SBANK40P
019600                                                                  SBANK40P
019700 SCREEN40-BUILD-AND-SEND-CICS.                                    SBANK40P
019800     IF BANK-LAST-MAP IS EQUAL TO 'BANK40A'                       SBANK40P
019900        GO TO BANK40-BUILD-AND-SEND-CICS                          SBANK40P
020000     END-IF.                                                      SBANK40P
020100     IF BANK-LAST-MAP IS EQUAL TO 'HELP40A'                       SBANK40P
020200        GO TO HELP40-BUILD-AND-SEND-CICS                          SBANK40P
020300     END-IF.                                                      SBANK40P
020400     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK40P
020500     MOVE '0003' TO ABEND-CODE                                    SBANK40P
020600     MOVE SPACES TO ABEND-REASON                                  SBANK40P
020700     COPY CABENDPO.                                               SBANK40P
020800     GOBACK.                                                      SBANK40P
020900                                                                  SBANK40P
021000 BANK40-BUILD-AND-SEND-CICS.                                      SBANK40P
021100     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK40AO==.        SBANK40P
021200     MOVE WS-TRAN-ID TO TRANO IN BANK40AO.                        SBANK40P
021300     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK40AO.                    SBANK40P
021400     MOVE DDO-DATA TO DATEO IN BANK40AO.                          SBANK40P
021500* Move in any error message                                       SBANK40P
021600     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK40AO.                  SBANK40P
021700* Move in screen specific fields                                  SBANK40P
021800     MOVE BANK-SCR40-ACC TO ACCNOO IN BANK40AO.                   SBANK40P
021900     MOVE BANK-SCR40-ACCTYPE TO ACCTYPEO IN BANK40AO.             SBANK40P
022000                                                                  SBANK40P
022100     EVALUATE TRUE                                                SBANK40P
022200       WHEN BANK-PAGING-OFF                                       SBANK40P
022300         MOVE DFHBMDAR TO TXT09A IN BANK40AI                      SBANK40P
022400         MOVE SPACES TO MOREO IN BANK40AO                         SBANK40P
022500         MOVE SPACES TO PAGINGO IN BANK40AO                       SBANK40P
022600       WHEN BANK-PAGING-FIRST                                     SBANK40P
022700         MOVE ' /+' TO MOREO IN BANK40AO                          SBANK40P
022800         MOVE '        F8=Forward' TO PAGINGO IN BANK40AO         SBANK40P
022900       WHEN BANK-PAGING-MIDDLE                                    SBANK40P
023000         MOVE '-/+' TO MOREO IN BANK40AO                          SBANK40P
023100         MOVE 'F7=Back F8=Forward' TO PAGINGO IN BANK40AO         SBANK40P
023200       WHEN BANK-PAGING-LAST                                      SBANK40P
023300         MOVE '-/ ' TO MOREO IN BANK40AO                          SBANK40P
023400         MOVE 'F7=Back           ' TO PAGINGO IN BANK40AO         SBANK40P
023500       WHEN OTHER                                                 SBANK40P
023600         MOVE DFHBMDAR TO TXT09A IN BANK40AI                      SBANK40P
023700         MOVE SPACES TO MOREO IN BANK40AO                         SBANK40P
023800         MOVE SPACES TO PAGINGO IN BANK40AO                       SBANK40P
023900     END-EVALUATE.                                                SBANK40P
024000                                                                  SBANK40P
024100     MOVE BANK-SCR40-DAT1 TO DAT1O IN BANK40AO.                   SBANK40P
024200     MOVE BANK-SCR40-TIM1 TO TIM1O IN BANK40AO.                   SBANK40P
024300     MOVE BANK-SCR40-AMT1 TO AMT1O IN BANK40AO.                   SBANK40P
024400     MOVE BANK-SCR40-DSC1 TO DSC1O IN BANK40AO.                   SBANK40P
024500     MOVE BANK-SCR40-DAT2 TO DAT2O IN BANK40AO.                   SBANK40P
024600     MOVE BANK-SCR40-TIM2 TO TIM2O IN BANK40AO.                   SBANK40P
024700     MOVE BANK-SCR40-AMT2 TO AMT2O IN BANK40AO.                   SBANK40P
024800     MOVE BANK-SCR40-DSC2 TO DSC2O IN BANK40AO.                   SBANK40P
024900     MOVE BANK-SCR40-DAT3 TO DAT3O IN BANK40AO.                   SBANK40P
025000     MOVE BANK-SCR40-TIM3 TO TIM3O IN BANK40AO.                   SBANK40P
025100     MOVE BANK-SCR40-AMT3 TO AMT3O IN BANK40AO.                   SBANK40P
025200     MOVE BANK-SCR40-DSC3 TO DSC3O IN BANK40AO.                   SBANK40P
025300     MOVE BANK-SCR40-DAT4 TO DAT4O IN BANK40AO.                   SBANK40P
025400     MOVE BANK-SCR40-TIM4 TO TIM4O IN BANK40AO.                   SBANK40P
025500     MOVE BANK-SCR40-AMT4 TO AMT4O IN BANK40AO.                   SBANK40P
025600     MOVE BANK-SCR40-DSC4 TO DSC4O IN BANK40AO.                   SBANK40P
025700     MOVE BANK-SCR40-DAT5 TO DAT5O IN BANK40AO.                   SBANK40P
025800     MOVE BANK-SCR40-TIM5 TO TIM5O IN BANK40AO.                   SBANK40P
025900     MOVE BANK-SCR40-AMT5 TO AMT5O IN BANK40AO.                   SBANK40P
026000     MOVE BANK-SCR40-DSC5 TO DSC5O IN BANK40AO.                   SBANK40P
026100     MOVE BANK-SCR40-DAT6 TO DAT6O IN BANK40AO.                   SBANK40P
026200     MOVE BANK-SCR40-TIM6 TO TIM6O IN BANK40AO.                   SBANK40P
026300     MOVE BANK-SCR40-AMT6 TO AMT6O IN BANK40AO.                   SBANK40P
026400     MOVE BANK-SCR40-DSC6 TO DSC6O IN BANK40AO.                   SBANK40P
026500     MOVE BANK-SCR40-DAT7 TO DAT7O IN BANK40AO.                   SBANK40P
026600     MOVE BANK-SCR40-TIM7 TO TIM7O IN BANK40AO.                   SBANK40P
026700     MOVE BANK-SCR40-AMT7 TO AMT7O IN BANK40AO.                   SBANK40P
026800     MOVE BANK-SCR40-DSC7 TO DSC7O IN BANK40AO.                   SBANK40P
026900     MOVE BANK-SCR40-DAT8 TO DAT8O IN BANK40AO.                   SBANK40P
027000     MOVE BANK-SCR40-TIM8 TO TIM8O IN BANK40AO.                   SBANK40P
027100     MOVE BANK-SCR40-AMT8 TO AMT8O IN BANK40AO.                   SBANK40P
027200     MOVE BANK-SCR40-DSC8 TO DSC8O IN BANK40AO.                   SBANK40P
027300* Turn colour off if requ8red                                     SBANK40P
027400     IF COLOUR-OFF                                                SBANK40P
027500        MOVE DFHGREEN TO TXT01C IN BANK40AO                       SBANK40P
027600        MOVE DFHGREEN TO SCRNC IN BANK40AO                        SBANK40P
027700        MOVE DFHGREEN TO HEAD1C IN BANK40AO                       SBANK40P
027800        MOVE DFHGREEN TO DATEC IN BANK40AO                        SBANK40P
027900        MOVE DFHGREEN TO TXT02C IN BANK40AO                       SBANK40P
028000        MOVE DFHGREEN TO TRANC IN BANK40AO                        SBANK40P
028100        MOVE DFHGREEN TO HEAD2C IN BANK40AO                       SBANK40P
028200        MOVE DFHGREEN TO TIMEC IN BANK40AO                        SBANK40P
028300        MOVE DFHGREEN TO TXT03C IN BANK40AO                       SBANK40P
028400        MOVE DFHGREEN TO ACCNOC IN BANK40AO                       SBANK40P
028500        MOVE DFHGREEN TO TXT04C IN BANK40AO                       SBANK40P
028600        MOVE DFHGREEN TO ACCTYPEC IN BANK40AO                     SBANK40P
028700        MOVE DFHGREEN TO TXT05C IN BANK40AO                       SBANK40P
028800        MOVE DFHGREEN TO TXT06C IN BANK40AO                       SBANK40P
028900        MOVE DFHGREEN TO TXT07C IN BANK40AO                       SBANK40P
029000        MOVE DFHGREEN TO TXT08C IN BANK40AO                       SBANK40P
029100        MOVE DFHGREEN TO TXT09C IN BANK40AO                       SBANK40P
029200        MOVE DFHGREEN TO MOREC IN BANK40AO                        SBANK40P
029300        MOVE DFHGREEN TO DAT1C IN BANK40AO                        SBANK40P
029400        MOVE DFHGREEN TO TIM1C IN BANK40AO                        SBANK40P
029500        MOVE DFHGREEN TO AMT1C IN BANK40AO                        SBANK40P
029600        MOVE DFHGREEN TO DSC1C IN BANK40AO                        SBANK40P
029700        MOVE DFHGREEN TO DAT2C IN BANK40AO                        SBANK40P
029800        MOVE DFHGREEN TO TIM2C IN BANK40AO                        SBANK40P
029900        MOVE DFHGREEN TO AMT2C IN BANK40AO                        SBANK40P
030000        MOVE DFHGREEN TO DSC2C IN BANK40AO                        SBANK40P
030100        MOVE DFHGREEN TO DAT3C IN BANK40AO                        SBANK40P
030200        MOVE DFHGREEN TO TIM3C IN BANK40AO                        SBANK40P
030300        MOVE DFHGREEN TO AMT3C IN BANK40AO                        SBANK40P
030400        MOVE DFHGREEN TO DSC3C IN BANK40AO                        SBANK40P
030500        MOVE DFHGREEN TO DAT4C IN BANK40AO                        SBANK40P
030600        MOVE DFHGREEN TO TIM4C IN BANK40AO                        SBANK40P
030700        MOVE DFHGREEN TO AMT4C IN BANK40AO                        SBANK40P
030800        MOVE DFHGREEN TO DSC4C IN BANK40AO                        SBANK40P
030900        MOVE DFHGREEN TO DAT5C IN BANK40AO                        SBANK40P
031000        MOVE DFHGREEN TO TIM5C IN BANK40AO                        SBANK40P
031100        MOVE DFHGREEN TO AMT5C IN BANK40AO                        SBANK40P
031200        MOVE DFHGREEN TO DSC5C IN BANK40AO                        SBANK40P
031300        MOVE DFHGREEN TO DAT6C IN BANK40AO                        SBANK40P
031400        MOVE DFHGREEN TO TIM6C IN BANK40AO                        SBANK40P
031500        MOVE DFHGREEN TO AMT6C IN BANK40AO                        SBANK40P
031600        MOVE DFHGREEN TO DSC6C IN BANK40AO                        SBANK40P
031700        MOVE DFHGREEN TO DAT7C IN BANK40AO                        SBANK40P
031800        MOVE DFHGREEN TO TIM7C IN BANK40AO                        SBANK40P
031900        MOVE DFHGREEN TO AMT7C IN BANK40AO                        SBANK40P
032000        MOVE DFHGREEN TO DSC7C IN BANK40AO                        SBANK40P
032100        MOVE DFHGREEN TO DAT8C IN BANK40AO                        SBANK40P
032200        MOVE DFHGREEN TO TIM8C IN BANK40AO                        SBANK40P
032300        MOVE DFHGREEN TO AMT8C IN BANK40AO                        SBANK40P
032400        MOVE DFHGREEN TO DSC8C IN BANK40AO                        SBANK40P
032500        MOVE DFHGREEN TO ERRMSGC IN BANK40AO                      SBANK40P
032600        MOVE DFHGREEN TO TXT10C IN BANK40AO                       SBANK40P
032700        MOVE DFHGREEN TO PAGINGC IN BANK40AO                      SBANK40P
032800     END-IF.                                                      SBANK40P
032900                                                                  SBANK40P
033000     EXEC CICS SEND MAP('BANK40A')                                SBANK40P
033100                    MAPSET('MBANK40')                             SBANK40P
033200                    ERASE                                         SBANK40P
033300                    FREEKB                                        SBANK40P
033400     END-EXEC.                                                    SBANK40P
033500     GO TO SCREEN40-BUILD-AND-SEND-EXIT.                          SBANK40P
033600                                                                  SBANK40P
033700 HELP40-BUILD-AND-SEND-CICS.                                      SBANK40P
033800     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK40P
033900                             ==<<SCRN>>== BY ==HELP40AO==.        SBANK40P
034000                                                                  SBANK40P
034100     EXEC CICS SEND MAP('HELP40A')                                SBANK40P
034200                    MAPSET('MBANK40')                             SBANK40P
034300                    ERASE                                         SBANK40P
034400                    FREEKB                                        SBANK40P
034500     END-EXEC.                                                    SBANK40P
034600     GO TO SCREEN40-BUILD-AND-SEND-EXIT.                          SBANK40P
034700                                                                  SBANK40P
034800 SCREEN40-BUILD-AND-SEND-INET.                                    SBANK40P
034900     MOVE SPACES TO EXT-OP-DATA.                                  SBANK40P
035000     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK40P
035100     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK40P
035200     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK40P
035300     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK40P
035400     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK40P
035500     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK40P
035600* Move in screen name                                             SBANK40P
035700     MOVE 'BANK40' TO EXT-OP-SCREEN.                              SBANK40P
035800* Move in userid and any error message                            SBANK40P
035900     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK40P
036000     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK40P
036100     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK40P
036200* Move in screen specific fields                                  SBANK40P
036300     MOVE BANK-SCR40-ACC TO EXT-OP40-ACCNO.                       SBANK40P
036400     MOVE BANK-SCR40-ACCTYPE TO EXT-OP40-ACCTYPE.                 SBANK40P
036500     MOVE BANK-PAGING-STATUS TO EXT-OP40-PAGING-STATUS.           SBANK40P
036600     MOVE 0 TO WS-SUB1.                                           SBANK40P
036700     PERFORM SCREEN40-BUILD-AND-SEND-INET1 8 TIMES.               SBANK40P
036800     GO TO SCREEN40-BUILD-AND-SEND-EXIT.                          SBANK40P
036900 SCREEN40-BUILD-AND-SEND-INET1.                                   SBANK40P
037000     ADD 1 TO WS-SUB1.                                            SBANK40P
037100     MOVE BANK-SCR40-DATE (WS-SUB1) TO EXT-OP40-DATE (WS-SUB1).   SBANK40P
037200     MOVE BANK-SCR40-TIME (WS-SUB1) TO EXT-OP40-TIME (WS-SUB1).   SBANK40P
037300     MOVE BANK-SCR40-AMNT (WS-SUB1) TO EXT-OP40-AMNT (WS-SUB1).   SBANK40P
037400     MOVE BANK-SCR40-DESC (WS-SUB1) TO EXT-OP40-DESC (WS-SUB1).   SBANK40P
037500                                                                  SBANK40P
037600                                                                  SBANK40P
037700 SCREEN40-BUILD-AND-SEND-EXIT.                                    SBANK40P
037800     EXIT.                                                        SBANK40P
037900                                                                  SBANK40P
038000***************************************************************** SBANK40P
038100* Call common routine to perform date conversions               * SBANK40P
038200***************************************************************** SBANK40P
038300 CALL-DATECONV.                                                   SBANK40P
038400     MOVE BANK-ENV TO DD-ENV.                                     SBANK40P
038500     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK40P
038600     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK40P
038700 CALL-DATECONV-EXIT.                                              SBANK40P
038800     EXIT.                                                        SBANK40P
038900                                                                  SBANK40P
039000* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK40P
