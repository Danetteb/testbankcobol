000100***************************************************************** SCASH20P
000200*                                                               * SCASH20P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SCASH20P
000400*   This demonstration program is provided for use by users     * SCASH20P
000500*   of Micro Focus products and may be used, modified and       * SCASH20P
000600*   distributed as part of your application provided that       * SCASH20P
000700*   you properly acknowledge the copyright of Micro Focus       * SCASH20P
000800*   in this material.                                           * SCASH20P
000900*                                                               * SCASH20P
001000***************************************************************** SCASH20P
001100                                                                  SCASH20P
001200***************************************************************** SCASH20P
001300* Program:     SCASH20P.CBL (CICS Version)                      * SCASH20P
001400* Layer:       'Screen' handling                                * SCASH20P
001500* Function:    Display ATM enabled accounts                     * SCASH20P
001600***************************************************************** SCASH20P
001700                                                                  SCASH20P
001800 IDENTIFICATION DIVISION.                                         SCASH20P
001900 PROGRAM-ID.                                                      SCASH20P
002000     SCASH20P.                                                    SCASH20P
002100 DATE-WRITTEN.                                                    SCASH20P
002200     September 2002.                                              SCASH20P
002300 DATE-COMPILED.                                                   SCASH20P
002400     Today.                                                       SCASH20P
002500                                                                  SCASH20P
002600 ENVIRONMENT DIVISION.                                            SCASH20P
002700                                                                  SCASH20P
002800 DATA DIVISION.                                                   SCASH20P
002900 WORKING-STORAGE SECTION.                                         SCASH20P
003000 01  WS-MISC-STORAGE.                                             SCASH20P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SCASH20P
003200       VALUE 'SCASH20P'.                                          SCASH20P
003300   05  WS-SAVED-EIBCALEN                     PIC 9(5).            SCASH20P
003400   05  WS-TRAN-ID                            PIC X(4).            SCASH20P
003500   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SCASH20P
003600       VALUE SPACES.                                              SCASH20P
003700   05  WS-DYNAMIC-PGM                        PIC X(8)             SCASH20P
003800       VALUE 'UNKNOWN'.                                           SCASH20P
003900   05  WS-SUB1                               PIC S9(4) COMP.      SCASH20P
004000   05  WS-LEN1                               PIC S9(4) COMP.      SCASH20P
004100   05  WS-INPUT-SOURCE                       PIC X(1).            SCASH20P
004200     88  INPUT-UNKNOWN                       VALUE '0'.           SCASH20P
004300     88  INPUT-FROM-SCREEN                   VALUE '1'.           SCASH20P
004400     88  INPUT-FROM-ATM                      VALUE '2'.           SCASH20P
004500   05  WS-CALL-BUSINESS-LOGIC                PIC X(1).            SCASH20P
004600     88  CALL-BUSINESS-LOGIC-OK              VALUE '1'.           SCASH20P
004700     88  CALL-BUSINESS-LOGIC-NOT-OK          VALUE '2'.           SCASH20P
004800                                                                  SCASH20P
004900 01  WS-SCREEN-ITEMS.                                             SCASH20P
005000   05  WS-IP-SCREEN-ITEMS.                                        SCASH20P
005100     10  WS-IP-SCREEN-STATUS                 PIC X(1).            SCASH20P
005200       88  IP-SCREEN-STATUS-UNKNOWN          VALUE '0'.           SCASH20P
005300       88  IP-SCREEN-STATUS-OK               VALUE '1'.           SCASH20P
005400       88  IP-SCREEN-STATUS-NOT-OK           VALUE '2'.           SCASH20P
005500     10  WS-IP-SCREEN-DATA                   PIC X(512).          SCASH20P
005600     10  WS-IP-SCREEN-LENGTH                 PIC 9(4) BINARY.     SCASH20P
005700     10  WS-IP-MAX-SCREEN-LENGTH             PIC 9(4) BINARY.     SCASH20P
005800     10  WS-IP-SCREEN-TRAN                   PIC X(4).            SCASH20P
005900     10  WS-IP-SCREEN-USERID                 PIC X(5).            SCASH20P
006000     10  WS-IP-SCREEN-PIN                    PIC X(4).            SCASH20P
006100   05  WS-OP-SCREEN-ITEMS.                                        SCASH20P
006200     10  WS-OP-SCREEN-DATA                   PIC X(1920).         SCASH20P
006300     10  WS-OP-SCREEN-LENGTH                 PIC 9(4) BINARY.     SCASH20P
006400     10  WS-OP-MAX-SCREEN-LENGTH             PIC 9(4) BINARY.     SCASH20P
006500                                                                  SCASH20P
006600 01  WS-CASH-DATA-AREAS.                                          SCASH20P
006700   05  WS-CASH-DATA.                                              SCASH20P
006800 COPY CCASHDAT.                                                   SCASH20P
006900   05  WS-CASH-EXT-DATA.                                          SCASH20P
007000 COPY CCASHEXT.                                                   SCASH20P
007100                                                                  SCASH20P
007200 COPY CABENDD.                                                    SCASH20P
007300                                                                  SCASH20P
007400 LINKAGE SECTION.                                                 SCASH20P
007500 01  DFHCOMMAREA.                                                 SCASH20P
007600   05  FILLER                                PIC X(1024).         SCASH20P
007700                                                                  SCASH20P
007800 PROCEDURE DIVISION.                                              SCASH20P
007900***************************************************************** SCASH20P
008000* Write entry to log to show we have been invoked               * SCASH20P
008100***************************************************************** SCASH20P
008200*    COPY CTRACE.                                                 SCASH20P
008300                                                                  SCASH20P
008400***************************************************************** SCASH20P
008500* Store our transaction-id                                      * SCASH20P
008600***************************************************************** SCASH20P
008700     MOVE EIBTRNID TO WS-TRAN-ID.                                 SCASH20P
008800     MOVE EIBCALEN TO WS-SAVED-EIBCALEN.                          SCASH20P
008900                                                                  SCASH20P
009000     SET INPUT-UNKNOWN TO TRUE.                                   SCASH20P
009100     SET CALL-BUSINESS-LOGIC-OK TO TRUE.                          SCASH20P
009200                                                                  SCASH20P
009300     IF EIBCALEN IS EQUAL TO 0                                    SCASH20P
009400        SET INPUT-FROM-SCREEN TO TRUE                             SCASH20P
009500        SET IP-SCREEN-STATUS-UNKNOWN TO TRUE                      SCASH20P
009600        EXEC CICS RECEIVE                                         SCASH20P
009700                  INTO(WS-IP-SCREEN-DATA)                         SCASH20P
009800                  LENGTH(WS-IP-SCREEN-LENGTH)                     SCASH20P
009900                  MAXLENGTH(LENGTH OF WS-IP-SCREEN-DATA)          SCASH20P
010000        END-EXEC                                                  SCASH20P
010100      ELSE                                                        SCASH20P
010200        IF EIBCALEN IS LESS THAN 100                              SCASH20P
010300           MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                    SCASH20P
010400           MOVE '0001' TO ABEND-CODE                              SCASH20P
010500           MOVE 'EIBCALEN less than 100' TO ABEND-REASON          SCASH20P
010600           COPY CABENDPO.                                         SCASH20P
010700        ELSE                                                      SCASH20P
010800           MOVE SPACES TO WS-CASH-DATA                            SCASH20P
010900           SET INPUT-FROM-ATM TO TRUE                             SCASH20P
011000           MOVE DFHCOMMAREA(1:WS-SAVED-EIBCALEN)                  SCASH20P
011100             TO WS-CASH-EXT-DATA(1:WS-SAVED-EIBCALEN)             SCASH20P
011200        END-IF                                                    SCASH20P
011300     END-IF.                                                      SCASH20P
011400                                                                  SCASH20P
011500***************************************************************** SCASH20P
011600* This is the main process                                      * SCASH20P
011700***************************************************************** SCASH20P
011800                                                                  SCASH20P
011900***************************************************************** SCASH20P
012000* Determine what we have to do                                  * SCASH20P
012100***************************************************************** SCASH20P
012200* Obtain and format the input                                     SCASH20P
012300     PERFORM READ-INPUT THRU                                      SCASH20P
012400             READ-INPUT-EXIT.                                     SCASH20P
012500                                                                  SCASH20P
012600* Call the appropriate routine to check the userid & PIN          SCASH20P
012700     IF CALL-BUSINESS-LOGIC-OK                                    SCASH20P
012800        MOVE 'BCASH10P' TO WS-BUSINESS-LOGIC-PGM                  SCASH20P
012900        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SCASH20P
013000                       COMMAREA(WS-CASH-DATA)                     SCASH20P
013100                       LENGTH(LENGTH OF WS-CASH-DATA)             SCASH20P
013200        END-EXEC                                                  SCASH20P
013300     END-IF.                                                      SCASH20P
013400     IF NOT CASH-PIN-STATUS-OK                                    SCASH20P
013500        SET CALL-BUSINESS-LOGIC-NOT-OK TO TRUE                    SCASH20P
013600     END-IF.                                                      SCASH20P
013700                                                                  SCASH20P
013800* Call the appropriate routine to handle the business logic       SCASH20P
013900     IF CALL-BUSINESS-LOGIC-OK                                    SCASH20P
014000        MOVE 'BCASH20P' TO WS-BUSINESS-LOGIC-PGM                  SCASH20P
014100        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SCASH20P
014200                       COMMAREA(WS-CASH-DATA)                     SCASH20P
014300                       LENGTH(LENGTH OF WS-CASH-DATA)             SCASH20P
014400        END-EXEC                                                  SCASH20P
014500     END-IF.                                                      SCASH20P
014600* Format and return the outpput                                   SCASH20P
014700     PERFORM SEND-OUTPUT THRU                                     SCASH20P
014800             SEND-OUTPUT-EXIT.                                    SCASH20P
014900                                                                  SCASH20P
015000***************************************************************** SCASH20P
015100* Now we have to have finished and can return to our invoker.   * SCASH20P
015200***************************************************************** SCASH20P
015300* Now return to CICS                                              SCASH20P
015400     IF WS-SAVED-EIBCALEN IS GREATER THAN 0                       SCASH20P
015500        MOVE WS-CASH-EXT-DATA(1:WS-SAVED-EIBCALEN)                SCASH20P
015600          TO DFHCOMMAREA(1:WS-SAVED-EIBCALEN)                     SCASH20P
015700     END-IF.                                                      SCASH20P
015800     EXEC CICS                                                    SCASH20P
015900          RETURN                                                  SCASH20P
016000     END-EXEC.                                                    SCASH20P
016100     GOBACK.                                                      SCASH20P
016200                                                                  SCASH20P
016300***************************************************************** SCASH20P
016400* Retrieve input data and format it                             * SCASH20P
016500***************************************************************** SCASH20P
016600 READ-INPUT.                                                      SCASH20P
016700     MOVE SPACES TO WS-CASH-DATA.                                 SCASH20P
016800     IF INPUT-FROM-SCREEN                                         SCASH20P
016900        GO TO READ-INPUT-SCREEN                                   SCASH20P
017000     END-IF.                                                      SCASH20P
017100     IF INPUT-FROM-ATM                                            SCASH20P
017200        GO TO READ-INPUT-ATM                                      SCASH20P
017300     END-IF.                                                      SCASH20P
017400                                                                  SCASH20P
017500     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT.                         SCASH20P
017600     MOVE '0002' TO ABEND-CODE.                                   SCASH20P
017700     MOVE 'Cannot determine input source' TO ABEND-REASON.        SCASH20P
017800     COPY CABENDPO. .                                             SCASH20P
017900                                                                  SCASH20P
018000 READ-INPUT-SCREEN.                                               SCASH20P
018100* Expect the format to be tttt^uuuuu^ppppp where:                 SCASH20P
018200*   ^ is a space                                                  SCASH20P
018300*   tttt is the tranid                                            SCASH20P
018400*   uuuuu is the userid                                           SCASH20P
018500*   ppppp is the password                                         SCASH20P
018600     MOVE SPACES TO WS-IP-SCREEN-TRAN.                            SCASH20P
018700     MOVE SPACES TO WS-IP-SCREEN-USERID.                          SCASH20P
018800     MOVE SPACES TO WS-IP-SCREEN-PIN.                             SCASH20P
018900     UNSTRING WS-IP-SCREEN-DATA                                   SCASH20P
019000        DELIMITED BY ALL SPACES INTO                              SCASH20P
019100          WS-IP-SCREEN-TRAN                                       SCASH20P
019200          WS-IP-SCREEN-USERID                                     SCASH20P
019300          WS-IP-SCREEN-PIN.                                       SCASH20P
019400     IF WS-IP-SCREEN-USERID IS EQUAL TO SPACES                    SCASH20P
019500        SET CALL-BUSINESS-LOGIC-NOT-OK TO TRUE                    SCASH20P
019600        MOVE 'Must send userid' TO CASH-ERROR-MSG                 SCASH20P
019700     ELSE                                                         SCASH20P
019800        MOVE WS-IP-SCREEN-USERID TO CASH-SIGNON-ID                SCASH20P
019900        MOVE WS-IP-SCREEN-USERID TO CASH-USERID                   SCASH20P
020000        MOVE WS-IP-SCREEN-PIN TO CASH-PIN                         SCASH20P
020100     END-IF.                                                      SCASH20P
020200     GO TO READ-INPUT-EXIT.                                       SCASH20P
020300                                                                  SCASH20P
020400 READ-INPUT-ATM.                                                  SCASH20P
020500     MOVE ATM-IP00-USERID TO CASH-SIGNON-ID.                      SCASH20P
020600     MOVE ATM-IP00-USERID TO CASH-USERID.                         SCASH20P
020700     MOVE ATM-IP00-PIN TO CASH-PIN.                               SCASH20P
020800     GO TO READ-INPUT-EXIT.                                       SCASH20P
020900                                                                  SCASH20P
021000 READ-INPUT-EXIT.                                                 SCASH20P
021100     EXIT.                                                        SCASH20P
021200                                                                  SCASH20P
021300***************************************************************** SCASH20P
021400* Build the output screen and send it                           * SCASH20P
021500***************************************************************** SCASH20P
021600 SEND-OUTPUT.                                                     SCASH20P
021700     IF INPUT-FROM-SCREEN                                         SCASH20P
021800        GO TO SEND-OUTPUT-TO-SCREEN                               SCASH20P
021900     END-IF.                                                      SCASH20P
022000     IF INPUT-FROM-ATM                                            SCASH20P
022100        GO TO SEND-OUTPUT-TO-ATM                                  SCASH20P
022200     END-IF.                                                      SCASH20P
022300                                                                  SCASH20P
022400     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT.                         SCASH20P
022500     MOVE '0003' TO ABEND-CODE.                                   SCASH20P
022600     MOVE 'Cannot determine output dest' TO ABEND-REASON.         SCASH20P
022700     COPY CABENDPO.                                               SCASH20P
022800     GOBACK.                                                      SCASH20P
022900                                                                  SCASH20P
023000 SEND-OUTPUT-TO-SCREEN.                                           SCASH20P
023100     MOVE 1 TO WS-SUB1.                                           SCASH20P
023200     MOVE LOW-VALUES TO WS-OP-SCREEN-DATA.                        SCASH20P
023300                                                                  SCASH20P
023400     MOVE 'Input:' TO WS-OP-SCREEN-DATA(WS-SUB1:6).               SCASH20P
023500     ADD 80 TO WS-SUB1.                                           SCASH20P
023600                                                                  SCASH20P
023700     MOVE WS-IP-SCREEN-DATA(1:WS-IP-SCREEN-LENGTH)                SCASH20P
023800       TO WS-OP-SCREEN-DATA(WS-SUB1:WS-IP-SCREEN-LENGTH).         SCASH20P
023900     COMPUTE WS-LEN1 = (((WS-IP-SCREEN-LENGTH + 79) / 80 ) * 80). SCASH20P
024000     ADD WS-LEN1 TO WS-SUB1.                                      SCASH20P
024100                                                                  SCASH20P
024200     ADD 80 TO WS-SUB1.                                           SCASH20P
024300                                                                  SCASH20P
024400     MOVE 'Output:' TO WS-OP-SCREEN-DATA(WS-SUB1:7).              SCASH20P
024500     ADD 80 TO WS-SUB1.                                           SCASH20P
024600                                                                  SCASH20P
024700     MOVE 'CASH-ERROR-MSG:'                                       SCASH20P
024800       TO WS-OP-SCREEN-DATA(WS-SUB1:15).                          SCASH20P
024900     ADD 80 TO WS-SUB1.                                           SCASH20P
025000                                                                  SCASH20P
025100     MOVE CASH-ERROR-MSG                                          SCASH20P
025200       TO WS-OP-SCREEN-DATA(WS-SUB1:LENGTH OF CASH-ERROR-MSG).    SCASH20P
025300     COMPUTE WS-LEN1 = (((lENGTH OF CASH-ERROR-MSG + 79)          SCASH20P
025400                         / 80 ) * 80).                            SCASH20P
025500     ADD WS-LEN1 TO WS-SUB1.                                      SCASH20P
025600                                                                  SCASH20P
025700     MOVE 'USER-DETAILS:' TO WS-OP-SCREEN-DATA(WS-SUB1:13).       SCASH20P
025800     ADD 80 TO WS-SUB1.                                           SCASH20P
025900                                                                  SCASH20P
026000     MOVE CASH-USER-DETAILS TO WS-OP-SCREEN-DATA(WS-SUB1:5).      SCASH20P
026100     COMPUTE WS-LEN1 = (((lENGTH OF CASH-USER-DETAILS + 79)       SCASH20P
026200                         / 80 ) * 80).                            SCASH20P
026300     ADD WS-LEN1 TO WS-SUB1.                                      SCASH20P
026400                                                                  SCASH20P
026500     MOVE 'CASH-ATM-DATA:' TO WS-OP-SCREEN-DATA(WS-SUB1:14).      SCASH20P
026600     ADD 80 TO WS-SUB1.                                           SCASH20P
026700                                                                  SCASH20P
026800     MOVE CASH-ATM-DATA                                           SCASH20P
026900       TO WS-OP-SCREEN-DATA(WS-SUB1:LENGTH OF CASH-ATM-DATA).     SCASH20P
027000     ADD LENGTH OF CASH-ATM-DATA TO WS-SUB1.                      SCASH20P
027100                                                                  SCASH20P
027200     EXEC CICS SEND                                               SCASH20P
027300                    FROM(WS-OP-SCREEN-DATA)                       SCASH20P
027400                    LENGTH(WS-SUB1)                               SCASH20P
027500                    ERASE                                         SCASH20P
027600     END-EXEC.                                                    SCASH20P
027700     GO TO SEND-OUTPUT-EXIT.                                      SCASH20P
027800                                                                  SCASH20P
027900 SEND-OUTPUT-TO-ATM.                                              SCASH20P
028000     MOVE SPACES TO ATM-OP-DATA.                                  SCASH20P
028100* Move in userid and any error message                            SCASH20P
028200     MOVE CASH-ERROR-MSG TO ATM-OP-ERR-MSG.                       SCASH20P
028300     MOVE CASH-USERID TO ATM-OP-USERID.                           SCASH20P
028400     MOVE CASH-PIN-STATUS TO ATM-OP-PIN-STATUS.                   SCASH20P
028500* Move in screen specific fields                                  SCASH20P
028600     MOVE CASH-ATM2-ACC1 TO ATM-OP20-ACC1.                        SCASH20P
028700     MOVE CASH-ATM2-DSC1 TO ATM-OP20-DSC1.                        SCASH20P
028800     MOVE CASH-ATM2-ACC2 TO ATM-OP20-ACC2.                        SCASH20P
028900     MOVE CASH-ATM2-DSC2 TO ATM-OP20-DSC2.                        SCASH20P
029000     MOVE CASH-ATM2-ACC3 TO ATM-OP20-ACC3.                        SCASH20P
029100     MOVE CASH-ATM2-DSC3 TO ATM-OP20-DSC3.                        SCASH20P
029200     MOVE CASH-ATM2-ACC4 TO ATM-OP20-ACC4.                        SCASH20P
029300     MOVE CASH-ATM2-DSC4 TO ATM-OP20-DSC4.                        SCASH20P
029400     MOVE CASH-ATM2-ACC5 TO ATM-OP20-ACC5.                        SCASH20P
029500     MOVE CASH-ATM2-DSC5 TO ATM-OP20-DSC5.                        SCASH20P
029600                                                                  SCASH20P
029700 SEND-OUTPUT-EXIT.                                                SCASH20P
029800     EXIT.                                                        SCASH20P
029900                                                                  SCASH20P
030000* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SCASH20P
