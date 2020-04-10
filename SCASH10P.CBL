000100***************************************************************** SCASH10P
000200*                                                               * SCASH10P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SCASH10P
000400*   This demonstration program is provided for use by users     * SCASH10P
000500*   of Micro Focus products and may be used, modified and       * SCASH10P
000600*   distributed as part of your application provided that       * SCASH10P
000700*   you properly acknowledge the copyright of Micro Focus       * SCASH10P
000800*   in this material.                                           * SCASH10P
000900*                                                               * SCASH10P
001000***************************************************************** SCASH10P
001100                                                                  SCASH10P
001200***************************************************************** SCASH10P
001300* Program:     SCASH10P.CBL (CICS Version)                      * SCASH10P
001400* Layer:       'Screen' handling                                * SCASH10P
001500* Function:    Validate PIN for Userid                          * SCASH10P
001600***************************************************************** SCASH10P
001700                                                                  SCASH10P
001800 IDENTIFICATION DIVISION.                                         SCASH10P
001900 PROGRAM-ID.                                                      SCASH10P
002000     SCASH10P.                                                    SCASH10P
002100 DATE-WRITTEN.                                                    SCASH10P
002200     September 2002.                                              SCASH10P
002300 DATE-COMPILED.                                                   SCASH10P
002400     Today.                                                       SCASH10P
002500                                                                  SCASH10P
002600 ENVIRONMENT DIVISION.                                            SCASH10P
002700                                                                  SCASH10P
002800 DATA DIVISION.                                                   SCASH10P
002900 WORKING-STORAGE SECTION.                                         SCASH10P
003000 01  WS-MISC-STORAGE.                                             SCASH10P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SCASH10P
003200       VALUE 'SCASH10P'.                                          SCASH10P
003300   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SCASH10P
003400   05  WS-TRAN-ID                            PIC X(4).            SCASH10P
003500   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SCASH10P
003600       VALUE SPACES.                                              SCASH10P
003700   05  WS-DYNAMIC-PGM                        PIC X(8)             SCASH10P
003800       VALUE 'UNKNOWN'.                                           SCASH10P
003900   05  WS-SUB1                               PIC S9(4) COMP.      SCASH10P
004000   05  WS-LEN1                               PIC S9(4) COMP.      SCASH10P
004100   05  WS-INPUT-SOURCE                       PIC X(1).            SCASH10P
004200     88  INPUT-UNKNOWN                       VALUE '0'.           SCASH10P
004300     88  INPUT-FROM-SCREEN                   VALUE '1'.           SCASH10P
004400     88  INPUT-FROM-ATM                      VALUE '2'.           SCASH10P
004500   05  WS-CALL-BUSINESS-LOGIC                PIC X(1).            SCASH10P
004600     88  CALL-BUSINESS-LOGIC-OK              VALUE '1'.           SCASH10P
004700     88  CALL-BUSINESS-LOGIC-NOT-OK          VALUE '2'.           SCASH10P
004800                                                                  SCASH10P
004900 01  WS-SCREEN-ITEMS.                                             SCASH10P
005000   05  WS-IP-SCREEN-ITEMS.                                        SCASH10P
005100     10  WS-IP-SCREEN-STATUS                 PIC X(1).            SCASH10P
005200       88  IP-SCREEN-STATUS-UNKNOWN          VALUE '0'.           SCASH10P
005300       88  IP-SCREEN-STATUS-OK               VALUE '1'.           SCASH10P
005400       88  IP-SCREEN-STATUS-NOT-OK           VALUE '2'.           SCASH10P
005500     10  WS-IP-SCREEN-DATA                   PIC X(512).          SCASH10P
005600     10  WS-IP-SCREEN-LENGTH                 PIC 9(4) BINARY.     SCASH10P
005700     10  WS-IP-MAX-SCREEN-LENGTH             PIC 9(4) BINARY.     SCASH10P
005800     10  WS-IP-SCREEN-TRAN                   PIC X(4).            SCASH10P
005900     10  WS-IP-SCREEN-USERID                 PIC X(5).            SCASH10P
006000     10  WS-IP-SCREEN-PIN                    PIC X(4).            SCASH10P
006100   05  WS-OP-SCREEN-ITEMS.                                        SCASH10P
006200     10  WS-OP-SCREEN-DATA                   PIC X(1920).         SCASH10P
006300     10  WS-OP-SCREEN-LENGTH                 PIC 9(4) BINARY.     SCASH10P
006400     10  WS-OP-MAX-SCREEN-LENGTH             PIC 9(4) BINARY.     SCASH10P
006500                                                                  SCASH10P
006600 01  WS-CASH-DATA-AREAS.                                          SCASH10P
006700   05  WS-CASH-DATA.                                              SCASH10P
006800 COPY CCASHDAT.                                                   SCASH10P
006900   05  WS-CASH-EXT-DATA.                                          SCASH10P
007000 COPY CCASHEXT.                                                   SCASH10P
007100                                                                  SCASH10P
007200 COPY CABENDD.                                                    SCASH10P
007300                                                                  SCASH10P
007400 LINKAGE SECTION.                                                 SCASH10P
007500 01  DFHCOMMAREA.                                                 SCASH10P
007600   05  FILLER                                PIC X(1024).         SCASH10P
007700                                                                  SCASH10P
007800 PROCEDURE DIVISION.                                              SCASH10P
007900***************************************************************** SCASH10P
008000* Write entry to log to show we have been invoked               * SCASH10P
008100***************************************************************** SCASH10P
008200*    COPY CTRACE.                                                 SCASH10P
008300                                                                  SCASH10P
008400***************************************************************** SCASH10P
008500* Store our transaction-id                                      * SCASH10P
008600***************************************************************** SCASH10P
008700     MOVE EIBTRNID TO WS-TRAN-ID.                                 SCASH10P
008800     MOVE EIBCALEN TO WS-SAVED-EIBCALEN.                          SCASH10P
008900                                                                  SCASH10P
009000     SET INPUT-UNKNOWN TO TRUE.                                   SCASH10P
009100     SET CALL-BUSINESS-LOGIC-OK TO TRUE.                          SCASH10P
009200                                                                  SCASH10P
009300     IF EIBCALEN IS EQUAL TO 0                                    SCASH10P
009400        SET INPUT-FROM-SCREEN TO TRUE                             SCASH10P
009500        SET IP-SCREEN-STATUS-UNKNOWN TO TRUE                      SCASH10P
009600        EXEC CICS RECEIVE                                         SCASH10P
009700                  INTO(WS-IP-SCREEN-DATA)                         SCASH10P
009800                  LENGTH(WS-IP-SCREEN-LENGTH)                     SCASH10P
009900                  MAXLENGTH(LENGTH OF WS-IP-SCREEN-DATA)          SCASH10P
010000        END-EXEC                                                  SCASH10P
010100      ELSE                                                        SCASH10P
010200        IF EIBCALEN IS LESS THAN 100                              SCASH10P
010300           MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                    SCASH10P
010400           MOVE '0001' TO ABEND-CODE                              SCASH10P
010500           MOVE 'EIBCALEN less than 100' TO ABEND-REASON          SCASH10P
010600           COPY CABENDPO.                                         SCASH10P
010700        ELSE                                                      SCASH10P
010800           MOVE SPACES TO WS-CASH-DATA                            SCASH10P
010900           SET INPUT-FROM-ATM TO TRUE                             SCASH10P
011000           MOVE DFHCOMMAREA(1:WS-SAVED-EIBCALEN)                  SCASH10P
011100             TO WS-CASH-EXT-DATA(1:WS-SAVED-EIBCALEN)             SCASH10P
011200        END-IF                                                    SCASH10P
011300     END-IF.                                                      SCASH10P
011400                                                                  SCASH10P
011500***************************************************************** SCASH10P
011600* This is the main process                                      * SCASH10P
011700***************************************************************** SCASH10P
011800                                                                  SCASH10P
011900***************************************************************** SCASH10P
012000* Determine what we have to do                                  * SCASH10P
012100***************************************************************** SCASH10P
012200* Obtain and format the input                                     SCASH10P
012300     PERFORM READ-INPUT THRU                                      SCASH10P
012400             READ-INPUT-EXIT.                                     SCASH10P
012500                                                                  SCASH10P
012600* Call the appropriate routine to handle the business logic       SCASH10P
012700     IF CALL-BUSINESS-LOGIC-OK                                    SCASH10P
012800        MOVE 'BCASH10P' TO WS-BUSINESS-LOGIC-PGM                  SCASH10P
012900        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SCASH10P
013000                       COMMAREA(WS-CASH-DATA)                     SCASH10P
013100                       LENGTH(LENGTH OF WS-CASH-DATA)             SCASH10P
013200        END-EXEC                                                  SCASH10P
013300     END-IF.                                                      SCASH10P
013400* Format and return the outpput                                   SCASH10P
013500     PERFORM SEND-OUTPUT THRU                                     SCASH10P
013600             SEND-OUTPUT-EXIT.                                    SCASH10P
013700                                                                  SCASH10P
013800***************************************************************** SCASH10P
013900* Now we have to have finished and can return to our invoker.   * SCASH10P
014000***************************************************************** SCASH10P
014100* Now return to CICS                                              SCASH10P
014200     IF WS-SAVED-EIBCALEN IS GREATER THAN 0                       SCASH10P
014300        MOVE WS-CASH-EXT-DATA(1:WS-SAVED-EIBCALEN)                SCASH10P
014400          TO DFHCOMMAREA(1:WS-SAVED-EIBCALEN)                     SCASH10P
014500     END-IF.                                                      SCASH10P
014600     EXEC CICS                                                    SCASH10P
014700          RETURN                                                  SCASH10P
014800     END-EXEC.                                                    SCASH10P
014900     GOBACK.                                                      SCASH10P
015000                                                                  SCASH10P
015100***************************************************************** SCASH10P
015200* Retrieve input data and format it                             * SCASH10P
015300***************************************************************** SCASH10P
015400 READ-INPUT.                                                      SCASH10P
015500     MOVE SPACES TO WS-CASH-DATA.                                 SCASH10P
015600     IF INPUT-FROM-SCREEN                                         SCASH10P
015700        GO TO READ-INPUT-SCREEN                                   SCASH10P
015800     END-IF.                                                      SCASH10P
015900     IF INPUT-FROM-ATM                                            SCASH10P
016000        GO TO READ-INPUT-ATM                                      SCASH10P
016100     END-IF.                                                      SCASH10P
016200                                                                  SCASH10P
016300     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT.                         SCASH10P
016400     MOVE '0002' TO ABEND-CODE.                                   SCASH10P
016500     MOVE 'Cannot determine input source' TO ABEND-REASON.        SCASH10P
016600     COPY CABENDPO. .                                             SCASH10P
016700                                                                  SCASH10P
016800 READ-INPUT-SCREEN.                                               SCASH10P
016900* Expect the format to be tttt^uuuuu^pppp where:                  SCASH10P
017000*   ^ is a space                                                  SCASH10P
017100*   tttt is the tranid                                            SCASH10P
017200*   uuuuu is the userid                                           SCASH10P
017300*   ppppp is the pin                                              SCASH10P
017400     MOVE SPACES TO WS-IP-SCREEN-TRAN.                            SCASH10P
017500     MOVE SPACES TO WS-IP-SCREEN-USERID.                          SCASH10P
017600     MOVE SPACES TO WS-IP-SCREEN-PIN.                             SCASH10P
017700     UNSTRING WS-IP-SCREEN-DATA                                   SCASH10P
017800        DELIMITED BY ALL SPACES INTO                              SCASH10P
017900          WS-IP-SCREEN-TRAN                                       SCASH10P
018000          WS-IP-SCREEN-USERID                                     SCASH10P
018100          WS-IP-SCREEN-PIN.                                       SCASH10P
018200     IF WS-IP-SCREEN-USERID IS EQUAL TO SPACES                    SCASH10P
018300        SET CALL-BUSINESS-LOGIC-NOT-OK TO TRUE                    SCASH10P
018400        MOVE 'Must send userid' TO CASH-ERROR-MSG                 SCASH10P
018500     ELSE                                                         SCASH10P
018600        MOVE WS-IP-SCREEN-USERID TO CASH-SIGNON-ID                SCASH10P
018700        MOVE WS-IP-SCREEN-USERID TO CASH-USERID                   SCASH10P
018800        MOVE WS-IP-SCREEN-PIN TO CASH-PIN                         SCASH10P
018900     END-IF.                                                      SCASH10P
019000     GO TO READ-INPUT-EXIT.                                       SCASH10P
019100                                                                  SCASH10P
019200 READ-INPUT-ATM.                                                  SCASH10P
019300     MOVE ATM-IP00-USERID TO CASH-SIGNON-ID.                      SCASH10P
019400     MOVE ATM-IP00-USERID TO CASH-USERID.                         SCASH10P
019500     MOVE ATM-IP00-PIN TO CASH-PIN.                               SCASH10P
019600     GO TO READ-INPUT-EXIT.                                       SCASH10P
019700                                                                  SCASH10P
019800 READ-INPUT-EXIT.                                                 SCASH10P
019900     EXIT.                                                        SCASH10P
020000                                                                  SCASH10P
020100***************************************************************** SCASH10P
020200* Build the output screen and send it                           * SCASH10P
020300***************************************************************** SCASH10P
020400 SEND-OUTPUT.                                                     SCASH10P
020500     IF INPUT-FROM-SCREEN                                         SCASH10P
020600        GO TO SEND-OUTPUT-TO-SCREEN                               SCASH10P
020700     END-IF.                                                      SCASH10P
020800     IF INPUT-FROM-ATM                                            SCASH10P
020900        GO TO SEND-OUTPUT-TO-ATM                                  SCASH10P
021000     END-IF.                                                      SCASH10P
021100                                                                  SCASH10P
021200     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT.                         SCASH10P
021300     MOVE '0003' TO ABEND-CODE.                                   SCASH10P
021400     MOVE 'Cannot determine output dest' TO ABEND-REASON.         SCASH10P
021500     COPY CABENDPO.                                               SCASH10P
021600     GOBACK.                                                      SCASH10P
021700                                                                  SCASH10P
021800 SEND-OUTPUT-TO-SCREEN.                                           SCASH10P
021900     MOVE 1 TO WS-SUB1.                                           SCASH10P
022000     MOVE LOW-VALUES TO WS-OP-SCREEN-DATA.                        SCASH10P
022100                                                                  SCASH10P
022200     MOVE 'Input:' TO WS-OP-SCREEN-DATA(WS-SUB1:6).               SCASH10P
022300     ADD 80 TO WS-SUB1.                                           SCASH10P
022400                                                                  SCASH10P
022500     MOVE WS-IP-SCREEN-DATA(1:WS-IP-SCREEN-LENGTH)                SCASH10P
022600       TO WS-OP-SCREEN-DATA(WS-SUB1:WS-IP-SCREEN-LENGTH).         SCASH10P
022700     COMPUTE WS-LEN1 = (((WS-IP-SCREEN-LENGTH + 79) / 80 ) * 80). SCASH10P
022800     ADD WS-LEN1 TO WS-SUB1.                                      SCASH10P
022900                                                                  SCASH10P
023000     ADD 80 TO WS-SUB1.                                           SCASH10P
023100                                                                  SCASH10P
023200     MOVE 'Output:' TO WS-OP-SCREEN-DATA(WS-SUB1:7).              SCASH10P
023300     ADD 80 TO WS-SUB1.                                           SCASH10P
023400                                                                  SCASH10P
023500     MOVE 'CASH-ERROR-MSG:'                                       SCASH10P
023600       TO WS-OP-SCREEN-DATA(WS-SUB1:15).                          SCASH10P
023700     ADD 80 TO WS-SUB1.                                           SCASH10P
023800                                                                  SCASH10P
023900     MOVE CASH-ERROR-MSG                                          SCASH10P
024000       TO WS-OP-SCREEN-DATA(WS-SUB1:LENGTH OF CASH-ERROR-MSG).    SCASH10P
024100     COMPUTE WS-LEN1 = (((lENGTH OF CASH-ERROR-MSG + 79)          SCASH10P
024200                         / 80 ) * 80).                            SCASH10P
024300     ADD WS-LEN1 TO WS-SUB1.                                      SCASH10P
024400                                                                  SCASH10P
024500     MOVE 'USER-DETAILS:' TO WS-OP-SCREEN-DATA(WS-SUB1:13).       SCASH10P
024600     ADD 80 TO WS-SUB1.                                           SCASH10P
024700                                                                  SCASH10P
024800     MOVE CASH-USER-DETAILS TO WS-OP-SCREEN-DATA(WS-SUB1:5).      SCASH10P
024900     COMPUTE WS-LEN1 = (((lENGTH OF CASH-USER-DETAILS + 79)       SCASH10P
025000                         / 80 ) * 80).                            SCASH10P
025100     ADD WS-LEN1 TO WS-SUB1.                                      SCASH10P
025200                                                                  SCASH10P
025300     MOVE 'CASH-ATM-DATA:' TO WS-OP-SCREEN-DATA(WS-SUB1:14).      SCASH10P
025400     ADD 80 TO WS-SUB1.                                           SCASH10P
025500                                                                  SCASH10P
025600     MOVE CASH-ATM-DATA                                           SCASH10P
025700       TO WS-OP-SCREEN-DATA(WS-SUB1:LENGTH OF CASH-ATM-DATA).     SCASH10P
025800     ADD LENGTH OF CASH-ATM-DATA TO WS-SUB1.                      SCASH10P
025900                                                                  SCASH10P
026000     EXEC CICS SEND                                               SCASH10P
026100                    FROM(WS-OP-SCREEN-DATA)                       SCASH10P
026200                    LENGTH(WS-SUB1)                               SCASH10P
026300                    ERASE                                         SCASH10P
026400     END-EXEC.                                                    SCASH10P
026500     GO TO SEND-OUTPUT-EXIT.                                      SCASH10P
026600                                                                  SCASH10P
026700 SEND-OUTPUT-TO-ATM.                                              SCASH10P
026800     MOVE SPACES TO ATM-OP-DATA.                                  SCASH10P
026900* Move in userid and any error message                            SCASH10P
027000     MOVE CASH-ERROR-MSG TO ATM-OP-ERR-MSG.                       SCASH10P
027100     MOVE CASH-USERID TO ATM-OP-USERID.                           SCASH10P
027200     MOVE CASH-PIN-STATUS TO ATM-OP-PIN-STATUS.                   SCASH10P
027300* Move in screen specific fields                                  SCASH10P
027400                                                                  SCASH10P
027500 SEND-OUTPUT-EXIT.                                                SCASH10P
027600     EXIT.                                                        SCASH10P
027700                                                                  SCASH10P
027800* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SCASH10P
