000100***************************************************************** DBANK51P
000200*                                                               * DBANK51P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK51P
000400*   This demonstration program is provided for use by users     * DBANK51P
000500*   of Micro Focus products and may be used, modified and       * DBANK51P
000600*   distributed as part of your application provided that       * DBANK51P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK51P
000800*   in this material.                                           * DBANK51P
000900*                                                               * DBANK51P
001000***************************************************************** DBANK51P
001100                                                                  DBANK51P
001200***************************************************************** DBANK51P
001300* Program:     DBANK51P.CBL                                     * DBANK51P
001400* Function:    Sequential read of bank data for batch job       * DBANK51P
001500*              VSAM version                                     * DBANK51P
001600***************************************************************** DBANK51P
001700                                                                  DBANK51P
001800 IDENTIFICATION DIVISION.                                         DBANK51P
001900 PROGRAM-ID.                                                      DBANK51P
002000     DBANK51P.                                                    DBANK51P
002100 DATE-WRITTEN.                                                    DBANK51P
002200     September 2002.                                              DBANK51P
002300 DATE-COMPILED.                                                   DBANK51P
002400     Today.                                                       DBANK51P
002500                                                                  DBANK51P
002600 ENVIRONMENT DIVISION.                                            DBANK51P
002700                                                                  DBANK51P
002800 INPUT-OUTPUT   SECTION.                                          DBANK51P
002900   FILE-CONTROL.                                                  DBANK51P
003000     SELECT BNKACC-FILE                                           DBANK51P
003100            ASSIGN       TO BNKACC                                DBANK51P
003200            ORGANIZATION IS INDEXED                               DBANK51P
003300            ACCESS MODE  IS SEQUENTIAL                            DBANK51P
003400            RECORD KEY   IS BAC-REC-ACCNO                         DBANK51P
003500            ALTERNATE KEY IS BAC-REC-PID WITH DUPLICATES          DBANK51P
003600            FILE STATUS  IS WS-BNKACC-STATUS.                     DBANK51P
003700                                                                  DBANK51P
003800     SELECT BNKCUST-FILE                                          DBANK51P
003900            ASSIGN       TO BNKCUST                               DBANK51P
004000            ORGANIZATION IS INDEXED                               DBANK51P
004100            ACCESS MODE  IS RANDOM                                DBANK51P
004200            RECORD KEY   IS BCS-REC-PID                           DBANK51P
004300            ALTERNATE KEY IS BCS-REC-NAME                         DBANK51P
004400              WITH DUPLICATES                                     DBANK51P
004500            ALTERNATE KEY IS BCS-REC-NAME-FF                      DBANK51P
004600              WITH DUPLICATES                                     DBANK51P
004700            FILE STATUS  IS WS-BNKCUST-STATUS.                    DBANK51P
004800                                                                  DBANK51P
004900     SELECT BNKATYP-FILE                                          DBANK51P
005000            ASSIGN       TO BNKATYP                               DBANK51P
005100            ORGANIZATION IS INDEXED                               DBANK51P
005200            ACCESS MODE  IS RANDOM                                DBANK51P
005300            RECORD KEY   IS BAT-REC-TYPE                          DBANK51P
005400            FILE STATUS  IS WS-BNKATYP-STATUS.                    DBANK51P
005500                                                                  DBANK51P
005600 DATA DIVISION.                                                   DBANK51P
005700                                                                  DBANK51P
005800 FILE SECTION.                                                    DBANK51P
005900 FD  BNKACC-FILE.                                                 DBANK51P
006000 01  BNKACC-REC.                                                  DBANK51P
006100 COPY CBANKVAC.                                                   DBANK51P
006200                                                                  DBANK51P
006300 FD  BNKCUST-FILE.                                                DBANK51P
006400 01  BNKCUST-REC.                                                 DBANK51P
006500 COPY CBANKVCS.                                                   DBANK51P
006600                                                                  DBANK51P
006700 FD  BNKATYP-FILE.                                                DBANK51P
006800 01  BNKATYP-REC.                                                 DBANK51P
006900 COPY CBANKVAT.                                                   DBANK51P
007000                                                                  DBANK51P
007100 WORKING-STORAGE SECTION.                                         DBANK51P
007200 01  WS-MISC-STORAGE.                                             DBANK51P
007300   05  WS-PROGRAM-ID                         PIC X(8)             DBANK51P
007400       VALUE 'DBANK51P'.                                          DBANK51P
007500   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK51P
007600   05  WS-SUB1                               PIC S9(4) COMP.      DBANK51P
007700                                                                  DBANK51P
007800   05  WS-BNKACC-STATUS.                                          DBANK51P
007900     10  WS-BNKACC-STAT1                     PIC X(1).            DBANK51P
008000     10  WS-BNKACC-STAT2                     PIC X(1).            DBANK51P
008100                                                                  DBANK51P
008200   05  WS-BNKCUST-STATUS.                                         DBANK51P
008300     10  WS-BNKCUST-STAT1                    PIC X(1).            DBANK51P
008400     10  WS-BNKCUST-STAT2                    PIC X(1).            DBANK51P
008500                                                                  DBANK51P
008600   05  WS-BNKATYP-STATUS.                                         DBANK51P
008700     10  WS-BNKATYP-STAT1                    PIC X(1).            DBANK51P
008800     10  WS-BNKATYP-STAT2                    PIC X(1).            DBANK51P
008900                                                                  DBANK51P
009000 01  WS-COMMAREA.                                                 DBANK51P
009100 COPY CIOFUNCS.                                                   DBANK51P
009200 COPY CBANKD51.                                                   DBANK51P
009300                                                                  DBANK51P
009400 LINKAGE SECTION.                                                 DBANK51P
009500 01  DFHCOMMAREA.                                                 DBANK51P
009600   05  LK-COMMAREA                           PIC X(1)             DBANK51P
009700         OCCURS 1 TO 4096 TIMES                                   DBANK51P
009800           DEPENDING ON WS-COMMAREA-LENGTH.                       DBANK51P
009900                                                                  DBANK51P
010000 PROCEDURE DIVISION USING DFHCOMMAREA.                            DBANK51P
010100***************************************************************** DBANK51P
010200* Move the passed data to our area                              * DBANK51P
010300***************************************************************** DBANK51P
010400     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK51P
010500     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK51P
010600                                                                  DBANK51P
010700***************************************************************** DBANK51P
010800* Initialize our output area                                    * DBANK51P
010900***************************************************************** DBANK51P
011000     MOVE SPACES TO CD51O-DATA.                                   DBANK51P
011100                                                                  DBANK51P
011200***************************************************************** DBANK51P
011300* Check what is required                                        * DBANK51P
011400***************************************************************** DBANK51P
011500     EVALUATE TRUE                                                DBANK51P
011600       WHEN IO-REQUEST-FUNCTION-OPEN                              DBANK51P
011700        PERFORM OPEN-FILE THRU                                    DBANK51P
011800                OPEN-FILE-EXIT                                    DBANK51P
011900       WHEN IO-REQUEST-FUNCTION-READ                              DBANK51P
012000        PERFORM READ-FILE THRU                                    DBANK51P
012100                READ-FILE-EXIT                                    DBANK51P
012200       WHEN IO-REQUEST-FUNCTION-CLOSE                             DBANK51P
012300        PERFORM CLOSE-FILE THRU                                   DBANK51P
012400                CLOSE-FILE-EXIT                                   DBANK51P
012500       WHEN OTHER                                                 DBANK51P
012600        SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK51P
012700     END-EVALUATE.                                                DBANK51P
012800                                                                  DBANK51P
012900***************************************************************** DBANK51P
013000* Move the result back to the callers area                      * DBANK51P
013100***************************************************************** DBANK51P
013200     MOVE WS-COMMAREA TO DFHCOMMAREA  (1:WS-COMMAREA-LENGTH).     DBANK51P
013300                                                                  DBANK51P
013400***************************************************************** DBANK51P
013500* Return to our caller                                          * DBANK51P
013600***************************************************************** DBANK51P
013700     GOBACK.                                                      DBANK51P
013800                                                                  DBANK51P
013900                                                                  DBANK51P
014000***************************************************************** DBANK51P
014100* Open the file so we can read ACC sequentially, others randomly* DBANK51P
014200***************************************************************** DBANK51P
014300 OPEN-FILE.                                                       DBANK51P
014400     OPEN INPUT BNKACC-FILE.                                      DBANK51P
014500     OPEN INPUT BNKCUST-FILE.                                     DBANK51P
014600     OPEN INPUT BNKATYP-FILE.                                     DBANK51P
014700     IF CD51-REQUESTED-ALL                                        DBANK51P
014800        MOVE LOW-VALUES TO BAC-REC-PID                            DBANK51P
014900        START BNKACC-FILE KEY GREATER THAN BAC-REC-PID            DBANK51P
015000     ELSE                                                         DBANK51P
015100        MOVE CD51I-PID TO BAC-REC-PID                             DBANK51P
015200        START BNKACC-FILE KEY EQUAL BAC-REC-PID                   DBANK51P
015300     END-IF                                                       DBANK51P
015400     IF WS-BNKACC-STATUS = '00' AND                               DBANK51P
015500        WS-BNKCUST-STATUS = '00' AND                              DBANK51P
015600        WS-BNKATYP-STATUS = '00'                                  DBANK51P
015700        SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK51P
015800     ELSE                                                         DBANK51P
015900        SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK51P
016000     END-IF.                                                      DBANK51P
016100 OPEN-FILE-EXIT.                                                  DBANK51P
016200     EXIT.                                                        DBANK51P
016300                                                                  DBANK51P
016400***************************************************************** DBANK51P
016500* Read sequentially through the customer file                   * DBANK51P
016600***************************************************************** DBANK51P
016700 READ-FILE.                                                       DBANK51P
016800     READ BNKACC-FILE.                                            DBANK51P
016900* If key is greater than the one we want, fake end-of-file        DBANK51P
017000     IF NOT CD51-REQUESTED-ALL AND                                DBANK51P
017100        BAC-REC-PID IS NOT EQUAL TO CD51I-PID                     DBANK51P
017200        MOVE '10' TO WS-BNKACC-STATUS                             DBANK51P
017300     END-IF.                                                      DBANK51P
017400* Was read ok?                                                    DBANK51P
017500     IF WS-BNKACC-STATUS IS EQUAL TO '00'                         DBANK51P
017600        SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK51P
017700     END-IF.                                                      DBANK51P
017800* Was read a duplicate key?                                       DBANK51P
017900     IF WS-BNKACC-STATUS IS EQUAL TO '02'                         DBANK51P
018000        MOVE '00' TO WS-BNKACC-STATUS                             DBANK51P
018100        SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK51P
018200     END-IF.                                                      DBANK51P
018300* Was read at end-of-file?                                        DBANK51P
018400     IF WS-BNKACC-STATUS IS EQUAL TO '10'                         DBANK51P
018500        SET IO-REQUEST-STATUS-EOF TO TRUE                         DBANK51P
018600     END-IF.                                                      DBANK51P
018700     IF WS-BNKACC-STATUS IS NOT EQUAL TO '00' AND                 DBANK51P
018800        WS-BNKACC-STATUS IS NOT EQUAL TO '10'                     DBANK51P
018900        SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK51P
019000     END-IF.                                                      DBANK51P
019100     IF WS-BNKACC-STATUS IS EQUAL TO '00'                         DBANK51P
019200        MOVE BAC-REC-PID TO CD51O-PID                             DBANK51P
019300        MOVE BAC-REC-ACCNO TO CD51O-ACC-NO                        DBANK51P
019400        MOVE BAC-REC-BALANCE TO CD51O-ACC-CURR-BAL                DBANK51P
019500        MOVE BAC-REC-LAST-STMT-DTE TO CD51O-ACC-LAST-STMT-DTE     DBANK51P
019600        MOVE BAC-REC-LAST-STMT-BAL TO CD51O-ACC-LAST-STMT-BAL     DBANK51P
019700        IF BAC-REC-PID IS NOT EQUAL TO BCS-REC-PID                DBANK51P
019800           MOVE BAC-REC-PID TO BCS-REC-PID                        DBANK51P
019900           READ BNKCUST-FILE                                      DBANK51P
020000           IF WS-BNKCUST-STATUS IS NOT EQUAL TO '00'              DBANK51P
020100              MOVE SPACES TO BCS-RECORD                           DBANK51P
020200              MOVE 'Customer name unavailable' TO BCS-REC-NAME    DBANK51P
020300           END-IF                                                 DBANK51P
020400        END-IF                                                    DBANK51P
020500                                                                  DBANK51P
020600        MOVE BCS-REC-NAME TO CD51O-NAME                           DBANK51P
020700        MOVE BCS-REC-ADDR1 TO CD51O-ADDR1                         DBANK51P
020800        MOVE BCS-REC-ADDR2 TO CD51O-ADDR2                         DBANK51P
020900        MOVE BCS-REC-STATE TO CD51O-STATE                         DBANK51P
021000        MOVE BCS-REC-CNTRY TO CD51O-CNTRY                         DBANK51P
021100        MOVE BCS-REC-POST-CODE TO CD51O-POST-CODE                 DBANK51P
021200        MOVE BCS-REC-EMAIL TO CD51O-EMAIL                         DBANK51P
021300                                                                  DBANK51P
021400        MOVE BAC-REC-TYPE TO BAT-REC-TYPE                         DBANK51P
021500        READ BNKATYP-FILE                                         DBANK51P
021600        IF WS-BNKATYP-STATUS IS NOT EQUAL TO '00'                 DBANK51P
021700           MOVE 'A/C description unavailable' TO CD51O-ACC-DESC   DBANK51P
021800        ELSE                                                      DBANK51P
021900           MOVE BAT-REC-DESC TO CD51O-ACC-DESC                    DBANK51P
022000        END-IF                                                    DBANK51P
022100     END-IF.                                                      DBANK51P
022200 READ-FILE-EXIT.                                                  DBANK51P
022300     EXIT.                                                        DBANK51P
022400                                                                  DBANK51P
022500***************************************************************** DBANK51P
022600* Close the file                                                * DBANK51P
022700***************************************************************** DBANK51P
022800 CLOSE-FILE.                                                      DBANK51P
022900     CLOSE BNKCUST-FILE.                                          DBANK51P
023000     CLOSE BNKACC-FILE.                                           DBANK51P
023100     CLOSE BNKATYP-FILE.                                          DBANK51P
023200     IF WS-BNKCUST-STATUS = '00' AND                              DBANK51P
023300        WS-BNKACC-STATUS = '00' AND                               DBANK51P
023400        WS-BNKATYP-STATUS = '00'                                  DBANK51P
023500        SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK51P
023600     ELSE                                                         DBANK51P
023700       SET IO-REQUEST-STATUS-ERROR TO TRUE                        DBANK51P
023800     END-IF.                                                      DBANK51P
023900 CLOSE-FILE-EXIT.                                                 DBANK51P
024000     EXIT.                                                        DBANK51P
024100                                                                  DBANK51P
024200* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK51P
