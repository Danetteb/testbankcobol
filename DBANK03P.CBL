000100***************************************************************** DBANK03P
000200*                                                               * DBANK03P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK03P
000400*   This demonstration program is provided for use by users     * DBANK03P
000500*   of Micro Focus products and may be used, modified and       * DBANK03P
000600*   distributed as part of your application provided that       * DBANK03P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK03P
000800*   in this material.                                           * DBANK03P
000900*                                                               * DBANK03P
001000***************************************************************** DBANK03P
001100                                                                  DBANK03P
001200***************************************************************** DBANK03P
001300* Program:     DBANK03P.CBL                                     * DBANK03P
001400* Function:    Obtain Bank Account balances                     * DBANK03P
001500*              VSAM version                                     * DBANK03P
001600***************************************************************** DBANK03P
001700                                                                  DBANK03P
001800 IDENTIFICATION DIVISION.                                         DBANK03P
001900 PROGRAM-ID.                                                      DBANK03P
002000     DBANK03P.                                                    DBANK03P
002100 DATE-WRITTEN.                                                    DBANK03P
002200     September 2002.                                              DBANK03P
002300 DATE-COMPILED.                                                   DBANK03P
002400     Today.                                                       DBANK03P
002500                                                                  DBANK03P
002600 ENVIRONMENT DIVISION.                                            DBANK03P
002700                                                                  DBANK03P
002800 DATA DIVISION.                                                   DBANK03P
002900                                                                  DBANK03P
003000 WORKING-STORAGE SECTION.                                         DBANK03P
003100 01  WS-MISC-STORAGE.                                             DBANK03P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK03P
003300       VALUE 'DBANK03P'.                                          DBANK03P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK03P
003500   05  WS-RESP                               PIC S9(8) COMP.      DBANK03P
003600   05  WS-SUB1                               PIC S9(4) COMP.      DBANK03P
003700   05  WS-SUB2                               PIC S9(4) COMP.      DBANK03P
003800   05  WS-COUNT                              PIC S9(4) COMP.      DBANK03P
003900   05  WS-REC-TYPE                           PIC X(1).            DBANK03P
004000   05  WS-REC-TYPE-N REDEFINES WS-REC-TYPE   PIC 9(1).            DBANK03P
004100   05  WS-MOVED-FLAG                         PIC X(1).            DBANK03P
004200     88  ENTRY-MOVED-TRUE                    VALUE '1'.           DBANK03P
004300     88  ENTRY-MOVED-FALSE                   VALUE '0'.           DBANK03P
004400   05  WS-BNKACCT-AIX1-RID                   PIC X(5).            DBANK03P
004500   05  WS-BNKATYP-RID                        PIC X(1).            DBANK03P
004600   05  WS-BNKTXN-AIX1-RID                    PIC X(31).           DBANK03P
004700   05  WS-BNKTXN-AIX1-RID-LEN                PIC X(31).           DBANK03P
004800                                                                  DBANK03P
004900 01  WS-BNKACCT-REC.                                              DBANK03P
005000 COPY CBANKVAC.                                                   DBANK03P
005100                                                                  DBANK03P
005200 01  WS-BNKATYP-REC.                                              DBANK03P
005300 COPY CBANKVAT.                                                   DBANK03P
005400                                                                  DBANK03P
005500 01  WS-BNKTXN-REC.                                               DBANK03P
005600 COPY CBANKVTX.                                                   DBANK03P
005700                                                                  DBANK03P
005800 01  WS-COMMAREA.                                                 DBANK03P
005900 COPY CBANKD03.                                                   DBANK03P
006000                                                                  DBANK03P
006100 COPY CABENDD.                                                    DBANK03P
006200                                                                  DBANK03P
006300 LINKAGE SECTION.                                                 DBANK03P
006400 01  DFHCOMMAREA.                                                 DBANK03P
006500   05  LK-COMMAREA                           PIC X(1)             DBANK03P
006600       OCCURS 1 TO 4096 TIMES                                     DBANK03P
006700         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK03P
006800                                                                  DBANK03P
006900 COPY CENTRY.                                                     DBANK03P
007000***************************************************************** DBANK03P
007100* Move the passed data to our area                              * DBANK03P
007200***************************************************************** DBANK03P
007300     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK03P
007400     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK03P
007500                                                                  DBANK03P
007600***************************************************************** DBANK03P
007700* Initialize our output area                                    * DBANK03P
007800***************************************************************** DBANK03P
007900     MOVE SPACES TO CD03O-DATA.                                   DBANK03P
008000                                                                  DBANK03P
008100***************************************************************** DBANK03P
008200* Set up the start position for the browse                      * DBANK03P
008300***************************************************************** DBANK03P
008400     MOVE CD03I-CONTACT-ID TO WS-BNKACCT-AIX1-RID.                DBANK03P
008500                                                                  DBANK03P
008600***************************************************************** DBANK03P
008700* Start browsing the file                                       * DBANK03P
008800***************************************************************** DBANK03P
008900     EXEC CICS STARTBR FILE('BNKACC1')                            DBANK03P
009000                       RIDFLD(WS-BNKACCT-AIX1-RID)                DBANK03P
009100                       GTEQ                                       DBANK03P
009200     END-EXEC.                                                    DBANK03P
009300                                                                  DBANK03P
009400***************************************************************** DBANK03P
009500* Now browse the selected recs and move into our area           * DBANK03P
009600***************************************************************** DBANK03P
009700     DIVIDE LENGTH OF CD03O-ACC-INFO(1) INTO LENGTH OF CD03O-DATA DBANK03P
009800       GIVING WS-COUNT.                                           DBANK03P
009900     MOVE 0 TO WS-SUB1.                                           DBANK03P
010000 ACCOUNT-FETCH-LOOP.                                              DBANK03P
010100     ADD 1 TO WS-SUB1.                                            DBANK03P
010200     IF WS-SUB1 IS GREATER THAN WS-COUNT                          DBANK03P
010300        GO TO ACCOUNT-FETCH-LOOP-EXIT                             DBANK03P
010400     END-IF.                                                      DBANK03P
010500     EXEC CICS READNEXT FILE('BNKACC1')                           DBANK03P
010600                        INTO(WS-BNKACCT-REC)                      DBANK03P
010700                        LENGTH(LENGTH OF WS-BNKACCT-REC)          DBANK03P
010800                        RIDFLD(WS-BNKACCT-AIX1-RID)               DBANK03P
010900                        RESP(WS-RESP)                             DBANK03P
011000     END-EXEC.                                                    DBANK03P
011100     IF (WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL) AND              DBANK03P
011200         WS-RESP IS NOT EQUAL TO DFHRESP(DUPKEY)) OR              DBANK03P
011300        BAC-REC-PID IS NOT EQUAL TO CD03I-CONTACT-ID              DBANK03P
011400        GO TO ACCOUNT-FETCH-LOOP-EXIT                             DBANK03P
011500     END-IF                                                       DBANK03P
011600                                                                  DBANK03P
011700***************************************************************** DBANK03P
011800* We got an account record ok, save no & bal, get description   * DBANK03P
011900* Note: We wnat the account in type order but as the files do   * DBANK03P
012000*       not accomodate this, we put them in the "entry" in the  * DBANK03P
012100*       output area based on type. Once all records have been   * DBANK03P
012200*       obtained we examine the output area to move entries up  * DBANK03P
012300*       if necessary to eliminate any embedded blank entries.   * DBANK03P
012400***************************************************************** DBANK03P
012500     MOVE BAC-REC-TYPE TO WS-REC-TYPE.                            DBANK03P
012600     MOVE WS-REC-TYPE-N TO WS-SUB2.                               DBANK03P
012700     MOVE BAC-REC-ACCNO TO CD03O-ACC-NO (WS-SUB2).                DBANK03P
012800     MOVE BAC-REC-BALANCE TO CD03O-ACC-BAL-N (WS-SUB2).           DBANK03P
012900     MOVE BAC-REC-LAST-STMT-DTE TO CD03O-DTE (WS-SUB2).           DBANK03P
013000     MOVE BAC-REC-TYPE TO WS-BNKATYP-RID.                         DBANK03P
013100     EXEC CICS READ FILE('BNKATYPE')                              DBANK03P
013200                        INTO(WS-BNKATYP-REC)                      DBANK03P
013300                        LENGTH(LENGTH OF WS-BNKATYP-REC)          DBANK03P
013400                        RIDFLD(WS-BNKATYP-RID)                    DBANK03P
013500                        RESP(WS-RESP)                             DBANK03P
013600     END-EXEC.                                                    DBANK03P
013700     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DBANK03P
013800        MOVE BAT-REC-DESC TO CD03O-ACC-DESC (WS-SUB2)             DBANK03P
013900     ELSE                                                         DBANK03P
014000        MOVE 'Unkown' TO CD03O-ACC-DESC (WS-SUB2)                 DBANK03P
014100     END-IF.                                                      DBANK03P
014200                                                                  DBANK03P
014300***************************************************************** DBANK03P
014400* Now see if there are any transactions for this account        * DBANK03P
014500***************************************************************** DBANK03P
014600     MOVE BAC-REC-ACCNO TO WS-BNKTXN-AIX1-RID.                    DBANK03P
014700     EXEC CICS READ FILE('BNKTXN1')                               DBANK03P
014800                        INTO(WS-BNKTXN-REC)                       DBANK03P
014900                        LENGTH(LENGTH OF WS-BNKTXN-REC)           DBANK03P
015000                        RIDFLD(WS-BNKTXN-AIX1-RID)                DBANK03P
015100                        KEYLENGTH(9)                              DBANK03P
015200                        GENERIC                                   DBANK03P
015300                        RESP(WS-RESP)                             DBANK03P
015400     END-EXEC.                                                    DBANK03P
015500     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DBANK03P
015600        MOVE '*' TO CD03O-TXN (WS-SUB2)                           DBANK03P
015700     ELSE                                                         DBANK03P
015800        MOVE ' ' TO CD03O-TXN (WS-SUB2)                           DBANK03P
015900     END-IF.                                                      DBANK03P
016000     GO TO ACCOUNT-FETCH-LOOP.                                    DBANK03P
016100                                                                  DBANK03P
016200***************************************************************** DBANK03P
016300* We quit the loop for some reason                              * DBANK03P
016400***************************************************************** DBANK03P
016500 ACCOUNT-FETCH-LOOP-EXIT.                                         DBANK03P
016600     EXEC CICS ENDBR FILE('BNKACC1')                              DBANK03P
016700     END-EXEC.                                                    DBANK03P
016800                                                                  DBANK03P
016900***************************************************************** DBANK03P
017000* We examine the output area an move entries as required so as  * DBANK03P
017100* to eliminate any embedded blank entries                       * DBANK03P
017200***************************************************************** DBANK03P
017300 COMPRESS-000.                                                    DBANK03P
017400     MOVE 1 TO WS-SUB1.                                           DBANK03P
017500     MOVE 1 TO WS-SUB2.                                           DBANK03P
017600     SET ENTRY-MOVED-FALSE TO TRUE.                               DBANK03P
017700 COMPRESS-100.                                                    DBANK03P
017800* Check to see if we have done it all                             DBANK03P
017900     IF WS-SUB1 IS EQUAL WS-COUNT                                 DBANK03P
018000        GO TO COMPRESS-200                                        DBANK03P
018100     END-IF.                                                      DBANK03P
018200     ADD 1 TO WS-SUB2                                             DBANK03P
018300     IF CD03O-ACC-INFO(WS-SUB1) IS EQUAL TO SPACES AND            DBANK03P
018400        WS-SUB2 IS NOT GREATER THAN WS-COUNT                      DBANK03P
018500        IF CD03O-ACC-INFO(WS-SUB2) IS NOT EQUAL TO SPACES         DBANK03P
018600           MOVE CD03O-ACC-INFO(WS-SUB2) TO CD03O-ACC-INFO(WS-SUB1)DBANK03P
018700           MOVE SPACES TO CD03O-ACC-INFO(WS-SUB2)                 DBANK03P
018800           ADD 1 TO WS-SUB1                                       DBANK03P
018900           MOVE WS-SUB1 TO WS-SUB2                                DBANK03P
019000           SET ENTRY-MOVED-TRUE TO TRUE                           DBANK03P
019100           GO TO COMPRESS-000                                     DBANK03P
019200        END-IF                                                    DBANK03P
019300     ELSE                                                         DBANK03P
019400       ADD 1 TO WS-SUB1                                           DBANK03P
019500       GO TO COMPRESS-100                                         DBANK03P
019600     END-IF.                                                      DBANK03P
019700     GO TO COMPRESS-100.                                          DBANK03P
019800 COMPRESS-200.                                                    DBANK03P
019900     IF ENTRY-MOVED-TRUE                                          DBANK03P
020000        GO TO COMPRESS-000                                        DBANK03P
020100     END-IF.                                                      DBANK03P
020200                                                                  DBANK03P
020300***************************************************************** DBANK03P
020400* Move the result back to the callers area                      * DBANK03P
020500***************************************************************** DBANK03P
020600     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK03P
020700                                                                  DBANK03P
020800***************************************************************** DBANK03P
020900* Return to our caller                                          * DBANK03P
021000***************************************************************** DBANK03P
021100 COPY CRETURN.                                                    DBANK03P
021200                                                                  DBANK03P
021300* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK03P
