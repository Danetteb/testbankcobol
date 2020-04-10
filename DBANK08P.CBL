000100***************************************************************** DBANK08P
000200*                                                               * DBANK08P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK08P
000400*   This demonstration program is provided for use by users     * DBANK08P
000500*   of Micro Focus products and may be used, modified and       * DBANK08P
000600*   distributed as part of your application provided that       * DBANK08P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK08P
000800*   in this material.                                           * DBANK08P
000900*                                                               * DBANK08P
001000***************************************************************** DBANK08P
001100                                                                  DBANK08P
001200***************************************************************** DBANK08P
001300* Program:     DBANK08P.CBL                                     * DBANK08P
001400* Function:    Obtain count of number of accounts user has      * DBANK08P
001500*              VSAM version                                     * DBANK08P
001600***************************************************************** DBANK08P
001700                                                                  DBANK08P
001800 IDENTIFICATION DIVISION.                                         DBANK08P
001900 PROGRAM-ID.                                                      DBANK08P
002000     DBANK08P.                                                    DBANK08P
002100 DATE-WRITTEN.                                                    DBANK08P
002200     September 2002.                                              DBANK08P
002300 DATE-COMPILED.                                                   DBANK08P
002400     Today.                                                       DBANK08P
002500                                                                  DBANK08P
002600 ENVIRONMENT DIVISION.                                            DBANK08P
002700                                                                  DBANK08P
002800 DATA DIVISION.                                                   DBANK08P
002900                                                                  DBANK08P
003000 WORKING-STORAGE SECTION.                                         DBANK08P
003100 01  WS-MISC-STORAGE.                                             DBANK08P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK08P
003300       VALUE 'DBANK08P'.                                          DBANK08P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK08P
003500   05  WS-RESP                               PIC S9(8) COMP.      DBANK08P
003600   05  WS-SUB1                               PIC S9(4) COMP.      DBANK08P
003700   05  WS-BNKACCT-AIX1-RID                   PIC X(5).            DBANK08P
003800                                                                  DBANK08P
003900 01  WS-BNKACCT-REC.                                              DBANK08P
004000 COPY CBANKVAC.                                                   DBANK08P
004100                                                                  DBANK08P
004200 01  WS-COMMAREA.                                                 DBANK08P
004300 COPY CBANKD08.                                                   DBANK08P
004400                                                                  DBANK08P
004500 COPY CABENDD.                                                    DBANK08P
004600                                                                  DBANK08P
004700 LINKAGE SECTION.                                                 DBANK08P
004800 01  DFHCOMMAREA.                                                 DBANK08P
004900   05  LK-COMMAREA                           PIC X(1)             DBANK08P
005000       OCCURS 1 TO 4096 TIMES                                     DBANK08P
005100         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK08P
005200                                                                  DBANK08P
005300 COPY CENTRY.                                                     DBANK08P
005400***************************************************************** DBANK08P
005500* Move the passed data to our area                              * DBANK08P
005600***************************************************************** DBANK08P
005700     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK08P
005800     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK08P
005900                                                                  DBANK08P
006000***************************************************************** DBANK08P
006100* Initialize our output area                                    * DBANK08P
006200***************************************************************** DBANK08P
006300     MOVE SPACES TO CD08O-DATA.                                   DBANK08P
006400                                                                  DBANK08P
006500***************************************************************** DBANK08P
006600* Set up the start position for the browse                      * DBANK08P
006700***************************************************************** DBANK08P
006800     MOVE CD08I-CONTACT-ID TO WS-BNKACCT-AIX1-RID.                DBANK08P
006900                                                                  DBANK08P
007000***************************************************************** DBANK08P
007100* Start browsing the file                                       * DBANK08P
007200***************************************************************** DBANK08P
007300     EXEC CICS STARTBR FILE('BNKACC1')                            DBANK08P
007400                       RIDFLD(WS-BNKACCT-AIX1-RID)                DBANK08P
007500                       GTEQ                                       DBANK08P
007600     END-EXEC.                                                    DBANK08P
007700                                                                  DBANK08P
007800***************************************************************** DBANK08P
007900* Now browse the selected recs and move up to 5 into our area   * DBANK08P
008000***************************************************************** DBANK08P
008100     MOVE 0 TO CD08O-COUNT.                                       DBANK08P
008200 ACCOUNT-FETCH-LOOP.                                              DBANK08P
008300     EXEC CICS READNEXT FILE('BNKACC1')                           DBANK08P
008400                        INTO(WS-BNKACCT-REC)                      DBANK08P
008500                        LENGTH(LENGTH OF WS-BNKACCT-REC)          DBANK08P
008600                        RIDFLD(WS-BNKACCT-AIX1-RID)               DBANK08P
008700                        RESP(WS-RESP)                             DBANK08P
008800     END-EXEC.                                                    DBANK08P
008900     IF (WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL) AND              DBANK08P
009000         WS-RESP IS NOT EQUAL TO DFHRESP(DUPKEY)) OR              DBANK08P
009100        BAC-REC-PID IS NOT EQUAL TO CD08I-CONTACT-ID              DBANK08P
009200        GO TO ACCOUNT-FETCH-LOOP-EXIT                             DBANK08P
009300     ELSE                                                         DBANK08P
009400        ADD 1 TO CD08O-COUNT                                      DBANK08P
009500        GO TO ACCOUNT-FETCH-LOOP                                  DBANK08P
009600     END-IF.                                                      DBANK08P
009700                                                                  DBANK08P
009800***************************************************************** DBANK08P
009900* We quit the loop for some reason                              * DBANK08P
010000***************************************************************** DBANK08P
010100 ACCOUNT-FETCH-LOOP-EXIT.                                         DBANK08P
010200     EXEC CICS ENDBR FILE('BNKACC1')                              DBANK08P
010300     END-EXEC.                                                    DBANK08P
010400                                                                  DBANK08P
010500***************************************************************** DBANK08P
010600* Move the result back to the callers area                      * DBANK08P
010700***************************************************************** DBANK08P
010800     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK08P
010900                                                                  DBANK08P
011000***************************************************************** DBANK08P
011100* Return to our caller                                          * DBANK08P
011200***************************************************************** DBANK08P
011300 COPY CRETURN.                                                    DBANK08P
011400                                                                  DBANK08P
011500* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK08P
