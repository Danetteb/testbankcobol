000100***************************************************************** DCASH02P
000200*                                                               * DCASH02P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DCASH02P
000400*   This demonstration program is provided for use by users     * DCASH02P
000500*   of Micro Focus products and may be used, modified and       * DCASH02P
000600*   distributed as part of your application provided that       * DCASH02P
000700*   you properly acknowledge the copyright of Micro Focus       * DCASH02P
000800*   in this material.                                           * DCASH02P
000900*                                                               * DCASH02P
001000***************************************************************** DCASH02P
001100                                                                  DCASH02P
001200***************************************************************** DCASH02P
001300* Program:     DCASH02P.CBL                                     * DCASH02P
001400* Function:    Obtain ATM enabled account details               * DCASH02P
001500*              VSAM version                                     * DCASH02P
001600***************************************************************** DCASH02P
001700                                                                  DCASH02P
001800 IDENTIFICATION DIVISION.                                         DCASH02P
001900 PROGRAM-ID.                                                      DCASH02P
002000     DCASH02P.                                                    DCASH02P
002100 DATE-WRITTEN.                                                    DCASH02P
002200     September 2002.                                              DCASH02P
002300 DATE-COMPILED.                                                   DCASH02P
002400     Today.                                                       DCASH02P
002500                                                                  DCASH02P
002600 ENVIRONMENT DIVISION.                                            DCASH02P
002700                                                                  DCASH02P
002800 DATA DIVISION.                                                   DCASH02P
002900                                                                  DCASH02P
003000 WORKING-STORAGE SECTION.                                         DCASH02P
003100 01  WS-MISC-STORAGE.                                             DCASH02P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DCASH02P
003300       VALUE 'DCASH02P'.                                          DCASH02P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DCASH02P
003500   05  WS-RESP                               PIC S9(8) COMP.      DCASH02P
003600   05  WS-SUB1                               PIC S9(4) COMP.      DCASH02P
003700   05  WS-BNKACCT-AIX1-RID                   PIC X(5).            DCASH02P
003800   05  WS-BNKATYP-RID                        PIC X(1).            DCASH02P
003900   05  WS-BNKTXN-AIX1-RID                    PIC X(31).           DCASH02P
004000   05  WS-BNKTXN-AIX1-RID-LEN                PIC X(31).           DCASH02P
004100                                                                  DCASH02P
004200 01  WS-BNKACCT-REC.                                              DCASH02P
004300 COPY CBANKVAC.                                                   DCASH02P
004400                                                                  DCASH02P
004500 01  WS-BNKATYP-REC.                                              DCASH02P
004600 COPY CBANKVAT.                                                   DCASH02P
004700                                                                  DCASH02P
004800 01  WS-COMMAREA.                                                 DCASH02P
004900 COPY CCASHD02.                                                   DCASH02P
005000                                                                  DCASH02P
005100 COPY CABENDD.                                                    DCASH02P
005200                                                                  DCASH02P
005300 LINKAGE SECTION.                                                 DCASH02P
005400 01  DFHCOMMAREA.                                                 DCASH02P
005500   05  LK-COMMAREA                           PIC X(1)             DCASH02P
005600       OCCURS 1 TO 4096 TIMES                                     DCASH02P
005700         DEPENDING ON WS-COMMAREA-LENGTH.                         DCASH02P
005800                                                                  DCASH02P
005900 COPY CENTRY.                                                     DCASH02P
006000***************************************************************** DCASH02P
006100* Move the passed data to our area                              * DCASH02P
006200***************************************************************** DCASH02P
006300     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DCASH02P
006400     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DCASH02P
006500                                                                  DCASH02P
006600***************************************************************** DCASH02P
006700* Initialize our output area                                    * DCASH02P
006800***************************************************************** DCASH02P
006900     MOVE SPACES TO CD02O-DATA.                                   DCASH02P
007000                                                                  DCASH02P
007100***************************************************************** DCASH02P
007200* Set up the start position for the browse                      * DCASH02P
007300***************************************************************** DCASH02P
007400     MOVE CD02I-CONTACT-ID TO WS-BNKACCT-AIX1-RID.                DCASH02P
007500                                                                  DCASH02P
007600***************************************************************** DCASH02P
007700* Start browsing the file                                       * DCASH02P
007800***************************************************************** DCASH02P
007900     EXEC CICS STARTBR FILE('BNKACC1')                            DCASH02P
008000                       RIDFLD(WS-BNKACCT-AIX1-RID)                DCASH02P
008100                       GTEQ                                       DCASH02P
008200     END-EXEC.                                                    DCASH02P
008300                                                                  DCASH02P
008400***************************************************************** DCASH02P
008500* Now browse the selected recs and move up to 5 into our area   * DCASH02P
008600***************************************************************** DCASH02P
008700     MOVE 0 TO WS-SUB1.                                           DCASH02P
008800 ACCOUNT-FETCH-LOOP.                                              DCASH02P
008900     ADD 1 TO WS-SUB1.                                            DCASH02P
009000     IF WS-SUB1 IS GREATER THAN 5                                 DCASH02P
009100        GO TO ACCOUNT-FETCH-LOOP-EXIT                             DCASH02P
009200     END-IF.                                                      DCASH02P
009300     EXEC CICS READNEXT FILE('BNKACC1')                           DCASH02P
009400                        INTO(WS-BNKACCT-REC)                      DCASH02P
009500                        LENGTH(LENGTH OF WS-BNKACCT-REC)          DCASH02P
009600                        RIDFLD(WS-BNKACCT-AIX1-RID)               DCASH02P
009700                        RESP(WS-RESP)                             DCASH02P
009800     END-EXEC.                                                    DCASH02P
009900     IF (WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL) AND              DCASH02P
010000         WS-RESP IS NOT EQUAL TO DFHRESP(DUPKEY)) OR              DCASH02P
010100        BAC-REC-PID IS NOT EQUAL TO CD02I-CONTACT-ID OR           DCASH02P
010200        BAC-REC-ATM-ENABLED IS NOT EQUAL TO 'Y'                   DCASH02P
010300        GO TO ACCOUNT-FETCH-LOOP-EXIT                             DCASH02P
010400     END-IF.                                                      DCASH02P
010500                                                                  DCASH02P
010600***************************************************************** DCASH02P
010700* We got an account record ok, save no & bal, get description   * DCASH02P
010800***************************************************************** DCASH02P
010900     MOVE BAC-REC-ACCNO TO CD02O-ACC-NO (WS-SUB1).                DCASH02P
011000     MOVE BAC-REC-TYPE TO WS-BNKATYP-RID.                         DCASH02P
011100     EXEC CICS READ FILE('BNKATYPE')                              DCASH02P
011200                        INTO(WS-BNKATYP-REC)                      DCASH02P
011300                        LENGTH(LENGTH OF WS-BNKATYP-REC)          DCASH02P
011400                        RIDFLD(WS-BNKATYP-RID)                    DCASH02P
011500                        RESP(WS-RESP)                             DCASH02P
011600     END-EXEC.                                                    DCASH02P
011700     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DCASH02P
011800        MOVE BAT-REC-DESC TO CD02O-ACC-DESC (WS-SUB1)             DCASH02P
011900     ELSE                                                         DCASH02P
012000        MOVE 'Unkown' TO CD02O-ACC-DESC (WS-SUB1)                 DCASH02P
012100     END-IF.                                                      DCASH02P
012200     GO TO ACCOUNT-FETCH-LOOP.                                    DCASH02P
012300                                                                  DCASH02P
012400***************************************************************** DCASH02P
012500* We quit the loop for some reason                              * DCASH02P
012600***************************************************************** DCASH02P
012700 ACCOUNT-FETCH-LOOP-EXIT.                                         DCASH02P
012800     EXEC CICS ENDBR FILE('BNKACC1')                              DCASH02P
012900     END-EXEC.                                                    DCASH02P
013000                                                                  DCASH02P
013100***************************************************************** DCASH02P
013200* Move the result back to the callers area                      * DCASH02P
013300***************************************************************** DCASH02P
013400     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DCASH02P
013500                                                                  DCASH02P
013600***************************************************************** DCASH02P
013700* Return to our caller                                          * DCASH02P
013800***************************************************************** DCASH02P
013900 COPY CRETURN.                                                    DCASH02P
014000                                                                  DCASH02P
014100* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DCASH02P
