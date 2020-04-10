000100***************************************************************** DBANK06P
000200*                                                               * DBANK06P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK06P
000400*   This demonstration program is provided for use by users     * DBANK06P
000500*   of Micro Focus products and may be used, modified and       * DBANK06P
000600*   distributed as part of your application provided that       * DBANK06P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK06P
000800*   in this material.                                           * DBANK06P
000900*                                                               * DBANK06P
001000***************************************************************** DBANK06P
001100                                                                  DBANK06P
001200***************************************************************** DBANK06P
001300* Program:     DBANK06P.CBL                                     * DBANK06P
001400* Function:    Write transaction records for audit trail        * DBANK06P
001500*              VSAM Version                                     * DBANK06P
001600***************************************************************** DBANK06P
001700                                                                  DBANK06P
001800 IDENTIFICATION DIVISION.                                         DBANK06P
001900 PROGRAM-ID.                                                      DBANK06P
002000     DBANK06P.                                                    DBANK06P
002100 DATE-WRITTEN.                                                    DBANK06P
002200     September 2002.                                              DBANK06P
002300 DATE-COMPILED.                                                   DBANK06P
002400     Today.                                                       DBANK06P
002500                                                                  DBANK06P
002600 ENVIRONMENT DIVISION.                                            DBANK06P
002700                                                                  DBANK06P
002800 DATA DIVISION.                                                   DBANK06P
002900                                                                  DBANK06P
003000 WORKING-STORAGE SECTION.                                         DBANK06P
003100 01  WS-MISC-STORAGE.                                             DBANK06P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK06P
003300       VALUE 'DBANK06P'.                                          DBANK06P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK06P
003500   05  WS-RESP                               PIC S9(8) COMP.      DBANK06P
003600   05  WS-BNKTXN-RID                         PIC X(26).           DBANK06P
003700                                                                  DBANK06P
003800 01  WS-BNKTXN-REC.                                               DBANK06P
003900 COPY CBANKVTX.                                                   DBANK06P
004000                                                                  DBANK06P
004100 01  WS-TWOS-COMP.                                                DBANK06P
004200   05  WS-TWOS-COMP-REQ                      PIC X(1).            DBANK06P
004300     88  WS-TWOS-COMP-REQ-YES                VALUE 'Y'.           DBANK06P
004400     88  WS-TWOS-COMP-REQ-NO                 VALUE 'N'.           DBANK06P
004500   05  WS-TWOS-COMP-LEN                      PIC S9(4) COMP.      DBANK06P
004600   05  WS-TWOS-COMP-INPUT                    PIC X(256).          DBANK06P
004700   05  WS-TWOS-COMP-OUTPUT                   PIC X(256).          DBANK06P
004800                                                                  DBANK06P
004900 01  WS-COMMAREA.                                                 DBANK06P
005000 COPY CBANKD06.                                                   DBANK06P
005100                                                                  DBANK06P
005200 COPY CBANKTXD.                                                   DBANK06P
005300                                                                  DBANK06P
005400 COPY CABENDD.                                                    DBANK06P
005500                                                                  DBANK06P
005600 LINKAGE SECTION.                                                 DBANK06P
005700 01  DFHCOMMAREA.                                                 DBANK06P
005800   05  LK-COMMAREA                           PIC X(1)             DBANK06P
005900       OCCURS 1 TO 4096 TIMES                                     DBANK06P
006000         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK06P
006100                                                                  DBANK06P
006200 COPY CENTRY.                                                     DBANK06P
006300***************************************************************** DBANK06P
006400* Move the passed data to our area                              * DBANK06P
006500***************************************************************** DBANK06P
006600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK06P
006700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK06P
006800                                                                  DBANK06P
006900***************************************************************** DBANK06P
007000* Initialize our output area                                    * DBANK06P
007100***************************************************************** DBANK06P
007200     MOVE SPACES TO CD06O-DATA.                                   DBANK06P
007300                                                                  DBANK06P
007400***************************************************************** DBANK06P
007500* Insert two rows/records into the database/file                * DBANK06P
007600***************************************************************** DBANK06P
007700***************************************************************** DBANK06P
007800* First row/record is for the from-to transaction               * DBANK06P
007900***************************************************************** DBANK06P
008000     MOVE '0' TO CD06I-TIMESTAMP (26:1).                          DBANK06P
008100     MOVE CD06I-FROM-PID TO BTX-REC-PID.                          DBANK06P
008200     MOVE '1' TO BTX-REC-TYPE.                                    DBANK06P
008300     MOVE '1' TO BTX-REC-SUB-TYPE.                                DBANK06P
008400     MOVE CD06I-FROM-ACC TO BTX-REC-ACCNO.                        DBANK06P
008500     MOVE CD06I-TIMESTAMP TO BTX-REC-TIMESTAMP.                   DBANK06P
008600     MOVE CD06I-FROM-AMOUNT TO BTX-REC-AMOUNT.                    DBANK06P
008700     MOVE SPACES TO TXN-DATA.                                     DBANK06P
008800     MOVE CD06I-FROM-DESC TO TXN-T1-OLD-DESC.                     DBANK06P
008900     MOVE TXN-DATA-OLD TO BTX-REC-DATA-OLD.                       DBANK06P
009000     MOVE TXN-DATA-NEW TO BTX-REC-DATA-NEW.                       DBANK06P
009100     MOVE CD06I-TIMESTAMP TO WS-TWOS-COMP-INPUT.                  DBANK06P
009200     MOVE LOW-VALUES TO WS-TWOS-COMP-OUTPUT.                      DBANK06P
009300     MOVE LENGTH OF CD06I-TIMESTAMP TO WS-TWOS-COMP-LEN.          DBANK06P
009400     CALL 'UTWOSCMP' USING WS-TWOS-COMP-LEN                       DBANK06P
009500                           WS-TWOS-COMP-INPUT                     DBANK06P
009600                           WS-TWOS-COMP-OUTPUT.                   DBANK06P
009700     MOVE WS-TWOS-COMP-OUTPUT TO BTX-REC-TIMESTAMP-FF.            DBANK06P
009800     MOVE CD06I-TIMESTAMP TO WS-BNKTXN-RID.                       DBANK06P
009900     EXEC CICS WRITE FILE('BNKTXN')                               DBANK06P
010000                          FROM(WS-BNKTXN-REC)                     DBANK06P
010100                          LENGTH(LENGTH OF WS-BNKTXN-REC)         DBANK06P
010200                          RIDFLD(WS-BNKTXN-RID)                   DBANK06P
010300                          KEYLENGTH(LENGTH OF WS-BNKTXN-RID)      DBANK06P
010400                          RESP(WS-RESP)                           DBANK06P
010500     END-EXEC.                                                    DBANK06P
010600                                                                  DBANK06P
010700***************************************************************** DBANK06P
010800* Did we get the record OK                                      * DBANK06P
010900***************************************************************** DBANK06P
011000     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK06P
011100        SET CD06O-UPDATE-FAIL TO TRUE                             DBANK06P
011200        MOVE 'Unable to write FROM transaction record'            DBANK06P
011300          TO CD06O-MSG                                            DBANK06P
011400     END-IF.                                                      DBANK06P
011500                                                                  DBANK06P
011600***************************************************************** DBANK06P
011700* Second row/record is for the to-from transaction              * DBANK06P
011800***************************************************************** DBANK06P
011900     MOVE '1' TO CD06I-TIMESTAMP (26:1).                          DBANK06P
012000     MOVE CD06I-TO-PID TO BTX-REC-PID.                            DBANK06P
012100     MOVE '1' TO BTX-REC-TYPE.                                    DBANK06P
012200     MOVE '2' TO BTX-REC-SUB-TYPE.                                DBANK06P
012300     MOVE CD06I-TO-ACC TO BTX-REC-ACCNO.                          DBANK06P
012400     MOVE CD06I-TIMESTAMP TO BTX-REC-TIMESTAMP.                   DBANK06P
012500     MOVE CD06I-TO-AMOUNT TO BTX-REC-AMOUNT.                      DBANK06P
012600     MOVE SPACES TO TXN-DATA.                                     DBANK06P
012700     MOVE CD06I-TO-DESC TO TXN-T1-OLD-DESC.                       DBANK06P
012800     MOVE TXN-DATA-OLD TO BTX-REC-DATA-OLD.                       DBANK06P
012900     MOVE TXN-DATA-NEW TO BTX-REC-DATA-NEW.                       DBANK06P
013000     MOVE CD06I-TIMESTAMP TO WS-TWOS-COMP-INPUT.                  DBANK06P
013100     MOVE LOW-VALUES TO WS-TWOS-COMP-OUTPUT.                      DBANK06P
013200     MOVE LENGTH OF CD06I-TIMESTAMP TO WS-TWOS-COMP-LEN.          DBANK06P
013300     CALL 'UTWOSCMP' USING WS-TWOS-COMP-LEN                       DBANK06P
013400                           WS-TWOS-COMP-INPUT                     DBANK06P
013500                           WS-TWOS-COMP-OUTPUT.                   DBANK06P
013600     MOVE WS-TWOS-COMP-OUTPUT TO BTX-REC-TIMESTAMP-FF.            DBANK06P
013700     MOVE CD06I-TIMESTAMP TO WS-BNKTXN-RID.                       DBANK06P
013800     EXEC CICS WRITE FILE('BNKTXN')                               DBANK06P
013900                          FROM(WS-BNKTXN-REC)                     DBANK06P
014000                          LENGTH(LENGTH OF WS-BNKTXN-REC)         DBANK06P
014100                          RIDFLD(WS-BNKTXN-RID)                   DBANK06P
014200                          KEYLENGTH(LENGTH OF WS-BNKTXN-RID)      DBANK06P
014300                          RESP(WS-RESP)                           DBANK06P
014400     END-EXEC.                                                    DBANK06P
014500                                                                  DBANK06P
014600***************************************************************** DBANK06P
014700* Did we get the record OK                                      * DBANK06P
014800***************************************************************** DBANK06P
014900     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK06P
015000        SET CD06O-UPDATE-FAIL TO TRUE                             DBANK06P
015100        MOVE 'Unable to write TO transaction record'              DBANK06P
015200          TO CD06O-MSG                                            DBANK06P
015300     END-IF.                                                      DBANK06P
015400                                                                  DBANK06P
015500                                                                  DBANK06P
015600***************************************************************** DBANK06P
015700* Move the result back to the callers area                      * DBANK06P
015800***************************************************************** DBANK06P
015900     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK06P
016000                                                                  DBANK06P
016100***************************************************************** DBANK06P
016200* Return to our caller                                          * DBANK06P
016300***************************************************************** DBANK06P
016400 COPY CRETURN.                                                    DBANK06P
016500                                                                  DBANK06P
016600* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK06P
