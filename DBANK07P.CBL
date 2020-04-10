000100***************************************************************** DBANK07P
000200*                                                               * DBANK07P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK07P
000400*   This demonstration program is provided for use by users     * DBANK07P
000500*   of Micro Focus products and may be used, modified and       * DBANK07P
000600*   distributed as part of your application provided that       * DBANK07P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK07P
000800*   in this material.                                           * DBANK07P
000900*                                                               * DBANK07P
001000***************************************************************** DBANK07P
001100                                                                  DBANK07P
001200***************************************************************** DBANK07P
001300* Program:     DBANK07P.CBL                                     * DBANK07P
001400* Function:    Write transaction records for audit trail        * DBANK07P
001500*              VSAM Version                                     * DBANK07P
001600***************************************************************** DBANK07P
001700                                                                  DBANK07P
001800 IDENTIFICATION DIVISION.                                         DBANK07P
001900 PROGRAM-ID.                                                      DBANK07P
002000     DBANK07P.                                                    DBANK07P
002100 DATE-WRITTEN.                                                    DBANK07P
002200     September 2002.                                              DBANK07P
002300 DATE-COMPILED.                                                   DBANK07P
002400     Today.                                                       DBANK07P
002500                                                                  DBANK07P
002600 ENVIRONMENT DIVISION.                                            DBANK07P
002700                                                                  DBANK07P
002800 DATA DIVISION.                                                   DBANK07P
002900                                                                  DBANK07P
003000 WORKING-STORAGE SECTION.                                         DBANK07P
003100 01  WS-MISC-STORAGE.                                             DBANK07P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK07P
003300       VALUE 'DBANK07P'.                                          DBANK07P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK07P
003500   05  WS-RESP                               PIC S9(8) COMP.      DBANK07P
003600   05  WS-BNKTXN-RID                         PIC X(26).           DBANK07P
003700                                                                  DBANK07P
003800 01  WS-BNKTXN-REC.                                               DBANK07P
003900 COPY CBANKVTX.                                                   DBANK07P
004000                                                                  DBANK07P
004100 COPY CBANKTXD.                                                   DBANK07P
004200                                                                  DBANK07P
004300 01  WS-TWOS-COMP.                                                DBANK07P
004400   05  WS-TWOS-COMP-REQ                      PIC X(1).            DBANK07P
004500     88  WS-TWOS-COMP-REQ-YES                VALUE 'Y'.           DBANK07P
004600     88  WS-TWOS-COMP-REQ-NO                 VALUE 'N'.           DBANK07P
004700   05  WS-TWOS-COMP-LEN                      PIC S9(4) COMP.      DBANK07P
004800   05  WS-TWOS-COMP-INPUT                    PIC X(256).          DBANK07P
004900   05  WS-TWOS-COMP-OUTPUT                   PIC X(256).          DBANK07P
005000                                                                  DBANK07P
005100 01  WS-COMMAREA.                                                 DBANK07P
005200 COPY CBANKD07.                                                   DBANK07P
005300                                                                  DBANK07P
005400 COPY CTSTAMPD.                                                   DBANK07P
005500                                                                  DBANK07P
005600 COPY CABENDD.                                                    DBANK07P
005700                                                                  DBANK07P
005800 LINKAGE SECTION.                                                 DBANK07P
005900 01  DFHCOMMAREA.                                                 DBANK07P
006000   05  LK-COMMAREA                           PIC X(1)             DBANK07P
006100       OCCURS 1 TO 4096 TIMES                                     DBANK07P
006200         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK07P
006300                                                                  DBANK07P
006400 COPY CENTRY.                                                     DBANK07P
006500***************************************************************** DBANK07P
006600* Move the passed data to our area                              * DBANK07P
006700***************************************************************** DBANK07P
006800     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK07P
006900     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK07P
007000                                                                  DBANK07P
007100***************************************************************** DBANK07P
007200* Initialize our output area                                    * DBANK07P
007300***************************************************************** DBANK07P
007400     MOVE SPACES TO CD07O-DATA.                                   DBANK07P
007500                                                                  DBANK07P
007600***************************************************************** DBANK07P
007700* Insert row/record into the database/file                      * DBANK07P
007800***************************************************************** DBANK07P
007900 COPY CTSTAMPP.                                                   DBANK07P
008000     MOVE WS-TIMESTAMP TO CD07I-TIMESTAMP.                        DBANK07P
008100     MOVE SPACES TO BTX-RECORD.                                   DBANK07P
008200     MOVE '0' TO CD07I-TIMESTAMP (26:1).                          DBANK07P
008300     MOVE CD07I-PERSON-PID TO BTX-REC-PID.                        DBANK07P
008400     MOVE '2' TO BTX-REC-TYPE.                                    DBANK07P
008500     MOVE ' ' TO BTX-REC-SUB-TYPE.                                DBANK07P
008600     MOVE SPACES TO BTX-REC-ACCNO.                                DBANK07P
008700     MOVE CD07I-TIMESTAMP TO BTX-REC-TIMESTAMP.                   DBANK07P
008800     MOVE ZERO TO BTX-REC-AMOUNT.                                 DBANK07P
008900     MOVE CD07I-OLD-DATA TO BTX-REC-DATA-OLD.                     DBANK07P
009000     MOVE CD07I-NEW-DATA TO BTX-REC-DATA-NEW.                     DBANK07P
009100     MOVE CD07I-TIMESTAMP TO WS-TWOS-COMP-INPUT.                  DBANK07P
009200     MOVE LOW-VALUES TO WS-TWOS-COMP-OUTPUT.                      DBANK07P
009300     MOVE LENGTH OF CD07I-TIMESTAMP TO WS-TWOS-COMP-LEN.          DBANK07P
009400     CALL 'UTWOSCMP' USING WS-TWOS-COMP-LEN                       DBANK07P
009500                           WS-TWOS-COMP-INPUT                     DBANK07P
009600                           WS-TWOS-COMP-OUTPUT.                   DBANK07P
009700     MOVE WS-TWOS-COMP-OUTPUT TO BTX-REC-TIMESTAMP-FF.            DBANK07P
009800     MOVE CD07I-TIMESTAMP TO WS-BNKTXN-RID.                       DBANK07P
009900     EXEC CICS WRITE FILE('BNKTXN')                               DBANK07P
010000                          FROM(WS-BNKTXN-REC)                     DBANK07P
010100                          LENGTH(LENGTH OF WS-BNKTXN-REC)         DBANK07P
010200                          RIDFLD(WS-BNKTXN-RID)                   DBANK07P
010300                          KEYLENGTH(LENGTH OF WS-BNKTXN-RID)      DBANK07P
010400                          RESP(WS-RESP)                           DBANK07P
010500     END-EXEC.                                                    DBANK07P
010600                                                                  DBANK07P
010700***************************************************************** DBANK07P
010800* Did we get the record OK                                      * DBANK07P
010900***************************************************************** DBANK07P
011000     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK07P
011100        SET CD07O-UPDATE-FAIL TO TRUE                             DBANK07P
011200        MOVE 'Unable to insert contact info audit record'         DBANK07P
011300          TO CD07O-MSG                                            DBANK07P
011400     END-IF.                                                      DBANK07P
011500                                                                  DBANK07P
011600***************************************************************** DBANK07P
011700* Move the result back to the callers area                      * DBANK07P
011800***************************************************************** DBANK07P
011900     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK07P
012000                                                                  DBANK07P
012100***************************************************************** DBANK07P
012200* Return to our caller                                          * DBANK07P
012300***************************************************************** DBANK07P
012400 COPY CRETURN.                                                    DBANK07P
012500                                                                  DBANK07P
012600* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK07P
