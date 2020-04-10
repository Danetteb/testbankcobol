000100***************************************************************** DBANK01P
000200*                                                               * DBANK01P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK01P
000400*   This demonstration program is provided for use by users     * DBANK01P
000500*   of Micro Focus products and may be used, modified and       * DBANK01P
000600*   distributed as part of your application provided that       * DBANK01P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK01P
000800*   in this material.                                           * DBANK01P
000900*                                                               * DBANK01P
001000***************************************************************** DBANK01P
001100                                                                  DBANK01P
001200***************************************************************** DBANK01P
001300* Program:     DBANK01P.CBL                                     * DBANK01P
001400* Function:    Obtain User details                              * DBANK01P
001500*              VSAM version                                     * DBANK01P
001600***************************************************************** DBANK01P
001700                                                                  DBANK01P
001800 IDENTIFICATION DIVISION.                                         DBANK01P
001900 PROGRAM-ID.                                                      DBANK01P
002000     DBANK01P.                                                    DBANK01P
002100 DATE-WRITTEN.                                                    DBANK01P
002200     September 2002.                                              DBANK01P
002300 DATE-COMPILED.                                                   DBANK01P
002400     Today.                                                       DBANK01P
002500                                                                  DBANK01P
002600 ENVIRONMENT DIVISION.                                            DBANK01P
002700                                                                  DBANK01P
002800 DATA DIVISION.                                                   DBANK01P
002900                                                                  DBANK01P
003000 WORKING-STORAGE SECTION.                                         DBANK01P
003100 01  WS-MISC-STORAGE.                                             DBANK01P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK01P
003300       VALUE 'DBANK01P'.                                          DBANK01P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK01P
003500   05  WS-RESP                               PIC S9(8) COMP.      DBANK01P
003600   05  WS-BNKCUST-RID                        PIC X(5).            DBANK01P
003700                                                                  DBANK01P
003800 01 WS-BNKCUST-REC.                                               DBANK01P
003900 COPY CBANKVCS.                                                   DBANK01P
004000                                                                  DBANK01P
004100 01  WS-COMMAREA.                                                 DBANK01P
004200 COPY CBANKD01.                                                   DBANK01P
004300                                                                  DBANK01P
004400 COPY CABENDD.                                                    DBANK01P
004500                                                                  DBANK01P
004600 LINKAGE SECTION.                                                 DBANK01P
004700 01  DFHCOMMAREA.                                                 DBANK01P
004800   05  LK-COMMAREA                           PIC X(1)             DBANK01P
004900       OCCURS 1 TO 4096 TIMES                                     DBANK01P
005000         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK01P
005100                                                                  DBANK01P
005200 COPY CENTRY.                                                     DBANK01P
005300***************************************************************** DBANK01P
005400* Move the passed data to our area                              * DBANK01P
005500***************************************************************** DBANK01P
005600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK01P
005700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK01P
005800                                                                  DBANK01P
005900***************************************************************** DBANK01P
006000* Initialize our output area                                    * DBANK01P
006100***************************************************************** DBANK01P
006200     MOVE SPACES TO CD01O-DATA.                                   DBANK01P
006300                                                                  DBANK01P
006400***************************************************************** DBANK01P
006500* Now attempt to get the requested record                       * DBANK01P
006600***************************************************************** DBANK01P
006700     MOVE CD01I-PERSON-PID TO WS-BNKCUST-RID.                     DBANK01P
006800     EXEC CICS READ FILE('BNKCUST')                               DBANK01P
006900                    INTO(WS-BNKCUST-REC)                          DBANK01P
007000                    LENGTH(LENGTH OF WS-BNKCUST-REC)              DBANK01P
007100                    RIDFLD(WS-BNKCUST-RID)                        DBANK01P
007200                    RESP(WS-RESP)                                 DBANK01P
007300     END-EXEC.                                                    DBANK01P
007400                                                                  DBANK01P
007500***************************************************************** DBANK01P
007600* Did we get the record OK                                      * DBANK01P
007700***************************************************************** DBANK01P
007800     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DBANK01P
007900        MOVE BCS-REC-PID TO CD01O-PERSON-PID                      DBANK01P
008000        MOVE BCS-REC-NAME TO CD01O-PERSON-NAME                    DBANK01P
008100     END-IF.                                                      DBANK01P
008200                                                                  DBANK01P
008300***************************************************************** DBANK01P
008400* Was the record not found?                                     * DBANK01P
008500***************************************************************** DBANK01P
008600     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK01P
008700        MOVE 'Person not found' TO CD01O-PERSON-NAME              DBANK01P
008800     END-IF.                                                      DBANK01P
008900                                                                  DBANK01P
009000***************************************************************** DBANK01P
009100* Move the result back to the callers area                      * DBANK01P
009200***************************************************************** DBANK01P
009300     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK01P
009400                                                                  DBANK01P
009500***************************************************************** DBANK01P
009600* Return to our caller                                          * DBANK01P
009700***************************************************************** DBANK01P
009800 COPY CRETURN.                                                    DBANK01P
009900                                                                  DBANK01P
010000* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK01P
