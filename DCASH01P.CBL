000100***************************************************************** DCASH01P
000200*                                                               * DCASH01P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DCASH01P
000400*   This demonstration program is provided for use by users     * DCASH01P
000500*   of Micro Focus products and may be used, modified and       * DCASH01P
000600*   distributed as part of your application provided that       * DCASH01P
000700*   you properly acknowledge the copyright of Micro Focus       * DCASH01P
000800*   in this material.                                           * DCASH01P
000900*                                                               * DCASH01P
001000***************************************************************** DCASH01P
001100                                                                  DCASH01P
001200***************************************************************** DCASH01P
001300* Program:     DCASH01P.CBL                                     * DCASH01P
001400* Function:    Obtain users PIN                                 * DCASH01P
001500*              VSAM version                                     * DCASH01P
001600***************************************************************** DCASH01P
001700                                                                  DCASH01P
001800 IDENTIFICATION DIVISION.                                         DCASH01P
001900 PROGRAM-ID.                                                      DCASH01P
002000     DCASH01P.                                                    DCASH01P
002100 DATE-WRITTEN.                                                    DCASH01P
002200     September 2002.                                              DCASH01P
002300 DATE-COMPILED.                                                   DCASH01P
002400     Today.                                                       DCASH01P
002500                                                                  DCASH01P
002600 ENVIRONMENT DIVISION.                                            DCASH01P
002700                                                                  DCASH01P
002800 DATA DIVISION.                                                   DCASH01P
002900                                                                  DCASH01P
003000 WORKING-STORAGE SECTION.                                         DCASH01P
003100 01  WS-MISC-STORAGE.                                             DCASH01P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DCASH01P
003300       VALUE 'DCASH01P'.                                          DCASH01P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DCASH01P
003500   05  WS-SUB1                               PIC S9(4) COMP.      DCASH01P
003600   05  WS-RESP                               PIC S9(8) COMP.      DCASH01P
003700   05  WS-BNKCUST-RID                        PIC X(5).            DCASH01P
003800                                                                  DCASH01P
003900 01 WS-BNKCUST-REC.                                               DCASH01P
004000 COPY CBANKVCS.                                                   DCASH01P
004100                                                                  DCASH01P
004200 01  WS-COMMAREA.                                                 DCASH01P
004300 COPY CCASHD01.                                                   DCASH01P
004400                                                                  DCASH01P
004500 COPY CABENDD.                                                    DCASH01P
004600                                                                  DCASH01P
004700 LINKAGE SECTION.                                                 DCASH01P
004800 01  DFHCOMMAREA.                                                 DCASH01P
004900   05  LK-COMMAREA                           PIC X(1)             DCASH01P
005000       OCCURS 1 TO 4096 TIMES                                     DCASH01P
005100         DEPENDING ON WS-COMMAREA-LENGTH.                         DCASH01P
005200                                                                  DCASH01P
005300 COPY CENTRY.                                                     DCASH01P
005400***************************************************************** DCASH01P
005500* Move the passed data to our area                              * DCASH01P
005600***************************************************************** DCASH01P
005700     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DCASH01P
005800     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DCASH01P
005900                                                                  DCASH01P
006000***************************************************************** DCASH01P
006100* Initialize our output area                                    * DCASH01P
006200***************************************************************** DCASH01P
006300     MOVE SPACES TO CD01o-DATA.                                   DCASH01P
006400                                                                  DCASH01P
006500***************************************************************** DCASH01P
006600* Now attempt to get the requested record                       * DCASH01P
006700***************************************************************** DCASH01P
006800     MOVE CD01I-CONTACT-ID TO WS-BNKCUST-RID.                     DCASH01P
006900     EXEC CICS READ FILE('BNKCUST')                               DCASH01P
007000                    INTO(WS-BNKCUST-REC)                          DCASH01P
007100                    LENGTH(LENGTH OF WS-BNKCUST-REC)              DCASH01P
007200                    RIDFLD(WS-BNKCUST-RID)                        DCASH01P
007300                    RESP(WS-RESP)                                 DCASH01P
007400     END-EXEC.                                                    DCASH01P
007500                                                                  DCASH01P
007600***************************************************************** DCASH01P
007700* Did we get the record OK                                      * DCASH01P
007800***************************************************************** DCASH01P
007900     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DCASH01P
008000        MOVE BCS-REC-ATM-PIN TO CD01O-PIN                         DCASH01P
008100     END-IF.                                                      DCASH01P
008200                                                                  DCASH01P
008300***************************************************************** DCASH01P
008400* Was the record not found?                                     * DCASH01P
008500***************************************************************** DCASH01P
008600     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DCASH01P
008700        MOVE '????' TO CD01O-PIN                                  DCASH01P
008800     END-IF.                                                      DCASH01P
008900                                                                  DCASH01P
009000                                                                  DCASH01P
009100***************************************************************** DCASH01P
009200* Move the result back to the callers area                      * DCASH01P
009300***************************************************************** DCASH01P
009400     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DCASH01P
009500                                                                  DCASH01P
009600***************************************************************** DCASH01P
009700* Return to our caller                                          * DCASH01P
009800***************************************************************** DCASH01P
009900 COPY CRETURN.                                                    DCASH01P
010000                                                                  DCASH01P
010100* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DCASH01P
