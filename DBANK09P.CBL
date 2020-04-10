000100***************************************************************** DBANK09P
000200*                                                               * DBANK09P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK09P
000400*   This demonstration program is provided for use by users     * DBANK09P
000500*   of Micro Focus products and may be used, modified and       * DBANK09P
000600*   distributed as part of your application provided that       * DBANK09P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK09P
000800*   in this material.                                           * DBANK09P
000900*                                                               * DBANK09P
001000***************************************************************** DBANK09P
001100                                                                  DBANK09P
001200***************************************************************** DBANK09P
001300* Program:     DBANK09P.CBL                                     * DBANK09P
001400* Function:    Obtain contact information for statements        * DBANK09P
001500*              VSAM version                                     * DBANK09P
001600***************************************************************** DBANK09P
001700                                                                  DBANK09P
001800 IDENTIFICATION DIVISION.                                         DBANK09P
001900 PROGRAM-ID.                                                      DBANK09P
002000     DBANK09P.                                                    DBANK09P
002100 DATE-WRITTEN.                                                    DBANK09P
002200     September 2002.                                              DBANK09P
002300 DATE-COMPILED.                                                   DBANK09P
002400     Today.                                                       DBANK09P
002500                                                                  DBANK09P
002600 ENVIRONMENT DIVISION.                                            DBANK09P
002700                                                                  DBANK09P
002800 DATA DIVISION.                                                   DBANK09P
002900                                                                  DBANK09P
003000 WORKING-STORAGE SECTION.                                         DBANK09P
003100 01  WS-MISC-STORAGE.                                             DBANK09P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK09P
003300       VALUE 'DBANK09P'.                                          DBANK09P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK09P
003500   05  WS-RESP                               PIC S9(8) COMP.      DBANK09P
003600   05  WS-BNKCUST-RID                        PIC X(5).            DBANK09P
003700                                                                  DBANK09P
003800 01 WS-BNKCUST-REC.                                               DBANK09P
003900 COPY CBANKVCS.                                                   DBANK09P
004000                                                                  DBANK09P
004100 01  WS-COMMAREA.                                                 DBANK09P
004200 COPY CBANKD09.                                                   DBANK09P
004300                                                                  DBANK09P
004400 COPY CABENDD.                                                    DBANK09P
004500                                                                  DBANK09P
004600 LINKAGE SECTION.                                                 DBANK09P
004700 01  DFHCOMMAREA.                                                 DBANK09P
004800   05  LK-COMMAREA                           PIC X(1)             DBANK09P
004900       OCCURS 1 TO 4096 TIMES                                     DBANK09P
005000         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK09P
005100                                                                  DBANK09P
005200 COPY CENTRY.                                                     DBANK09P
005300***************************************************************** DBANK09P
005400* Move the passed data to our area                              * DBANK09P
005500***************************************************************** DBANK09P
005600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK09P
005700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK09P
005800                                                                  DBANK09P
005900***************************************************************** DBANK09P
006000* Initialize our output area                                    * DBANK09P
006100***************************************************************** DBANK09P
006200     MOVE SPACES TO CD09O-DATA.                                   DBANK09P
006300                                                                  DBANK09P
006400***************************************************************** DBANK09P
006500* Now attempt to get the requested record                       * DBANK09P
006600***************************************************************** DBANK09P
006700     MOVE CD09I-CONTACT-ID TO WS-BNKCUST-RID.                     DBANK09P
006800     EXEC CICS READ FILE('BNKCUST')                               DBANK09P
006900                    INTO(WS-BNKCUST-REC)                          DBANK09P
007000                    LENGTH(LENGTH OF WS-BNKCUST-REC)              DBANK09P
007100                    RIDFLD(WS-BNKCUST-RID)                        DBANK09P
007200                    RESP(WS-RESP)                                 DBANK09P
007300     END-EXEC.                                                    DBANK09P
007400                                                                  DBANK09P
007500***************************************************************** DBANK09P
007600* Did we get the record OK                                      * DBANK09P
007700***************************************************************** DBANK09P
007800     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DBANK09P
007900        MOVE BCS-REC-PID TO CD09O-CONTACT-ID                      DBANK09P
008000        MOVE BCS-REC-NAME TO CD09O-CONTACT-NAME                   DBANK09P
008100        MOVE BCS-REC-ADDR1 TO CD09O-CONTACT-ADDR1                 DBANK09P
008200        MOVE BCS-REC-ADDR2 TO CD09O-CONTACT-ADDR2                 DBANK09P
008300        MOVE BCS-REC-STATE TO CD09O-CONTACT-STATE                 DBANK09P
008400        MOVE BCS-REC-CNTRY TO CD09O-CONTACT-CNTRY                 DBANK09P
008500        MOVE BCS-REC-POST-CODE TO CD09O-CONTACT-PSTCDE            DBANK09P
008600        MOVE BCS-REC-EMAIL TO CD09O-CONTACT-EMAIL                 DBANK09P
008700     END-IF.                                                      DBANK09P
008800                                                                  DBANK09P
008900***************************************************************** DBANK09P
009000* Was the record not found?                                     * DBANK09P
009100***************************************************************** DBANK09P
009200     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK09P
009300        MOVE SPACES TO CD09O-DATA                                 DBANK09P
009400        MOVE HIGH-VALUES TO CD09O-CONTACT-ID                      DBANK09P
009500        MOVE 'Bad VSAM read' TO CD09O-CONTACT-NAME                DBANK09P
009600     END-IF.                                                      DBANK09P
009700                                                                  DBANK09P
009800***************************************************************** DBANK09P
009900* Move the result back to the callers area                      * DBANK09P
010000***************************************************************** DBANK09P
010100     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK09P
010200                                                                  DBANK09P
010300***************************************************************** DBANK09P
010400* Return to our caller                                          * DBANK09P
010500***************************************************************** DBANK09P
010600 COPY CRETURN.                                                    DBANK09P
010700                                                                  DBANK09P
010800* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK09P
