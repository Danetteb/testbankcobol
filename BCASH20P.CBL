000100***************************************************************** BCASH20P
000200*                                                               * BCASH20P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * BCASH20P
000400*   This demonstration program is provided for use by users     * BCASH20P
000500*   of Micro Focus products and may be used, modified and       * BCASH20P
000600*   distributed as part of your application provided that       * BCASH20P
000700*   you properly acknowledge the copyright of Micro Focus       * BCASH20P
000800*   in this material.                                           * BCASH20P
000900*                                                               * BCASH20P
001000***************************************************************** BCASH20P
001100                                                                  BCASH20P
001200***************************************************************** BCASH20P
001300* Program:     BCASH20P.CBL                                     * BCASH20P
001400* Layer:       Business logic                                   * BCASH20P
001500* Function:    Obtain list of ATM enabled accounts              * BCASH20P
001600***************************************************************** BCASH20P
001700                                                                  BCASH20P
001800 IDENTIFICATION DIVISION.                                         BCASH20P
001900 PROGRAM-ID.                                                      BCASH20P
002000     BCASH20P.                                                    BCASH20P
002100 DATE-WRITTEN.                                                    BCASH20P
002200     September 2002.                                              BCASH20P
002300 DATE-COMPILED.                                                   BCASH20P
002400     Today.                                                       BCASH20P
002500                                                                  BCASH20P
002600 ENVIRONMENT DIVISION.                                            BCASH20P
002700                                                                  BCASH20P
002800 DATA DIVISION.                                                   BCASH20P
002900 WORKING-STORAGE SECTION.                                         BCASH20P
003000 01  WS-MISC-STORAGE.                                             BCASH20P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BCASH20P
003200       VALUE 'BCASH20P'.                                          BCASH20P
003300   05  WS-COMMAREA-LENGTH                    PIC 9(5).            BCASH20P
003400                                                                  BCASH20P
003500 01  WS-CASH-DATA.                                                BCASH20P
003600 COPY CCASHDAT.                                                   BCASH20P
003700                                                                  BCASH20P
003800 01  WS-ACCOUNT-DATA.                                             BCASH20P
003900 COPY CCASHD02.                                                   BCASH20P
004000                                                                  BCASH20P
004100 COPY CABENDD.                                                    BCASH20P
004200                                                                  BCASH20P
004300 LINKAGE SECTION.                                                 BCASH20P
004400 01  DFHCOMMAREA.                                                 BCASH20P
004500   05  LK-COMMAREA                           PIC X(6144).         BCASH20P
004600                                                                  BCASH20P
004700 COPY CENTRY.                                                     BCASH20P
004800***************************************************************** BCASH20P
004900* Make ourselves re-entrant                                     * BCASH20P
005000***************************************************************** BCASH20P
005100                                                                  BCASH20P
005200***************************************************************** BCASH20P
005300* Move the passed area to our area                              * BCASH20P
005400***************************************************************** BCASH20P
005500     MOVE DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA) TO WS-CASH-DATA. BCASH20P
005600                                                                  BCASH20P
005700***************************************************************** BCASH20P
005800* Ensure error message is cleared                               * BCASH20P
005900***************************************************************** BCASH20P
006000     MOVE SPACES TO CASH-ERROR-MSG.                               BCASH20P
006100                                                                  BCASH20P
006200***************************************************************** BCASH20P
006300* This is the main process                                      * BCASH20P
006400***************************************************************** BCASH20P
006500     MOVE SPACES TO CD02-DATA.                                    BCASH20P
006600     MOVE CASH-USERID TO CD02I-CONTACT-ID.                        BCASH20P
006700* Now go get the data                                             BCASH20P
006800 COPY CCASHX02.                                                   BCASH20P
006900     MOVE CD02O-ACC1 TO CASH-ATM2-ACC1.                           BCASH20P
007000     MOVE CD02O-DSC1 TO CASH-ATM2-DSC1.                           BCASH20P
007100                                                                  BCASH20P
007200     MOVE CD02O-ACC2 TO CASH-ATM2-ACC2.                           BCASH20P
007300     MOVE CD02O-DSC2 TO CASH-ATM2-DSC2.                           BCASH20P
007400                                                                  BCASH20P
007500     MOVE CD02O-ACC3 TO CASH-ATM2-ACC3.                           BCASH20P
007600     MOVE CD02O-DSC3 TO CASH-ATM2-DSC3.                           BCASH20P
007700                                                                  BCASH20P
007800     MOVE CD02O-ACC4 TO CASH-ATM2-ACC4.                           BCASH20P
007900     MOVE CD02O-DSC4 TO CASH-ATM2-DSC4.                           BCASH20P
008000                                                                  BCASH20P
008100     MOVE CD02O-ACC5 TO CASH-ATM2-ACC5.                           BCASH20P
008200     MOVE CD02O-DSC5 TO CASH-ATM2-DSC5.                           BCASH20P
008300                                                                  BCASH20P
008400 COMMON-RETURN.                                                   BCASH20P
008500     MOVE WS-CASH-DATA TO DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA). BCASH20P
008600 COPY CRETURN.                                                    BCASH20P
008700                                                                  BCASH20P
008800* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        BCASH20P
