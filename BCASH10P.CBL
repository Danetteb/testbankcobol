000100***************************************************************** BCASH10P
000200*                                                               * BCASH10P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * BCASH10P
000400*   This demonstration program is provided for use by users     * BCASH10P
000500*   of Micro Focus products and may be used, modified and       * BCASH10P
000600*   distributed as part of your application provided that       * BCASH10P
000700*   you properly acknowledge the copyright of Micro Focus       * BCASH10P
000800*   in this material.                                           * BCASH10P
000900*                                                               * BCASH10P
001000***************************************************************** BCASH10P
001100                                                                  BCASH10P
001200***************************************************************** BCASH10P
001300* Program:     BCASH10P.CBL                                     * BCASH10P
001400* Layer:       Business logic                                   * BCASH10P
001500* Function:    Validate a user's PIN                            * BCASH10P
001600***************************************************************** BCASH10P
001700                                                                  BCASH10P
001800 IDENTIFICATION DIVISION.                                         BCASH10P
001900 PROGRAM-ID.                                                      BCASH10P
002000     BCASH10P.                                                    BCASH10P
002100 DATE-WRITTEN.                                                    BCASH10P
002200     September 2002.                                              BCASH10P
002300 DATE-COMPILED.                                                   BCASH10P
002400     Today.                                                       BCASH10P
002500                                                                  BCASH10P
002600 ENVIRONMENT DIVISION.                                            BCASH10P
002700                                                                  BCASH10P
002800 DATA DIVISION.                                                   BCASH10P
002900 WORKING-STORAGE SECTION.                                         BCASH10P
003000 01  WS-MISC-STORAGE.                                             BCASH10P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BCASH10P
003200       VALUE 'BCASH10P'.                                          BCASH10P
003300   05  WS-COMMAREA-LENGTH                    PIC 9(5).            BCASH10P
003400                                                                  BCASH10P
003500 01  WS-CASH-DATA.                                                BCASH10P
003600 COPY CCASHDAT.                                                   BCASH10P
003700                                                                  BCASH10P
003800 01  WS-PERSON-DATA.                                              BCASH10P
003900 COPY CCASHD01.                                                   BCASH10P
004000                                                                  BCASH10P
004100 COPY CABENDD.                                                    BCASH10P
004200                                                                  BCASH10P
004300 LINKAGE SECTION.                                                 BCASH10P
004400 01  DFHCOMMAREA.                                                 BCASH10P
004500   05  LK-COMMAREA                           PIC X(6144).         BCASH10P
004600                                                                  BCASH10P
004700 COPY CENTRY.                                                     BCASH10P
004800***************************************************************** BCASH10P
004900* Make ourselves re-entrant                                     * BCASH10P
005000***************************************************************** BCASH10P
005100                                                                  BCASH10P
005200***************************************************************** BCASH10P
005300* Move the passed area to our area                              * BCASH10P
005400***************************************************************** BCASH10P
005500     MOVE DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA) TO WS-CASH-DATA. BCASH10P
005600                                                                  BCASH10P
005700***************************************************************** BCASH10P
005800* Ensure error message is cleared                               * BCASH10P
005900***************************************************************** BCASH10P
006000     MOVE SPACES TO CASH-ERROR-MSG.                               BCASH10P
006100                                                                  BCASH10P
006200***************************************************************** BCASH10P
006300* This is the main process                                      * BCASH10P
006400***************************************************************** BCASH10P
006500     MOVE SPACES TO CD01-DATA.                                    BCASH10P
006600     MOVE CASH-USERID TO CD01I-CONTACT-ID.                        BCASH10P
006700* Now go get the data                                             BCASH10P
006800 COPY CCASHX01.                                                   BCASH10P
006900     SET CASH-PIN-STATUS-UNKNOWN TO TRUE.                         BCASH10P
007000     EVALUATE TRUE                                                BCASH10P
007100       WHEN CD01O-PIN IS EQUAL TO '????'                          BCASH10P
007200         SET CASH-PIN-STATUS-NO-USER TO TRUE                      BCASH10P
007300         MOVE 'Unknow user' TO CASH-ERROR-MSG                     BCASH10P
007400       WHEN CD01O-PIN IS EQUAL TO '    '                          BCASH10P
007500         SET CASH-PIN-STATUS-NO-PIN TO TRUE                       BCASH10P
007600         MOVE 'No PIN on file for user' TO CASH-ERROR-MSG         BCASH10P
007700       WHEN CD01O-PIN IS EQUAL TO CASH-PIN                        BCASH10P
007800         SET CASH-PIN-STATUS-OK TO TRUE                           BCASH10P
007900         MOVE SPACES TO CASH-ERROR-MSG                            BCASH10P
008000       WHEN OTHER                                                 BCASH10P
008100         SET CASH-PIN-STATUS-INVALID TO TRUE                      BCASH10P
008200         MOVE 'PIN invalid' TO CASH-ERROR-MSG                     BCASH10P
008300     END-EVALUATE.                                                BCASH10P
008400                                                                  BCASH10P
008500 COMMON-RETURN.                                                   BCASH10P
008600     MOVE WS-CASH-DATA TO DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA). BCASH10P
008700 COPY CRETURN.                                                    BCASH10P
008800                                                                  BCASH10P
008900* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        BCASH10P
