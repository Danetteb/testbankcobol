000100***************************************************************** BBANK90P
000200*                                                               * BBANK90P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * BBANK90P
000400*   This demonstration program is provided for use by users     * BBANK90P
000500*   of Micro Focus products and may be used, modified and       * BBANK90P
000600*   distributed as part of your application provided that       * BBANK90P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK90P
000800*   in this material.                                           * BBANK90P
000900*                                                               * BBANK90P
001000***************************************************************** BBANK90P
001100                                                                  BBANK90P
001200***************************************************************** BBANK90P
001300* Program:     BBANK90P.CBL                                     * BBANK90P
001400* Layer:       Business logic                                   * BBANK90P
001500* Function:    Obtain data for "more information"               * BBANK90P
001600***************************************************************** BBANK90P
001700                                                                  BBANK90P
001800 IDENTIFICATION DIVISION.                                         BBANK90P
001900 PROGRAM-ID.                                                      BBANK90P
002000     BBANK90P.                                                    BBANK90P
002100 DATE-WRITTEN.                                                    BBANK90P
002200     September 2002.                                              BBANK90P
002300 DATE-COMPILED.                                                   BBANK90P
002400     Today.                                                       BBANK90P
002500                                                                  BBANK90P
002600 ENVIRONMENT DIVISION.                                            BBANK90P
002700                                                                  BBANK90P
002800 DATA DIVISION.                                                   BBANK90P
002900 WORKING-STORAGE SECTION.                                         BBANK90P
003000 01  WS-MISC-STORAGE.                                             BBANK90P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK90P
003200       VALUE 'BBANK90P'.                                          BBANK90P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK90P
003400     88  INPUT-OK                            VALUE '0'.           BBANK90P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK90P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK90P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK90P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK90P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK90P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK90P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK90P
004200     88  PFK-VALID                           VALUE '0'.           BBANK90P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK90P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK90P
004500   05  WS-SUB1                               PIC S9(4) COMP.      BBANK90P
004600   05  WS-SUB1-LIMIT                         PIC S9(4) COMP.      BBANK90P
004700                                                                  BBANK90P
004800 01  WS-INF-DATA.                                                 BBANK90P
004900   05  WS-INF-DATA01.                                             BBANK90P
005000     10  FILLER                              PIC X(40)            BBANK90P
005100         VALUE 'Sorry. The information you requested is '.        BBANK90P
005200     10  FILLER                              PIC X(35)            BBANK90P
005300         VALUE 'not available at this time.        '.             BBANK90P
005400   05  WS-INF-DATA03.                                             BBANK90P
005500     10  FILLER                              PIC X(40)            BBANK90P
005600         VALUE 'Please try our web site at:             '.        BBANK90P
005700     10  FILLER                              PIC X(35)            BBANK90P
005800         VALUE '                                   '.             BBANK90P
005900   05  WS-INF-DATA05.                                             BBANK90P
006000     10  FILLER                              PIC X(40)            BBANK90P
006100         VALUE '     http://www.microfocus.com          '.        BBANK90P
006200     10  FILLER                              PIC X(35)            BBANK90P
006300         VALUE '                                   '.             BBANK90P
006400   05  WS-INF-DATA07.                                             BBANK90P
006500     10  FILLER                              PIC X(40)            BBANK90P
006600         VALUE 'or call our office at 1-800-VS-COBOL    '.        BBANK90P
006700     10  FILLER                              PIC X(35)            BBANK90P
006800         VALUE '                                   '.             BBANK90P
006900   05  WS-INF-DATA08.                                             BBANK90P
007000     10  FILLER                              PIC X(40)            BBANK90P
007100         VALUE '                     (1-800-872-6265)   '.        BBANK90P
007200     10  FILLER                              PIC X(35)            BBANK90P
007300         VALUE '                                   '.             BBANK90P
007400   05  WS-INF-DATA10.                                             BBANK90P
007500     10  FILLER                              PIC X(40)            BBANK90P
007600         VALUE 'Thank you for your interest.            '.        BBANK90P
007700     10  FILLER                              PIC X(35)            BBANK90P
007800         VALUE '                                   '.             BBANK90P
007900                                                                  BBANK90P
008000 01  WS-BANK-DATA.                                                BBANK90P
008100 COPY CBANKDAT.                                                   BBANK90P
008200                                                                  BBANK90P
008300 01  WS-HELP-DATA.                                                BBANK90P
008400 COPY CHELPD01.                                                   BBANK90P
008500                                                                  BBANK90P
008600 COPY CABENDD.                                                    BBANK90P
008700                                                                  BBANK90P
008800 LINKAGE SECTION.                                                 BBANK90P
008900 01  DFHCOMMAREA.                                                 BBANK90P
009000   05  LK-COMMAREA                           PIC X(6144).         BBANK90P
009100                                                                  BBANK90P
009200 COPY CENTRY.                                                     BBANK90P
009300***************************************************************** BBANK90P
009400* Make ourselves re-entrant                                     * BBANK90P
009500***************************************************************** BBANK90P
009600     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK90P
009700                                                                  BBANK90P
009800***************************************************************** BBANK90P
009900* Move the passed area to our area                              * BBANK90P
010000***************************************************************** BBANK90P
010100     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK90P
010200                                                                  BBANK90P
010300***************************************************************** BBANK90P
010400* Ensure error message is cleared                               * BBANK90P
010500***************************************************************** BBANK90P
010600     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK90P
010700                                                                  BBANK90P
010800***************************************************************** BBANK90P
010900* This is the main process                                      * BBANK90P
011000***************************************************************** BBANK90P
011100                                                                  BBANK90P
011200***************************************************************** BBANK90P
011300* Save the passed return message and then turn it off           * BBANK90P
011400***************************************************************** BBANK90P
011500     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       BBANK90P
011600     SET BANK-RETURN-MSG-OFF TO TRUE.                             BBANK90P
011700                                                                  BBANK90P
011800     MOVE WS-RETURN-MSG TO WS-ERROR-MSG.                          BBANK90P
011900                                                                  BBANK90P
012000***************************************************************** BBANK90P
012100* Check the AID to see if its valid at this point               * BBANK90P
012200***************************************************************** BBANK90P
012300     SET PFK-INVALID TO TRUE.                                     BBANK90P
012400     IF BANK-AID-ENTER OR                                         BBANK90P
012500        BANK-AID-PFK03 OR                                         BBANK90P
012600        BANK-AID-PFK04                                            BBANK90P
012700        SET PFK-VALID TO TRUE                                     BBANK90P
012800     END-IF.                                                      BBANK90P
012900     IF BANK-AID-PFK01 AND                                        BBANK90P
013000        BANK-HELP-INACTIVE                                        BBANK90P
013100        SET BANK-HELP-ACTIVE TO TRUE                              BBANK90P
013200        SET PFK-VALID TO TRUE                                     BBANK90P
013300     END-IF.                                                      BBANK90P
013400     IF PFK-INVALID                                               BBANK90P
013500        SET BANK-AID-ENTER TO TRUE                                BBANK90P
013600     END-IF.                                                      BBANK90P
013700                                                                  BBANK90P
013800***************************************************************** BBANK90P
013900* Check the AID to see if we have to quit                       * BBANK90P
014000***************************************************************** BBANK90P
014100     IF BANK-AID-PFK03                                            BBANK90P
014200        MOVE 'BBANK90P' TO BANK-LAST-PROG                         BBANK90P
014300        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK90P
014400        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK90P
014500        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK90P
014600        GO TO COMMON-RETURN                                       BBANK90P
014700     END-IF.                                                      BBANK90P
014800                                                                  BBANK90P
014900***************************************************************** BBANK90P
015000* Check the to see if user needs or has been using help         * BBANK90P
015100***************************************************************** BBANK90P
015200     IF BANK-HELP-ACTIVE                                          BBANK90P
015300        IF BANK-AID-PFK04                                         BBANK90P
015400           SET BANK-HELP-INACTIVE TO TRUE                         BBANK90P
015500           MOVE 00 TO BANK-HELP-SCREEN                            BBANK90P
015600           MOVE 'BBANK90P' TO BANK-LAST-PROG                      BBANK90P
015700           MOVE 'BBANK90P' TO BANK-NEXT-PROG                      BBANK90P
015800           MOVE 'MBANK90' TO BANK-LAST-MAPSET                     BBANK90P
015900           MOVE 'HELP90A' TO BANK-LAST-MAP                        BBANK90P
016000           MOVE 'MBANK90' TO BANK-NEXT-MAPSET                     BBANK90P
016100           MOVE 'BANK90A' TO BANK-NEXT-MAP                        BBANK90P
016200           PERFORM POPULATE-SCR90-DATA THRU                       BBANK90P
016300                   POPULATE-SCR90-DATA-EXIT                       BBANK90P
016400           GO TO COMMON-RETURN                                    BBANK90P
016500        ELSE                                                      BBANK90P
016600           MOVE 01 TO BANK-HELP-SCREEN                            BBANK90P
016700           MOVE 'BBANK90P' TO BANK-LAST-PROG                      BBANK90P
016800           MOVE 'BBANK90P' TO BANK-NEXT-PROG                      BBANK90P
016900           MOVE 'MBANK90' TO BANK-LAST-MAPSET                     BBANK90P
017000           MOVE 'BANK90A' TO BANK-LAST-MAP                        BBANK90P
017100           MOVE 'MBANK90' TO BANK-NEXT-MAPSET                     BBANK90P
017200           MOVE 'HELP90A' TO BANK-NEXT-MAP                        BBANK90P
017300           MOVE 'BANK90' TO HELP01I-SCRN                          BBANK90P
017400           COPY CHELPX01.                                         BBANK90P
017500           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK90P
017600           GO TO COMMON-RETURN                                    BBANK90P
017700     END-IF.                                                      BBANK90P
017800                                                                  BBANK90P
017900***************************************************************** BBANK90P
018000* Check the AID to see if we have to return to previous screen  * BBANK90P
018100***************************************************************** BBANK90P
018200     IF BANK-AID-PFK04                                            BBANK90P
018300        MOVE 'BBANK90P' TO BANK-LAST-PROG                         BBANK90P
018400        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK90P
018500        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK90P
018600        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK90P
018700        SET BANK-AID-ENTER TO TRUE                                BBANK90P
018800        SET BANK-NO-CONV-IN-PROGRESS TO TRUE                      BBANK90P
018900        GO TO COMMON-RETURN                                       BBANK90P
019000     END-IF.                                                      BBANK90P
019100                                                                  BBANK90P
019200* Check if we have set the screen up before or is this 1st time   BBANK90P
019300     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK90'                BBANK90P
019400        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANK90P
019500        MOVE 'BBANK90P' TO BANK-LAST-PROG                         BBANK90P
019600        MOVE 'BBANK90P' TO BANK-NEXT-PROG                         BBANK90P
019700        MOVE 'MBANK90' TO BANK-LAST-MAPSET                        BBANK90P
019800        MOVE 'BANK90A' TO BANK-LAST-MAP                           BBANK90P
019900        MOVE 'MBANK90' TO BANK-NEXT-MAPSET                        BBANK90P
020000        MOVE 'BANK90A' TO BANK-NEXT-MAP                           BBANK90P
020100        PERFORM POPULATE-SCR90-DATA THRU                          BBANK90P
020200                POPULATE-SCR90-DATA-EXIT                          BBANK90P
020300        GO TO COMMON-RETURN                                       BBANK90P
020400     END-IF.                                                      BBANK90P
020500                                                                  BBANK90P
020600     PERFORM VALIDATE-DATA THRU                                   BBANK90P
020700             VALIDATE-DATA-EXIT.                                  BBANK90P
020800                                                                  BBANK90P
020900* If we had an error display error and return                     BBANK90P
021000     IF INPUT-ERROR                                               BBANK90P
021100        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK90P
021200        MOVE 'BBANK90P' TO BANK-LAST-PROG                         BBANK90P
021300        MOVE 'BBANK90P' TO BANK-NEXT-PROG                         BBANK90P
021400        MOVE 'MBANK90' TO BANK-LAST-MAPSET                        BBANK90P
021500        MOVE 'BANK90A' TO BANK-LAST-MAP                           BBANK90P
021600        MOVE 'MBANK90' TO BANK-NEXT-MAPSET                        BBANK90P
021700        MOVE 'BANK90A' TO BANK-NEXT-MAP                           BBANK90P
021800        GO TO COMMON-RETURN                                       BBANK90P
021900     END-IF.                                                      BBANK90P
022000                                                                  BBANK90P
022100     PERFORM POPULATE-SCR90-DATA THRU                             BBANK90P
022200             POPULATE-SCR90-DATA-EXIT.                            BBANK90P
022300     GO TO COMMON-RETURN.                                         BBANK90P
022400                                                                  BBANK90P
022500***************************************************************** BBANK90P
022600* If we get this far then we have an error in our logic as we   * BBANK90P
022700* don't know where to go next.                                  * BBANK90P
022800***************************************************************** BBANK90P
022900     IF BANK-ENV-CICS                                             BBANK90P
023000        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK90P
023100        MOVE '0001' TO ABEND-CODE                                 BBANK90P
023200        MOVE SPACES TO ABEND-REASON                               BBANK90P
023300        COPY CABENDPO.                                            BBANK90P
023400     END-IF.                                                      BBANK90P
023500     GOBACK.                                                      BBANK90P
023600                                                                  BBANK90P
023700 COMMON-RETURN.                                                   BBANK90P
023800     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK90P
023900 COPY CRETURN.                                                    BBANK90P
024000                                                                  BBANK90P
024100 VALIDATE-DATA.                                                   BBANK90P
024200     SET INPUT-OK TO TRUE.                                        BBANK90P
024300                                                                  BBANK90P
024400     GO TO VALIDATE-DATA-EXIT.                                    BBANK90P
024500 VALIDATE-DATA-ERROR.                                             BBANK90P
024600     SET INPUT-ERROR TO TRUE.                                     BBANK90P
024700 VALIDATE-DATA-EXIT.                                              BBANK90P
024800     EXIT.                                                        BBANK90P
024900                                                                  BBANK90P
025000 POPULATE-SCR90-DATA.                                             BBANK90P
025100     MOVE 'INFO90' TO HELP01I-SCRN.                               BBANK90P
025200     COPY CHELPX01.                                               BBANK90P
025300     IF HELP-NOT-FOUND                                            BBANK90P
025400        MOVE SPACES TO HELP01O-INDIVIDUAL-LINES                   BBANK90P
025500        MOVE WS-INF-DATA01 TO HELP01O-L01                         BBANK90P
025600        MOVE WS-INF-DATA03 TO HELP01O-L03                         BBANK90P
025700        MOVE WS-INF-DATA05 TO HELP01O-L05                         BBANK90P
025800        MOVE WS-INF-DATA07 TO HELP01O-L07                         BBANK90P
025900        MOVE WS-INF-DATA08 TO HELP01O-L08                         BBANK90P
026000        MOVE WS-INF-DATA10 TO HELP01O-L10                         BBANK90P
026100     END-IF.                                                      BBANK90P
026200     MOVE HELP01O-DATA TO BANK-SCREEN90-DATA.                     BBANK90P
026300 POPULATE-SCR90-DATA-EXIT.                                        BBANK90P
026400     EXIT.                                                        BBANK90P
026500                                                                  BBANK90P
026600* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        BBANK90P
