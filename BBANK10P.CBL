000100***************************************************************** BBANK10P
000200*                                                               * BBANK10P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * BBANK10P
000400*   This demonstration program is provided for use by users     * BBANK10P
000500*   of Micro Focus products and may be used, modified and       * BBANK10P
000600*   distributed as part of your application provided that       * BBANK10P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK10P
000800*   in this material.                                           * BBANK10P
000900*                                                               * BBANK10P
001000***************************************************************** BBANK10P
001100                                                                  BBANK10P
001200***************************************************************** BBANK10P
001300* Program:     BBANK10P.CBL                                     * BBANK10P
001400* Layer:       Business logic                                   * BBANK10P
001500* Function:    Signon to system to identify user                * BBANK10P
001600***************************************************************** BBANK10P
001700                                                                  BBANK10P
001800 IDENTIFICATION DIVISION.                                         BBANK10P
001900 PROGRAM-ID.                                                      BBANK10P
002000     BBANK10P.                                                    BBANK10P
002100 DATE-WRITTEN.                                                    BBANK10P
002200     September 2002.                                              BBANK10P
002300 DATE-COMPILED.                                                   BBANK10P
002400     Today.                                                       BBANK10P
002500                                                                  BBANK10P
002600 ENVIRONMENT DIVISION.                                            BBANK10P
002700                                                                  BBANK10P
002800 DATA DIVISION.                                                   BBANK10P
002900 WORKING-STORAGE SECTION.                                         BBANK10P
003000 01  WS-MISC-STORAGE.                                             BBANK10P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK10P
003200       VALUE 'BBANK10P'.                                          BBANK10P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK10P
003400     88  INPUT-OK                            VALUE '0'.           BBANK10P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK10P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK10P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK10P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK10P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK10P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK10P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK10P
004200     88  PFK-VALID                           VALUE '0'.           BBANK10P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK10P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK10P
004500                                                                  BBANK10P
004600 01  WS-BANK-DATA.                                                BBANK10P
004700 COPY CBANKDAT.                                                   BBANK10P
004800                                                                  BBANK10P
004900 01  WS-HELP-DATA.                                                BBANK10P
005000 COPY CHELPD01.                                                   BBANK10P
005100                                                                  BBANK10P
005200 01  WS-PERSON.                                                   BBANK10P
005300 COPY CBANKD01.                                                   BBANK10P
005400                                                                  BBANK10P
005500 COPY CABENDD.                                                    BBANK10P
005600                                                                  BBANK10P
005700 LINKAGE SECTION.                                                 BBANK10P
005800 01  DFHCOMMAREA.                                                 BBANK10P
005900   05  LK-COMMAREA                           PIC X(6144).         BBANK10P
006000                                                                  BBANK10P
006100 COPY CENTRY.                                                     BBANK10P
006200***************************************************************** BBANK10P
006300* Make ourselves re-entrant                                     * BBANK10P
006400***************************************************************** BBANK10P
006500     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK10P
006600                                                                  BBANK10P
006700***************************************************************** BBANK10P
006800* Move the passed area to our area                              * BBANK10P
006900***************************************************************** BBANK10P
007000     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK10P
007100                                                                  BBANK10P
007200***************************************************************** BBANK10P
007300* Ensure error message is cleared                               * BBANK10P
007400***************************************************************** BBANK10P
007500     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK10P
007600                                                                  BBANK10P
007700***************************************************************** BBANK10P
007800* If this is the first time in, then we have to do set up the   * BBANK10P
007900* COMMAREA and ask for the first map to be displayed.           * BBANK10P
008000***************************************************************** BBANK10P
008100     IF BANK-NO-CONV-IN-PROGRESS                                  BBANK10P
008200        SET BANK-CONV-IN-PROGRESS TO TRUE                         BBANK10P
008300        MOVE 'BBANK10P' TO BANK-LAST-PROG                         BBANK10P
008400        MOVE 'BBANK10P' TO BANK-NEXT-PROG                         BBANK10P
008500        MOVE LOW-VALUES TO BANK-SIGNON-ID                         BBANK10P
008600        MOVE LOW-VALUES TO BANK-USERID                            BBANK10P
008700        MOVE LOW-VALUES TO BANK-PSWD                              BBANK10P
008800        MOVE SPACES TO BANK-LAST-MAPSET                           BBANK10P
008900        MOVE SPACES TO BANK-LAST-MAP                              BBANK10P
009000        MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        BBANK10P
009100        MOVE 'BANK10A' TO BANK-NEXT-MAP                           BBANK10P
009200        GO TO COMMON-RETURN                                       BBANK10P
009300     END-IF.                                                      BBANK10P
009400                                                                  BBANK10P
009500***************************************************************** BBANK10P
009600* This is the main process                                      * BBANK10P
009700***************************************************************** BBANK10P
009800                                                                  BBANK10P
009900***************************************************************** BBANK10P
010000* Save the passed return message and then turn it off           * BBANK10P
010100***************************************************************** BBANK10P
010200     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       BBANK10P
010300     SET BANK-RETURN-MSG-OFF TO TRUE.                             BBANK10P
010400                                                                  BBANK10P
010500***************************************************************** BBANK10P
010600* Check the AID to see if its valid at this point               * BBANK10P
010700***************************************************************** BBANK10P
010800     SET PFK-INVALID TO TRUE.                                     BBANK10P
010900     IF BANK-AID-ENTER OR                                         BBANK10P
011000        BANK-AID-PFK03                                            BBANK10P
011100        SET PFK-VALID TO TRUE                                     BBANK10P
011200     END-IF.                                                      BBANK10P
011300     IF BANK-AID-PFK01 AND                                        BBANK10P
011400        BANK-HELP-INACTIVE                                        BBANK10P
011500        SET BANK-HELP-ACTIVE TO TRUE                              BBANK10P
011600        SET PFK-VALID TO TRUE                                     BBANK10P
011700     END-IF.                                                      BBANK10P
011800     IF BANK-AID-PFK04 AND                                        BBANK10P
011900        BANK-HELP-ACTIVE                                          BBANK10P
012000        SET PFK-VALID TO TRUE                                     BBANK10P
012100     END-IF.                                                      BBANK10P
012200     IF PFK-INVALID                                               BBANK10P
012300        SET BANK-AID-ENTER TO TRUE                                BBANK10P
012400     END-IF.                                                      BBANK10P
012500                                                                  BBANK10P
012600***************************************************************** BBANK10P
012700* Check the AID to see if we have to quit                       * BBANK10P
012800***************************************************************** BBANK10P
012900     IF BANK-AID-PFK03                                            BBANK10P
013000        MOVE 'BBANK10P' TO BANK-LAST-PROG                         BBANK10P
013100        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK10P
013200        MOVE 'MBANK10' TO BANK-LAST-MAPSET                        BBANK10P
013300        MOVE 'BANK10A' TO BANK-LAST-MAP                           BBANK10P
013400        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK10P
013500        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK10P
013600        GO TO COMMON-RETURN                                       BBANK10P
013700     END-IF.                                                      BBANK10P
013800                                                                  BBANK10P
013900***************************************************************** BBANK10P
014000* Check the to see if user needs or has been using help         * BBANK10P
014100***************************************************************** BBANK10P
014200     IF BANK-HELP-ACTIVE                                          BBANK10P
014300        IF BANK-AID-PFK04                                         BBANK10P
014400           SET BANK-HELP-INACTIVE TO TRUE                         BBANK10P
014500           MOVE 00 TO BANK-HELP-SCREEN                            BBANK10P
014600           MOVE 'BBANK10P' TO BANK-LAST-PROG                      BBANK10P
014700           MOVE 'BBANK10P' TO BANK-NEXT-PROG                      BBANK10P
014800           MOVE 'MBANK10' TO BANK-LAST-MAPSET                     BBANK10P
014900           MOVE 'HELP10A' TO BANK-LAST-MAP                        BBANK10P
015000           MOVE 'MBANK10' TO BANK-NEXT-MAPSET                     BBANK10P
015100           MOVE 'BANK10A' TO BANK-NEXT-MAP                        BBANK10P
015200           GO TO COMMON-RETURN                                    BBANK10P
015300        ELSE                                                      BBANK10P
015400           MOVE 01 TO BANK-HELP-SCREEN                            BBANK10P
015500           MOVE 'BBANK10P' TO BANK-LAST-PROG                      BBANK10P
015600           MOVE 'BBANK10P' TO BANK-NEXT-PROG                      BBANK10P
015700           MOVE 'MBANK10' TO BANK-LAST-MAPSET                     BBANK10P
015800           MOVE 'BANK10A' TO BANK-LAST-MAP                        BBANK10P
015900           MOVE 'MBANK10' TO BANK-NEXT-MAPSET                     BBANK10P
016000           MOVE 'HELP10A' TO BANK-NEXT-MAP                        BBANK10P
016100           MOVE 'BANK10' TO HELP01I-SCRN                          BBANK10P
016200           COPY CHELPX01.                                         BBANK10P
016300           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK10P
016400           GO TO COMMON-RETURN                                    BBANK10P
016500     END-IF.                                                      BBANK10P
016600                                                                  BBANK10P
016700     PERFORM VALIDATE-USER THRU                                   BBANK10P
016800             VALIDATE-USER-EXIT.                                  BBANK10P
016900                                                                  BBANK10P
017000* If we had an error display error and return                     BBANK10P
017100     IF INPUT-ERROR                                               BBANK10P
017200        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK10P
017300        MOVE 'SBANK10P' TO BANK-LAST-PROG                         BBANK10P
017400        MOVE 'SBANK10P' TO BANK-NEXT-PROG                         BBANK10P
017500        MOVE 'MBANK10' TO BANK-LAST-MAPSET                        BBANK10P
017600        MOVE 'BANK10A' TO BANK-LAST-MAP                           BBANK10P
017700        MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        BBANK10P
017800        MOVE 'BANK10A' TO BANK-NEXT-MAP                           BBANK10P
017900        GO TO COMMON-RETURN                                       BBANK10P
018000     END-IF.                                                      BBANK10P
018100                                                                  BBANK10P
018200     MOVE 'BBANK20P' TO BANK-NEXT-PROG.                           BBANK10P
018300     GO TO COMMON-RETURN.                                         BBANK10P
018400                                                                  BBANK10P
018500 COMMON-RETURN.                                                   BBANK10P
018600     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK10P
018700 COPY CRETURN.                                                    BBANK10P
018800                                                                  BBANK10P
018900 VALIDATE-USER.                                                   BBANK10P
019000     SET INPUT-OK TO TRUE.                                        BBANK10P
019100     INSPECT BANK-SIGNON-ID                                       BBANK10P
019200       CONVERTING 'abcdefghijklmnopqrstuvwxyz'                    BBANK10P
019300               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   BBANK10P
019400     IF BANK-SIGNON-ID IS EQUAL TO 'GUEST'                        BBANK10P
019500        MOVE 'GUEST' TO BANK-USERID                               BBANK10P
019600        MOVE 'Guest' TO BANK-USERID-NAME                          BBANK10P
019700        GO TO VALIDATE-USER-EXIT                                  BBANK10P
019800     END-IF.                                                      BBANK10P
019900     IF BANK-SIGNON-ID IS EQUAL TO LOW-VALUES                     BBANK10P
020000        MOVE 'Please input user id' TO WS-ERROR-MSG               BBANK10P
020100        GO TO VALIDATE-USER-ERROR                                 BBANK10P
020200     END-IF.                                                      BBANK10P
020300     IF BANK-PSWD IS EQUAL TO LOW-VALUES                          BBANK10P
020400        MOVE 'Please input password' TO WS-ERROR-MSG              BBANK10P
020500        GO TO VALIDATE-USER-ERROR                                 BBANK10P
020600     END-IF.                                                      BBANK10P
020700* We now make sure the user is valid.......                       BBANK10P
020800     MOVE SPACES TO CD01-DATA.                                    BBANK10P
020900     MOVE BANK-SIGNON-ID TO CD01I-PERSON-PID.                     BBANK10P
021000* If user starts with "Z" then treat as "B"                       BBANK10P
021100     IF CD01I-PERSON-PID(1:1) IS EQUAL TO 'Z'                     BBANK10P
021200        MOVE 'B' TO  CD01I-PERSON-PID(1:1)                        BBANK10P
021300     END-IF.                                                      BBANK10P
021400 COPY CBANKX01.                                                   BBANK10P
021500     IF CD01O-PERSON-PID IS EQUAL TO SPACES                       BBANK10P
021600        MOVE CD01O-PERSON-NAME TO WS-ERROR-MSG                    BBANK10P
021700        GO TO VALIDATE-USER-ERROR                                 BBANK10P
021800     ELSE                                                         BBANK10P
021900        MOVE CD01O-PERSON-NAME TO BANK-USERID-NAME                BBANK10P
022000        MOVE BANK-SIGNON-ID TO BANK-USERID                        BBANK10P
022100        IF BANK-USERID(1:1) IS EQUAL TO 'Z'                       BBANK10P
022200           MOVE 'B' TO  BANK-USERID(1:1)                          BBANK10P
022300        END-IF                                                    BBANK10P
022400        GO TO VALIDATE-USER-EXIT                                  BBANK10P
022500     END-IF.                                                      BBANK10P
022600 VALIDATE-USER-ERROR.                                             BBANK10P
022700     SET INPUT-ERROR TO TRUE.                                     BBANK10P
022800 VALIDATE-USER-EXIT.                                              BBANK10P
022900     EXIT.                                                        BBANK10P
023000                                                                  BBANK10P
023100* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        BBANK10P
