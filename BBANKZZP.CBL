000100***************************************************************** BBANKZZP
000200*                                                               * BBANKZZP
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * BBANKZZP
000400*   This demonstration program is provided for use by users     * BBANKZZP
000500*   of Micro Focus products and may be used, modified and       * BBANKZZP
000600*   distributed as part of your application provided that       * BBANKZZP
000700*   you properly acknowledge the copyright of Micro Focus       * BBANKZZP
000800*   in this material.                                           * BBANKZZP
000900*                                                               * BBANKZZP
001000***************************************************************** BBANKZZP
001100                                                                  BBANKZZP
001200***************************************************************** BBANKZZP
001300* Program:     BBANKZZP.CBL                                     * BBANKZZP
001400* Layer:       Business logic                                   * BBANKZZP
001500* Function:    Create various problem / error conditions        * BBANKZZP
001600***************************************************************** BBANKZZP
001700                                                                  BBANKZZP
001800 IDENTIFICATION DIVISION.                                         BBANKZZP
001900 PROGRAM-ID.                                                      BBANKZZP
002000     BBANKZZP.                                                    BBANKZZP
002100 DATE-WRITTEN.                                                    BBANKZZP
002200     September 2002.                                              BBANKZZP
002300 DATE-COMPILED.                                                   BBANKZZP
002400     Today.                                                       BBANKZZP
002500                                                                  BBANKZZP
002600 ENVIRONMENT DIVISION.                                            BBANKZZP
002700                                                                  BBANKZZP
002800 DATA DIVISION.                                                   BBANKZZP
002900 WORKING-STORAGE SECTION.                                         BBANKZZP
003000 01  WS-MISC-STORAGE.                                             BBANKZZP
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANKZZP
003200       VALUE 'BBANKZZP'.                                          BBANKZZP
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANKZZP
003400     88  INPUT-OK                            VALUE '0'.           BBANKZZP
003500     88  INPUT-ERROR                         VALUE '1'.           BBANKZZP
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANKZZP
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANKZZP
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANKZZP
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANKZZP
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANKZZP
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANKZZP
004200     88  PFK-VALID                           VALUE '0'.           BBANKZZP
004300     88  PFK-INVALID                         VALUE '1'.           BBANKZZP
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANKZZP
004500   05  WS-SUB1                               PIC S9(4) COMP.      BBANKZZP
004600   05  WS-SUB1-LIMIT                         PIC S9(4) COMP.      BBANKZZP
004700   05  WS-SEL-COUNT                          PIC 9(1).            BBANKZZP
004800   05  WS-SEL-OPTION                         PIC X(1).            BBANKZZP
004900     88  WS-SEL-OPTION-NULL                  VALUES ' ', '.'.     BBANKZZP
005000     88  WS-SEL-OPTION-1                     VALUE '1'.           BBANKZZP
005100     88  WS-SEL-OPTION-2                     VALUE '2'.           BBANKZZP
005200     88  WS-SEL-OPTION-3                     VALUE '3'.           BBANKZZP
005300     88  WS-SEL-OPTION-4                     VALUE '4'.           BBANKZZP
005400     88  WS-SEL-OPTION-5                     VALUE '5'.           BBANKZZP
005500     88  WS-SEL-OPTION-6                     VALUE '6'.           BBANKZZP
005600     88  WS-SEL-OPTION-7                     VALUE '7'.           BBANKZZP
005700     88  WS-SEL-OPTION-8                     VALUE '8'.           BBANKZZP
005800   05  WS-SEL-MATRIX                         PIC X(8).            BBANKZZP
005900                                                                  BBANKZZP
006000 01  WS-PROBLEM-DATA.                                             BBANKZZP
006100   05  WS-PROB-SUB                           PIC 9(8) COMP.       BBANKZZP
006200   05  WS-PROB-TABLE.                                             BBANKZZP
006300     10  WS-PROB-TABLE-ITEM                  PIC 9(3)             BBANKZZP
006400         OCCURS 10 TIMES.                                         BBANKZZP
006500   05  WS-PROB-MODULE                        PIC X(8).            BBANKZZP
006600   05  WS-FLD-A                              PIC X(9).            BBANKZZP
006700   05  WS-FLD-A-NUM REDEFINES WS-FLD-A       PIC 9(5).99-.        BBANKZZP
006800   05  WS-FLD-B                              PIC S9(5)V99.        BBANKZZP
006900                                                                  BBANKZZP
007000 01  WS-BANK-DATA.                                                BBANKZZP
007100 COPY CBANKDAT.                                                   BBANKZZP
007200                                                                  BBANKZZP
007300 01  WS-HELP-DATA.                                                BBANKZZP
007400 COPY CHELPD01.                                                   BBANKZZP
007500                                                                  BBANKZZP
007600 01  WS-ACCOUNT-DATA.                                             BBANKZZP
007700 COPY CBANKD08.                                                   BBANKZZP
007800                                                                  BBANKZZP
007900 COPY CABENDD.                                                    BBANKZZP
008000                                                                  BBANKZZP
008100 LINKAGE SECTION.                                                 BBANKZZP
008200 01  DFHCOMMAREA.                                                 BBANKZZP
008300   05  LK-COMMAREA                           PIC X(6144).         BBANKZZP
008400                                                                  BBANKZZP
008500 01  LK-PROB-LINKAGE                         PIC X(1).            BBANKZZP
008600                                                                  BBANKZZP
008700 COPY CENTRY.                                                     BBANKZZP
008800***************************************************************** BBANKZZP
008900* Make ourselves re-entrant                                     * BBANKZZP
009000***************************************************************** BBANKZZP
009100     MOVE SPACES TO WS-ERROR-MSG.                                 BBANKZZP
009200                                                                  BBANKZZP
009300***************************************************************** BBANKZZP
009400* Move the passed area to our area                              * BBANKZZP
009500***************************************************************** BBANKZZP
009600     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANKZZP
009700                                                                  BBANKZZP
009800***************************************************************** BBANKZZP
009900* Ensure error message is cleared                               * BBANKZZP
010000***************************************************************** BBANKZZP
010100     MOVE SPACES TO BANK-ERROR-MSG.                               BBANKZZP
010200                                                                  BBANKZZP
010300***************************************************************** BBANKZZP
010400* This is the main process                                      * BBANKZZP
010500***************************************************************** BBANKZZP
010600                                                                  BBANKZZP
010700***************************************************************** BBANKZZP
010800* Initialize the list of options available                      * BBANKZZP
010900***************************************************************** BBANKZZP
011000     MOVE SPACES TO WS-SEL-MATRIX.                                BBANKZZP
011100                                                                  BBANKZZP
011200***************************************************************** BBANKZZP
011300* Save the passed return message and then turn it off           * BBANKZZP
011400***************************************************************** BBANKZZP
011500     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       BBANKZZP
011600     SET BANK-RETURN-MSG-OFF TO TRUE.                             BBANKZZP
011700                                                                  BBANKZZP
011800     MOVE WS-RETURN-MSG TO WS-ERROR-MSG.                          BBANKZZP
011900                                                                  BBANKZZP
012000***************************************************************** BBANKZZP
012100* Check the AID to see if its valid at this point               * BBANKZZP
012200***************************************************************** BBANKZZP
012300     SET PFK-INVALID TO TRUE.                                     BBANKZZP
012400     IF BANK-AID-ENTER OR                                         BBANKZZP
012500        BANK-AID-PFK03 OR                                         BBANKZZP
012600        BANK-AID-PFK04                                            BBANKZZP
012700        SET PFK-VALID TO TRUE                                     BBANKZZP
012800     END-IF.                                                      BBANKZZP
012900     IF BANK-AID-PFK01 AND                                        BBANKZZP
013000        BANK-HELP-INACTIVE                                        BBANKZZP
013100        SET BANK-HELP-ACTIVE TO TRUE                              BBANKZZP
013200        SET PFK-VALID TO TRUE                                     BBANKZZP
013300     END-IF.                                                      BBANKZZP
013400     IF PFK-INVALID                                               BBANKZZP
013500        SET BANK-AID-ENTER TO TRUE                                BBANKZZP
013600     END-IF.                                                      BBANKZZP
013700                                                                  BBANKZZP
013800***************************************************************** BBANKZZP
013900* Check the AID to see if we have to quit                       * BBANKZZP
014000***************************************************************** BBANKZZP
014100     IF BANK-AID-PFK03                                            BBANKZZP
014200        MOVE 'BBANKZZP' TO BANK-LAST-PROG                         BBANKZZP
014300        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANKZZP
014400        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANKZZP
014500        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANKZZP
014600        GO TO COMMON-RETURN                                       BBANKZZP
014700     END-IF.                                                      BBANKZZP
014800                                                                  BBANKZZP
014900***************************************************************** BBANKZZP
015000* Check the to see if user needs or has been using help         * BBANKZZP
015100***************************************************************** BBANKZZP
015200     IF BANK-HELP-ACTIVE                                          BBANKZZP
015300        IF BANK-AID-PFK04                                         BBANKZZP
015400           SET BANK-HELP-INACTIVE TO TRUE                         BBANKZZP
015500           MOVE 00 TO BANK-HELP-SCREEN                            BBANKZZP
015600           MOVE 'BBANKZZP' TO BANK-LAST-PROG                      BBANKZZP
015700           MOVE 'BBANKZZP' TO BANK-NEXT-PROG                      BBANKZZP
015800           MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                     BBANKZZP
015900           MOVE 'HELP20A' TO BANK-LAST-MAP                        BBANKZZP
016000           MOVE 'MBANKZZ' TO BANK-NEXT-MAPSET                     BBANKZZP
016100           MOVE 'BANKZZA' TO BANK-NEXT-MAP                        BBANKZZP
016200           PERFORM POPULATE-OPTIONS THRU                          BBANKZZP
016300                   POPULATE-OPTIONS-EXIT                          BBANKZZP
016400           GO TO COMMON-RETURN                                    BBANKZZP
016500        ELSE                                                      BBANKZZP
016600           MOVE 01 TO BANK-HELP-SCREEN                            BBANKZZP
016700           MOVE 'BBANKZZP' TO BANK-LAST-PROG                      BBANKZZP
016800           MOVE 'BBANKZZP' TO BANK-NEXT-PROG                      BBANKZZP
016900           MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                     BBANKZZP
017000           MOVE 'BANKZZA' TO BANK-LAST-MAP                        BBANKZZP
017100           MOVE 'MBANKZZ' TO BANK-NEXT-MAPSET                     BBANKZZP
017200           MOVE 'HELP20A' TO BANK-NEXT-MAP                        BBANKZZP
017300           MOVE 'BANKZZ' TO HELP01I-SCRN                          BBANKZZP
017400           COPY CHELPX01.                                         BBANKZZP
017500           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANKZZP
017600           GO TO COMMON-RETURN                                    BBANKZZP
017700     END-IF.                                                      BBANKZZP
017800                                                                  BBANKZZP
017900***************************************************************** BBANKZZP
018000* Check the AID to see if we have to return to previous screen  * BBANKZZP
018100***************************************************************** BBANKZZP
018200     IF BANK-AID-PFK04                                            BBANKZZP
018300        MOVE 'BBANKZZP' TO BANK-LAST-PROG                         BBANKZZP
018400        MOVE 'BBANK10P' TO BANK-NEXT-PROG                         BBANKZZP
018500        MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        BBANKZZP
018600        MOVE 'BANK10A' TO BANK-NEXT-MAP                           BBANKZZP
018700        SET BANK-AID-ENTER TO TRUE                                BBANKZZP
018800        SET BANK-NO-CONV-IN-PROGRESS TO TRUE                      BBANKZZP
018900        GO TO COMMON-RETURN                                       BBANKZZP
019000     END-IF.                                                      BBANKZZP
019100                                                                  BBANKZZP
019200* Check if we have set the screen up before or is this 1st time   BBANKZZP
019300     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANKZZ'                BBANKZZP
019400        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANKZZP
019500        MOVE 'BBANKZZP' TO BANK-LAST-PROG                         BBANKZZP
019600        MOVE 'BBANKZZP' TO BANK-NEXT-PROG                         BBANKZZP
019700        MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                        BBANKZZP
019800        MOVE 'BANKZZA' TO BANK-LAST-MAP                           BBANKZZP
019900        MOVE 'MBANKZZ' TO BANK-NEXT-MAPSET                        BBANKZZP
020000        MOVE 'BANKZZA' TO BANK-NEXT-MAP                           BBANKZZP
020100        MOVE LOW-VALUES TO BANK-SCRZZ-SEL1IP                      BBANKZZP
020200        MOVE LOW-VALUES TO BANK-SCRZZ-SEL2IP                      BBANKZZP
020300        MOVE LOW-VALUES TO BANK-SCRZZ-SEL3IP                      BBANKZZP
020400        MOVE LOW-VALUES TO BANK-SCRZZ-SEL4IP                      BBANKZZP
020500        MOVE LOW-VALUES TO BANK-SCRZZ-SEL5IP                      BBANKZZP
020600        MOVE LOW-VALUES TO BANK-SCRZZ-SEL6IP                      BBANKZZP
020700        MOVE LOW-VALUES TO BANK-SCRZZ-SEL7IP                      BBANKZZP
020800        MOVE LOW-VALUES TO BANK-SCRZZ-SEL8IP                      BBANKZZP
020900        IF GUEST                                                  BBANKZZP
021000           MOVE 'LI      ' TO WS-SEL-MATRIX                       BBANKZZP
021100        ELSE                                                      BBANKZZP
021200          MOVE SPACES TO CD08-DATA                                BBANKZZP
021300          MOVE BANK-USERID TO CD08I-CONTACT-ID                    BBANKZZP
021400* Now go get the data                                             BBANKZZP
021500          COPY CBANKX08.                                          BBANKZZP
021600          IF CD08O-COUNT IS EQUAL TO 0                            BBANKZZP
021700             IF PROBLEM-USER                                      BBANKZZP
021800                MOVE '123     ' TO WS-SEL-MATRIX                  BBANKZZP
021900             ELSE                                                 BBANKZZP
022000                MOVE '12      ' TO WS-SEL-MATRIX                  BBANKZZP
022100             END-IF                                               BBANKZZP
022200          END-IF                                                  BBANKZZP
022300          IF CD08O-COUNT IS EQUAL TO 1                            BBANKZZP
022400             IF PROBLEM-USER                                      BBANKZZP
022500                MOVE '123456  ' TO WS-SEL-MATRIX                  BBANKZZP
022600             ELSE                                                 BBANKZZP
022700                MOVE '12345   ' TO WS-SEL-MATRIX                  BBANKZZP
022800             END-IF                                               BBANKZZP
022900          END-IF                                                  BBANKZZP
023000          IF CD08O-COUNT IS GREATER THAN 1                        BBANKZZP
023100             IF PROBLEM-USER                                      BBANKZZP
023200                MOVE '12345678' TO WS-SEL-MATRIX                  BBANKZZP
023300             ELSE                                                 BBANKZZP
023400                MOVE '1234567 ' TO WS-SEL-MATRIX                  BBANKZZP
023500             END-IF                                               BBANKZZP
023600          END-IF                                                  BBANKZZP
023700        END-IF                                                    BBANKZZP
023800        PERFORM POPULATE-OPTIONS THRU                             BBANKZZP
023900                POPULATE-OPTIONS-EXIT                             BBANKZZP
024000        GO TO COMMON-RETURN                                       BBANKZZP
024100     END-IF.                                                      BBANKZZP
024200                                                                  BBANKZZP
024300     PERFORM VALIDATE-DATA THRU                                   BBANKZZP
024400             VALIDATE-DATA-EXIT.                                  BBANKZZP
024500                                                                  BBANKZZP
024600* If we had an error display error and return                     BBANKZZP
024700     IF INPUT-ERROR                                               BBANKZZP
024800        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANKZZP
024900        MOVE 'BBANKZZP' TO BANK-LAST-PROG                         BBANKZZP
025000        MOVE 'BBANKZZP' TO BANK-NEXT-PROG                         BBANKZZP
025100        MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                        BBANKZZP
025200        MOVE 'BANKZZA' TO BANK-LAST-MAP                           BBANKZZP
025300        MOVE 'MBANKZZ' TO BANK-NEXT-MAPSET                        BBANKZZP
025400        MOVE 'BANKZZA' TO BANK-NEXT-MAP                           BBANKZZP
025500        PERFORM POPULATE-OPTIONS THRU                             BBANKZZP
025600                POPULATE-OPTIONS-EXIT                             BBANKZZP
025700        GO TO COMMON-RETURN                                       BBANKZZP
025800     END-IF.                                                      BBANKZZP
025900                                                                  BBANKZZP
026000     IF BANK-SCRZZ-SEL1IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
026100        MOVE BANK-SCRZZ-SEL1ID TO WS-SEL-OPTION                   BBANKZZP
026200     END-IF.                                                      BBANKZZP
026300     IF BANK-SCRZZ-SEL2IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
026400        MOVE BANK-SCRZZ-SEL2ID TO WS-SEL-OPTION                   BBANKZZP
026500     END-IF.                                                      BBANKZZP
026600     IF BANK-SCRZZ-SEL3IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
026700        MOVE BANK-SCRZZ-SEL3ID TO WS-SEL-OPTION                   BBANKZZP
026800     END-IF.                                                      BBANKZZP
026900     IF BANK-SCRZZ-SEL4IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
027000        MOVE BANK-SCRZZ-SEL4ID TO WS-SEL-OPTION                   BBANKZZP
027100     END-IF.                                                      BBANKZZP
027200     IF BANK-SCRZZ-SEL5IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
027300        MOVE BANK-SCRZZ-SEL5ID TO WS-SEL-OPTION                   BBANKZZP
027400     END-IF.                                                      BBANKZZP
027500     IF BANK-SCRZZ-SEL6IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
027600        MOVE BANK-SCRZZ-SEL6ID TO WS-SEL-OPTION                   BBANKZZP
027700     END-IF.                                                      BBANKZZP
027800     IF BANK-SCRZZ-SEL7IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
027900        MOVE BANK-SCRZZ-SEL7ID TO WS-SEL-OPTION                   BBANKZZP
028000     END-IF.                                                      BBANKZZP
028100     IF BANK-SCRZZ-SEL8IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
028200        MOVE BANK-SCRZZ-SEL8ID TO WS-SEL-OPTION                   BBANKZZP
028300     END-IF.                                                      BBANKZZP
028400                                                                  BBANKZZP
028500     MOVE ALL '*' TO WS-PROBLEM-DATA.                             BBANKZZP
028600                                                                  BBANKZZP
028700     EVALUATE TRUE                                                BBANKZZP
028800        WHEN WS-SEL-OPTION IS EQUAL TO '1'                        BBANKZZP
028900          PERFORM SCENARIO-1 THRU                                 BBANKZZP
029000                  SCENARIO-1-EXIT                                 BBANKZZP
029100        WHEN WS-SEL-OPTION IS EQUAL TO '2'                        BBANKZZP
029200          PERFORM SCENARIO-2 THRU                                 BBANKZZP
029300                  SCENARIO-2-EXIT                                 BBANKZZP
029400        WHEN WS-SEL-OPTION IS EQUAL TO '3'                        BBANKZZP
029500          PERFORM SCENARIO-3 THRU                                 BBANKZZP
029600                  SCENARIO-3-EXIT                                 BBANKZZP
029700        WHEN WS-SEL-OPTION IS EQUAL TO '4'                        BBANKZZP
029800          PERFORM SCENARIO-4 THRU                                 BBANKZZP
029900                  SCENARIO-4-EXIT                                 BBANKZZP
030000        WHEN WS-SEL-OPTION IS EQUAL TO '5'                        BBANKZZP
030100          PERFORM SCENARIO-5 THRU                                 BBANKZZP
030200                  SCENARIO-5-EXIT                                 BBANKZZP
030300        WHEN WS-SEL-OPTION IS EQUAL TO '6'                        BBANKZZP
030400          PERFORM SCENARIO-6 THRU                                 BBANKZZP
030500                  SCENARIO-6-EXIT                                 BBANKZZP
030600        WHEN WS-SEL-OPTION IS EQUAL TO '7'                        BBANKZZP
030700          PERFORM SCENARIO-7 THRU                                 BBANKZZP
030800                  SCENARIO-7-EXIT                                 BBANKZZP
030900        WHEN WS-SEL-OPTION IS EQUAL TO '8'                        BBANKZZP
031000          PERFORM SCENARIO-8 THRU                                 BBANKZZP
031100                  SCENARIO-8-EXIT                                 BBANKZZP
031200        WHEN OTHER                                                BBANKZZP
031300          PERFORM SCENARIO-9 THRU                                 BBANKZZP
031400                  SCENARIO-9-EXIT                                 BBANKZZP
031500     END-EVALUATE.                                                BBANKZZP
031600                                                                  BBANKZZP
031700     GO TO COMMON-RETURN.                                         BBANKZZP
031800                                                                  BBANKZZP
031900***************************************************************** BBANKZZP
032000* If we get this far then we have an error in our logic as we   * BBANKZZP
032100* don't know where to go next.                                  * BBANKZZP
032200***************************************************************** BBANKZZP
032300     IF BANK-ENV-CICS                                             BBANKZZP
032400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANKZZP
032500        MOVE '0001' TO ABEND-CODE                                 BBANKZZP
032600        MOVE SPACES TO ABEND-REASON                               BBANKZZP
032700        COPY CABENDPO.                                            BBANKZZP
032800     END-IF.                                                      BBANKZZP
032900     GOBACK.                                                      BBANKZZP
033000                                                                  BBANKZZP
033100 COMMON-RETURN.                                                   BBANKZZP
033200     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANKZZP
033300 COPY CRETURN.                                                    BBANKZZP
033400                                                                  BBANKZZP
033500 VALIDATE-DATA.                                                   BBANKZZP
033600     SET INPUT-OK TO TRUE.                                        BBANKZZP
033700     MOVE ZERO TO WS-SEL-COUNT.                                   BBANKZZP
033800                                                                  BBANKZZP
033900     IF BANK-SCRZZ-SEL1IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
034000        ADD 1 TO WS-SEL-COUNT                                     BBANKZZP
034100     END-IF.                                                      BBANKZZP
034200     IF BANK-SCRZZ-SEL2IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
034300        ADD 1 TO WS-SEL-COUNT                                     BBANKZZP
034400     END-IF.                                                      BBANKZZP
034500     IF BANK-SCRZZ-SEL3IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
034600        ADD 1 TO WS-SEL-COUNT                                     BBANKZZP
034700     END-IF.                                                      BBANKZZP
034800     IF BANK-SCRZZ-SEL4IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
034900        ADD 1 TO WS-SEL-COUNT                                     BBANKZZP
035000     END-IF.                                                      BBANKZZP
035100     IF BANK-SCRZZ-SEL5IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
035200        ADD 1 TO WS-SEL-COUNT                                     BBANKZZP
035300     END-IF.                                                      BBANKZZP
035400     IF BANK-SCRZZ-SEL6IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
035500        ADD 1 TO WS-SEL-COUNT                                     BBANKZZP
035600     END-IF.                                                      BBANKZZP
035700     IF BANK-SCRZZ-SEL7IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
035800        ADD 1 TO WS-SEL-COUNT                                     BBANKZZP
035900     END-IF.                                                      BBANKZZP
036000     IF BANK-SCRZZ-SEL8IP IS NOT EQUAL TO LOW-VALUES              BBANKZZP
036100        ADD 1 TO WS-SEL-COUNT                                     BBANKZZP
036200     END-IF.                                                      BBANKZZP
036300                                                                  BBANKZZP
036400     IF WS-SEL-COUNT IS EQUAL TO ZERO                             BBANKZZP
036500        MOVE 'Please select an option' TO WS-ERROR-MSG            BBANKZZP
036600        GO TO VALIDATE-DATA-ERROR                                 BBANKZZP
036700     END-IF.                                                      BBANKZZP
036800                                                                  BBANKZZP
036900     IF WS-SEL-COUNT IS GREATER THAN 1                            BBANKZZP
037000        MOVE 'Please select a single option' TO WS-ERROR-MSG      BBANKZZP
037100        GO TO VALIDATE-DATA-ERROR                                 BBANKZZP
037200     END-IF.                                                      BBANKZZP
037300                                                                  BBANKZZP
037400     GO TO VALIDATE-DATA-EXIT.                                    BBANKZZP
037500 VALIDATE-DATA-ERROR.                                             BBANKZZP
037600     SET INPUT-ERROR TO TRUE.                                     BBANKZZP
037700 VALIDATE-DATA-EXIT.                                              BBANKZZP
037800     EXIT.                                                        BBANKZZP
037900                                                                  BBANKZZP
038000 POPULATE-OPTIONS.                                                BBANKZZP
038100     MOVE 0 TO WS-SUB1.                                           BBANKZZP
038200     DIVIDE LENGTH BANK-SCREENZZ-FIELD                            BBANKZZP
038300       INTO LENGTH OF BANK-SCREENZZ-DATA-R                        BBANKZZP
038400         GIVING WS-SUB1-LIMIT.                                    BBANKZZP
038500 POPULATE-OPTIONS-LOOP.                                           BBANKZZP
038600     ADD 1 TO WS-SUB1.                                            BBANKZZP
038700     IF WS-SUB1 IS GREATER THAN WS-SUB1-LIMIT                     BBANKZZP
038800        GO TO POPULATE-OPTIONS-EXIT                               BBANKZZP
038900     END-IF.                                                      BBANKZZP
039000     IF WS-SEL-MATRIX IS NOT EQUAL TO SPACES                      BBANKZZP
039100        MOVE WS-SEL-MATRIX (WS-SUB1:1) TO BANK-SCRZZ-ID (WS-SUB1) BBANKZZP
039200     END-IF.                                                      BBANKZZP
039300     MOVE SPACES TO BANK-SCRZZ-TX (WS-SUB1).                      BBANKZZP
039400     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '1'                   BBANKZZP
039500        MOVE 'Problem scenario 1'                                 BBANKZZP
039600          TO BANK-SCRZZ-TX (WS-SUB1)                              BBANKZZP
039700     END-IF.                                                      BBANKZZP
039800     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '2'                   BBANKZZP
039900        MOVE 'Problem scenario 2'                                 BBANKZZP
040000          TO BANK-SCRZZ-TX (WS-SUB1)                              BBANKZZP
040100     END-IF.                                                      BBANKZZP
040200     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '3'                   BBANKZZP
040300        MOVE 'Problem scenario 3'                                 BBANKZZP
040400          TO BANK-SCRZZ-TX (WS-SUB1)                              BBANKZZP
040500     END-IF.                                                      BBANKZZP
040600     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '4'                   BBANKZZP
040700        MOVE 'Problem scenario 4'                                 BBANKZZP
040800          TO BANK-SCRZZ-TX (WS-SUB1)                              BBANKZZP
040900     END-IF.                                                      BBANKZZP
041000     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '5'                   BBANKZZP
041100        MOVE 'Problem scenario 5'                                 BBANKZZP
041200          TO BANK-SCRZZ-TX (WS-SUB1)                              BBANKZZP
041300     END-IF.                                                      BBANKZZP
041400     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '6'                   BBANKZZP
041500        MOVE 'Problem scenario 6'                                 BBANKZZP
041600          TO BANK-SCRZZ-TX (WS-SUB1)                              BBANKZZP
041700     END-IF.                                                      BBANKZZP
041800     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '7'                   BBANKZZP
041900        MOVE 'Problem scenario 7'                                 BBANKZZP
042000          TO BANK-SCRZZ-TX (WS-SUB1)                              BBANKZZP
042100     END-IF.                                                      BBANKZZP
042200     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '8'                   BBANKZZP
042300        MOVE 'Problem scenario 8'                                 BBANKZZP
042400          TO BANK-SCRZZ-TX (WS-SUB1)                              BBANKZZP
042500     END-IF.                                                      BBANKZZP
042600     GO TO POPULATE-OPTIONS-LOOP.                                 BBANKZZP
042700 POPULATE-OPTIONS-EXIT.                                           BBANKZZP
042800     EXIT.                                                        BBANKZZP
042900                                                                  BBANKZZP
043000***************************************************************** BBANKZZP
043100* Problem scenario 1                                            * BBANKZZP
043200***************************************************************** BBANKZZP
043300 SCENARIO-1.                                                      BBANKZZP
043400     MOVE 99 TO WS-PROB-SUB                                       BBANKZZP
043500     MOVE WS-PROB-TABLE-ITEM(1)                                   BBANKZZP
043600       TO WS-PROB-TABLE-ITEM(WS-PROB-SUB).                        BBANKZZP
043700 SCENARIO-1-EXIT.                                                 BBANKZZP
043800     EXIT.                                                        BBANKZZP
043900                                                                  BBANKZZP
044000***************************************************************** BBANKZZP
044100* Problem scenario 2                                            * BBANKZZP
044200***************************************************************** BBANKZZP
044300 SCENARIO-2.                                                      BBANKZZP
044400     ADD 1 TO WS-PROB-TABLE-ITEM(1).                              BBANKZZP
044500 SCENARIO-2-EXIT.                                                 BBANKZZP
044600     EXIT.                                                        BBANKZZP
044700                                                                  BBANKZZP
044800***************************************************************** BBANKZZP
044900* Problem scenario 3                                            * BBANKZZP
045000***************************************************************** BBANKZZP
045100 SCENARIO-3.                                                      BBANKZZP
045200     CALL WS-PROB-MODULE.                                         BBANKZZP
045300 SCENARIO-3-EXIT.                                                 BBANKZZP
045400     EXIT.                                                        BBANKZZP
045500                                                                  BBANKZZP
045600***************************************************************** BBANKZZP
045700* Problem scenario 4                                            * BBANKZZP
045800***************************************************************** BBANKZZP
045900 SCENARIO-4.                                                      BBANKZZP
046000     MOVE 40960 TO WS-PROB-SUB.                                   BBANKZZP
046100     MOVE LOW-VALUES TO WS-PROBLEM-DATA(1:WS-PROB-SUB).           BBANKZZP
046200 SCENARIO-4-EXIT.                                                 BBANKZZP
046300     EXIT.                                                        BBANKZZP
046400                                                                  BBANKZZP
046500***************************************************************** BBANKZZP
046600* Problem scenario 5                                            * BBANKZZP
046700***************************************************************** BBANKZZP
046800 SCENARIO-5.                                                      BBANKZZP
046900     MOVE ALL '*' TO LK-PROB-LINKAGE.                             BBANKZZP
047000 SCENARIO-5-EXIT.                                                 BBANKZZP
047100     EXIT.                                                        BBANKZZP
047200                                                                  BBANKZZP
047300***************************************************************** BBANKZZP
047400* Problem scenario 6                                            * BBANKZZP
047500***************************************************************** BBANKZZP
047600 SCENARIO-6.                                                      BBANKZZP
047700     GO TO NOWHERE.                                               BBANKZZP
047800 SCENARIO-6-EXIT.                                                 BBANKZZP
047900     EXIT.                                                        BBANKZZP
048000                                                                  BBANKZZP
048100***************************************************************** BBANKZZP
048200* Problem scenario 7                                            * BBANKZZP
048300***************************************************************** BBANKZZP
048400 SCENARIO-7.                                                      BBANKZZP
048500     PERFORM SCENARIO-7.                                          BBANKZZP
048600 SCENARIO-7-EXIT.                                                 BBANKZZP
048700     EXIT.                                                        BBANKZZP
048800                                                                  BBANKZZP
048900***************************************************************** BBANKZZP
049000* Problem scenario 8                                            * BBANKZZP
049100***************************************************************** BBANKZZP
049200 SCENARIO-8.                                                      BBANKZZP
049300     call 'CBL_DEBUGBREAK'.                                       BBANKZZP
049400     MOVE '  123.45 ' TO WS-FLD-A.                                BBANKZZP
049500     MOVE 99999.99 TO WS-FLD-B.                                   BBANKZZP
049600     MOVE WS-FLD-A-NUM TO WS-FLD-B .                              BBANKZZP
049700 SCENARIO-8-EXIT.                                                 BBANKZZP
049800     EXIT.                                                        BBANKZZP
049900                                                                  BBANKZZP
050000***************************************************************** BBANKZZP
050100* Problem scenario 9                                            * BBANKZZP
050200***************************************************************** BBANKZZP
050300 SCENARIO-9.                                                      BBANKZZP
050400 SCENARIO-9-EXIT.                                                 BBANKZZP
050500     EXIT.                                                        BBANKZZP
050600                                                                  BBANKZZP
050700 NOWHERE.                                                         BBANKZZP
050800                                                                  BBANKZZP
050900* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        BBANKZZP
