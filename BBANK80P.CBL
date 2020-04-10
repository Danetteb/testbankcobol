000100***************************************************************** BBANK80P
000200*                                                               * BBANK80P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * BBANK80P
000400*   This demonstration program is provided for use by users     * BBANK80P
000500*   of Micro Focus products and may be used, modified and       * BBANK80P
000600*   distributed as part of your application provided that       * BBANK80P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK80P
000800*   in this material.                                           * BBANK80P
000900*                                                               * BBANK80P
001000***************************************************************** BBANK80P
001100                                                                  BBANK80P
001200***************************************************************** BBANK80P
001300* Program:     BBANK80P.CBL                                     * BBANK80P
001400* Layer:       Business logic                                   * BBANK80P
001500* Function:    Print statements                                 * BBANK80P
001600***************************************************************** BBANK80P
001700                                                                  BBANK80P
001800 IDENTIFICATION DIVISION.                                         BBANK80P
001900 PROGRAM-ID.                                                      BBANK80P
002000     BBANK80P.                                                    BBANK80P
002100 DATE-WRITTEN.                                                    BBANK80P
002200     September 2002.                                              BBANK80P
002300 DATE-COMPILED.                                                   BBANK80P
002400     Today.                                                       BBANK80P
002500                                                                  BBANK80P
002600 ENVIRONMENT DIVISION.                                            BBANK80P
002700                                                                  BBANK80P
002800 DATA DIVISION.                                                   BBANK80P
002900 WORKING-STORAGE SECTION.                                         BBANK80P
003000 01  WS-MISC-STORAGE.                                             BBANK80P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK80P
003200       VALUE 'BBANK80P'.                                          BBANK80P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK80P
003400     88  INPUT-OK                            VALUE '0'.           BBANK80P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK80P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK80P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK80P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK80P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK80P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK80P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK80P
004200     88  PFK-VALID                           VALUE '0'.           BBANK80P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK80P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK80P
004500                                                                  BBANK80P
004600 01  WS-BANK-DATA.                                                BBANK80P
004700 COPY CBANKDAT.                                                   BBANK80P
004800                                                                  BBANK80P
004900 01  WS-HELP-DATA.                                                BBANK80P
005000 COPY CHELPD01.                                                   BBANK80P
005100                                                                  BBANK80P
005200 01  WS-ADDR-DATA.                                                BBANK80P
005300 COPY CBANKD09.                                                   BBANK80P
005400                                                                  BBANK80P
005500 01  WS-STMT-REQUEST-DATA.                                        BBANK80P
005600 COPY CSTMTD01.                                                   BBANK80P
005700                                                                  BBANK80P
005800 COPY CSTATESD.                                                   BBANK80P
005900                                                                  BBANK80P
006000 COPY CABENDD.                                                    BBANK80P
006100                                                                  BBANK80P
006200 LINKAGE SECTION.                                                 BBANK80P
006300 01  DFHCOMMAREA.                                                 BBANK80P
006400   05  LK-COMMAREA                           PIC X(6144).         BBANK80P
006500                                                                  BBANK80P
006600 COPY CENTRY.                                                     BBANK80P
006700***************************************************************** BBANK80P
006800* Make ourselves re-entrant                                     * BBANK80P
006900***************************************************************** BBANK80P
007000     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK80P
007100                                                                  BBANK80P
007200***************************************************************** BBANK80P
007300* Move the passed area to our area                              * BBANK80P
007400***************************************************************** BBANK80P
007500     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK80P
007600                                                                  BBANK80P
007700***************************************************************** BBANK80P
007800* Ensure error message is cleared                               * BBANK80P
007900***************************************************************** BBANK80P
008000     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK80P
008100                                                                  BBANK80P
008200***************************************************************** BBANK80P
008300* This is the main process                                      * BBANK80P
008400***************************************************************** BBANK80P
008500                                                                  BBANK80P
008600***************************************************************** BBANK80P
008700* Save the passed return flag and then turn it off              * BBANK80P
008800***************************************************************** BBANK80P
008900     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     BBANK80P
009000     SET BANK-RETURN-FLAG-OFF TO TRUE.                            BBANK80P
009100                                                                  BBANK80P
009200***************************************************************** BBANK80P
009300* Check the AID to see if its valid at this point               * BBANK80P
009400***************************************************************** BBANK80P
009500     SET PFK-INVALID TO TRUE.                                     BBANK80P
009600     IF BANK-AID-ENTER OR                                         BBANK80P
009700        BANK-AID-PFK03 OR                                         BBANK80P
009800        BANK-AID-PFK04 OR                                         BBANK80P
009900        BANK-AID-PFK10                                            BBANK80P
010000        SET PFK-VALID TO TRUE                                     BBANK80P
010100     END-IF.                                                      BBANK80P
010200     IF BANK-AID-PFK01 AND                                        BBANK80P
010300        BANK-HELP-INACTIVE                                        BBANK80P
010400        SET BANK-HELP-ACTIVE TO TRUE                              BBANK80P
010500        SET PFK-VALID TO TRUE                                     BBANK80P
010600     END-IF.                                                      BBANK80P
010700     IF PFK-INVALID                                               BBANK80P
010800        SET BANK-AID-ENTER TO TRUE                                BBANK80P
010900     END-IF.                                                      BBANK80P
011000                                                                  BBANK80P
011100***************************************************************** BBANK80P
011200* Check the AID to see if we have to quit                       * BBANK80P
011300***************************************************************** BBANK80P
011400     IF BANK-AID-PFK03                                            BBANK80P
011500        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
011600        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK80P
011700        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK80P
011800        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK80P
011900        GO TO COMMON-RETURN                                       BBANK80P
012000     END-IF.                                                      BBANK80P
012100                                                                  BBANK80P
012200***************************************************************** BBANK80P
012300* Check the to see if user needs or has been using help         * BBANK80P
012400***************************************************************** BBANK80P
012500     IF BANK-HELP-ACTIVE                                          BBANK80P
012600        IF BANK-AID-PFK04                                         BBANK80P
012700           SET BANK-HELP-INACTIVE TO TRUE                         BBANK80P
012800           MOVE 00 TO BANK-HELP-SCREEN                            BBANK80P
012900           MOVE 'BBANK80P' TO BANK-LAST-PROG                      BBANK80P
013000           MOVE 'BBANK80P' TO BANK-NEXT-PROG                      BBANK80P
013100           MOVE 'MBANK80' TO BANK-LAST-MAPSET                     BBANK80P
013200           MOVE 'HELP80A' TO BANK-LAST-MAP                        BBANK80P
013300           MOVE 'MBANK80' TO BANK-NEXT-MAPSET                     BBANK80P
013400           MOVE 'BANK80A' TO BANK-NEXT-MAP                        BBANK80P
013500           GO TO COMMON-RETURN                                    BBANK80P
013600        ELSE                                                      BBANK80P
013700           MOVE 01 TO BANK-HELP-SCREEN                            BBANK80P
013800           MOVE 'BBANK80P' TO BANK-LAST-PROG                      BBANK80P
013900           MOVE 'BBANK80P' TO BANK-NEXT-PROG                      BBANK80P
014000           MOVE 'MBANK80' TO BANK-LAST-MAPSET                     BBANK80P
014100           MOVE 'BANK80A' TO BANK-LAST-MAP                        BBANK80P
014200           MOVE 'MBANK80' TO BANK-NEXT-MAPSET                     BBANK80P
014300           MOVE 'HELP80A' TO BANK-NEXT-MAP                        BBANK80P
014400           MOVE 'BANK80' TO HELP01I-SCRN                          BBANK80P
014500           COPY CHELPX01.                                         BBANK80P
014600           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK80P
014700           GO TO COMMON-RETURN                                    BBANK80P
014800     END-IF.                                                      BBANK80P
014900                                                                  BBANK80P
015000***************************************************************** BBANK80P
015100* Check the AID to see if we have to return to previous screen  * BBANK80P
015200***************************************************************** BBANK80P
015300     IF BANK-AID-PFK04                                            BBANK80P
015400        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
015500        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK80P
015600        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK80P
015700        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK80P
015800        SET BANK-AID-ENTER TO TRUE                                BBANK80P
015900        GO TO COMMON-RETURN                                       BBANK80P
016000     END-IF.                                                      BBANK80P
016100                                                                  BBANK80P
016200* Check if we have set the screen up before or is this 1st time   BBANK80P
016300     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK80'                BBANK80P
016400        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANK80P
016500        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
016600        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         BBANK80P
016700        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        BBANK80P
016800        MOVE 'BANK80A' TO BANK-LAST-MAP                           BBANK80P
016900        MOVE 'MBANK80' TO BANK-NEXT-MAPSET                        BBANK80P
017000        MOVE 'BANK80A' TO BANK-NEXT-MAP                           BBANK80P
017100        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK80P
017200                POPULATE-SCREEN-DATA-EXIT                         BBANK80P
017300        IF BANK-SCR80-EMAIL IS EQUAL TO SPACES                    BBANK80P
017400           MOVE 'Please use F10 to confirm request.'              BBANK80P
017500             TO WS-ERROR-MSG                                      BBANK80P
017600           SET PRINT-CONFIRM TO TRUE                              BBANK80P
017700           MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                    BBANK80P
017800        ELSE                                                      BBANK80P
017900           SET PRINT-REQUEST TO TRUE                              BBANK80P
018000        END-IF                                                    BBANK80P
018100        GO TO COMMON-RETURN                                       BBANK80P
018200     END-IF.                                                      BBANK80P
018300                                                                  BBANK80P
018400***************************************************************** BBANK80P
018500* Check  the data                                                *BBANK80P
018600***************************************************************** BBANK80P
018700     PERFORM VALIDATE-DATA THRU                                   BBANK80P
018800             VALIDATE-DATA-EXIT.                                  BBANK80P
018900     IF INPUT-ERROR                                               BBANK80P
019000        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK80P
019100        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
019200        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         BBANK80P
019300        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        BBANK80P
019400        MOVE 'BANK80A' TO BANK-LAST-MAP                           BBANK80P
019500        MOVE 'MBANK80' TO BANK-NEXT-MAPSET                        BBANK80P
019600        MOVE 'BANK80A' TO BANK-NEXT-MAP                           BBANK80P
019700        GO TO COMMON-RETURN                                       BBANK80P
019800     END-IF                                                       BBANK80P
019900                                                                  BBANK80P
020000* We want to verify the request                                   BBANK80P
020100     IF PRINT-REQUEST                                             BBANK80P
020200        MOVE 'Please use F10 to confirm request' TO WS-ERROR-MSG  BBANK80P
020300        SET PRINT-CONFIRM TO TRUE                                 BBANK80P
020400        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK80P
020500        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
020600        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         BBANK80P
020700        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        BBANK80P
020800        MOVE 'BANK80A' TO BANK-LAST-MAP                           BBANK80P
020900        MOVE 'MBANK80' TO BANK-NEXT-MAPSET                        BBANK80P
021000        MOVE 'BANK80A' TO BANK-NEXT-MAP                           BBANK80P
021100        GO TO COMMON-RETURN                                       BBANK80P
021200     END-IF.                                                      BBANK80P
021300* Data was changed and verified                                   BBANK80P
021400     IF PRINT-CONFIRM AND                                         BBANK80P
021500        BANK-AID-PFK10                                            BBANK80P
021600        MOVE SPACES TO CSTMTD01-DATA                              BBANK80P
021700        MOVE BANK-SCR80-CONTACT-ID TO CSTMTD01I-CONTACT-ID        BBANK80P
021800        IF BANK-SCR80-EMAIL IS EQUAL TO SPACES                    BBANK80P
021900           SET CSTMTD01I-POST TO TRUE                             BBANK80P
022000        END-IF                                                    BBANK80P
022100        IF BANK-SCR80-OPT1 IS NOT EQUAL TO LOW-VALUES             BBANK80P
022200           SET CSTMTD01I-POST TO TRUE                             BBANK80P
022300        END-IF                                                    BBANK80P
022400        IF BANK-SCR80-OPT2 IS NOT EQUAL TO LOW-VALUES             BBANK80P
022500           SET CSTMTD01I-EMAIL TO TRUE                            BBANK80P
022600        END-IF                                                    BBANK80P
022700* all the routine that will invoke the print process              BBANK80P
022800 COPY CSTMTX01.                                                   BBANK80P
022900        IF CSTMTD01I-POST                                         BBANK80P
023000           STRING 'Statement print has been requested'            BBANK80P
023100                     DELIMITED BY SIZE                            BBANK80P
023200                  ' and will be sent to your postal address'      BBANK80P
023300                    DELIMITED BY SIZE                             BBANK80P
023400             INTO BANK-RETURN-MSG                                 BBANK80P
023500        ELSE                                                      BBANK80P
023600           STRING 'Statement print has been requested'            BBANK80P
023700                     DELIMITED BY SIZE                            BBANK80P
023800                  ' and will be sent to your E-Mail address'      BBANK80P
023900                    DELIMITED BY SIZE                             BBANK80P
024000             INTO BANK-RETURN-MSG                                 BBANK80P
024100        END-IF                                                    BBANK80P
024200        MOVE SPACES TO BANK-SCREEN80-DATA                         BBANK80P
024300        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
024400        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK80P
024500        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK80P
024600        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK80P
024700        SET BANK-AID-ENTER TO TRUE                                BBANK80P
024800        GO TO COMMON-RETURN                                       BBANK80P
024900     END-IF.                                                      BBANK80P
025000                                                                  BBANK80P
025100* Turn off update flags and redisplay                             BBANK80P
025200     SET PRINT-REQUEST TO TRUE.                                   BBANK80P
025300     MOVE 'BBANK80P' TO BANK-LAST-PROG                            BBANK80P
025400     MOVE 'BBANK80P' TO BANK-NEXT-PROG                            BBANK80P
025500     MOVE 'MBANK80' TO BANK-LAST-MAPSET                           BBANK80P
025600     MOVE 'BANK80A' TO BANK-LAST-MAP                              BBANK80P
025700     MOVE 'MBANK80' TO BANK-NEXT-MAPSET                           BBANK80P
025800     MOVE 'BANK80A' TO BANK-NEXT-MAP                              BBANK80P
025900     GO TO COMMON-RETURN.                                         BBANK80P
026000                                                                  BBANK80P
026100***************************************************************** BBANK80P
026200* If we get this far then we have an error in our logic as we   * BBANK80P
026300* don't know where to go next.                                  * BBANK80P
026400***************************************************************** BBANK80P
026500     IF NOT BANK-ENV-CICS                                         BBANK80P
026600        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK80P
026700        MOVE '0001' TO ABEND-CODE                                 BBANK80P
026800        MOVE SPACES TO ABEND-REASON                               BBANK80P
026900        COPY CABENDPO.                                            BBANK80P
027000     END-IF.                                                      BBANK80P
027100     GOBACK.                                                      BBANK80P
027200                                                                  BBANK80P
027300 COMMON-RETURN.                                                   BBANK80P
027400     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK80P
027500 COPY CRETURN.                                                    BBANK80P
027600                                                                  BBANK80P
027700 VALIDATE-DATA.                                                   BBANK80P
027800     SET INPUT-OK TO TRUE.                                        BBANK80P
027900     IF BANK-SCR80-EMAIL IS NOT EQUAL TO SPACES                   BBANK80P
028000        IF BANK-SCR80-OPT1 IS EQUAL TO LOW-VALUES AND             BBANK80P
028100           BANK-SCR80-OPT2 IS EQUAL TO LOW-VALUES                 BBANK80P
028200           MOVE 'Must select an option' TO WS-ERROR-MSG           BBANK80P
028300           GO TO VALIDATE-DATA-ERROR                              BBANK80P
028400        END-IF                                                    BBANK80P
028500        IF BANK-SCR80-OPT1 IS NOT EQUAL TO LOW-VALUES AND         BBANK80P
028600           BANK-SCR80-OPT2 IS NOT EQUAL TO LOW-VALUES             BBANK80P
028700           MOVE 'Select only one of mail or e-mail'               BBANK80P
028800             TO WS-ERROR-MSG                                      BBANK80P
028900           GO TO VALIDATE-DATA-ERROR                              BBANK80P
029000        END-IF                                                    BBANK80P
029100     END-IF.                                                      BBANK80P
029200* Disallow email as we cant really send it                        BBANK80P
029300     IF BANK-SCR80-OPT2 IS NOT EQUAL TO LOW-VALUES                BBANK80P
029400        MOVE SPACES TO WS-ERROR-MSG                               BBANK80P
029500        STRING 'Could not validate email address. '               BBANK80P
029600                 DELIMITED BY SIZE                                BBANK80P
029700               'Please select "mail" or Return'                   BBANK80P
029800                 DELIMITED BY SIZE                                BBANK80P
029900          INTO WS-ERROR-MSG                                       BBANK80P
030000        MOVE HIGH-VALUES TO BANK-SCR80-OPT2                       BBANK80P
030100        GO TO VALIDATE-DATA-ERROR                                 BBANK80P
030200     END-IF.                                                      BBANK80P
030300                                                                  BBANK80P
030400     GO TO VALIDATE-DATA-EXIT.                                    BBANK80P
030500                                                                  BBANK80P
030600 VALIDATE-DATA-ERROR.                                             BBANK80P
030700     SET INPUT-ERROR TO TRUE.                                     BBANK80P
030800 VALIDATE-DATA-EXIT.                                              BBANK80P
030900     EXIT.                                                        BBANK80P
031000                                                                  BBANK80P
031100 POPULATE-SCREEN-DATA.                                            BBANK80P
031200     MOVE SPACES TO CD09-DATA.                                    BBANK80P
031300     MOVE BANK-USERID TO BANK-SCR80-CONTACT-ID.                   BBANK80P
031400     MOVE BANK-SCR80-CONTACT-ID TO CD09I-CONTACT-ID.              BBANK80P
031500* Now go get the data                                             BBANK80P
031600 COPY CBANKX09.                                                   BBANK80P
031700     MOVE SPACES TO BANK-SCR80-DETS.                              BBANK80P
031800     MOVE '_' TO BANK-SCR80-OPT1.                                 BBANK80P
031900     MOVE '_' TO BANK-SCR80-OPT2.                                 BBANK80P
032000     IF CD09O-CONTACT-ID IS EQUAL TO CD09I-CONTACT-ID             BBANK80P
032100        MOVE CD09O-CONTACT-ID TO BANK-SCR80-CONTACT-ID            BBANK80P
032200        MOVE CD09O-CONTACT-NAME TO BANK-SCR80-CONTACT-NAME        BBANK80P
032300        MOVE CD09O-CONTACT-ADDR1 TO BANK-SCR80-ADDR1              BBANK80P
032400        MOVE CD09O-CONTACT-ADDR2 TO BANK-SCR80-ADDR2              BBANK80P
032500        MOVE CD09O-CONTACT-STATE TO BANK-SCR80-STATE              BBANK80P
032600        MOVE CD09O-CONTACT-CNTRY TO BANK-SCR80-CNTRY              BBANK80P
032700        MOVE CD09O-CONTACT-PSTCDE TO BANK-SCR80-PSTCDE            BBANK80P
032800        MOVE CD09O-CONTACT-EMAIL TO BANK-SCR80-EMAIL              BBANK80P
032900     ELSE                                                         BBANK80P
033000        MOVE CD09O-CONTACT-NAME TO BANK-SCR80-CONTACT-NAME        BBANK80P
033100     END-IF.                                                      BBANK80P
033200 POPULATE-SCREEN-DATA-EXIT.                                       BBANK80P
033300     EXIT.                                                        BBANK80P
033400                                                                  BBANK80P
033500* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        BBANK80P
