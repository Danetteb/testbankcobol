000100***************************************************************** DBANK02P
000200*                                                               * DBANK02P
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANK02P
000400*   This demonstration program is provided for use by users     * DBANK02P
000500*   of Micro Focus products and may be used, modified and       * DBANK02P
000600*   distributed as part of your application provided that       * DBANK02P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK02P
000800*   in this material.                                           * DBANK02P
000900*                                                               * DBANK02P
001000***************************************************************** DBANK02P
001100                                                                  DBANK02P
001200***************************************************************** DBANK02P
001300* Program:     DBANK02P.CBL                                     * DBANK02P
001400* Function:    Obtain/update address information                * DBANK02P
001500*              VSAM version                                     * DBANK02P
001600***************************************************************** DBANK02P
001700                                                                  DBANK02P
001800 IDENTIFICATION DIVISION.                                         DBANK02P
001900 PROGRAM-ID.                                                      DBANK02P
002000     DBANK02P.                                                    DBANK02P
002100 DATE-WRITTEN.                                                    DBANK02P
002200     September 2002.                                              DBANK02P
002300 DATE-COMPILED.                                                   DBANK02P
002400     Today.                                                       DBANK02P
002500                                                                  DBANK02P
002600 ENVIRONMENT DIVISION.                                            DBANK02P
002700                                                                  DBANK02P
002800 DATA DIVISION.                                                   DBANK02P
002900                                                                  DBANK02P
003000 WORKING-STORAGE SECTION.                                         DBANK02P
003100 01  WS-MISC-STORAGE.                                             DBANK02P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK02P
003300       VALUE 'DBANK02P'.                                          DBANK02P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK02P
003500   05  WS-RESP                               PIC S9(8) COMP.      DBANK02P
003600   05  WS-BNKCUST-RID                        PIC X(5).            DBANK02P
003700                                                                  DBANK02P
003800 01 WS-BNKCUST-REC.                                               DBANK02P
003900 COPY CBANKVCS.                                                   DBANK02P
004000                                                                  DBANK02P
004100 01  WS-COMMAREA.                                                 DBANK02P
004200 COPY CBANKD02.                                                   DBANK02P
004300                                                                  DBANK02P
004400 COPY CABENDD.                                                    DBANK02P
004500                                                                  DBANK02P
004600 LINKAGE SECTION.                                                 DBANK02P
004700 01  DFHCOMMAREA.                                                 DBANK02P
004800   05  LK-COMMAREA                           PIC X(1)             DBANK02P
004900       OCCURS 1 TO 4096 TIMES                                     DBANK02P
005000         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK02P
005100                                                                  DBANK02P
005200 COPY CENTRY.                                                     DBANK02P
005300***************************************************************** DBANK02P
005400* Move the passed data to our area                              * DBANK02P
005500***************************************************************** DBANK02P
005600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK02P
005700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK02P
005800                                                                  DBANK02P
005900***************************************************************** DBANK02P
006000* Initialize our output area                                    * DBANK02P
006100***************************************************************** DBANK02P
006200     MOVE SPACES TO CD02O-DATA.                                   DBANK02P
006300                                                                  DBANK02P
006400***************************************************************** DBANK02P
006500* See if we have a read or write request and react accordingly  * DBANK02P
006600***************************************************************** DBANK02P
006700     EVALUATE TRUE                                                DBANK02P
006800       WHEN CD02I-READ                                            DBANK02P
006900         PERFORM READ-PROCESSING THRU                             DBANK02P
007000                 READ-PROCESSING-EXIT                             DBANK02P
007100       WHEN CD02I-WRITE                                           DBANK02P
007200         PERFORM WRITE-PROCESSING THRU                            DBANK02P
007300                 WRITE-PROCESSING-EXIT                            DBANK02P
007400       WHEN OTHER                                                 DBANK02P
007500         MOVE HIGH-VALUES TO CD02O-CONTACT-ID                     DBANK02P
007600         MOVE 'Bad request code' TO CD02O-CONTACT-NAME            DBANK02P
007700     END-EVALUATE.                                                DBANK02P
007800                                                                  DBANK02P
007900***************************************************************** DBANK02P
008000* Move the result back to the callers area                      * DBANK02P
008100***************************************************************** DBANK02P
008200     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK02P
008300                                                                  DBANK02P
008400***************************************************************** DBANK02P
008500* Return to our caller                                          * DBANK02P
008600***************************************************************** DBANK02P
008700 COPY CRETURN.                                                    DBANK02P
008800                                                                  DBANK02P
008900***************************************************************** DBANK02P
009000* Read request                                                  * DBANK02P
009100***************************************************************** DBANK02P
009200 READ-PROCESSING.                                                 DBANK02P
009300                                                                  DBANK02P
009400***************************************************************** DBANK02P
009500* Now attempt to get the requested record                       * DBANK02P
009600***************************************************************** DBANK02P
009700     MOVE CD02I-CONTACT-ID TO WS-BNKCUST-RID.                     DBANK02P
009800     EXEC CICS READ FILE('BNKCUST')                               DBANK02P
009900                    INTO(WS-BNKCUST-REC)                          DBANK02P
010000                    LENGTH(LENGTH OF WS-BNKCUST-REC)              DBANK02P
010100                    RIDFLD(WS-BNKCUST-RID)                        DBANK02P
010200                    RESP(WS-RESP)                                 DBANK02P
010300     END-EXEC.                                                    DBANK02P
010400                                                                  DBANK02P
010500***************************************************************** DBANK02P
010600* Did we get the record OK                                      * DBANK02P
010700***************************************************************** DBANK02P
010800     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DBANK02P
010900        MOVE BCS-REC-PID TO CD02O-CONTACT-ID                      DBANK02P
011000        MOVE BCS-REC-NAME TO CD02O-CONTACT-NAME                   DBANK02P
011100        MOVE BCS-REC-ADDR1 TO CD02O-CONTACT-ADDR1                 DBANK02P
011200        MOVE BCS-REC-ADDR2 TO CD02O-CONTACT-ADDR2                 DBANK02P
011300        MOVE BCS-REC-STATE TO CD02O-CONTACT-STATE                 DBANK02P
011400        MOVE BCS-REC-CNTRY TO CD02O-CONTACT-CNTRY                 DBANK02P
011500        MOVE BCS-REC-POST-CODE TO CD02O-CONTACT-PSTCDE            DBANK02P
011600        MOVE BCS-REC-TEL TO CD02O-CONTACT-TELNO                   DBANK02P
011700        MOVE BCS-REC-EMAIL TO CD02O-CONTACT-EMAIL                 DBANK02P
011800        MOVE BCS-REC-SEND-MAIL TO CD02O-CONTACT-SEND-MAIL         DBANK02P
011900        MOVE BCS-REC-SEND-EMAIL TO CD02O-CONTACT-SEND-EMAIL       DBANK02P
012000     END-IF.                                                      DBANK02P
012100                                                                  DBANK02P
012200***************************************************************** DBANK02P
012300* Was the record not found?                                     * DBANK02P
012400***************************************************************** DBANK02P
012500     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK02P
012600        MOVE SPACES TO CD02O-DATA                                 DBANK02P
012700        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      DBANK02P
012800        MOVE 'Bad VSAM read' TO CD02O-CONTACT-NAME                DBANK02P
012900     END-IF.                                                      DBANK02P
013000                                                                  DBANK02P
013100 READ-PROCESSING-EXIT.                                            DBANK02P
013200     EXIT.                                                        DBANK02P
013300                                                                  DBANK02P
013400***************************************************************** DBANK02P
013500* Write request                                                 * DBANK02P
013600***************************************************************** DBANK02P
013700 WRITE-PROCESSING.                                                DBANK02P
013800                                                                  DBANK02P
013900***************************************************************** DBANK02P
014000* Now attempt to get the requested record for update            * DBANK02P
014100***************************************************************** DBANK02P
014200     MOVE CD02I-CONTACT-ID TO WS-BNKCUST-RID.                     DBANK02P
014300     EXEC CICS READ FILE('BNKCUST')                               DBANK02P
014400                    UPDATE                                        DBANK02P
014500                    INTO(WS-BNKCUST-REC)                          DBANK02P
014600                    LENGTH(LENGTH OF WS-BNKCUST-REC)              DBANK02P
014700                    RIDFLD(WS-BNKCUST-RID)                        DBANK02P
014800                    RESP(WS-RESP)                                 DBANK02P
014900     END-EXEC.                                                    DBANK02P
015000                                                                  DBANK02P
015100***************************************************************** DBANK02P
015200* Did we get the record for update                              * DBANK02P
015300***************************************************************** DBANK02P
015400     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK02P
015500        MOVE SPACES TO CD02O-DATA                                 DBANK02P
015600        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      DBANK02P
015700        MOVE 'Unable to lock record' TO CD02O-CONTACT-NAME        DBANK02P
015800        GO TO WRITE-PROCESSING-EXIT                               DBANK02P
015900     END-IF.                                                      DBANK02P
016000                                                                  DBANK02P
016100     MOVE CD02I-CONTACT-ADDR1 TO BCS-REC-ADDR1.                   DBANK02P
016200     MOVE CD02I-CONTACT-ADDR2 TO BCS-REC-ADDR2.                   DBANK02P
016300     MOVE CD02I-CONTACT-STATE TO BCS-REC-STATE.                   DBANK02P
016400     MOVE CD02I-CONTACT-CNTRY TO BCS-REC-CNTRY.                   DBANK02P
016500     MOVE CD02I-CONTACT-PSTCDE TO BCS-REC-POST-CODE.              DBANK02P
016600     MOVE CD02I-CONTACT-STATE TO BCS-REC-STATE.                   DBANK02P
016700     MOVE CD02I-CONTACT-EMAIL TO BCS-REC-EMAIL.                   DBANK02P
016800     MOVE CD02I-CONTACT-SEND-MAIL TO BCS-REC-SEND-MAIL.           DBANK02P
016900     MOVE CD02I-CONTACT-SEND-EMAIL TO BCS-REC-SEND-EMAIL.         DBANK02P
017000     EXEC CICS REWRITE FILE('BNKCUST')                            DBANK02P
017100                       FROM(WS-BNKCUST-REC)                       DBANK02P
017200                       LENGTH(LENGTH OF WS-BNKCUST-REC)           DBANK02P
017300                       RESP(WS-RESP)                              DBANK02P
017400     END-EXEC.                                                    DBANK02P
017500                                                                  DBANK02P
017600***************************************************************** DBANK02P
017700* Did we update the record OK                                   * DBANK02P
017800***************************************************************** DBANK02P
017900     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DBANK02P
018000        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      DBANK02P
018100        MOVE 'Update OK' TO CD02O-CONTACT-NAME                    DBANK02P
018200     END-IF.                                                      DBANK02P
018300                                                                  DBANK02P
018400***************************************************************** DBANK02P
018500* The record update failed                                      * DBANK02P
018600***************************************************************** DBANK02P
018700     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   DBANK02P
018800        MOVE SPACES TO CD02O-DATA                                 DBANK02P
018900        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      DBANK02P
019000        MOVE 'Update failed' TO CD02O-CONTACT-NAME                DBANK02P
019100     END-IF.                                                      DBANK02P
019200                                                                  DBANK02P
019300 WRITE-PROCESSING-EXIT.                                           DBANK02P
019400     EXIT.                                                        DBANK02P
019500                                                                  DBANK02P
019600* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANK02P
