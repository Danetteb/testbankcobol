000100***************************************************************** SBANK00P
000200*                                                               * SBANK00P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SBANK00P
000400*   This demonstration program is provided for use by users     * SBANK00P
000500*   of Micro Focus products and may be used, modified and       * SBANK00P
000600*   distributed as part of your application provided that       * SBANK00P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK00P
000800*   in this material.                                           * SBANK00P
000900*                                                               * SBANK00P
001000***************************************************************** SBANK00P
001100                                                                  SBANK00P
001200***************************************************************** SBANK00P
001300* Program:     SBANK00P.CBL (CICS Version)                      * SBANK00P
001400* Layer:       Screen handling                                  * SBANK00P
001500* Function:    Screen handling control module                   * SBANK00P
001600***************************************************************** SBANK00P
001700                                                                  SBANK00P
001800 IDENTIFICATION DIVISION.                                         SBANK00P
001900 PROGRAM-ID.                                                      SBANK00P
002000     SBANK00P.                                                    SBANK00P
002100 DATE-WRITTEN.                                                    SBANK00P
002200     September 2002.                                              SBANK00P
002300 DATE-COMPILED.                                                   SBANK00P
002400     Today.                                                       SBANK00P
002500                                                                  SBANK00P
002600 ENVIRONMENT DIVISION.                                            SBANK00P
002700                                                                  SBANK00P
002800 DATA DIVISION.                                                   SBANK00P
002900 WORKING-STORAGE SECTION.                                         SBANK00P
003000 01  WS-MISC-STORAGE.                                             SBANK00P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK00P
003200       VALUE 'SBANK00P'.                                          SBANK00P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK00P
003400   05  WS-SCREEN-LOGIC-PGM                   PIC X(8)             SBANK00P
003500       VALUE SPACES.                                              SBANK00P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK00P
003700       VALUE 'UNKNOWN'.                                           SBANK00P
003800   05  WS-ABSTIME                            PIC S9(15) COMP-3.   SBANK00P
003900   05  WS-RESP                               PIC S9(8) COMP.      SBANK00P
004000   05  WS-INPUT-SOURCE-MSG.                                       SBANK00P
004100     10  FILLER                              PIC X(20)            SBANK00P
004200         VALUE 'Input received from '.                            SBANK00P
004300     10  WS-INPUT-SOURCE-MSG-CALL-TYPE       PIC X(8).            SBANK00P
004400 01  WS-BANK-DATA-AREAS.                                          SBANK00P
004500   05  WS-BANK-DATA.                                              SBANK00P
004600 COPY CBANKDAT.                                                   SBANK00P
004700   05  WS-BANK-EXT-DATA.                                          SBANK00P
004800 COPY CBANKEXT.                                                   SBANK00P
004900                                                                  SBANK00P
005000 01  TS-DATA.                                                     SBANK00P
005100   05  TS-QUEUE-NAME                         PIC X(8).            SBANK00P
005200   05  TS-QUEUE-NAME-PARTS REDEFINES TS-QUEUE-NAME.               SBANK00P
005300     10  TS-QUEUE-NAME-PART1                 PIC X(4).            SBANK00P
005400     10  TS-QUEUE-NAME-PART2                 PIC 9(4).            SBANK00P
005500   05  TS-QUEUE-LEN                          PIC S9(4) COMP.      SBANK00P
005600   05  TS-QUEUE-ITEM                         PIC S9(4) COMP.      SBANK00P
005700   05  TS-QUEUE-DATA                         PIC X(6144).         SBANK00P
005800                                                                  SBANK00P
005900 COPY DFHAID.                                                     SBANK00P
006000                                                                  SBANK00P
006100 COPY DFHBMSCA.                                                   SBANK00P
006200                                                                  SBANK00P
006300 COPY CABENDD.                                                    SBANK00P
006400                                                                  SBANK00P
006500 01  load-ptr pointer.                                            SBANK00P
006600                                                                  SBANK00P
006700                                                                  SBANK00P
006800 LINKAGE SECTION.                                                 SBANK00P
006900 01  DFHCOMMAREA.                                                 SBANK00P
007000   05  LK-TS-QUEUE-NAME                      PIC X(8).            SBANK00P
007100   05  LK-CALL-TYPE                          PIC X(8).            SBANK00P
007200     88  CALL-TYPE-CICSECI                   VALUE 'CICSECI'.     SBANK00P
007300     88  CALL-TYPE-WEBSERV                   VALUE 'WEBSERV'.     SBANK00P
007400   05  LK-PASSED-DATA                        PIC X(1024).         SBANK00P
007500*COPY CBANKEXT.                                                   SBANK00P
007600                                                                  SBANK00P
007700 PROCEDURE DIVISION.                                              SBANK00P
007800***************************************************************** SBANK00P
007900* Write entry to log to show we have been invoked               * SBANK00P
008000***************************************************************** SBANK00P
008100     COPY CTRACE.                                                 SBANK00P
008200                                                                  SBANK00P
008300***************************************************************** SBANK00P
008400* Store our transaction-id                                      * SBANK00P
008500***************************************************************** SBANK00P
008600     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK00P
008700                                                                  SBANK00P
008800***************************************************************** SBANK00P
008900* If we have a commarea then its either not the first time in   * SBANK00P
009000* from a terminal or we have come from other than a terminal so * SBANK00P
009100* display the call type so we know where we came from           * SBANK00P
009200***************************************************************** SBANK00P
009300     IF EIBCALEN IS NOT LESS THAN 8                               SBANK00P
009400        IF CALL-TYPE-CICSECI OR                                   SBANK00P
009500           CALL-TYPE-WEBSERV                                      SBANK00P
009600           MOVE LK-CALL-TYPE TO WS-INPUT-SOURCE-MSG-CALL-TYPE     SBANK00P
009700*          EXEC CICS WRITE OPERATOR                               SBANK00P
009800*                    TEXT(WS-INPUT-SOURCE-MSG)                    SBANK00P
009900*                    TEXTLENGTH(LENGTH OF WS-INPUT-SOURCE-MSG)    SBANK00P
010000*          END-EXEC                                               SBANK00P
010100         END-IF                                                   SBANK00P
010200      END-IF.                                                     SBANK00P
010300                                                                  SBANK00P
010400***************************************************************** SBANK00P
010500* If this is the first time in, then we assume we are running   * SBANK00P
010600* from a CICS terminal so we display map BANK10M and return with* SBANK00P
010700* with our COMMAREA set up.                                     * SBANK00P
010800***************************************************************** SBANK00P
010900     IF (EIBCALEN IS EQUAL TO 0) OR                               SBANK00P
011000        (EIBCALEN IS NOT EQUAL TO 0 AND                           SBANK00P
011100         LK-TS-QUEUE-NAME IS EQUAL TO 'INET****')                 SBANK00P
011200        MOVE LOW-VALUES TO WS-BANK-DATA-AREAS                     SBANK00P
011300        IF EIBCALEN IS EQUAL TO 0                                 SBANK00P
011400           SET BANK-ENV-CICS TO TRUE                              SBANK00P
011500           EXEC CICS RETRIEVE                                     SBANK00P
011600                     INTO(TS-DATA)                                SBANK00P
011700                     LENGTH(LENGTH OF TS-DATA)                    SBANK00P
011800                     RESP(WS-RESP)                                SBANK00P
011900           END-EXEC                                               SBANK00P
012000           IF TS-DATA(1:7) IS EQUAL TO 'COLOUR='                  SBANK00P
012100              MOVE TS-DATA(8:1) TO BANK-COLOUR-SETTING            SBANK00P
012200           END-IF                                                 SBANK00P
012300        ELSE                                                      SBANK00P
012400           SET BANK-ENV-INET TO TRUE                              SBANK00P
012500        END-IF                                                    SBANK00P
012600        SET BANK-NO-CONV-IN-PROGRESS TO TRUE                      SBANK00P
012700        MOVE SPACES TO BANK-LAST-MAPSET                           SBANK00P
012800        MOVE SPACES TO BANK-LAST-MAP                              SBANK00P
012900        MOVE SPACES TO BANK-LAST-PROG                             SBANK00P
013000        MOVE SPACES TO BANK-NEXT-PROG                             SBANK00P
013100        MOVE WS-TRAN-ID TO BANK-CICS-TRANCODE                     SBANK00P
013200        EXEC CICS ASKTIME                                         SBANK00P
013300                  ABSTIME(WS-ABSTIME)                             SBANK00P
013400        END-EXEC                                                  SBANK00P
013500        MOVE BANK-ENV TO TS-QUEUE-NAME-PART1                      SBANK00P
013600        MOVE WS-ABSTIME TO TS-QUEUE-NAME-PART2                    SBANK00P
013700        EXEC CICS DELETEQ TS                                      SBANK00P
013800                  QUEUE(TS-QUEUE-NAME)                            SBANK00P
013900                  RESP(WS-RESP)                                   SBANK00P
014000        END-EXEC                                                  SBANK00P
014100        MOVE SPACES TO TS-QUEUE-DATA                              SBANK00P
014200        MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN              SBANK00P
014300        MOVE 0 TO TS-QUEUE-ITEM                                   SBANK00P
014400        MOVE 0 TO WS-RESP                                         SBANK00P
014500        EXEC CICS WRITEQ TS                                       SBANK00P
014600                  QUEUE(TS-QUEUE-NAME)                            SBANK00P
014700                  FROM(TS-QUEUE-DATA)                             SBANK00P
014800                  LENGTH(TS-QUEUE-LEN)                            SBANK00P
014900                  ITEM(TS-QUEUE-ITEM)                             SBANK00P
015000                  RESP(WS-RESP)                                   SBANK00P
015100        END-EXEC                                                  SBANK00P
015200        IF BANK-ENV-INET                                          SBANK00P
015300           MOVE TS-QUEUE-NAME TO LK-TS-QUEUE-NAME                 SBANK00P
015400        END-IF                                                    SBANK00P
015500     ELSE                                                         SBANK00P
015600        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK00P
015700        MOVE LK-TS-QUEUE-NAME TO TS-QUEUE-NAME                    SBANK00P
015800        MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN              SBANK00P
015900        MOVE 1 TO TS-QUEUE-ITEM                                   SBANK00P
016000        EXEC CICS READQ TS                                        SBANK00P
016100                  QUEUE(TS-QUEUE-NAME)                            SBANK00P
016200                  INTO(TS-QUEUE-DATA)                             SBANK00P
016300                  LENGTH(TS-QUEUE-LEN)                            SBANK00P
016400                  ITEM(TS-QUEUE-ITEM)                             SBANK00P
016500        END-EXEC                                                  SBANK00P
016600        MOVE TS-QUEUE-DATA TO WS-BANK-DATA                        SBANK00P
016700        IF BANK-ENV-INET                                          SBANK00P
016800           MOVE LK-PASSED-DATA (1:EIBCALEN) TO WS-BANK-EXT-DATA   SBANK00P
016900           IF CALL-TYPE-WEBSERV                                   SBANK00P
017000              INSPECT WS-BANK-EXT-DATA REPLACING ALL '~' BY       SBANK00P
017100                LOW-VALUES                                        SBANK00P
017200           END-IF                                                 SBANK00P
017300        END-IF                                                    SBANK00P
017400     END-IF.                                                      SBANK00P
017500                                                                  SBANK00P
017600***************************************************************** SBANK00P
017700* If we get this far then this is not the first time in as we   * SBANK00P
017800* have a COMMAREA. Check that BANK-ENV is set correctly to      * SBANK00P
017900* ensure we are running in the correct environment etc          * SBANK00P
018000***************************************************************** SBANK00P
018100     IF NOT BANK-ENV-CICS AND                                     SBANK00P
018200        NOT BANK-ENV-INET                                         SBANK00P
018300        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK00P
018400        MOVE 'S001' TO ABEND-CODE                                 SBANK00P
018500        MOVE 'Inavlid environment' TO ABEND-REASON                SBANK00P
018600        COPY CABENDPO.                                            SBANK00P
018700     END-IF.                                                      SBANK00P
018800                                                                  SBANK00P
018900***************************************************************** SBANK00P
019000* This is the main process                                      * SBANK00P
019100***************************************************************** SBANK00P
019200                                                                  SBANK00P
019300***************************************************************** SBANK00P
019400* Map the AID in the EIB to our common area                     * SBANK00P
019500***************************************************************** SBANK00P
019600     IF BANK-ENV-INET                                             SBANK00P
019700        MOVE EXT-IP-AID TO BANK-AID                               SBANK00P
019800     ELSE                                                         SBANK00P
019900        EVALUATE TRUE                                             SBANK00P
020000          WHEN EIBAID IS EQUAL TO DFHENTER                        SBANK00P
020100            SET BANK-AID-ENTER TO TRUE                            SBANK00P
020200          WHEN EIBAID IS EQUAL TO DFHCLEAR                        SBANK00P
020300            SET BANK-AID-CLEAR TO TRUE                            SBANK00P
020400          WHEN EIBAID IS EQUAL TO DFHPA1                          SBANK00P
020500            SET BANK-AID-PA1   TO TRUE                            SBANK00P
020600          WHEN EIBAID IS EQUAL TO DFHPA2                          SBANK00P
020700            SET BANK-AID-PA2   TO TRUE                            SBANK00P
020800          WHEN EIBAID IS EQUAL TO DFHPF1                          SBANK00P
020900            SET BANK-AID-PFK01 TO TRUE                            SBANK00P
021000          WHEN EIBAID IS EQUAL TO DFHPF2                          SBANK00P
021100            SET BANK-AID-PFK02 TO TRUE                            SBANK00P
021200          WHEN EIBAID IS EQUAL TO DFHPF3                          SBANK00P
021300            SET BANK-AID-PFK03 TO TRUE                            SBANK00P
021400          WHEN EIBAID IS EQUAL TO DFHPF4                          SBANK00P
021500            SET BANK-AID-PFK04 TO TRUE                            SBANK00P
021600          WHEN EIBAID IS EQUAL TO DFHPF5                          SBANK00P
021700            SET BANK-AID-PFK05 TO TRUE                            SBANK00P
021800          WHEN EIBAID IS EQUAL TO DFHPF6                          SBANK00P
021900            SET BANK-AID-PFK06 TO TRUE                            SBANK00P
022000          WHEN EIBAID IS EQUAL TO DFHPF7                          SBANK00P
022100            SET BANK-AID-PFK07 TO TRUE                            SBANK00P
022200          WHEN EIBAID IS EQUAL TO DFHPF8                          SBANK00P
022300            SET BANK-AID-PFK08 TO TRUE                            SBANK00P
022400          WHEN EIBAID IS EQUAL TO DFHPF9                          SBANK00P
022500            SET BANK-AID-PFK09 TO TRUE                            SBANK00P
022600          WHEN EIBAID IS EQUAL TO DFHPF10                         SBANK00P
022700            SET BANK-AID-PFK10 TO TRUE                            SBANK00P
022800          WHEN EIBAID IS EQUAL TO DFHPF11                         SBANK00P
022900            SET BANK-AID-PFK11 TO TRUE                            SBANK00P
023000          WHEN EIBAID IS EQUAL TO DFHPF12                         SBANK00P
023100            SET BANK-AID-PFK12 TO TRUE                            SBANK00P
023200          WHEN EIBAID IS EQUAL TO DFHPF13                         SBANK00P
023300            SET BANK-AID-PFK01 TO TRUE                            SBANK00P
023400          WHEN EIBAID IS EQUAL TO DFHPF14                         SBANK00P
023500           SET BANK-AID-PFK02 TO TRUE                             SBANK00P
023600          WHEN EIBAID IS EQUAL TO DFHPF15                         SBANK00P
023700            SET BANK-AID-PFK03 TO TRUE                            SBANK00P
023800          WHEN EIBAID IS EQUAL TO DFHPF16                         SBANK00P
023900            SET BANK-AID-PFK04 TO TRUE                            SBANK00P
024000          WHEN EIBAID IS EQUAL TO DFHPF17                         SBANK00P
024100            SET BANK-AID-PFK05 TO TRUE                            SBANK00P
024200          WHEN EIBAID IS EQUAL TO DFHPF18                         SBANK00P
024300            SET BANK-AID-PFK06 TO TRUE                            SBANK00P
024400          WHEN EIBAID IS EQUAL TO DFHPF19                         SBANK00P
024500            SET BANK-AID-PFK07 TO TRUE                            SBANK00P
024600          WHEN EIBAID IS EQUAL TO DFHPF20                         SBANK00P
024700            SET BANK-AID-PFK08 TO TRUE                            SBANK00P
024800          WHEN EIBAID IS EQUAL TO DFHPF21                         SBANK00P
024900            SET BANK-AID-PFK09 TO TRUE                            SBANK00P
025000          WHEN EIBAID IS EQUAL TO DFHPF22                         SBANK00P
025100            SET BANK-AID-PFK10 TO TRUE                            SBANK00P
025200          WHEN EIBAID IS EQUAL TO DFHPF23                         SBANK00P
025300            SET BANK-AID-PFK11 TO TRUE                            SBANK00P
025400          WHEN EIBAID IS EQUAL TO DFHPF24                         SBANK00P
025500            SET BANK-AID-PFK12 TO TRUE                            SBANK00P
025600          WHEN OTHER                                              SBANK00P
025700            SET BANK-AID-ENTER TO TRUE                            SBANK00P
025800        END-EVALUATE                                              SBANK00P
025900     END-IF.                                                      SBANK00P
026000                                                                  SBANK00P
026100***************************************************************** SBANK00P
026200* Check the AID to see if we have to toggle the colour setting  * SBANK00P
026300***************************************************************** SBANK00P
026400     IF BANK-AID-PFK02                                            SBANK00P
026500        SET BANK-AID-ENTER TO TRUE                                SBANK00P
026600        IF COLOUR-ON                                              SBANK00P
026700           SET COLOUR-OFF TO TRUE                                 SBANK00P
026800        ELSE                                                      SBANK00P
026900           SET COLOUR-ON TO TRUE                                  SBANK00P
027000        END-IF                                                    SBANK00P
027100     END-IF.                                                      SBANK00P
027200                                                                  SBANK00P
027300***************************************************************** SBANK00P
027400* If the BANK-NEXT-PROG is not the same as BANK-LAST-PROG then  * SBANK00P
027500* we have to go to the next program                             * SBANK00P
027600***************************************************************** SBANK00P
027700 CHECK-PROGRAM-SWITCH.                                            SBANK00P
027800     IF BANK-NEXT-PROG IS NOT EQUAL TO BANK-LAST-PROG             SBANK00P
027900        EXEC CICS LINK PROGRAM(BANK-NEXT-PROG)                    SBANK00P
028000                       COMMAREA(WS-BANK-DATA-AREAS)               SBANK00P
028100                       LENGTH(LENGTH OF WS-BANK-DATA-AREAS)       SBANK00P
028200        END-EXEC                                                  SBANK00P
028300        GO TO CHECK-PROGRAM-SWITCH                                SBANK00P
028400     END-IF.                                                      SBANK00P
028500                                                                  SBANK00P
028600***************************************************************** SBANK00P
028700* We determine what the last screen displayed was and call the  * SBANK00P
028800* the appropriate routine to handle it.                         * SBANK00P
028900***************************************************************** SBANK00P
029000     EVALUATE TRUE                                                SBANK00P
029100       WHEN BANK-LAST-MAPSET IS EQUAL TO SPACES                   SBANK00P
029200         MOVE 'SBANK10P' TO WS-SCREEN-LOGIC-PGM                   SBANK00P
029300       WHEN OTHER                                                 SBANK00P
029400         STRING 'SBANK' DELIMITED BY SIZE                         SBANK00P
029500                BANK-LAST-MAPSET(6:2) DELIMITED BY SIZE           SBANK00P
029600                'P' DELIMITED BY SIZE                             SBANK00P
029700           INTO WS-SCREEN-LOGIC-PGM                               SBANK00P
029800     END-EVALUATE.                                                SBANK00P
029900     SET BANK-MAP-FUNCTION-GET TO TRUE.                           SBANK00P
030000     EXEC CICS LINK PROGRAM(WS-SCREEN-LOGIC-PGM)                  SBANK00P
030100                    COMMAREA(WS-BANK-DATA-AREAS)                  SBANK00P
030200                    LENGTH(LENGTH OF WS-BANK-DATA-AREAS)          SBANK00P
030300     END-EXEC.                                                    SBANK00P
030400                                                                  SBANK00P
030500***************************************************************** SBANK00P
030600* Now we have to see what is required from the business logic   * SBANK00P
030700* Essentially the choices will be switch to another program     * SBANK00P
030800* (which will be in BANK-NEXT-PROG) or display thge next screen * SBANK00P
030900* (which will be in BANK-NEXT-MAPSET/BANK-NEXT-MAP)             * SBANK00P
031000***************************************************************** SBANK00P
031100* Check for a program switch first                                SBANK00P
031200 CHECK-FOR-PGM-SWITCH.                                            SBANK00P
031300     IF BANK-NEXT-PROG IS NOT EQUAL TO BANK-LAST-PROG             SBANK00P
031400        EXEC CICS LINK PROGRAM(BANK-NEXT-PROG)                    SBANK00P
031500                       COMMAREA(WS-BANK-DATA-AREAS)               SBANK00P
031600                       LENGTH(LENGTH OF WS-BANK-DATA-AREAS)       SBANK00P
031700        END-EXEC                                                  SBANK00P
031800        GO TO CHECK-FOR-PGM-SWITCH                                SBANK00P
031900     END-IF.                                                      SBANK00P
032000                                                                  SBANK00P
032100***************************************************************** SBANK00P
032200* We determine which screen we have to display and call the     * SBANK00P
032300* appropriate routine to handle it.                             * SBANK00P
032400***************************************************************** SBANK00P
032500*    MOVE LOW-VALUE TO MAPAREA.                                   SBANK00P
032600     STRING 'SBANK' DELIMITED BY SIZE                             SBANK00P
032700             BANK-NEXT-MAPSET(6:2) DELIMITED BY SIZE              SBANK00P
032800            'P' DELIMITED BY SIZE                                 SBANK00P
032900        INTO WS-SCREEN-LOGIC-PGM.                                 SBANK00P
033000     SET BANK-MAP-FUNCTION-PUT TO TRUE.                           SBANK00P
033100     EXEC CICS LINK PROGRAM(WS-SCREEN-LOGIC-PGM)                  SBANK00P
033200                    COMMAREA(WS-BANK-DATA-AREAS)                  SBANK00P
033300                    LENGTH(LENGTH OF WS-BANK-DATA-AREAS)          SBANK00P
033400     END-EXEC.                                                    SBANK00P
033500                                                                  SBANK00P
033600***************************************************************** SBANK00P
033700* Now we have to have finished and can return to our invoker.   * SBANK00P
033800* Before retuning, we write out any data we wish to preserve    * SBANK00P
033900* to TS. So we can retrieve this data we keep the TS queue id   * SBANK00P
034000***************************************************************** SBANK00P
034100* Now return to CICS                                              SBANK00P
034200     MOVE WS-BANK-DATA TO TS-QUEUE-DATA.                          SBANK00P
034300     MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN.                SBANK00P
034400     MOVE 1 TO TS-QUEUE-ITEM.                                     SBANK00P
034500     MOVE 0 TO WS-RESP.                                           SBANK00P
034600     EXEC CICS WRITEQ TS                                          SBANK00P
034700               QUEUE(TS-QUEUE-NAME)                               SBANK00P
034800               FROM(TS-QUEUE-DATA)                                SBANK00P
034900               LENGTH(TS-QUEUE-LEN)                               SBANK00P
035000               ITEM(TS-QUEUE-ITEM)                                SBANK00P
035100               REWRITE                                            SBANK00P
035200               RESP(WS-RESP)                                      SBANK00P
035300     END-EXEC.                                                    SBANK00P
035400                                                                  SBANK00P
035500     IF BANK-ENV-INET                                             SBANK00P
035600        IF CALL-TYPE-WEBSERV                                      SBANK00P
035700           INSPECT WS-BANK-EXT-DATA REPLACING ALL LOW-VALUES BY   SBANK00P
035800             '~'                                                  SBANK00P
035900        END-IF                                                    SBANK00P
036000        MOVE WS-BANK-EXT-DATA TO LK-PASSED-DATA                   SBANK00P
036100     END-IF.                                                      SBANK00P
036200                                                                  SBANK00P
036300     IF BANK-CICS-TRANCODE IS EQUAL TO SPACES OR                  SBANK00P
036400        BANK-ENV-INET                                             SBANK00P
036500        EXEC CICS RETURN                                          SBANK00P
036600        END-EXEC                                                  SBANK00P
036700     ELSE                                                         SBANK00P
036800        EXEC CICS RETURN                                          SBANK00P
036900                  TRANSID(BANK-CICS-TRANCODE)                     SBANK00P
037000                  COMMAREA(TS-QUEUE-NAME)                         SBANK00P
037100                  LENGTH(LENGTH OF TS-QUEUE-NAME)                 SBANK00P
037200        END-EXEC                                                  SBANK00P
037300     END-IF.                                                      SBANK00P
037400     GOBACK.                                                      SBANK00P
037500                                                                  SBANK00P
037600* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SBANK00P
