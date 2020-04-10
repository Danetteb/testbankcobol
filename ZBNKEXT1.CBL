000100***************************************************************** ZBNKEXT1
000200*                                                               * ZBNKEXT1
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * ZBNKEXT1
000400*   This demonstration program is provided for use by users     * ZBNKEXT1
000500*   of Micro Focus products and may be used, modified and       * ZBNKEXT1
000600*   distributed as part of your application provided that       * ZBNKEXT1
000700*   you properly acknowledge the copyright of Micro Focus       * ZBNKEXT1
000800*   in this material.                                           * ZBNKEXT1
000900*                                                               * ZBNKEXT1
001000***************************************************************** ZBNKEXT1
001100$SET NOHOSTFD
001200*****************************************************************
000900* NOHOSTFD NEEDED FOR VARIABLE BLOCK FILE                       *
001200***************************************************************** ZBNKEXT1
001300* Prgram:      ZBNKEXT1.CBL                                     * ZBNKEXT1
001400* Function:    Extract data to print bank statements            * ZBNKEXT1
001500***************************************************************** ZBNKEXT1
001600 IDENTIFICATION DIVISION.                                         ZBNKEXT1
001700 PROGRAM-ID.                                                      ZBNKEXT1
001800     ZBNKEXT1.                                                    ZBNKEXT1
001900 DATE-WRITTEN.                                                    ZBNKEXT1
002000     September 2002.                                              ZBNKEXT1
002100 DATE-COMPILED.                                                   ZBNKEXT1
002200     Today.                                                       ZBNKEXT1
002300 ENVIRONMENT DIVISION.                                            ZBNKEXT1
002400 INPUT-OUTPUT   SECTION.                                          ZBNKEXT1
002500   FILE-CONTROL.                                                  ZBNKEXT1
002600     SELECT EXTRACT-FILE                                          ZBNKEXT1
002700            ASSIGN       TO EXTRACT                               ZBNKEXT1
002800            ORGANIZATION IS SEQUENTIAL                            ZBNKEXT1
002900            ACCESS MODE  IS SEQUENTIAL                            ZBNKEXT1
003000            FILE STATUS  IS WS-EXTRACT-STATUS.                    ZBNKEXT1
003100                                                                  ZBNKEXT1
003200 DATA DIVISION.                                                   ZBNKEXT1
003300 FILE SECTION.                                                    ZBNKEXT1
003400 FD  EXTRACT-FILE                                                 ZBNKEXT1
003500     RECORDING MODE IS V                                          ZBNKEXT1
003600     RECORD CONTAINS 66 TO 95 CHARACTERS.                         ZBNKEXT1
003700 COPY CBANKXT1.                                                   ZBNKEXT1
003800                                                                  ZBNKEXT1
003900 WORKING-STORAGE SECTION.                                         ZBNKEXT1
004000 COPY CTIMERD.                                                    ZBNKEXT1
004100                                                                  ZBNKEXT1
004200 01  WS-MISC-STORAGE.                                             ZBNKEXT1
004300   05  WS-PROGRAM-ID                         PIC X(8)             ZBNKEXT1
004400       VALUE 'ZBNKEXT1'.                                          ZBNKEXT1
004500   05  WS-EXTRACT-STATUS.                                         ZBNKEXT1
004600     10  WS-EXTRACT-STAT1                    PIC X(1).            ZBNKEXT1
004700     10  WS-EXTRACT-STAT2                    PIC X(1).            ZBNKEXT1
004800                                                                  ZBNKEXT1
004900   05  WS-IO-STATUS.                                              ZBNKEXT1
005000     10  WS-IO-STAT1                         PIC X(1).            ZBNKEXT1
005100     10  WS-IO-STAT2                         PIC X(1).            ZBNKEXT1
005200                                                                  ZBNKEXT1
005300   05  WS-TWO-BYTES.                                              ZBNKEXT1
005400     10  WS-TWO-BYTES-LEFT                   PIC X(1).            ZBNKEXT1
005500     10  WS-TWO-BYTES-RIGHT                  PIC X(1).            ZBNKEXT1
005600   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES                  ZBNKEXT1
005700                                             PIC 9(1) COMP.       ZBNKEXT1
005800                                                                  ZBNKEXT1
005900   05  WS-RECORD-COUNTER1                    PIC 9(5)             ZBNKEXT1
006000       VALUE ZERO.                                                ZBNKEXT1
006100   05  WS-RECORD-COUNTER2                    PIC 9(5)             ZBNKEXT1
006200       VALUE ZERO.                                                ZBNKEXT1
006300                                                                  ZBNKEXT1
006400   05  WS-LAST-PID                           PIC X(5)             ZBNKEXT1
006500       VALUE LOW-VALUES.                                          ZBNKEXT1
006600                                                                  ZBNKEXT1
006700 01  WS-ZBNKRPC1-FIELDS.                                          ZBNKEXT1
006800   05  WS-ZBNKRPC1-REQUESTED                 PIC X(1)             ZBNKEXT1
006900       VALUE LOW-VALUES.                                          ZBNKEXT1
007000     88  RPC-REQUESTED                       VALUE 'Y'.           ZBNKEXT1
007100   05  WS-ZBNKRPC1-PGM                       PIC X(8)             ZBNKEXT1
007200       VALUE SPACES.                                              ZBNKEXT1
007300   05  WS-ZBNKRPC1-IND                       PIC X(1)             ZBNKEXT1
007400       VALUE LOW-VALUES.                                          ZBNKEXT1
007500   05  WS-ZBNKRPC1-DATA.                                          ZBNKEXT1
007600     10  WS-ZBNKRPC1-DATA-PT1                PIC X(80).           ZBNKEXT1
007700     10  WS-ZBNKRPC1-DATA-PT2                PIC X(80).           ZBNKEXT1
007800                                                                  ZBNKEXT1
007900 01  WS-DATA-REPOSITORY.                                          ZBNKEXT1
008000   05  WS-DATA-ACCESS                        PIC X(3).            ZBNKEXT1
008100     88  DATA-ACCESS-DLI                     VALUE 'DLI'.         ZBNKEXT1
008200     88  DATA-ACCESS-SQL                     VALUE 'SQL'.         ZBNKEXT1
008300     88  DATA-ACCESS-VSM                     VALUE 'VSM'.         ZBNKEXT1
008400   05  WS-DATA-ACCESS-SQL-TYPE               PIC X(3).            ZBNKEXT1
008500     88  SQL-ACCESS-DB2                      VALUE 'DB2'.         ZBNKEXT1
008600     88  SQL-ACCESS-XDB                      VALUE 'XDB'.         ZBNKEXT1
008700                                                                  ZBNKEXT1
008800 01  WS-CONSOLE-MESSAGE                      PIC X(60).           ZBNKEXT1
008900                                                                  ZBNKEXT1
009000 01  WS-EXEC-PARM.                                                ZBNKEXT1
009100   05  WS-EXEC-PARM-LL                       PIC S9(4) COMP.      ZBNKEXT1
009200   05  WS-EXEC-PARM-DATA                     PIC X(12).           ZBNKEXT1
009300                                                                  ZBNKEXT1
009400 01  WS-PARM-PTR                             POINTER.             ZBNKEXT1
009500 01  WS-PARM-PTR-NUM REDEFINES WS-PARM-PTR   PIC X(4) COMP-5.     ZBNKEXT1
009600                                                                  ZBNKEXT1
009700 01  WS-COMMAREA.                                                 ZBNKEXT1
009800 COPY CIOFUNCS.                                                   ZBNKEXT1
009900 COPY CBANKD51.                                                   ZBNKEXT1
010000 COPY CBANKD52.                                                   ZBNKEXT1
010100                                                                  ZBNKEXT1
010200 COPY CABENDD.                                                    ZBNKEXT1
010300                                                                  ZBNKEXT1
010400 COPY CIMSCONS.                                                   ZBNKEXT1
010500                                                                  ZBNKEXT1
010600 COPY CIMSAIB.                                                    ZBNKEXT1
010700                                                                  ZBNKEXT1
010800 01  WS-ENV-AREA                             PIC X(200).          ZBNKEXT1
010900 01  WS-ENV-AREA-R REDEFINES WS-ENV-AREA.                         ZBNKEXT1
011000   05  WS-ENVIRON-DATA                       PIC X(100).          ZBNKEXT1
011100   05  WS-ENV-DATA REDEFINES WS-ENVIRON-DATA.                     ZBNKEXT1
011200     10  WS-ENV-ID                           PIC X(8).            ZBNKEXT1
011300     10  WS-ENV-REL                          PIC X(4).            ZBNKEXT1
011400     10  WS-ENV-CTLTYPE                      PIC X(8).            ZBNKEXT1
011500     10  WS-ENV-APPTYPE                      PIC X(8).            ZBNKEXT1
011600     10  WS-ENV-RGNID                        PIC X(4).            ZBNKEXT1
011700     10  WS-ENV-APPNAME                      PIC X(8).            ZBNKEXT1
011800     10  WS-ENV-PSBNAME                      PIC X(8).            ZBNKEXT1
011900     10  WS-ENV-TRNNAME                      PIC X(8).            ZBNKEXT1
012000     10  WS-ENV-UID                          PIC X(8).            ZBNKEXT1
012100     10  WS-ENV-GRPNAME                      PIC X(8).            ZBNKEXT1
012200     10  WS-ENV-STATUS                       PIC X(4).            ZBNKEXT1
012300     10  WS-ENV-RECTOK                       POINTER.             ZBNKEXT1
012400     10  WS-ENV-ADDRPRM                      POINTER.             ZBNKEXT1
012500     10  WS-ENV-SHRQ                         PIC X(4).            ZBNKEXT1
012600     10  WS-ENV-UADS                         PIC X(8).            ZBNKEXT1
012700     10  WS-ENV-UIND                         PIC X(4).            ZBNKEXT1
012800   05  WS-RECOVER-TOKEN                      PIC X(18).           ZBNKEXT1
012900                                                                  ZBNKEXT1
013000 LINKAGE SECTION.                                                 ZBNKEXT1
013100 01  LK-EXEC-PARM.                                                ZBNKEXT1
013200   05  LK-EXEC-PARM-LL                       PIC S9(4) COMP.      ZBNKEXT1
013300   05  LK-EXEC-PARM-DATA                     PIC X(32).           ZBNKEXT1
013400                                                                  ZBNKEXT1
013500 PROCEDURE DIVISION USING LK-EXEC-PARM.                           ZBNKEXT1
013600***************************************************************** ZBNKEXT1
013700* Perform RUN-TIME to initialse time and display start time     * ZBNKEXT1
013800***************************************************************** ZBNKEXT1
013900     PERFORM RUN-TIME.                                            ZBNKEXT1
014000                                                                  ZBNKEXT1
014100***************************************************************** ZBNKEXT1
014200* Call module to find out the data repository                   * ZBNKEXT1
014300***************************************************************** ZBNKEXT1
014400     CALL 'DBANKIOP' USING WS-DATA-REPOSITORY.                    ZBNKEXT1
014500                                                                  ZBNKEXT1
014600***************************************************************** ZBNKEXT1
014700* If we are using DLI the we will be using a IMS programs to    * ZBNKEXT1
014800* initiate this process and the exec parm is somewhat different * ZBNKEXT1
014900* to normal JCL processing.                                     * ZBNKEXT1
015000***************************************************************** ZBNKEXT1
015100     IF DATA-ACCESS-DLI                                           ZBNKEXT1
015200        MOVE LOW-VALUES TO DFSAIB                                 ZBNKEXT1
015300        MOVE LOW-VALUES TO WS-ENV-AREA                            ZBNKEXT1
015400        MOVE 'DFSAIB  ' TO AIBID                                  ZBNKEXT1
015500        MOVE LENGTH OF DFSAIB TO AIBLEN                           ZBNKEXT1
015600        MOVE 'IOPCB' TO AIBRSNM1                                  ZBNKEXT1
015700        MOVE 'ENVIRON' TO AIBSFUNC                                ZBNKEXT1
015800        MOVE LENGTH of WS-ENV-AREA to AIBAOLEN                    ZBNKEXT1
015900        CALL 'AIBTDLI' USING WS-IMS-INQY                          ZBNKEXT1
016000                             DFSAIB                               ZBNKEXT1
016100                             WS-ENV-AREA                          ZBNKEXT1
016200        IF WS-ENV-ADDRPRM IS NOT EQUAL TO NULL                    ZBNKEXT1
016300           SET ADDRESS OF LK-EXEC-PARM TO WS-ENV-ADDRPRM          ZBNKEXT1
016400           SUBTRACT LENGTH OF LK-EXEC-PARM-LL                     ZBNKEXT1
016500             FROM LK-EXEC-PARM-LL                                 ZBNKEXT1
016600        END-IF                                                    ZBNKEXT1
016700      END-IF                                                      ZBNKEXT1
016800                                                                  ZBNKEXT1
016900***************************************************************** ZBNKEXT1
017000* EXEC-CARD processing is slightly different from normal MVS    * ZBNKEXT1
017100* processing in that we check the pointer (or address) of the   * ZBNKEXT1
017200* parm area first. This is so that we can migrate it to         * ZBNKEXT1
017300* distributed (Windows/Unix) environment wihout change.         * ZBNKEXT1
017400***************************************************************** ZBNKEXT1
017500     MOVE ZEROES TO WS-EXEC-PARM-LL.                              ZBNKEXT1
017600     MOVE SPACES TO WS-EXEC-PARM-DATA.                            ZBNKEXT1
017700                                                                  ZBNKEXT1
017800     SET WS-PARM-PTR TO ADDRESS OF LK-EXEC-PARM.                  ZBNKEXT1
017900     IF WS-PARM-PTR-NUM IS NOT EQUAL TO ZEROS                     ZBNKEXT1
018000        MOVE LK-EXEC-PARM-LL TO WS-EXEC-PARM-LL                   ZBNKEXT1
018100        IF WS-EXEC-PARM-LL IS GREATER THAN                        ZBNKEXT1
018200             LENGTH OF WS-EXEC-PARM-DATA                          ZBNKEXT1
018300           MOVE LENGTH OF WS-EXEC-PARM-DATA TO WS-EXEC-PARM-LL    ZBNKEXT1
018400        END-IF                                                    ZBNKEXT1
018500        IF WS-EXEC-PARM-LL IS GREATER THAN ZERO                   ZBNKEXT1
018600           MOVE LK-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)             ZBNKEXT1
018700             TO WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)             ZBNKEXT1
018800        END-IF                                                    ZBNKEXT1
018900     END-IF.                                                      ZBNKEXT1
019000                                                                  ZBNKEXT1
019100     IF WS-EXEC-PARM-LL IS EQUAL TO ZERO                          ZBNKEXT1
019200        MOVE 'No exec card parm present'                          ZBNKEXT1
019300          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
019400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
019500        MOVE '  Selecting all records'                            ZBNKEXT1
019600          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
019700        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
019800        MOVE 3 TO WS-EXEC-PARM-LL                                 ZBNKEXT1
019900        MOVE 'ALL' TO WS-EXEC-PARM-DATA                           ZBNKEXT1
020000     ELSE                                                         ZBNKEXT1
020100       MOVE SPACES TO WS-CONSOLE-MESSAGE                          ZBNKEXT1
020200       STRING 'Exec parm is "' DELIMITED BY SIZE                  ZBNKEXT1
020300              WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)               ZBNKEXT1
020400                DELIMITED BY SIZE                                 ZBNKEXT1
020500              '"' DELIMITED BY SIZE                               ZBNKEXT1
020600         INTO WS-CONSOLE-MESSAGE                                  ZBNKEXT1
020700       PERFORM DISPLAY-CONSOLE-MESSAGE                            ZBNKEXT1
020800       MOVE SPACES TO WS-CONSOLE-MESSAGE                          ZBNKEXT1
020900       STRING '  Selecting records for ' DELIMITED BY SIZE        ZBNKEXT1
021000              WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)               ZBNKEXT1
021100                DELIMITED BY SIZE                                 ZBNKEXT1
021200              ' only' DELIMITED BY SIZE                           ZBNKEXT1
021300         INTO WS-CONSOLE-MESSAGE                                  ZBNKEXT1
021400       PERFORM DISPLAY-CONSOLE-MESSAGE                            ZBNKEXT1
021500     END-IF.                                                      ZBNKEXT1
021600     INSPECT WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)                ZBNKEXT1
021700       CONVERTING 'abcdefghijklmnopqrstuvwxyz'                    ZBNKEXT1
021800               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   ZBNKEXT1
021900                                                                  ZBNKEXT1
022000***************************************************************** ZBNKEXT1
022100* Check to see if we want to demonstrate MFE calling a module   * ZBNKEXT1
022200* that resides on the mainframe.                                * ZBNKEXT1
022300***************************************************************** ZBNKEXT1
022400     IF RPC-REQUESTED                                             ZBNKEXT1
022500        PERFORM RPC-PROCESS                                       ZBNKEXT1
022600     END-IF.                                                      ZBNKEXT1
022700                                                                  ZBNKEXT1
022800***************************************************************** ZBNKEXT1
022900* Open our output file                                          * ZBNKEXT1
023000***************************************************************** ZBNKEXT1
023100     PERFORM EXTRACT-OPEN.                                        ZBNKEXT1
023200                                                                  ZBNKEXT1
023300***************************************************************** ZBNKEXT1
023400* Open the customer details input then read the data and create * ZBNKEXT1
023500* output records as appropriate.                                * ZBNKEXT1
023600***************************************************************** ZBNKEXT1
023700     PERFORM SOURCE1-OPEN.                                        ZBNKEXT1
023800     PERFORM UNTIL IO-REQUEST-STATUS-EOF                          ZBNKEXT1
023900       IF NOT IO-REQUEST-STATUS-EOF                               ZBNKEXT1
024000          PERFORM SOURCE1-READ                                    ZBNKEXT1
024100          IF IO-REQUEST-STATUS-OK                                 ZBNKEXT1
024200             ADD 1 TO WS-RECORD-COUNTER1                          ZBNKEXT1
024300             IF WS-RECORD-COUNTER1 IS LESS THAN 6                 ZBNKEXT1
024400                MOVE WS-COMMAREA TO WS-CONSOLE-MESSAGE            ZBNKEXT1
024500                PERFORM DISPLAY-CONSOLE-MESSAGE                   ZBNKEXT1
024600             ELSE                                                 ZBNKEXT1
024700                IF WS-RECORD-COUNTER2 IS EQUAL TO 6               ZBNKEXT1
024800                   MOVE 'Suppressing record display...'           ZBNKEXT1
024900                      TO WS-CONSOLE-MESSAGE                       ZBNKEXT1
025000                   PERFORM DISPLAY-CONSOLE-MESSAGE                ZBNKEXT1
025100                END-IF                                            ZBNKEXT1
025200             END-IF                                               ZBNKEXT1
025300                                                                  ZBNKEXT1
025400             IF CD51O-PID IS NOT EQUAL TO WS-LAST-PID             ZBNKEXT1
025500                MOVE SPACES TO BANKXT01-REC1                      ZBNKEXT1
025600                MOVE '1' TO BANKXT01-1-TYPE                       ZBNKEXT1
025700                MOVE CD51O-PID TO BANKXT01-1-PID                  ZBNKEXT1
025800                MOVE CD51O-NAME TO BANKXT01-1-NAME                ZBNKEXT1
025900                MOVE CD51O-ADDR1 TO BANKXT01-1-ADDR1              ZBNKEXT1
026000                MOVE CD51O-ADDR2 TO BANKXT01-1-ADDR2              ZBNKEXT1
026100                MOVE CD51O-STATE TO BANKXT01-1-STATE              ZBNKEXT1
026200                MOVE CD51O-CNTRY TO BANKXT01-1-CNTRY              ZBNKEXT1
026300                MOVE CD51O-POST-CODE TO BANKXT01-1-PST-CDE        ZBNKEXT1
026400                PERFORM EXTRACT-PUT                               ZBNKEXT1
026500                MOVE CD51O-PID TO WS-LAST-PID                     ZBNKEXT1
026600             END-IF                                               ZBNKEXT1
026700             MOVE SPACES TO BANKXT01-REC2                         ZBNKEXT1
026800             MOVE '2' TO BANKXT01-2-TYPE                          ZBNKEXT1
026900             MOVE CD51O-PID TO BANKXT01-2-PID                     ZBNKEXT1
027000             MOVE CD51O-ACC-NO TO BANKXT01-2-ACC-NO               ZBNKEXT1
027100             MOVE CD51O-ACC-DESC TO BANKXT01-2-ACC-DESC           ZBNKEXT1
027200             MOVE CD51O-ACC-CURR-BAL TO BANKXT01-2-ACC-CURR-BAL   ZBNKEXT1
027300             MOVE CD51O-ACC-LAST-STMT-DTE                         ZBNKEXT1
027400               TO BANKXT01-2-ACC-LAST-STMT-DTE                    ZBNKEXT1
027500             MOVE CD51O-ACC-LAST-STMT-BAL                         ZBNKEXT1
027600               TO BANKXT01-2-ACC-LAST-STMT-BAL                    ZBNKEXT1
027700             PERFORM EXTRACT-PUT                                  ZBNKEXT1
027800          END-IF                                                  ZBNKEXT1
027900       END-IF                                                     ZBNKEXT1
028000     END-PERFORM.                                                 ZBNKEXT1
028100     PERFORM SOURCE1-CLOSE.                                       ZBNKEXT1
028200                                                                  ZBNKEXT1
028300***************************************************************** ZBNKEXT1
028400* Open the transactions details file then read the data and     * ZBNKEXT1
028500* create output records as appropriate.                         * ZBNKEXT1
028600***************************************************************** ZBNKEXT1
028700     PERFORM SOURCE2-OPEN.                                        ZBNKEXT1
028800     PERFORM UNTIL IO-REQUEST-STATUS-EOF                          ZBNKEXT1
028900       IF NOT IO-REQUEST-STATUS-EOF                               ZBNKEXT1
029000          PERFORM SOURCE2-READ                                    ZBNKEXT1
029100          IF IO-REQUEST-STATUS-OK                                 ZBNKEXT1
029200             ADD 1 TO WS-RECORD-COUNTER2                          ZBNKEXT1
029300             IF WS-RECORD-COUNTER2 IS LESS THAN 6                 ZBNKEXT1
029400                MOVE WS-COMMAREA TO WS-CONSOLE-MESSAGE            ZBNKEXT1
029500                PERFORM DISPLAY-CONSOLE-MESSAGE                   ZBNKEXT1
029600             ELSE                                                 ZBNKEXT1
029700                IF WS-RECORD-COUNTER2 IS EQUAL TO 6               ZBNKEXT1
029800                   MOVE 'Suppressing record display...'           ZBNKEXT1
029900                      TO WS-CONSOLE-MESSAGE                       ZBNKEXT1
030000                   PERFORM DISPLAY-CONSOLE-MESSAGE                ZBNKEXT1
030100                END-IF                                            ZBNKEXT1
030200             END-IF                                               ZBNKEXT1
030300                                                                  ZBNKEXT1
030400             MOVE SPACES TO BANKXT01-REC3                         ZBNKEXT1
030500             MOVE '3' TO BANKXT01-3-TYPE                          ZBNKEXT1
030600             MOVE CD52O-PID TO BANKXT01-3-PID                     ZBNKEXT1
030700             MOVE CD52O-ACC-NO TO BANKXT01-2-ACC-NO               ZBNKEXT1
030800             MOVE CD52O-AMOUNT TO BANKXT01-3-AMOUNT               ZBNKEXT1
030900             MOVE CD52O-TIMESTAMP TO BANKXT01-3-TIMESTAMP         ZBNKEXT1
031000             MOVE CD52O-DESC TO BANKXT01-3-DESC                   ZBNKEXT1
031100             PERFORM EXTRACT-PUT                                  ZBNKEXT1
031200          END-IF                                                  ZBNKEXT1
031300       END-IF                                                     ZBNKEXT1
031400     END-PERFORM.                                                 ZBNKEXT1
031500     PERFORM SOURCE2-CLOSE.                                       ZBNKEXT1
031600                                                                  ZBNKEXT1
031700***************************************************************** ZBNKEXT1
031800* Close our output file                                         * ZBNKEXT1
031900***************************************************************** ZBNKEXT1
032000     PERFORM EXTRACT-CLOSE.                                       ZBNKEXT1
032100                                                                  ZBNKEXT1
032200***************************************************************** ZBNKEXT1
032300* Display messages to show what we created                      * ZBNKEXT1
032400***************************************************************** ZBNKEXT1
032500     MOVE 'SOURCE data has been extracted'                        ZBNKEXT1
032600       TO WS-CONSOLE-MESSAGE.                                     ZBNKEXT1
032700     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKEXT1
032800     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBNKEXT1
032900     STRING WS-RECORD-COUNTER1 DELIMITED BY SIZE                  ZBNKEXT1
033000            ' from SOURCE1 (Customer details)'                    ZBNKEXT1
033100              DELIMITED BY SIZE                                   ZBNKEXT1
033200       INTO WS-CONSOLE-MESSAGE.                                   ZBNKEXT1
033300     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKEXT1
033400     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBNKEXT1
033500     STRING WS-RECORD-COUNTER2 DELIMITED BY SIZE                  ZBNKEXT1
033600            ' from SOURCE2 (Transactions)'                        ZBNKEXT1
033700              DELIMITED BY SIZE                                   ZBNKEXT1
033800       INTO WS-CONSOLE-MESSAGE.                                   ZBNKEXT1
033900     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKEXT1
034000     MOVE 'End Of Job'                                            ZBNKEXT1
034100       TO WS-CONSOLE-MESSAGE.                                     ZBNKEXT1
034200     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKEXT1
034300                                                                  ZBNKEXT1
034400***************************************************************** ZBNKEXT1
034500* Perform RUN-TIME to calculate run time and display end time   * ZBNKEXT1
034600***************************************************************** ZBNKEXT1
034700     PERFORM RUN-TIME.                                            ZBNKEXT1
034800                                                                  ZBNKEXT1
034900***************************************************************** ZBNKEXT1
035000* Step return code and return                                   * ZBNKEXT1
035100***************************************************************** ZBNKEXT1
035200     MOVE 0 TO RETURN-CODE.                                       ZBNKEXT1
035300                                                                  ZBNKEXT1
035400     GOBACK.                                                      ZBNKEXT1
035500                                                                  ZBNKEXT1
035600***************************************************************** ZBNKEXT1
035700* Open the source file                                          * ZBNKEXT1
035800***************************************************************** ZBNKEXT1
035900 SOURCE1-OPEN.                                                    ZBNKEXT1
036000     MOVE SPACES TO WS-COMMAREA.                                  ZBNKEXT1
036100     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.                         ZBNKEXT1
036200     SET IO-REQUEST-FUNCTION-OPEN TO TRUE.                        ZBNKEXT1
036300     CALL 'DBANK51P' USING WS-COMMAREA.                           ZBNKEXT1
036400     IF IO-REQUEST-STATUS-OK                                      ZBNKEXT1
036500        MOVE 'SOURCE1 (Customer details) file opened OK'          ZBNKEXT1
036600          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
036700        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
036800     ELSE                                                         ZBNKEXT1
036900        MOVE 'SOURCE1 (Customer details) file open failure...'    ZBNKEXT1
037000          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
037100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
037200        PERFORM ABORT-PROGRAM                                     ZBNKEXT1
037300        END-IF.                                                   ZBNKEXT1
037400 SOURCE2-OPEN.                                                    ZBNKEXT1
037500     MOVE SPACES TO WS-COMMAREA.                                  ZBNKEXT1
037600     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.                         ZBNKEXT1
037700     SET IO-REQUEST-FUNCTION-OPEN TO TRUE.                        ZBNKEXT1
037800     CALL 'DBANK52P' USING WS-COMMAREA.                           ZBNKEXT1
037900     IF IO-REQUEST-STATUS-OK                                      ZBNKEXT1
038000        MOVE 'SOURCE2 (Transactions) file opened OK'              ZBNKEXT1
038100          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
038200        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
038300     ELSE                                                         ZBNKEXT1
038400        MOVE 'SOURCE2 (Transactions) file open failure...'        ZBNKEXT1
038500          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
038600        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
038700        PERFORM ABORT-PROGRAM                                     ZBNKEXT1
038800        END-IF.                                                   ZBNKEXT1
038900                                                                  ZBNKEXT1
039000***************************************************************** ZBNKEXT1
039100* Read a record from the source file                            * ZBNKEXT1
039200***************************************************************** ZBNKEXT1
039300 SOURCE1-READ.                                                    ZBNKEXT1
039400     MOVE SPACES TO WS-COMMAREA.                                  ZBNKEXT1
039500     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.                         ZBNKEXT1
039600     SET IO-REQUEST-FUNCTION-READ TO TRUE.                        ZBNKEXT1
039700     CALL 'DBANK51P' USING WS-COMMAREA.                           ZBNKEXT1
039800     IF IO-REQUEST-STATUS-ERROR                                   ZBNKEXT1
039900        MOVE 'SOURCE1 (Customer details) Error reading file ...'  ZBNKEXT1
040000          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
040100         PERFORM DISPLAY-CONSOLE-MESSAGE                          ZBNKEXT1
040200         PERFORM ABORT-PROGRAM                                    ZBNKEXT1
040300     END-IF.                                                      ZBNKEXT1
040400 SOURCE2-READ.                                                    ZBNKEXT1
040500     MOVE SPACES TO WS-COMMAREA.                                  ZBNKEXT1
040600     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.                         ZBNKEXT1
040700     SET IO-REQUEST-FUNCTION-READ TO TRUE.                        ZBNKEXT1
040800     CALL 'DBANK52P' USING WS-COMMAREA.                           ZBNKEXT1
040900     IF IO-REQUEST-STATUS-ERROR                                   ZBNKEXT1
041000        MOVE 'SOURCE2 (Transactions) Error reading file ...'      ZBNKEXT1
041100          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
041200         PERFORM DISPLAY-CONSOLE-MESSAGE                          ZBNKEXT1
041300         PERFORM ABORT-PROGRAM                                    ZBNKEXT1
041400     END-IF.                                                      ZBNKEXT1
041500                                                                  ZBNKEXT1
041600***************************************************************** ZBNKEXT1
041700* Close the source file.                                        * ZBNKEXT1
041800***************************************************************** ZBNKEXT1
041900 SOURCE1-CLOSE.                                                   ZBNKEXT1
042000     MOVE SPACES TO WS-COMMAREA.                                  ZBNKEXT1
042100     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.                         ZBNKEXT1
042200     SET IO-REQUEST-FUNCTION-CLOSE TO TRUE.                       ZBNKEXT1
042300     CALL 'DBANK51P' USING WS-COMMAREA.                           ZBNKEXT1
042400     IF IO-REQUEST-STATUS-ERROR                                   ZBNKEXT1
042500        MOVE 'SOURCE1 (Customer details) Error closing file ...'  ZBNKEXT1
042600          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
042700         PERFORM DISPLAY-CONSOLE-MESSAGE                          ZBNKEXT1
042800         PERFORM ABORT-PROGRAM                                    ZBNKEXT1
042900     END-IF.                                                      ZBNKEXT1
043000 SOURCE2-CLOSE.                                                   ZBNKEXT1
043100     MOVE SPACES TO WS-COMMAREA.                                  ZBNKEXT1
043200     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.                         ZBNKEXT1
043300     SET IO-REQUEST-FUNCTION-CLOSE TO TRUE.                       ZBNKEXT1
043400     CALL 'DBANK52P' USING WS-COMMAREA.                           ZBNKEXT1
043500     IF IO-REQUEST-STATUS-ERROR                                   ZBNKEXT1
043600        MOVE 'SOURCE2 (Transactions) Error closing file ...'      ZBNKEXT1
043700          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
043800         PERFORM DISPLAY-CONSOLE-MESSAGE                          ZBNKEXT1
043900         PERFORM ABORT-PROGRAM                                    ZBNKEXT1
044000     END-IF.                                                      ZBNKEXT1
044100                                                                  ZBNKEXT1
044200***************************************************************** ZBNKEXT1
044300* Open the seqential extract file as output                     * ZBNKEXT1
044400***************************************************************** ZBNKEXT1
044500 EXTRACT-OPEN.                                                    ZBNKEXT1
044600     OPEN OUTPUT EXTRACT-FILE.                                    ZBNKEXT1
044700     IF WS-EXTRACT-STATUS = '00'                                  ZBNKEXT1
044800        MOVE 'EXTRACT file opened OK'                             ZBNKEXT1
044900          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
045000        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
045100     ELSE                                                         ZBNKEXT1
045200        MOVE 'EXTRACT file open failure...'                       ZBNKEXT1
045300          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
045400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
045500        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKEXT1
045600        PERFORM DISPLAY-IO-STATUS                                 ZBNKEXT1
045700        PERFORM ABORT-PROGRAM                                     ZBNKEXT1
045800        END-IF.                                                   ZBNKEXT1
045900                                                                  ZBNKEXT1
046000***************************************************************** ZBNKEXT1
046100* Write a record to the squential file                          * ZBNKEXT1
046200***************************************************************** ZBNKEXT1
046300 EXTRACT-PUT.                                                     ZBNKEXT1
046400     IF BANKXT01-1-TYPE IS EQUAL TO '1'                           ZBNKEXT1
046500        WRITE BANKXT01-REC1                                       ZBNKEXT1
046600     END-IF.                                                      ZBNKEXT1
046700     IF BANKXT01-2-TYPE IS EQUAL TO '2'                           ZBNKEXT1
046800        WRITE BANKXT01-REC2                                       ZBNKEXT1
046900     END-IF.                                                      ZBNKEXT1
047000     IF BANKXT01-3-TYPE IS EQUAL TO '3'                           ZBNKEXT1
047100        WRITE BANKXT01-REC3                                       ZBNKEXT1
047200     END-IF.                                                      ZBNKEXT1
047300     IF WS-EXTRACT-STATUS NOT = '00'                              ZBNKEXT1
047400        MOVE 'EXTRACT Error Writing file ...'                     ZBNKEXT1
047500          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
047600        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
047700        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKEXT1
047800        PERFORM DISPLAY-IO-STATUS                                 ZBNKEXT1
047900        PERFORM ABORT-PROGRAM                                     ZBNKEXT1
048000     END-IF.                                                      ZBNKEXT1
048100                                                                  ZBNKEXT1
048200***************************************************************** ZBNKEXT1
048300* Close the seqential extract file                              * ZBNKEXT1
048400***************************************************************** ZBNKEXT1
048500 EXTRACT-CLOSE.                                                   ZBNKEXT1
048600     CLOSE EXTRACT-FILE.                                          ZBNKEXT1
048700     IF WS-EXTRACT-STATUS = '00'                                  ZBNKEXT1
048800        MOVE 'EXTRACT file closed OK'                             ZBNKEXT1
048900          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
049000        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
049100     ELSE                                                         ZBNKEXT1
049200        MOVE 'EXTRACT file close failure...'                      ZBNKEXT1
049300          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
049400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
049500        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKEXT1
049600        PERFORM DISPLAY-IO-STATUS                                 ZBNKEXT1
049700        PERFORM ABORT-PROGRAM                                     ZBNKEXT1
049800     END-IF.                                                      ZBNKEXT1
049900                                                                  ZBNKEXT1
050000***************************************************************** ZBNKEXT1
050100* Display the file status bytes. This routine will display as   * ZBNKEXT1
050200* two digits if the full two byte file status is numeric. If    * ZBNKEXT1
050300* second byte is non-numeric then it will be treated as a       * ZBNKEXT1
050400* binary number.                                                * ZBNKEXT1
050500***************************************************************** ZBNKEXT1
050600 DISPLAY-IO-STATUS.                                               ZBNKEXT1
050700     IF WS-IO-STATUS NUMERIC                                      ZBNKEXT1
050800        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKEXT1
050900        STRING 'File status -' DELIMITED BY SIZE                  ZBNKEXT1
051000               WS-IO-STATUS DELIMITED BY SIZE                     ZBNKEXT1
051100          INTO WS-CONSOLE-MESSAGE                                 ZBNKEXT1
051200        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
051300     ELSE                                                         ZBNKEXT1
051400        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY     ZBNKEXT1
051500        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT                    ZBNKEXT1
051600        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKEXT1
051700        STRING 'File status -' DELIMITED BY SIZE                  ZBNKEXT1
051800               WS-IO-STAT1 DELIMITED BY SIZE                      ZBNKEXT1
051900               '/' DELIMITED BY SIZE                              ZBNKEXT1
052000               WS-TWO-BYTES DELIMITED BY SIZE                     ZBNKEXT1
052100          INTO WS-CONSOLE-MESSAGE                                 ZBNKEXT1
052200        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
052300     END-IF.                                                      ZBNKEXT1
052400                                                                  ZBNKEXT1
052500***************************************************************** ZBNKEXT1
052600* 'ABORT' the program.                                          * ZBNKEXT1
052700* Post a message to the console and issue a STOP RUN            * ZBNKEXT1
052800***************************************************************** ZBNKEXT1
052900 ABORT-PROGRAM.                                                   ZBNKEXT1
053000     IF WS-CONSOLE-MESSAGE NOT = SPACES                           ZBNKEXT1
053100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
053200     END-IF.                                                      ZBNKEXT1
053300     MOVE 'Program is abending...'  TO WS-CONSOLE-MESSAGE.        ZBNKEXT1
053400     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKEXT1
053500     MOVE 16 TO RETURN-CODE.                                      ZBNKEXT1
053600     GOBACK.                                                      ZBNKEXT1
053700                                                                  ZBNKEXT1
053800***************************************************************** ZBNKEXT1
053900* This process will attempt to call a small module which is     * ZBNKEXT1
054000* meant toreside on th emainframe                               * ZBNKEXT1
054100***************************************************************** ZBNKEXT1
054200 RPC-PROCESS.                                                     ZBNKEXT1
054300     MOVE '0' TO WS-ZBNKRPC1-IND.                                 ZBNKEXT1
054400     MOVE LOW-VALUES TO WS-ZBNKRPC1-DATA-PT1.                     ZBNKEXT1
054500     MOVE HIGH-VALUES TO WS-ZBNKRPC1-DATA-PT2.                    ZBNKEXT1
054600     MOVE 'ZBNKRPC1' TO WS-ZBNKRPC1-PGM.                          ZBNKEXT1
054700     CALL WS-ZBNKRPC1-PGM USING WS-ZBNKRPC1-DATA                  ZBNKEXT1
054800       ON EXCEPTION                                               ZBNKEXT1
054900         MOVE '1' TO WS-ZBNKRPC1-IND                              ZBNKEXT1
055000     END-CALL.                                                    ZBNKEXT1
055100     IF WS-ZBNKRPC1-IND IS EQUAL TO '1'                           ZBNKEXT1
055200        MOVE 'Call to ZBNKRPC1 failed. Program not found.'        ZBNKEXT1
055300          TO WS-CONSOLE-MESSAGE                                   ZBNKEXT1
055400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKEXT1
055500     ELSE                                                         ZBNKEXT1
055600        IF WS-ZBNKRPC1-DATA-PT1 IS EQUAL TO LOW-VALUES AND        ZBNKEXT1
055700           WS-ZBNKRPC1-DATA-PT2 IS EQUAL TO HIGH-VALUES           ZBNKEXT1
055800           MOVE 'Call to ZBNKRPC1 was to a stub program.'         ZBNKEXT1
055900             TO WS-CONSOLE-MESSAGE                                ZBNKEXT1
056000           PERFORM DISPLAY-CONSOLE-MESSAGE                        ZBNKEXT1
056100           MOVE 'Passed data area was unchanged.'                 ZBNKEXT1
056200             TO WS-CONSOLE-MESSAGE                                ZBNKEXT1
056300           PERFORM DISPLAY-CONSOLE-MESSAGE                        ZBNKEXT1
056400        ELSE                                                      ZBNKEXT1
056500           MOVE WS-ZBNKRPC1-DATA-PT1 TO WS-CONSOLE-MESSAGE        ZBNKEXT1
056600           PERFORM DISPLAY-CONSOLE-MESSAGE                        ZBNKEXT1
056700           MOVE WS-ZBNKRPC1-DATA-PT2 TO WS-CONSOLE-MESSAGE        ZBNKEXT1
056800           PERFORM DISPLAY-CONSOLE-MESSAGE                        ZBNKEXT1
056900        END-IF                                                    ZBNKEXT1
057000     END-IF.                                                      ZBNKEXT1
057100                                                                  ZBNKEXT1
057200***************************************************************** ZBNKEXT1
057300* Display CONSOLE messages...                                   * ZBNKEXT1
057400***************************************************************** ZBNKEXT1
057500 DISPLAY-CONSOLE-MESSAGE.                                         ZBNKEXT1
057600     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE.              ZBNKEXT1
057700     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE               ZBNKEXT1
057800       UPON CONSOLE.                                              ZBNKEXT1
057900     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       ZBNKEXT1
058000                                                                  ZBNKEXT1
058100 COPY CTIMERP.                                                    ZBNKEXT1
058200                                                                  ZBNKEXT1
058300* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        ZBNKEXT1
