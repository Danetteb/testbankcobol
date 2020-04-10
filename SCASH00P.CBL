000100***************************************************************** SCASH00P
000200*                                                               * SCASH00P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SCASH00P
000400*   This demonstration program is provided for use by users     * SCASH00P
000500*   of Micro Focus products and may be used, modified and       * SCASH00P
000600*   distributed as part of your application provided that       * SCASH00P
000700*   you properly acknowledge the copyright of Micro Focus       * SCASH00P
000800*   in this material.                                           * SCASH00P
000900*                                                               * SCASH00P
001000***************************************************************** SCASH00P
001100                                                                  SCASH00P
001200***************************************************************** SCASH00P
001300* Program:     SCASH00P.CBL (CICS Version)                      * SCASH00P
001400* Layer:       'Screen' handling                                * SCASH00P
001500* Function:    "Screen" routing module                          * SCASH00P
001600***************************************************************** SCASH00P
001700                                                                  SCASH00P
001800 IDENTIFICATION DIVISION.                                         SCASH00P
001900 PROGRAM-ID.                                                      SCASH00P
002000     SCASH00P.                                                    SCASH00P
002100 DATE-WRITTEN.                                                    SCASH00P
002200     September 2002.                                              SCASH00P
002300 DATE-COMPILED.                                                   SCASH00P
002400     Today.                                                       SCASH00P
002500                                                                  SCASH00P
002600 ENVIRONMENT DIVISION.                                            SCASH00P
002700                                                                  SCASH00P
002800 DATA DIVISION.                                                   SCASH00P
002900 WORKING-STORAGE SECTION.                                         SCASH00P
003000 01  WS-MISC-STORAGE.                                             SCASH00P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SCASH00P
003200       VALUE 'SCASH00P'.                                          SCASH00P
003300   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SCASH00P
003400   05  WS-TRAN-ID                            PIC X(4).            SCASH00P
003500   05  WS-SCREEN-FUNCTION-PGM                PIC X(8)             SCASH00P
003600       VALUE SPACES.                                              SCASH00P
003700   05  WS-DYNAMIC-PGM                        PIC X(8)             SCASH00P
003800       VALUE 'UNKNOWN'.                                           SCASH00P
003900   05  WS-TSQ-IO                             PIC X(8).            SCASH00P
004000                                                                  SCASH00P
004100                                                                  SCASH00P
004200 01  WS-CASH-DATA-AREAS.                                          SCASH00P
004300   05  WS-CASH-DATA.                                              SCASH00P
004400 COPY CCASHDAT.                                                   SCASH00P
004500   05  WS-CASH-EXT-DATA.                                          SCASH00P
004600 COPY CCASHEXT.                                                   SCASH00P
004700                                                                  SCASH00P
004800 COPY CABENDD.                                                    SCASH00P
004900                                                                  SCASH00P
005000 LINKAGE SECTION.                                                 SCASH00P
005100 01  DFHCOMMAREA.                                                 SCASH00P
005200   05  FILLER                                PIC X(1024).         SCASH00P
005300                                                                  SCASH00P
005400 PROCEDURE DIVISION.                                              SCASH00P
005500***************************************************************** SCASH00P
005600* Write entry to log to show we have been invoked               * SCASH00P
005700***************************************************************** SCASH00P
005800*    COPY CTRACE.                                                 SCASH00P
005900                                                                  SCASH00P
006000***************************************************************** SCASH00P
006100* Store our transaction-id                                      * SCASH00P
006200***************************************************************** SCASH00P
006300*    call 'CBL_DEBUGBREAK'.                                       SCASH00P
006400     MOVE EIBTRNID TO WS-TRAN-ID.                                 SCASH00P
006500     MOVE EIBCALEN TO WS-SAVED-EIBCALEN.                          SCASH00P
006600                                                                  SCASH00P
006700     IF EIBCALEN IS EQUAL TO 0                                    SCASH00P
006800        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SCASH00P
006900        MOVE '0001' TO ABEND-CODE                                 SCASH00P
007000        MOVE 'EIBCALEN 0. No COMMAREA' TO ABEND-REASON            SCASH00P
007100        COPY CABENDPO.                                            SCASH00P
007200     ELSE                                                         SCASH00P
007300        MOVE SPACES TO WS-CASH-DATA                               SCASH00P
007400        MOVE DFHCOMMAREA(1:WS-SAVED-EIBCALEN)                     SCASH00P
007500          TO WS-CASH-EXT-DATA(1:WS-SAVED-EIBCALEN)                SCASH00P
007600        MOVE 'INPUT' TO WS-TSQ-IO                                 SCASH00P
007700        EXEC CICS WRITEQ TS                                       SCASH00P
007800                         QUEUE('ATM')                             SCASH00P
007900                         FROM(WS-TSQ-IO)                          SCASH00P
008000                         LENGTH(LENGTH OF WS-TSQ-IO)              SCASH00P
008100        END-EXEC                                                  SCASH00P
008200        EXEC CICS WRITEQ TS                                       SCASH00P
008300                         QUEUE('ATM')                             SCASH00P
008400                         FROM(WS-CASH-EXT-DATA)                   SCASH00P
008500                         LENGTH(WS-SAVED-EIBCALEN)                SCASH00P
008600        END-EXEC                                                  SCASH00P
008700     END-IF.                                                      SCASH00P
008800                                                                  SCASH00P
008900***************************************************************** SCASH00P
009000* This is the main process                                      * SCASH00P
009100***************************************************************** SCASH00P
009200     IF NOT ATM-FUNC-VALID                                        SCASH00P
009300        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SCASH00P
009400        MOVE '0002' TO ABEND-CODE                                 SCASH00P
009500        MOVE 'ATM-IP00-FUNC invalid' TO ABEND-REASON              SCASH00P
009600        COPY CABENDPO.                                            SCASH00P
009700     END-IF.                                                      SCASH00P
009800                                                                  SCASH00P
009900***************************************************************** SCASH00P
010000* Determine what we have to do                                  * SCASH00P
010100***************************************************************** SCASH00P
010200                                                                  SCASH00P
010300* Call the appropriate routine to handle the business logic       SCASH00P
010400     IF ATM-FUNC-VALID                                            SCASH00P
010500        MOVE 'SCASH??P' TO WS-SCREEN-FUNCTION-PGM                 SCASH00P
010600        MOVE ATM-IP00-FUNCTION TO WS-SCREEN-FUNCTION-PGM(6:2)     SCASH00P
010700        EXEC CICS LINK PROGRAM(WS-SCREEN-FUNCTION-PGM)            SCASH00P
010800                       COMMAREA(WS-CASH-EXT-DATA)                 SCASH00P
010900                       LENGTH(LENGTH OF WS-CASH-EXT-DATA)         SCASH00P
011000        END-EXEC                                                  SCASH00P
011100     END-IF.                                                      SCASH00P
011200                                                                  SCASH00P
011300***************************************************************** SCASH00P
011400* Now we have to have finished and can return to our invoker.   * SCASH00P
011500***************************************************************** SCASH00P
011600* Now return to CICS                                              SCASH00P
011700     IF WS-SAVED-EIBCALEN IS GREATER THAN 0                       SCASH00P
011800        MOVE WS-CASH-EXT-DATA(1:WS-SAVED-EIBCALEN)                SCASH00P
011900          TO DFHCOMMAREA(1:WS-SAVED-EIBCALEN)                     SCASH00P
012000        MOVE 'OUPUT' TO WS-TSQ-IO                                 SCASH00P
012100        EXEC CICS WRITEQ TS                                       SCASH00P
012200                         QUEUE('ATM')                             SCASH00P
012300                         FROM(WS-TSQ-IO)                          SCASH00P
012400                         LENGTH(LENGTH OF WS-TSQ-IO)              SCASH00P
012500        END-EXEC                                                  SCASH00P
012600        EXEC CICS WRITEQ TS                                       SCASH00P
012700                         QUEUE('ATM')                             SCASH00P
012800                         FROM(WS-CASH-EXT-DATA)                   SCASH00P
012900                         LENGTH(WS-SAVED-EIBCALEN)                SCASH00P
013000        END-EXEC                                                  SCASH00P
013100     END-IF.                                                      SCASH00P
013200                                                                  SCASH00P
013300     EXEC CICS                                                    SCASH00P
013400          RETURN                                                  SCASH00P
013500     END-EXEC.                                                    SCASH00P
013600     GOBACK.                                                      SCASH00P
013700                                                                  SCASH00P
013800* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SCASH00P
