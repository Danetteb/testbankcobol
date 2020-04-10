000100***************************************************************** SCHAR00P
000200*                                                               * SCHAR00P
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * SCHAR00P
000400*   This demonstration program is provided for use by users     * SCHAR00P
000500*   of Micro Focus products and may be used, modified and       * SCHAR00P
000600*   distributed as part of your application provided that       * SCHAR00P
000700*   you properly acknowledge the copyright of Micro Focus       * SCHAR00P
000800*   in this material.                                           * SCHAR00P
000900*                                                               * SCHAR00P
001000***************************************************************** SCHAR00P
001100                                                                  SCHAR00P
001200***************************************************************** SCHAR00P
001300* Program:     SCHAR00P.CBL (CICS Version)                      * SCHAR00P
001400* Layer:       Screen handling                                  * SCHAR00P
001500* Function:    Determine charcater set (EBCDIC/ASCII)           * SCHAR00P
001600***************************************************************** SCHAR00P
001700                                                                  SCHAR00P
001800 IDENTIFICATION DIVISION.                                         SCHAR00P
001900 PROGRAM-ID.                                                      SCHAR00P
002000     SCHAR00P.                                                    SCHAR00P
002100 DATE-WRITTEN.                                                    SCHAR00P
002200     September 2002.                                              SCHAR00P
002300 DATE-COMPILED.                                                   SCHAR00P
002400     Today.                                                       SCHAR00P
002500                                                                  SCHAR00P
002600 ENVIRONMENT DIVISION.                                            SCHAR00P
002700                                                                  SCHAR00P
002800 DATA DIVISION.                                                   SCHAR00P
002900 WORKING-STORAGE SECTION.                                         SCHAR00P
003000 01  WS-MISC-STORAGE.                                             SCHAR00P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SCHAR00P
003200       VALUE 'SCHAR00P'.                                          SCHAR00P
003300                                                                  SCHAR00P
003400*COPY CABENDD.                                                    SCHAR00P
003500                                                                  SCHAR00P
003600                                                                  SCHAR00P
003700 01  ws-msg.                                                      SCHAR00P
003800   05  filler                                pic x(20)            SCHAR00P
003900       value 'SCHAR00P - EIBCALEN='.                              SCHAR00P
004000   05  ws-msg-calen                          pic 9(5).            SCHAR00P
004100   05  filler                                pic x(12)            SCHAR00P
004200       value ', EIBTRNID=<'.                                      SCHAR00P
004300   05  ws-msg-tran                           pic x(4).            SCHAR00P
004400   05  filler                                pic x(1)             SCHAR00P
004500       value '>'.                                                 SCHAR00P
004600                                                                  SCHAR00P
004700 LINKAGE SECTION.                                                 SCHAR00P
004800 01  DFHCOMMAREA.                                                 SCHAR00P
004900   05  LK-CHARSET-SPACES                     PIC X(1)             SCHAR00P
005000       OCCURS 1 TO 32000 TIMES DEPENDING ON EIBCALEN.             SCHAR00P
005100                                                                  SCHAR00P
005200 PROCEDURE DIVISION.                                              SCHAR00P
005300***************************************************************** SCHAR00P
005400* Move spaces to COMMAREA. This will be tested elsewhere to     * SCHAR00P
005500* determine if we are in EBCDIC or ASCII. Spaces are x'40' in   * SCHAR00P
005600* EBCDIC and x'20' in ASCII.                                    * SCHAR00P
005700***************************************************************** SCHAR00P
005800     move eibcalen to ws-msg-calen.                               SCHAR00P
005900     move eibtrnid to ws-msg-tran.                                SCHAR00P
006000*    exec cics write operator                                     SCHAR00P
006100*                    text(ws-msg)                                 SCHAR00P
006200*                    textlength(length of ws-msg)                 SCHAR00P
006300*    end-exec.                                                    SCHAR00P
006400                                                                  SCHAR00P
006500     IF EIBCALEN IS NOT EQUAL TO 0                                SCHAR00P
006600        MOVE SPACES TO DFHCOMMAREA(1:EIBCALEN)                    SCHAR00P
006700     END-IF.                                                      SCHAR00P
006800                                                                  SCHAR00P
006900***************************************************************** SCHAR00P
007000* Now we have to have finished and can return to our invoker.   * SCHAR00P
007100***************************************************************** SCHAR00P
007200     EXEC CICS                                                    SCHAR00P
007300          RETURN                                                  SCHAR00P
007400     END-EXEC.                                                    SCHAR00P
007500     GOBACK.                                                      SCHAR00P
007600                                                                  SCHAR00P
007700* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SCHAR00P
