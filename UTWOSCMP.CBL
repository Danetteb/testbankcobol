000100***************************************************************** UTWOSCMP
000200*                                                               * UTWOSCMP
000300*   Copyright (C) 1998-2006 Micro Focus. All Rights Reserved.   * UTWOSCMP
000400*   This demonstration program is provided for use by users     * UTWOSCMP
000500*   of Micro Focus products and may be used, modified and       * UTWOSCMP
000600*   distributed as part of your application provided that       * UTWOSCMP
000700*   you properly acknowledge the copyright of Micro Focus       * UTWOSCMP
000800*   in this material.                                           * UTWOSCMP
000900*                                                               * UTWOSCMP
001000***************************************************************** UTWOSCMP
001100                                                                  UTWOSCMP
001200***************************************************************** UTWOSCMP
001300* Program:     UTWOSCMP.CBL                                     * UTWOSCMP
001400* Function:    ??conversion utility routine                  *    UTWOSCMP
001500***************************************************************** UTWOSCMP
001600                                                                  UTWOSCMP
001700 IDENTIFICATION DIVISION.                                         UTWOSCMP
001800 PROGRAM-ID.                                                      UTWOSCMP
001900     UTWOSCMP.                                                    UTWOSCMP
002000 DATE-WRITTEN.                                                    UTWOSCMP
002100     September 2002.                                              UTWOSCMP
002200 DATE-COMPILED.                                                   UTWOSCMP
002300     Today.                                                       UTWOSCMP
002400                                                                  UTWOSCMP
002500 ENVIRONMENT DIVISION.                                            UTWOSCMP
002600                                                                  UTWOSCMP
002700 DATA DIVISION.                                                   UTWOSCMP
002800 WORKING-STORAGE SECTION.                                         UTWOSCMP
002900 01  WS-MISC-STORAGE.                                             UTWOSCMP
003000   05  WS-PROGRAM-ID                         PIC X(8)             UTWOSCMP
003100       VALUE 'UTWOSCMP'.                                          UTWOSCMP
003200   05  WS-LEN                                PIC X(4) COMP-5.     UTWOSCMP
003300                                                                  UTWOSCMP
003400 LINKAGE SECTION.                                                 UTWOSCMP
003500 01  LK-TWOS-CMP-LEN                         PIC S9(4) COMP.      UTWOSCMP
003600 01  LK-TWOS-CMP-INPUT                       PIC X(256).          UTWOSCMP
003700 01  LK-TWOS-CMP-OUTPUT                      PIC X(256).          UTWOSCMP
003800                                                                  UTWOSCMP
003900 PROCEDURE DIVISION USING LK-TWOS-CMP-LEN                         UTWOSCMP
004000                          LK-TWOS-CMP-INPUT                       UTWOSCMP
004100                          LK-TWOS-CMP-OUTPUT.                     UTWOSCMP
004200     MOVE HIGH-VALUES TO LK-TWOS-CMP-OUTPUT (1:LK-TWOS-CMP-LEN).  UTWOSCMP
004300     MOVE LK-TWOS-CMP-LEN TO WS-LEN.                              UTWOSCMP
004400     CALL 'CBL_XOR' USING LK-TWOS-CMP-INPUT                       UTWOSCMP
004500                          LK-TWOS-CMP-OUTPUT                      UTWOSCMP
004600                          BY VALUE WS-LEN.                        UTWOSCMP
004700     GOBACK.                                                      UTWOSCMP
004800                                                                  UTWOSCMP
004900* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        UTWOSCMP
