000100***************************************************************** DBANKIOP
000200*                                                               * DBANKIOP
000300*   Copyright (C) 1998-2007 Micro Focus. All Rights Reserved.   * DBANKIOP
000400*   This demonstration program is provided for use by users     * DBANKIOP
000500*   of Micro Focus products and may be used, modified and       * DBANKIOP
000600*   distributed as part of your application provided that       * DBANKIOP
000700*   you properly acknowledge the copyright of Micro Focus       * DBANKIOP
000800*   in this material.                                           * DBANKIOP
000900*                                                               * DBANKIOP
001000***************************************************************** DBANKIOP
001100                                                                  DBANKIOP
001200***************************************************************** DBANKIOP
001300* Program:     DBANKIOP.CBL                                     * DBANKIOP
001400* Function:    Return data access method                        * DBANKIOP
001500*              VSAM version                                     * DBANKIOP
001600***************************************************************** DBANKIOP
001700                                                                  DBANKIOP
001800 IDENTIFICATION DIVISION.                                         DBANKIOP
001900 PROGRAM-ID.                                                      DBANKIOP
002000     DBANKIOP.                                                    DBANKIOP
002100 DATE-WRITTEN.                                                    DBANKIOP
002200     September 2002.                                              DBANKIOP
002300 DATE-COMPILED.                                                   DBANKIOP
002400     Today.                                                       DBANKIOP
002500                                                                  DBANKIOP
002600 ENVIRONMENT DIVISION.                                            DBANKIOP
002700                                                                  DBANKIOP
002800 DATA DIVISION.                                                   DBANKIOP
002900                                                                  DBANKIOP
003000 WORKING-STORAGE SECTION.                                         DBANKIOP
003100 01  WS-MISC-STORAGE.                                             DBANKIOP
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANKIOP
003300       VALUE 'DBANKIOP'.                                          DBANKIOP
003400                                                                  DBANKIOP
003500 LINKAGE SECTION.                                                 DBANKIOP
003600 01  LK-PASS-AREA                            PIC X(6).            DBANKIOP
003700                                                                  DBANKIOP
003800 PROCEDURE DIVISION USING LK-PASS-AREA.                           DBANKIOP
003900***************************************************************** DBANKIOP
004000* Move the data to the passed area                              * DBANKIOP
004100***************************************************************** DBANKIOP
004200     MOVE 'VSM   ' TO LK-PASS-AREA.                               DBANKIOP
004300                                                                  DBANKIOP
004400***************************************************************** DBANKIOP
004500* Return to our caller                                          * DBANKIOP
004600***************************************************************** DBANKIOP
004700     GOBACK.                                                      DBANKIOP
004800                                                                  DBANKIOP
004900* $ Version 5.94a sequenced on Tuesday 18 Sep 2007 at 4:00pm      DBANKIOP
