000100 IDENTIFICATION DIVISION.                                         SCASHDRV
000200 PROGRAM-ID.                                                      SCASHDRV
000300     SCASHDRV.                                                    SCASHDRV
000400 DATE-WRITTEN.                                                    SCASHDRV
000500     September 2002.                                              SCASHDRV
000600 DATE-COMPILED.                                                   SCASHDRV
000700     Today.                                                       SCASHDRV
000800                                                                  SCASHDRV
000900 ENVIRONMENT DIVISION.                                            SCASHDRV
001000                                                                  SCASHDRV
001100 DATA DIVISION.                                                   SCASHDRV
001200 WORKING-STORAGE SECTION.                                         SCASHDRV
001300 01  WS-MISC-STORAGE.                                             SCASHDRV
001400   05  WS-PROGRAM-ID                         PIC X(8)             SCASHDRV
001500       VALUE 'SCASHDRV'.                                          SCASHDRV
001600                                                                  SCASHDRV
001700                                                                  SCASHDRV
001800 01  WS-CASH-DATA-AREAS.                                          SCASHDRV
001900   05  WS-CASH-DATA.                                              SCASHDRV
002000 COPY CCASHDAT.                                                   SCASHDRV
002100   05  WS-CASH-EXT-DATA.                                          SCASHDRV
002200 COPY CCASHEXT.                                                   SCASHDRV
002300                                                                  SCASHDRV
002400 COPY CABENDD.                                                    SCASHDRV
002500                                                                  SCASHDRV
002600                                                                  SCASHDRV
002700 PROCEDURE DIVISION.                                              SCASHDRV
002800     MOVE SPACES TO WS-CASH-DATA-AREAS.                           SCASHDRV
002900     SET ATM-FUNC-GET-ACCOUNTS TO TRUE.                           SCASHDRV
003000     MOVE 'B0004' TO ATM-IP00-USERID.                             SCASHDRV
003100     MOVE '0004' TO ATM-IP00-PIN.                                 SCASHDRV
003200                                                                  SCASHDRV
003300     EXEC CICS LINK PROGRAM('SCASH00P')                           SCASHDRV
003400                    COMMAREA(WS-CASH-EXT-DATA)                    SCASHDRV
003500                    LENGTH(LENGTH OF WS-CASH-EXT-DATA)            SCASHDRV
003600     END-EXEC.                                                    SCASHDRV
003700                                                                  SCASHDRV
003800***************************************************************** SCASHDRV
003900* Now we have to have finished and can return to our invoker.   * SCASHDRV
004000***************************************************************** SCASHDRV
004100* Now return to CICS                                              SCASHDRV
004200     EXEC CICS                                                    SCASHDRV
004300          RETURN                                                  SCASHDRV
004400     END-EXEC.                                                    SCASHDRV
004500     GOBACK.                                                      SCASHDRV
004600                                                                  SCASHDRV
004700* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        SCASHDRV
