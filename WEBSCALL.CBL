001800 IDENTIFICATION DIVISION.                                         WEBSCALL
001900 PROGRAM-ID. WEBSCALL.                                            WEBSCALL
002500                                                                  WEBSCALL
002600 ENVIRONMENT DIVISION.                                            WEBSCALL
002700                                                                  WEBSCALL
002800 DATA DIVISION.                                                   WEBSCALL
002900 WORKING-STORAGE SECTION.                                         WEBSCALL
003000 01  WS-MISC-STORAGE.                                             WEBSCALL
003100     05  C-WEBSCALC                          PIC X(8)             WEBSCALL
003200         VALUE 'WEBSCALC'.                                        WEBSCALL

009300 01  WS-LINKAGE.                                                  WEBSCALL
007900     10  WS-LOAN-PRINCIPAL                   PIC 9(7).            WEBSCALL
008000     10  WS-LOAN-INTEREST                    PIC 9(2)V99999999.   WEBSCALL
008100     10  WS-LOAN-TERM                        PIC 9(5).            WEBSCALL
008200     10  WS-LOAN-MONTHLY-PAYMENT             PIC 9(6)V99.         WEBSCALL

001400 PROCEDURE DIVISION.

007900     MOVE 99999  TO  WS-LOAN-PRINCIPAL.                           WEBSCALL
008000     MOVE 9.99   TO  WS-LOAN-INTEREST.                            WEBSCALL
008100     MOVE 9      TO  WS-LOAN-TERM.                                WEBSCALL
           MOVE ZEROES TO WS-LOAN-MONTHLY-PAYMENT.

           CALL C-WEBSCALC USING WS-LINKAGE.
024800                                                                  WEBSCALL
026000     GOBACK.                                                      WEBSCALL
026100                                                                  WEBSCALL
