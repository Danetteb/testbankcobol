001800 IDENTIFICATION DIVISION.                                         WEBSCALC
001900 PROGRAM-ID. WEBSCALC.                                            WEBSCALC
002500                                                                  WEBSCALC
002600 ENVIRONMENT DIVISION.                                            WEBSCALC
002700                                                                  WEBSCALC
002800 DATA DIVISION.                                                   WEBSCALC
002900 WORKING-STORAGE SECTION.                                         WEBSCALC
003000 01  WS-MISC-STORAGE.                                             WEBSCALC
003100   05  WS-PROGRAM-ID                        PIC X(8)              WEBSCALC
003200       VALUE 'WEBSCALC'.                                          WEBSCALC
009100                                                                  WEBSCALC
009300 01  WS-LINKAGE.                                                  WEBSCALC
007900     10  WS-LOAN-PRINCIPAL                  PIC S9(7).            WEBSCALC
008000     10  WS-LOAN-INTEREST                   PIC S9(2)V99999999.   WEBSCALC
008100     10  WS-LOAN-TERM                       PIC S9(5).            WEBSCALC
008200     10  WS-LOAN-MONTHLY-PAYMENT            PIC S9(6)V99.         WEBSCALC
022200
008000 01  WS-CALC-LOAN-INTEREST                  PIC SV99999999.       WEBSCALC

009200 LINKAGE SECTION.                                                 WEBSCALC

009300 01  L-LINKAGE.                                                   WEBSCALC
007900     10  L-LOAN-PRINCIPAL                   PIC S9(7).            WEBSCALC
008000     10  L-LOAN-INTEREST                    PIC S9(2)V99999999.   WEBSCALC
008100     10  L-LOAN-TERM                        PIC S9(5).            WEBSCALC
008200     10  L-LOAN-MONTHLY-PAYMENT             PIC S9(6)V99.         WEBSCALC
022200
001400 PROCEDURE DIVISION USING L-LINKAGE.

           MOVE L-LINKAGE TO WS-LINKAGE.

022800     MOVE ZERO TO WS-LOAN-MONTHLY-PAYMENT.                        WEBSCALC
022900
           DIVIDE WS-LOAN-INTEREST BY 100
             GIVING WS-CALC-LOAN-INTEREST.
                                                                        WEBSCALC
023000     DIVIDE WS-CALC-LOAN-INTEREST BY 12                           WEBSCALC
023100       GIVING WS-CALC-LOAN-INTEREST ROUNDED.                      WEBSCALC
023200     COMPUTE WS-LOAN-MONTHLY-PAYMENT ROUNDED =                    WEBSCALC
023300       ((WS-CALC-LOAN-INTEREST * ((1 + WS-CALC-LOAN-INTEREST)     WEBSCALC
023400           ** WS-LOAN-TERM)) /                                    WEBSCALC
023500       (((1 + WS-CALC-LOAN-INTEREST) ** WS-LOAN-TERM) - 1 ))      WEBSCALC
023600         * WS-LOAN-PRINCIPAL.                                     WEBSCALC
024800
           MOVE WS-LINKAGE TO L-LINKAGE.
                                                                        WEBSCALC
026000     GOBACK.                                                      WEBSCALC
026100                                                                  WEBSCALC
