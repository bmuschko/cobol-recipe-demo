       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTBATCH.
       AUTHOR. DEMO-AUTHOR.
      *****************************************************************
      * CUSTBATCH - Customer Batch Processing Program                 *
      * This program demonstrates COPY REPLACING and additional       *
      * COBOL patterns for recipe testing.                            *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INFILE
               FILE STATUS IS WS-INPUT-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO OUTFILE
               FILE STATUS IS WS-OUTPUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTFILE
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
      * Use COPY REPLACING to create input record from copybook
       COPY CUSTOMER REPLACING
           CUSTOMER-RECORD BY INPUT-RECORD
           CUST-ID BY IN-CUST-ID
           CUST-NAME BY IN-CUST-NAME
           CUST-FIRST-NAME BY IN-FIRST-NAME
           CUST-LAST-NAME BY IN-LAST-NAME
           CUST-ADDRESS BY IN-CUST-ADDRESS
           CUST-STREET BY IN-STREET
           CUST-CITY BY IN-CITY
           CUST-STATE BY IN-STATE
           CUST-ZIP BY IN-ZIP
           CUST-PHONE BY IN-PHONE
           CUST-EMAIL BY IN-EMAIL
           CUST-STATUS BY IN-STATUS
           CUST-BALANCE BY IN-BALANCE
           CUST-CREDIT-LIMIT BY IN-CREDIT-LIMIT
           CUST-LAST-UPDATE BY IN-LAST-UPDATE.

       FD  OUTPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
      * Use COPY REPLACING to create output record
       COPY CUSTOMER REPLACING
           CUSTOMER-RECORD BY OUTPUT-RECORD
           CUST-ID BY OUT-CUST-ID
           CUST-NAME BY OUT-CUST-NAME
           CUST-FIRST-NAME BY OUT-FIRST-NAME
           CUST-LAST-NAME BY OUT-LAST-NAME
           CUST-ADDRESS BY OUT-CUST-ADDRESS
           CUST-STREET BY OUT-STREET
           CUST-CITY BY OUT-CITY
           CUST-STATE BY OUT-STATE
           CUST-ZIP BY OUT-ZIP
           CUST-PHONE BY OUT-PHONE
           CUST-EMAIL BY OUT-EMAIL
           CUST-STATUS BY OUT-STATUS
           CUST-BALANCE BY OUT-BALANCE
           CUST-CREDIT-LIMIT BY OUT-CREDIT-LIMIT
           CUST-LAST-UPDATE BY OUT-LAST-UPDATE.

       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-LINE                 PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-INPUT-STATUS         PIC XX.
           05  WS-OUTPUT-STATUS        PIC XX.
           05  WS-REPORT-STATUS        PIC XX.

       01  WS-COUNTERS.
           05  WS-READ-COUNT           PIC 9(8) VALUE ZEROS.
           05  WS-WRITE-COUNT          PIC 9(8) VALUE ZEROS.
           05  WS-ERROR-COUNT          PIC 9(8) VALUE ZEROS.
           05  WS-SKIP-COUNT           PIC 9(8) VALUE ZEROS.

       01  WS-FLAGS.
           05  WS-EOF-FLAG             PIC X(1) VALUE 'N'.
               88  WS-END-OF-FILE      VALUE 'Y'.
               88  WS-NOT-EOF          VALUE 'N'.

       01  WS-REPORT-HEADER.
           05  FILLER                  PIC X(40)
               VALUE 'CUSTOMER BATCH PROCESSING REPORT'.
           05  FILLER                  PIC X(92) VALUE SPACES.

       01  WS-REPORT-DETAIL.
           05  WS-RPT-CUST-ID          PIC 9(8).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WS-RPT-NAME             PIC X(60).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WS-RPT-STATUS           PIC X(10).
           05  FILLER                  PIC X(50) VALUE SPACES.

       01  WS-REPORT-TOTAL.
           05  FILLER                  PIC X(20) VALUE 'RECORDS READ:'.
           05  WS-RPT-READ             PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(20) VALUE 'RECORDS WRITTEN:'.
           05  WS-RPT-WRITTEN          PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(66) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-FILE
               UNTIL WS-END-OF-FILE
           PERFORM 3000-WRITE-TOTALS
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           OPEN OUTPUT REPORT-FILE
           MOVE WS-REPORT-HEADER TO REPORT-LINE
           WRITE REPORT-LINE
           PERFORM 1100-READ-INPUT.

       1100-READ-INPUT.
           READ INPUT-FILE
               AT END SET WS-END-OF-FILE TO TRUE
           END-READ
           IF WS-NOT-EOF
               ADD 1 TO WS-READ-COUNT
           END-IF.

       2000-PROCESS-FILE.
           EVALUATE TRUE
               WHEN IN-STATUS = 'A'
                   PERFORM 2100-PROCESS-ACTIVE
               WHEN IN-STATUS = 'I'
                   PERFORM 2200-PROCESS-INACTIVE
               WHEN OTHER
                   PERFORM 2300-PROCESS-ERROR
           END-EVALUATE
           PERFORM 1100-READ-INPUT.

       2100-PROCESS-ACTIVE.
           MOVE INPUT-RECORD TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           ADD 1 TO WS-WRITE-COUNT
           PERFORM 2500-WRITE-DETAIL.

       2200-PROCESS-INACTIVE.
           ADD 1 TO WS-SKIP-COUNT.

       2300-PROCESS-ERROR.
           ADD 1 TO WS-ERROR-COUNT.

       2500-WRITE-DETAIL.
           MOVE IN-CUST-ID TO WS-RPT-CUST-ID
           STRING IN-FIRST-NAME DELIMITED SPACE
                  ' ' DELIMITED SIZE
                  IN-LAST-NAME DELIMITED SPACE
                  INTO WS-RPT-NAME
           MOVE 'ACTIVE' TO WS-RPT-STATUS
           MOVE WS-REPORT-DETAIL TO REPORT-LINE
           WRITE REPORT-LINE.

       3000-WRITE-TOTALS.
           MOVE WS-READ-COUNT TO WS-RPT-READ
           MOVE WS-WRITE-COUNT TO WS-RPT-WRITTEN
           MOVE WS-REPORT-TOTAL TO REPORT-LINE
           WRITE REPORT-LINE.

       9000-TERMINATE.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           CLOSE REPORT-FILE
           DISPLAY 'BATCH PROCESSING COMPLETE'.
