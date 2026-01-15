       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMGMT.
       AUTHOR. DEMO-AUTHOR.
      *****************************************************************
      * CUSTMGMT - Customer Management Program                        *
      * This program demonstrates various COBOL features for recipe   *
      * testing including copybook usage and debugging mode.          *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.
       OBJECT-COMPUTER. IBM-390.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY CUSTOMER.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS              PIC XX.
           88  WS-FILE-OK              VALUE '00'.
           88  WS-FILE-NOT-FOUND       VALUE '23'.
           88  WS-FILE-EOF             VALUE '10'.

       01  WS-WORK-AREAS.
           05  WS-SEARCH-ID            PIC 9(8).
           05  WS-RETURN-CODE          PIC S9(4) COMP.
           05  WS-ERROR-MESSAGE        PIC X(80).
           05  WS-CONTINUE-FLAG        PIC X(1).
               88  WS-CONTINUE         VALUE 'Y'.
               88  WS-STOP             VALUE 'N'.

       01  WS-DEBUG-AREA.
      D    05  WS-DEBUG-MSG            PIC X(80).
      D    05  WS-DEBUG-COUNTER        PIC 9(6) VALUE ZEROS.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
      D    MOVE 'ENTERING MAIN CONTROL' TO WS-DEBUG-MSG.
      D    DISPLAY WS-DEBUG-MSG.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CUSTOMERS
               UNTIL WS-STOP
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
      D    ADD 1 TO WS-DEBUG-COUNTER.
      D    DISPLAY 'DEBUG: INITIALIZING PROGRAM'.
           OPEN I-O CUSTOMER-FILE
           IF NOT WS-FILE-OK
               MOVE 'ERROR OPENING CUSTOMER FILE'
                   TO WS-ERROR-MESSAGE
               PERFORM 8000-ERROR-HANDLER
           END-IF
           SET WS-CONTINUE TO TRUE.

       2000-PROCESS-CUSTOMERS.
      D    DISPLAY 'DEBUG: PROCESSING CUSTOMERS'.
           DISPLAY 'ENTER CUSTOMER ID (0 TO EXIT): '
           ACCEPT WS-SEARCH-ID
           IF WS-SEARCH-ID = ZEROS
               SET WS-STOP TO TRUE
           ELSE
               PERFORM 3000-READ-CUSTOMER
           END-IF.

       3000-READ-CUSTOMER.
           MOVE WS-SEARCH-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY 'CUSTOMER NOT FOUND: ' CUST-ID
               NOT INVALID KEY
                   PERFORM 4000-DISPLAY-CUSTOMER
           END-READ.

       4000-DISPLAY-CUSTOMER.
      D    DISPLAY 'DEBUG: DISPLAYING CUSTOMER DATA'.
           DISPLAY '================================'
           DISPLAY 'CUSTOMER ID: ' CUST-ID
           DISPLAY 'NAME: ' CUST-FIRST-NAME ' ' CUST-LAST-NAME
           DISPLAY 'ADDRESS: ' CUST-STREET
           DISPLAY '         ' CUST-CITY ', ' CUST-STATE ' '
               CUST-ZIP
           DISPLAY 'PHONE: ' CUST-PHONE
           DISPLAY 'EMAIL: ' CUST-EMAIL
           DISPLAY 'STATUS: ' CUST-STATUS
           DISPLAY 'BALANCE: ' CUST-BALANCE
           DISPLAY '================================'.

       8000-ERROR-HANDLER.
           DISPLAY 'ERROR: ' WS-ERROR-MESSAGE
           DISPLAY 'FILE STATUS: ' WS-FILE-STATUS
           SET WS-STOP TO TRUE.

       9000-TERMINATE.
      D    DISPLAY 'DEBUG: TERMINATING PROGRAM'.
           CLOSE CUSTOMER-FILE.
