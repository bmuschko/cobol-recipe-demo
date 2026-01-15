       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTSQL.
       AUTHOR. DEMO-AUTHOR.
      *****************************************************************
      * CUSTSQL - Customer SQL Processing Program                     *
      * This program demonstrates embedded SQL, CALL statements,      *
      * and EXEC SQL INCLUDE for recipe testing.                      *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.
       OBJECT-COMPUTER. IBM-390.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Include SQL communication area
           EXEC SQL INCLUDE SQLCA END-EXEC.

      * Include customer record definition
           EXEC SQL INCLUDE CUSTOMER END-EXEC.

       01  WS-WORK-AREAS.
           05  WS-SEARCH-ID            PIC 9(8).
           05  WS-RETURN-CODE          PIC S9(4) COMP.
           05  WS-ERROR-MESSAGE        PIC X(80).
           05  WS-ROWS-AFFECTED        PIC S9(8) COMP.

       01  WS-SQL-VARS.
           05  WS-NEW-BALANCE          PIC S9(7)V99 COMP-3.
           05  WS-NEW-STATUS           PIC X(1).
           05  WS-UPDATE-DATE          PIC 9(8).

      D 01  WS-DEBUG-VARS.
      D     05  WS-DEBUG-FLAG          PIC X(1) VALUE 'Y'.
      D     05  WS-DEBUG-MSG           PIC X(80).

      * Declare cursor for customer listing
           EXEC SQL
               DECLARE CUST-CURSOR CURSOR FOR
               SELECT CUST_ID, CUST_FIRST_NAME, CUST_LAST_NAME,
                      CUST_BALANCE, CUST_STATUS
               FROM CUSTOMER_TABLE
               WHERE CUST_STATUS = 'A'
               ORDER BY CUST_LAST_NAME
           END-EXEC.

      * Declare cursor for order history
           EXEC SQL
               DECLARE ORDER-CURSOR CURSOR FOR
               SELECT O.ORDER_ID, O.ORDER_DATE, O.ORDER_TOTAL
               FROM ORDER_TABLE O
               INNER JOIN CUSTOMER_TABLE C
                   ON O.CUST_ID = C.CUST_ID
               WHERE C.CUST_ID = :WS-SEARCH-ID
           END-EXEC.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
      D    MOVE 'ENTERING CUSTSQL MAIN' TO WS-DEBUG-MSG.
      D    DISPLAY WS-DEBUG-MSG.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CUSTOMERS
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
      D    DISPLAY 'DEBUG: INITIALIZING SQL PROGRAM'.
           INITIALIZE WS-WORK-AREAS
           INITIALIZE WS-SQL-VARS
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-UPDATE-DATE.

       2000-PROCESS-CUSTOMERS.
           PERFORM 2100-SELECT-CUSTOMER
           PERFORM 2200-INSERT-CUSTOMER
           PERFORM 2300-UPDATE-CUSTOMER
           PERFORM 2400-DELETE-CUSTOMER
           PERFORM 2500-CALL-SUBROUTINE.

       2100-SELECT-CUSTOMER.
      D    DISPLAY 'DEBUG: SELECTING CUSTOMER'.
           MOVE 12345678 TO WS-SEARCH-ID
           EXEC SQL
               SELECT CUST_ID, CUST_FIRST_NAME, CUST_LAST_NAME,
                      CUST_BALANCE
               INTO :CUST-ID, :CUST-FIRST-NAME, :CUST-LAST-NAME,
                    :CUST-BALANCE
               FROM CUSTOMER_TABLE
               WHERE CUST_ID = :WS-SEARCH-ID
           END-EXEC
           IF SQLCODE NOT = 0
               PERFORM 8000-SQL-ERROR
           END-IF.

       2200-INSERT-CUSTOMER.
      D    DISPLAY 'DEBUG: INSERTING CUSTOMER'.
           MOVE 99999999 TO CUST-ID
           MOVE 'JOHN' TO CUST-FIRST-NAME
           MOVE 'DOE' TO CUST-LAST-NAME
           MOVE 1000.00 TO CUST-BALANCE
           MOVE 'A' TO CUST-STATUS
           EXEC SQL
               INSERT INTO CUSTOMER_TABLE
               (CUST_ID, CUST_FIRST_NAME, CUST_LAST_NAME,
                CUST_BALANCE, CUST_STATUS, CUST_LAST_UPDATE)
               VALUES (:CUST-ID, :CUST-FIRST-NAME, :CUST-LAST-NAME,
                       :CUST-BALANCE, :CUST-STATUS, :WS-UPDATE-DATE)
           END-EXEC
           IF SQLCODE NOT = 0
               PERFORM 8000-SQL-ERROR
           END-IF.

       2300-UPDATE-CUSTOMER.
      D    DISPLAY 'DEBUG: UPDATING CUSTOMER'.
           MOVE 500.00 TO WS-NEW-BALANCE
           EXEC SQL
               UPDATE CUSTOMER_TABLE
               SET CUST_BALANCE = CUST_BALANCE + :WS-NEW-BALANCE,
                   CUST_LAST_UPDATE = :WS-UPDATE-DATE
               WHERE CUST_ID = :CUST-ID
           END-EXEC
           IF SQLCODE NOT = 0
               PERFORM 8000-SQL-ERROR
           END-IF.

       2400-DELETE-CUSTOMER.
      D    DISPLAY 'DEBUG: DELETING INACTIVE CUSTOMERS'.
           EXEC SQL
               DELETE FROM CUSTOMER_TABLE
               WHERE CUST_STATUS = 'I'
               AND CUST_LAST_UPDATE < :WS-UPDATE-DATE
           END-EXEC
           MOVE SQLERRD(3) TO WS-ROWS-AFFECTED
           DISPLAY 'ROWS DELETED: ' WS-ROWS-AFFECTED.

       2500-CALL-SUBROUTINE.
      D    DISPLAY 'DEBUG: CALLING SUBROUTINES'.
      * Call customer validation routine
           CALL 'CUSTVAL' USING CUSTOMER-RECORD
                                WS-RETURN-CODE

      * Call audit logging routine
           CALL 'AUDITLOG' USING CUST-ID
                                 WS-UPDATE-DATE
                                 WS-RETURN-CODE

      * Call notification routine
           CALL 'NOTIFYSVC' USING CUST-EMAIL
                                  WS-ERROR-MESSAGE.

       8000-SQL-ERROR.
           MOVE SQLCODE TO WS-RETURN-CODE
           STRING 'SQL ERROR: ' DELIMITED SIZE
                  SQLCODE DELIMITED SIZE
                  INTO WS-ERROR-MESSAGE
           DISPLAY WS-ERROR-MESSAGE.

       9000-TERMINATE.
      D    DISPLAY 'DEBUG: TERMINATING SQL PROGRAM'.
           DISPLAY 'CUSTSQL PROGRAM COMPLETE'.
