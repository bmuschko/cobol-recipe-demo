       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTCICS.
      *****************************************************************
      * CICS Customer Inquiry Program                                  *
      * This program demonstrates CICS functionality for customer      *
      * lookup and display operations.                                 *
      *****************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESPONSE-CODE        PIC S9(8) COMP.
       01  WS-RESPONSE-CODE2       PIC S9(8) COMP.
       01  WS-CUSTOMER-ID          PIC X(10).
       01  WS-COMM-AREA            PIC X(100).

       01  WS-CUSTOMER-RECORD.
           05 WS-CUST-ID           PIC X(10).
           05 WS-CUST-NAME         PIC X(30).
           05 WS-CUST-ADDRESS      PIC X(50).
           05 WS-CUST-BALANCE      PIC S9(7)V99 COMP-3.

       01  WS-MAP-DATA.
           05 WS-MAP-CUSTID        PIC X(10).
           05 WS-MAP-NAME          PIC X(30).
           05 WS-MAP-ADDRESS       PIC X(50).
           05 WS-MAP-BALANCE       PIC ZZZ,ZZ9.99-.

       01  WS-FILE-STATUS          PIC XX.
       01  WS-ERROR-MSG            PIC X(50).

       COPY CUSTOMER.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(100).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUEST
           PERFORM 9000-RETURN-TO-CICS
           GOBACK.

       1000-INITIALIZE.
      *    Receive the map from the terminal
           EXEC CICS RECEIVE MAP('CUSTMAP')
                     MAPSET('CUSTSET')
                     INTO(WS-MAP-DATA)
                     RESP(WS-RESPONSE-CODE)
           END-EXEC

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'ERROR RECEIVING MAP' TO WS-ERROR-MSG
               PERFORM 8000-HANDLE-ERROR
           END-IF.

       2000-PROCESS-REQUEST.
           MOVE WS-MAP-CUSTID TO WS-CUSTOMER-ID

      *    Read customer record from VSAM file
           EXEC CICS READ FILE('CUSTFILE')
                     INTO(WS-CUSTOMER-RECORD)
                     RIDFLD(WS-CUSTOMER-ID)
                     RESP(WS-RESPONSE-CODE)
                     RESP2(WS-RESPONSE-CODE2)
           END-EXEC

           EVALUATE WS-RESPONSE-CODE
               WHEN DFHRESP(NORMAL)
                   PERFORM 3000-DISPLAY-CUSTOMER
               WHEN DFHRESP(NOTFND)
                   MOVE 'CUSTOMER NOT FOUND' TO WS-ERROR-MSG
                   PERFORM 8000-HANDLE-ERROR
               WHEN OTHER
                   MOVE 'FILE READ ERROR' TO WS-ERROR-MSG
                   PERFORM 8000-HANDLE-ERROR
           END-EVALUATE.

       3000-DISPLAY-CUSTOMER.
           MOVE WS-CUST-ID TO WS-MAP-CUSTID
           MOVE WS-CUST-NAME TO WS-MAP-NAME
           MOVE WS-CUST-ADDRESS TO WS-MAP-ADDRESS
           MOVE WS-CUST-BALANCE TO WS-MAP-BALANCE

      *    Send the map back to terminal
           EXEC CICS SEND MAP('CUSTMAP')
                     MAPSET('CUSTSET')
                     FROM(WS-MAP-DATA)
                     ERASE
                     RESP(WS-RESPONSE-CODE)
           END-EXEC

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'ERROR SENDING MAP' TO WS-ERROR-MSG
               PERFORM 8000-HANDLE-ERROR
           END-IF.

       4000-WRITE-CUSTOMER.
      *    Write a new customer record
           EXEC CICS WRITE FILE('CUSTFILE')
                     FROM(WS-CUSTOMER-RECORD)
                     RIDFLD(WS-CUSTOMER-ID)
                     RESP(WS-RESPONSE-CODE)
           END-EXEC

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'ERROR WRITING RECORD' TO WS-ERROR-MSG
               PERFORM 8000-HANDLE-ERROR
           END-IF.

       5000-DELETE-CUSTOMER.
      *    Delete a customer record
           EXEC CICS DELETE FILE('CUSTFILE')
                     RIDFLD(WS-CUSTOMER-ID)
                     RESP(WS-RESPONSE-CODE)
           END-EXEC

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'ERROR DELETING RECORD' TO WS-ERROR-MSG
               PERFORM 8000-HANDLE-ERROR
           END-IF.

       6000-LINK-TO-SUBPROGRAM.
      *    Link to another CICS program
           EXEC CICS LINK PROGRAM('CUSTSUB')
                     COMMAREA(WS-COMM-AREA)
                     LENGTH(100)
                     RESP(WS-RESPONSE-CODE)
           END-EXEC

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'ERROR LINKING PROGRAM' TO WS-ERROR-MSG
               PERFORM 8000-HANDLE-ERROR
           END-IF.

       7000-START-TRANSACTION.
      *    Start a new transaction
           EXEC CICS START TRANSID('CUST')
                     FROM(WS-CUSTOMER-ID)
                     LENGTH(10)
                     RESP(WS-RESPONSE-CODE)
           END-EXEC.

       8000-HANDLE-ERROR.
      *    Send error message to terminal
           EXEC CICS SEND TEXT
                     FROM(WS-ERROR-MSG)
                     LENGTH(50)
                     ERASE
           END-EXEC

      *    Write to transient data queue for logging
           EXEC CICS WRITEQ TD QUEUE('ERRQ')
                     FROM(WS-ERROR-MSG)
                     LENGTH(50)
           END-EXEC

      *    Abend the transaction
           EXEC CICS ABEND ABCODE('CERR')
           END-EXEC.

       9000-RETURN-TO-CICS.
      *    Return control to CICS
           EXEC CICS RETURN TRANSID('CUST')
                     COMMAREA(WS-COMM-AREA)
                     LENGTH(100)
           END-EXEC.
