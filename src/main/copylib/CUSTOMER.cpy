      *****************************************************************
      * CUSTOMER.CPY - Customer Record Copybook                       *
      * This copybook defines the customer record layout              *
      *****************************************************************
       01  CUSTOMER-RECORD.
           05  CUST-ID                 PIC 9(8).
           05  CUST-NAME.
               10  CUST-FIRST-NAME     PIC X(30).
               10  CUST-LAST-NAME      PIC X(30).
           05  CUST-ADDRESS.
               10  CUST-STREET         PIC X(50).
               10  CUST-CITY           PIC X(30).
               10  CUST-STATE          PIC X(2).
               10  CUST-ZIP            PIC 9(5).
           05  CUST-PHONE              PIC X(15).
           05  CUST-EMAIL              PIC X(50).
           05  CUST-STATUS             PIC X(1).
               88  CUST-ACTIVE         VALUE 'A'.
               88  CUST-INACTIVE       VALUE 'I'.
               88  CUST-SUSPENDED      VALUE 'S'.
           05  CUST-BALANCE            PIC S9(7)V99 COMP-3.
           05  CUST-CREDIT-LIMIT       PIC S9(7)V99 COMP-3.
           05  CUST-LAST-UPDATE        PIC 9(8).
