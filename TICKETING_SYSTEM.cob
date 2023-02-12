      ******************************************************************
      * Authors: Borabon, Datur, Mananquil, Pronto                      *
      * Date: March 2, 2021                                             *
      * System Title: Movie Ticket Reservation System                   *
      *******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVIE-TICKET-RESERVATION.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-CUSTOMER ASSIGN TO '..\LOGININFO.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS C-ACCID
           ALTERNATE KEY IS ALT-KEY
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-USERNAME-DETECT ASSIGN TO '..\LOGININFO.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS D-ACCID
           ALTERNATE KEY IS D-ALT-KEY
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-EMPLOYEES ASSIGN TO '..\EMPLOYEES.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS FE-ACCID
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-ADMIN ASSIGN TO '..\ADMIN.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS FA-ACCOUNT
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-LAYOUT ASSIGN TO '..\SEATLAYOUT1.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS SEAT-KEY
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-LAYOUT1 ASSIGN TO '..\SEATLAYOUT2.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS SEAT-KEY1
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-RESERVEDSEAT ASSIGN TO '..\RESERVEDSEAT.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS SEATID
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-MOVIES ASSIGN TO '..\MOVIES.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS FM-MOVIECODE
           FILE STATUS IS WS-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-ADMIN.
       01  FA-ACCOUNT.
           05 FA-ACCID             PIC 9(10).
           05 FA-NAME.
               10 FA-FNAME         PIC X(10).
               10 FA-LNAME         PIC X(10).
           05 FA-PASSCODE          PIC 9(4).

       FD  FD-EMPLOYEES.
       01  FE-ACCOUNT.
           05 FE-ACCID             PIC 9(10).
           05 FE-NAME.
               10 FE-FNAME         PIC X(10).
               10 FE-LNAME         PIC X(10).
           05 FE-PASSCODE          PIC X(6).
           05 FE-POSITION          PIC 99.

       FD  FD-MOVIES.
       01  FM-RECORD.
           05 FM-MOVIECODE         PIC 9(4).
           05 FM-TITLE             PIC X(30).
           05 FM-RDATE             PIC X(4).
           05 FM-RATINGS           PIC X(3).
           05 FM-SYNOPSIS          PIC X(800).

       FD  FD-LAYOUT.
       01  C-LAYOUT.
           05 SEAT-KEY             PIC 9.
           05 COL0                 PIC X.
           05 COL1                 PIC X.
           05 COL2                 PIC X.
           05 COL3                 PIC X.
           05 COL4                 PIC X.
           05 COL5                 PIC X.
           05 COL6                 PIC X.
           05 COL7                 PIC X.
           05 COL8                 PIC X.
           05 COL9                 PIC X.

       FD  FD-LAYOUT1.
       01  C-LAYOUT1.
           05 SEAT-KEY1            PIC 9.
           05 COL01                PIC X.
           05 COL11                PIC X.
           05 COL21                PIC X.
           05 COL31                PIC X.
           05 COL41                PIC X.
           05 COL51                PIC X.
           05 COL61                PIC X.
           05 COL71                PIC X.
           05 COL81                PIC X.
           05 COL91                PIC X.

       FD  FD-RESERVEDSEAT.
       01  C-RESERVEDSEAT.
           05 SEATID               PIC 9(4).
           05 CUSTOMER-ID          PIC 9(10).
           05 MOVIE-ID             PIC 9(4).
           05 MOVIE-TITLE          PIC X(30).
           05 CINEMA-NUM           PIC 9.
           05 LAYOUT-NUM           PIC 9.
           05 SEATNUMBER           PIC X.
           05 TIME-RESERVED        PIC XXXXXXXX.
           05 DATE-RESERVED        PIC X(10).
           05 RESERVE-STATUS       PIC X(6).

       FD  FD-CUSTOMER.
       01  C-ACCOUNT.
           05 C-ACCID              PIC 9(10).
           05 C-NAME.
               10 FC-FNAME         PIC X(10).
               10 FC-LNAME         PIC X(10).
           05 C-PHONENUM           PIC 9(11).
           05 C-USERNAME           PIC X(10).
           05 C-PASSCODE           PIC X(8).
           05 C-RESERVED           PIC 9.
           01 ALT-KEY              PIC X(10).

       FD  FD-USERNAME-DETECT.
       01  D-ACCOUNT.
           05 D-ACCID              PIC 9(10).
           05 D-NAME.
               10 FAD-FNAME        PIC X(10).
               10 FAD-LNAME        PIC X(10).
           05 D-PHONENUM           PIC 9(11).
           05 D-USERNAME           PIC X(10).
           05 D-PASSCODE           PIC X(8).
           05 D-RESERVED           PIC 9.
           01 D-ALT-KEY            PIC X(10).

       WORKING-STORAGE SECTION.
      *----AUTO CREATE DATABASE----
       01 WS-KEY-COUNTER           PIC 9 VALUE 1.
       01 WS-LAYOUT-CREATE.
           05 WS-SEAT-KEY-1        PIC 9.
           05 WS-COL-LAYOUT        PIC X VALUE "*".
           05 WS-COL1-LAYOUT       PIC X VALUE "*".
           05 WS-COL2-LAYOUT       PIC X VALUE "*".
           05 WS-COL3-LAYOUT       PIC X VALUE "*".
           05 WS-COL4-LAYOUT       PIC X VALUE "*".
           05 WS-COL5-LAYOUT       PIC X VALUE "*".
           05 WS-COL6-LAYOUT       PIC X VALUE "*".
           05 WS-COL7-LAYOUT       PIC X VALUE "*".
           05 WS-COL8-LAYOUT       PIC X VALUE "*".
           05 WS-COL9-LAYOUT       PIC X VALUE "*".
       *>ADMIN
       01 WSA-ACCOUNT.
           05 WSA-ACCID            PIC 9(10).
           05 WSA-NAME.
               10 WSA-FNAME        PIC X(10).
               10 WSA-LNAME        PIC X(10).
           05 WSA-PASSCODE         PIC X(6).
           05 WSA-POSITION         PIC 99.

       01 WS-DECISION              PIC 9.

       01 WS-MCODE.
           05 WS-MIN               PIC 9(4) VALUE 7.
           05 WS-MAX               PIC 9(4) VALUE 2013.
           05 WS-CODE              PIC 9(4).

       *>CUSTOMER
       01  CUSTOMER-CURRENT-TICKET PIC 9.
       01  SEAT-PROCEED            PIC X.
       01  DEFAULT-STATUS          PIC X(6) VALUES "UNPAID".
       01  IS-VALID-CHOICE-HOME    PIC X.
       01  WS-MOVIE-ID-SELECTED    PIC 9(4).
      *----FOR SEAT RESERVATION----
       01  WS-RESERVEDSEAT.
           05 WS-SEATID            PIC 9(4).
           05 WS-CUSTOMER-ID       PIC 9(10).
           05 WS-MOVIE-ID          PIC 9(4).
           05 WS-MOVIE-TITLE       PIC X(30).
           05 WS-CINEMA-NUM        PIC 9.
           05 WS-LAYOUT-NUM        PIC 9.
           05 WS-SEATNUMBER        PIC X.
           05 WS-TIME-RESERVED     PIC XXXXXXXX.
           05 WS-DATE-RESERVED     PIC X(10).
           05 WS-RESERVE-STATUS    PIC X(6).
      *------------------------------
       01  STOPER PIC X.
       01  CUSTOMER-CHOOSE-MOVIE   PIC 9.
       01  NOW-SHOWING-COUNT       PIC 9 VALUE 1.
       01  SEAT-VALID              PIC X.
       01  WS-MOVIE-INPUT          PIC 9(4).
      *----FOR DISPLAYING MOVIES----
       01  WS-MOVIES.
           05 WS-MOVIECODE         PIC 9(4).
           05 WS-TITLE             PIC X(30) VALUE SPACES.
           05 WS-RDATE             PIC X(4).
           05 WS-RATINGS           PIC X(3).
           05 WS-SYNOPSIS          PIC X(800) VALUE SPACES.
      *----FOR DISPLAYING SEAT LAYOUT----
       01  WS-LAYOUT.
           05 WS-SEAT-KEY          PIC 9.
           05 WS-COL0              PIC X.
           05 WS-COL1              PIC X.
           05 WS-COL2              PIC X.
           05 WS-COL3              PIC X.
           05 WS-COL4              PIC X.
           05 WS-COL5              PIC X.
           05 WS-COL6              PIC X.
           05 WS-COL7              PIC X.
           05 WS-COL8              PIC X.
           05 WS-COL9              PIC X.
      *----VARIABLES FOR INPUT IN LOGIN----
           01 INPUT-USERNAME       PIC X(10).
           01 INPUT-PASSCODE       PIC X(8).
           01 REXIST               PIC X.
           01 WS-ACCOUNT-1.
               05 WS-ACCID1        PIC 9(10).
               05 WS-NAME.
                   10 WS-FNAME1    PIC X(10).
                   10 WS-LNAME1    PIC X(10).
               05 WS-PHONENUMBER1  PIC 9(11).
               05 WS-USERNAME1     PIC X(10).
               05 WS-PASSCODE1     PIC X(8).
               05 WS-RESERVED1     PIC 9.
           01 WS-ACCOUNT.
               05 WS-ACCID         PIC 9(10).
               05 WS-NAME.
                   10 WS-FNAME     PIC X(10).
                   10 WS-LNAME     PIC X(10).
               05 WS-PHONENUMBER   PIC 9(11).
               05 WS-USERNAME      PIC X(10).
               05 WS-PASSCODE      PIC X(8).
               05 WS-RESERVED      PIC 9.
           01 WS-FILESTATUS        PIC XX.
           01 WS-MENU-CHOICE       PIC X.
           01 WS-CHOICE            PIC X.

           01 WS-GENERATE-DATA.
           05  WS-DATE.
               10 WS-YEAR          PIC 9(04).
               10 WS-MONTH         PIC 9(02).
           05  WS-TIME.
               10 WS-DAY           PIC 9(02).
               10 WS-HOURS         PIC 9(02).
               10 WS-MINUTE        PIC 9(02).
               10 WS-SECOND        PIC 9(02).
               10 WS-MILLISECONDS  PIC 9(02).
           01 WS-PASSCODE-TEMP     PIC 9(4).
           01 WS-FLAG              PIC 9.
           01 WS-EOF               PIC X.
           01 HOME-CHOICE          PIC X.
           01 SEAT-COUNTS          PIC 99.
           01 SEAT-INPUT           PIC XX.
           01 SEAT-KEY-INPUT       PIC X.
           01 SEAT-NUMBER-INPUT    PIC X.
           01 DATE-ID.
              05 YEAR              PIC 9(4).
              05 MONTH             PIC 99.
              05 DAYY              PIC 99.
      *----VARIABLES FOR TIME AND DATE----
       01  ORIGINAL-DATE           PIC XXXX/XX/XXBXX/XX.
       01  DATER                   PIC XXXXXXXXXX.
       01  TIMER                   PIC XXXXXX.
       01  HOUR                    PIC 99.
       01  MIN                     PIC 99.
       01  HOUR-TO-AM              PIC 99.
       01  AM-OR-PM                PIC XX.
       01  STANDARD-TIME           PIC 99 VALUE 12.
       01  TIME-STRINGER           PIC XXXXXXXX.
      *----------------------------------
       01  DEFAULTID               PIC 9(4).
       01  FINAL-SEAT-ID           PIC 9(11).
       01  AVAILABLE-SEAT-ONE      PIC 99.
       01  AVAILABLE-SEAT-TWO      PIC 99.
       01  LOGIN-SUCCESS           PIC X.
       01  RESERVE-ID-INPUT        PIC 9(4).
       01  RESERVATION-VALID       PIC X.
       01  IS-PROCEED              PIC 9.
       01  IS-PROCEED-EVAL         PIC X.
       01  CORRECT-ID              PIC X.
       01  VIEW-RESERVE-BACK       PIC 9.
       01  RESERVE-CONFIRM         PIC 9.
       01  RESERVE-STOP            PIC X.
       01  RESERVE-CONFIRM2        PIC 9.
       01  RESERVE-STOP2           PIC X.
       01  SELECT-REFRESH          PIC X.
       01  SELECT-VIEW             PIC X.
       01  YES-PAID                PIC X.
       01  YES-UNPAID              PIC X.
       01  REGISTER-CHECK          PIC X.
       01  LOGIN-HAVE-DATA         PIC 9.
       01  WS-ECHOICE              PIC 9.
       01  WS-MOVIE-COUNTER        PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       DATABASE-CONFIGURATION.
           DISPLAY "CONFIGURING DATABASE...".
           OPEN I-O FD-LAYOUT
           IF WS-FILESTATUS = 35 THEN
           OPEN OUTPUT FD-LAYOUT
           PERFORM UNTIL WS-KEY-COUNTER EQUAL 6
           OPEN I-O FD-LAYOUT
           MOVE WS-KEY-COUNTER TO WS-SEAT-KEY-1
           MOVE WS-LAYOUT-CREATE TO C-LAYOUT
           WRITE C-LAYOUT
           ADD 1 TO WS-KEY-COUNTER
           CLOSE FD-LAYOUT
           END-PERFORM
           END-IF
           MOVE 1 TO WS-KEY-COUNTER
           CLOSE FD-LAYOUT

           OPEN I-O FD-LAYOUT1
           IF WS-FILESTATUS = 35 THEN
           OPEN OUTPUT FD-LAYOUT1
           PERFORM UNTIL WS-KEY-COUNTER EQUAL 6
           OPEN I-O FD-LAYOUT1
           MOVE WS-KEY-COUNTER TO WS-SEAT-KEY-1
           MOVE WS-LAYOUT-CREATE TO C-LAYOUT1
           WRITE C-LAYOUT1
           ADD 1 TO WS-KEY-COUNTER
           CLOSE FD-LAYOUT1
           END-PERFORM
           END-IF
           MOVE 1 TO WS-KEY-COUNTER
           CLOSE FD-LAYOUT1

           OPEN I-O FD-ADMIN
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-ADMIN
           END-IF
           CLOSE FD-ADMIN

           OPEN I-O FD-CUSTOMER
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-CUSTOMER
           END-IF
           CLOSE FD-CUSTOMER

           OPEN I-O FD-EMPLOYEES
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-EMPLOYEES
           END-IF
           CLOSE FD-EMPLOYEES

           OPEN I-O FD-RESERVEDSEAT
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-RESERVEDSEAT
           END-IF
           CLOSE FD-RESERVEDSEAT

           OPEN I-O FD-ADMIN
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-ADMIN
           END-IF
           CLOSE FD-ADMIN

           OPEN I-O FD-MOVIES
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-MOVIES
           END-IF
           CLOSE FD-MOVIES.
           DISPLAY "CONFIGURATION SUCCESSFUL!".

       MAIN-PARA.
           DISPLAY " ".
           DISPLAY "********************************************"
           DISPLAY " WELCOME TO MOVIE TICKET RESERVATION SYSTEM".
           DISPLAY "********************************************"
           DISPLAY "I AM A/AN..."
           DISPLAY "1 - CUSTOMER".
           DISPLAY "2 - ADMIN".
           DISPLAY "3 - EMPLOYEE".
           DISPLAY "4 - EXIT".
           ACCEPT WS-CHOICE.

           IF WS-CHOICE = 1 THEN
               GO TO CUSTOMER-PARA
           ELSE IF WS-CHOICE = 2 THEN
               GO TO ADMIN-PARA
           ELSE IF WS-CHOICE = 3 THEN
               GO TO EMPLOYEE-PARA
           ELSE IF WS-CHOICE = 4 THEN
               STOP RUN
           ELSE
               DISPLAY "ERROR! INVALID CHOICE!"
               GO TO MAIN-PARA
           END-IF.
           STOP RUN.

       ADMIN-PARA.
           INITIALIZE WS-ACCOUNT.
           INITIALIZE FA-ACCOUNT.

           DISPLAY " ".
           DISPLAY "*************************"
           DISPLAY " WELCOME TO ADMIN PORTAL".
           DISPLAY "*************************"
           DISPLAY "1 - REGISTER NEW ADMIN".
           DISPLAY "2 - ADMIN LOGIN".
           DISPLAY "3 - GO TO MAIN MENU".
           ACCEPT WS-CHOICE.

           IF WS-CHOICE = 1 THEN
               GO TO ADMIN-REG
           ELSE IF WS-CHOICE = 2 THEN
               GO TO ADMIN-LOGIN
           ELSE IF WS-CHOICE = 3 THEN
               GO TO MAIN-PARA
           ELSE
               DISPLAY "ERROR! INVALID CHOICE!"
               GO TO ADMIN-PARA
           END-IF.
           STOP RUN.

       CUSTOMER-PARA.
           PERFORM CONVERT-TIME-PARA.
           INITIALIZE C-ACCOUNT.
           PERFORM UNTIL WS-MENU-CHOICE EQUAL 'C' OR
           WS-MENU-CHOICE EQUAL 'c'
           DISPLAY " "
           DISPLAY "*****************************"
           DISPLAY " WELCOME TO CUSTOMER PORTAL"
           DISPLAY "*****************************"
           DISPLAY "A - REGISTER AN ACCOUNT"
           DISPLAY "B - LOGIN TO AN EXISTING ACCOUNT"
           DISPLAY "C - GO TO MAIN MENU"
           ACCEPT WS-MENU-CHOICE
           EVALUATE WS-MENU-CHOICE
                WHEN 'A'
                WHEN 'a' PERFORM REGISTER-PARA
                WHEN 'B'
                WHEN 'b' PERFORM LOGIN-PARA
                WHEN 'C'
                WHEN 'c' PERFORM MAIN-PARA
                WHEN OTHER PERFORM DEFAULT-PARA
                PERFORM CUSTOMER-PARA
           END-EVALUATE
           END-PERFORM.

       CONVERT-TIME-PARA.
       MOVE FUNCTION WHEN-COMPILED TO ORIGINAL-DATE
       INSPECT ORIGINAL-DATE REPLACING ALL "/" BY ":" AFTER INITIAL
       SPACE

       UNSTRING ORIGINAL-DATE DELIMITED BY " "
       INTO DATER, TIMER
       END-UNSTRING

       UNSTRING TIMER DELIMITED BY ":"
       INTO HOUR,MIN
       END-UNSTRING

       IF HOUR >= 12 THEN
           MOVE "PM" TO AM-OR-PM
           IF HOUR EQUAL 12

           ELSE
           COMPUTE HOUR-TO-AM= HOUR - STANDARD-TIME
           END-IF
       ELSE
           MOVE "AM" TO AM-OR-PM
       END-IF

       STRING
       HOUR-TO-AM DELIMITED BY SPACE ":"
       DELIMITED BY SIZE
       MIN DELIMITED BY SPACE " "
       DELIMITED BY SIZE
       AM-OR-PM DELIMITED BY SPACE " "
       INTO TIME-STRINGER
       END-STRING

       UNSTRING DATER DELIMITED BY "/"
       INTO YEAR,MONTH,DAYY
       END-UNSTRING.

       REGISTER-PARA.
           MOVE 'N' TO REGISTER-CHECK
           DISPLAY " "
           DISPLAY "*********************"
           DISPLAY " REGISTER AN ACCOUNT"
           DISPLAY "*********************"
           DISPLAY "FIRST NAME:"
           ACCEPT FC-FNAME
           DISPLAY "LAST NAME:"
           ACCEPT FC-LNAME
           DISPLAY "PHONE NUMBER:"
           ACCEPT C-PHONENUM

           PERFORM UNTIL REGISTER-CHECK EQUAL 'Y'
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO REGISTER-CHECK
           MOVE 0 TO LOGIN-HAVE-DATA
           DISPLAY "USERNAME (NOTE: AT MOST 10 CHARACTERS):"
           ACCEPT C-USERNAME

           OPEN I-O FD-USERNAME-DETECT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-USERNAME-DETECT NEXT RECORD INTO WS-ACCOUNT-1
                   AT END MOVE 'Y' TO WS-EOF
                   NOT END ADD 1 TO LOGIN-HAVE-DATA
                   IF C-USERNAME EQUAL WS-USERNAME1
                    MOVE 'N' TO REGISTER-CHECK
                    MOVE 'Y' TO WS-EOF
                    ELSE
                    MOVE 'Y' TO REGISTER-CHECK
                   END-IF
               END-READ
           END-PERFORM
           CLOSE FD-USERNAME-DETECT
           IF LOGIN-HAVE-DATA EQUAL 0
               MOVE 'Y' TO REGISTER-CHECK
           END-IF
           IF REGISTER-CHECK NOT EQUAL 'Y'
              DISPLAY "THIS USERNAME IS ALREADY TAKEN. PLEASE TRY"
              " AGAIN."
           END-IF
           END-PERFORM

           DISPLAY "PASSWORD (NOTE: AT MOST 8 CHARACTERS):"
           ACCEPT C-PASSCODE
           MOVE 0 TO C-RESERVED
           MOVE FUNCTION CURRENT-DATE to WS-GENERATE-DATA
           MOVE WS-TIME TO C-ACCID.
           COMPUTE C-ACCID = FUNCTION
           RANDOM(WS-DATE) * C-ACCID.
           OPEN I-O FD-CUSTOMER.
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-CUSTOMER
           END-IF
           WRITE C-ACCOUNT
           CLOSE FD-CUSTOMER
           DISPLAY "ACCOUNT REGISTRATION SUCCESSFUL!".
           PERFORM CUSTOMER-PARA.

       LOGIN-PARA.
           PERFORM UNTIL LOGIN-SUCCESS EQUAL 'Y'
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO LOGIN-SUCCESS
           DISPLAY " "
           DISPLAY "******************************"
           DISPLAY " LOGIN TO AN EXISTING ACCOUNT"
           DISPLAY "******************************"
           DISPLAY "USERNAME:"
           ACCEPT INPUT-USERNAME
           DISPLAY "PASSWORD:"
           ACCEPT INPUT-PASSCODE
           OPEN INPUT FD-CUSTOMER
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-CUSTOMER NEXT RECORD INTO WS-ACCOUNT
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
               IF INPUT-USERNAME EQUAL WS-USERNAME AND
               INPUT-PASSCODE EQUAL WS-PASSCODE
                   MOVE 'Y' TO WS-EOF
                   MOVE 'Y' TO LOGIN-SUCCESS
               END-IF
           END-PERFORM
           CLOSE FD-CUSTOMER
           IF LOGIN-SUCCESS EQUAL 'Y'
               MOVE 'N' TO WS-EOF
               MOVE 'N' TO LOGIN-SUCCESS
               PERFORM HOME-PARA
           ELSE
             DISPLAY "LOGIN FAILED! PLEASE CHECK USERNAME AND PASSWORD!"
             MOVE 'N' TO WS-EOF
             PERFORM MAIN-PARA
           END-IF
           END-PERFORM.

       COUNT-AVAILABLE-SEATS-ONE.
           MOVE 'N' TO WS-EOF
           MOVE 00 TO AVAILABLE-SEAT-ONE
           OPEN INPUT FD-LAYOUT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-LAYOUT NEXT RECORD INTO WS-LAYOUT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-COL0 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL1 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL2 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL3 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL4 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL5 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL6 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL7 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL8 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               IF WS-COL9 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-ONE
               END-IF
               END-READ
           END-PERFORM
           CLOSE FD-LAYOUT.

           COUNT-AVAILABLE-SEATS-TWO.
           MOVE 'N' TO WS-EOF
           MOVE 00 TO AVAILABLE-SEAT-TWO
           OPEN INPUT FD-LAYOUT1
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-LAYOUT1 NEXT RECORD INTO WS-LAYOUT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-COL0 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL1 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL2 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL3 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL4 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL5 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL6 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL7 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL8 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               IF WS-COL9 EQUAL "*"
                   ADD 1 TO AVAILABLE-SEAT-TWO
               END-IF
               END-READ
           END-PERFORM
           CLOSE FD-LAYOUT1.

       RESERVE-SEAT.
           IF CUSTOMER-CURRENT-TICKET EQUAL 5
              DISPLAY "SORRY, YOU CAN ONLY RESERVE 5 TICKETS."
              MOVE 00 TO CUSTOMER-CURRENT-TICKET
              PERFORM HOME-PARA
           ELSE
               DISPLAY " "
           END-IF

           MOVE 'N' TO WS-EOF
           PERFORM COUNT-AVAILABLE-SEATS-ONE
           MOVE 'N' TO WS-EOF
           DISPLAY "**************************************"
           DISPLAY " NOW SHOWING MOVIES AVAILABLE FOR YOU"
           DISPLAY "**************************************"
           OPEN INPUT FD-MOVIES
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-MOVIES NEXT RECORD INTO WS-MOVIES
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF NOW-SHOWING-COUNT EQUAL 1
           DISPLAY NOW-SHOWING-COUNT " - " FUNCTION
                       UPPER-CASE(WS-TITLE)
           DISPLAY "AVAILABLE SEATS: " AVAILABLE-SEAT-ONE
           DISPLAY "YEAR: " WS-RDATE
           DISPLAY "MTRCB RATING: " FUNCTION
                       UPPER-CASE(WS-RATINGS)
           DISPLAY "SYNOPSIS: " FUNCTION
                       UPPER-CASE(WS-SYNOPSIS)
           DISPLAY " "
           ADD 1 TO NOW-SHOWING-COUNT
           ELSE IF NOW-SHOWING-COUNT EQUAL 2
           PERFORM COUNT-AVAILABLE-SEATS-TWO
           DISPLAY NOW-SHOWING-COUNT " - " FUNCTION
                       UPPER-CASE(WS-TITLE)
           DISPLAY "AVAILABLE SEATS: " AVAILABLE-SEAT-TWO
           DISPLAY "YEAR: " WS-RDATE
           DISPLAY "MTRCB RATING: " FUNCTION
                       UPPER-CASE(WS-RATINGS)
           DISPLAY "SYNOPSIS: " FUNCTION
                       UPPER-CASE(WS-SYNOPSIS)
           END-IF
           END-READ
           END-PERFORM
           CLOSE FD-MOVIES
           MOVE 1 TO NOW-SHOWING-COUNT
           DISPLAY " "

           DISPLAY "3 - EXIT"
           DISPLAY "SELECT MOVIE:"
           PERFORM UNTIL STOPER EQUAL 'Y'
           ACCEPT CUSTOMER-CHOOSE-MOVIE

      *----IF USER CHOOSES MOVIE----
           IF CUSTOMER-CHOOSE-MOVIE EQUAL 1
           MOVE 'N' TO WS-EOF
           OPEN INPUT FD-MOVIES
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-MOVIES NEXT RECORD INTO WS-MOVIES
                   NOT AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM
           CLOSE FD-MOVIES
           END-IF
           MOVE 'N' TO WS-EOF
           IF CUSTOMER-CHOOSE-MOVIE EQUAL 2
               OPEN INPUT FD-MOVIES
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-MOVIES NEXT RECORD INTO WS-MOVIES
                    AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM
           MOVE 'N' TO WS-EOF
           CLOSE FD-MOVIES
           END-IF
           EVALUATE CUSTOMER-CHOOSE-MOVIE
           WHEN 1 PERFORM NOW-SHOWING-ONE
                  MOVE 'Y' TO STOPER
                  DISPLAY "SEAT RESERVATION SUCCESS!"
           WHEN 2 PERFORM NOW-SHOWING-TWO
                  MOVE 'Y' TO STOPER
                  DISPLAY "SEAT RESERVATION SUCCESS!"
           WHEN 3 MOVE 'Y' TO STOPER
           WHEN OTHER PERFORM DEFAULT-PARA
           END-EVALUATE
           END-PERFORM

           MOVE 'N' TO WS-EOF
           MOVE 'N' TO STOPER.

       NOW-SHOWING-ONE.
           MOVE 'N' TO SEAT-VALID
           MOVE 'N' TO WS-EOF
           DISPLAY " "
           DISPLAY "SELECTED MOVIE: " FUNCTION
                       UPPER-CASE(WS-TITLE)
           MOVE 'N' TO WS-EOF
           DISPLAY "**********************"
           DISPLAY " CINEMA 1 SEAT LAYOUT"
           DISPLAY "**********************"
           DISPLAY "  A B C D E F G H I J"
           OPEN INPUT FD-LAYOUT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-LAYOUT NEXT RECORD INTO WS-LAYOUT
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
               DISPLAY WS-SEAT-KEY " " WS-COL0 " " WS-COL1 " " WS-COL2
               " " WS-COL3 " " WS-COL4 " " WS-COL5 " " WS-COL6 " "
               WS-COL7 " " WS-COL8 " " WS-COL9
               IF WS-SEAT-KEY EQUAL 5
                   MOVE 'Y' TO WS-EOF
               END-IF
           END-PERFORM
           CLOSE FD-LAYOUT

           PERFORM UNTIL SEAT-VALID EQUAL 'Y'
           DISPLAY "ENTER SEAT(E.G. 1C)"
           ACCEPT SEAT-INPUT
           MOVE SEAT-INPUT(1:1) TO SEAT-KEY-INPUT
           MOVE SEAT-INPUT(2:1) TO SEAT-NUMBER-INPUT
           PERFORM IS-SEAT-TAKEN
           END-PERFORM

           PERFORM UNTIL RESERVE-STOP EQUAL 'Y'
           DISPLAY "YOU ARE ABOUT TO RESERVE A SEAT. DO YOU WISH TO"
           " PROCEED?"
           DISPLAY "1 - YES"
           DISPLAY "2 - NO"
           DISPLAY "0 - BACK"
           ACCEPT RESERVE-CONFIRM
           EVALUATE RESERVE-CONFIRM
               WHEN 1
               MOVE 'Y' TO RESERVE-STOP
               WHEN 2 PERFORM HOME-PARA
               WHEN 3 PERFORM RESERVE-SEAT
               WHEN OTHER PERFORM DEFAULT-PARA
               END-PERFORM
               MOVE 'N' TO RESERVE-STOP

           MOVE 'N' TO WS-EOF
           OPEN I-O FD-LAYOUT.
           MOVE SEAT-KEY-INPUT TO SEAT-KEY.
           READ FD-LAYOUT
           KEY IS SEAT-KEY
           NOT INVALID KEY
           PERFORM PUT-X
           REWRITE C-LAYOUT
           INVALID KEY DISPLAY "KEY IS NOT EXISTING!"
           END-REWRITE
           END-READ
           CLOSE FD-LAYOUT
           MOVE 1 TO CINEMA-NUM
           PERFORM SEAT-RESERVE-SUCCESS.

       NOW-SHOWING-TWO.
           MOVE 'N' TO SEAT-VALID
           MOVE 'N' TO WS-EOF
           DISPLAY " "
           DISPLAY "SELECTED MOVIE: " FUNCTION
                       UPPER-CASE(WS-TITLE)
           MOVE 'N' TO WS-EOF
           DISPLAY "**********************"
           DISPLAY " CINEMA SEAT 2 LAYOUT"
           DISPLAY "**********************"
           DISPLAY "  A B C D E F G H I J"
           OPEN INPUT FD-LAYOUT1
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-LAYOUT1 NEXT RECORD INTO WS-LAYOUT
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
               DISPLAY WS-SEAT-KEY " " WS-COL0 " " WS-COL1 " " WS-COL2
               " " WS-COL3 " " WS-COL4 " " WS-COL5 " " WS-COL6 " "
               WS-COL7 " " WS-COL8 " " WS-COL9
               IF WS-SEAT-KEY EQUAL 5
                   MOVE 'Y' TO WS-EOF
               END-IF
           END-PERFORM
           CLOSE FD-LAYOUT1

           PERFORM UNTIL SEAT-VALID EQUAL 'Y'
           DISPLAY "ENTER SEAT(E.G. 1C)"
           ACCEPT SEAT-INPUT
           MOVE SEAT-INPUT(1:1) TO SEAT-KEY-INPUT
           MOVE SEAT-INPUT(2:1) TO SEAT-NUMBER-INPUT
           PERFORM IS-SEAT-TAKEN2
           END-PERFORM

           PERFORM UNTIL RESERVE-STOP2 EQUAL 'Y'
           DISPLAY "YOU ARE ABOUT TO RESERVE A SEAT. DO YOU WISH TO"
           " PROCEED?"
           DISPLAY "1 - YES"
           DISPLAY "2 - NO"
           DISPLAY "0 - BACK"
           ACCEPT RESERVE-CONFIRM2
           EVALUATE RESERVE-CONFIRM2
               WHEN 1
               MOVE 'Y' TO RESERVE-STOP2
               WHEN 2 PERFORM HOME-PARA
               WHEN 3 PERFORM RESERVE-SEAT
               WHEN OTHER PERFORM DEFAULT-PARA
               END-PERFORM
               MOVE 'N' TO RESERVE-STOP2

           MOVE 'N' TO WS-EOF
           OPEN I-O FD-LAYOUT1
           MOVE SEAT-KEY-INPUT TO SEAT-KEY1
           READ FD-LAYOUT1
           KEY IS SEAT-KEY1
           NOT INVALID KEY
           PERFORM PUT-X2
           REWRITE C-LAYOUT1
           END-REWRITE
           END-READ
           CLOSE FD-LAYOUT1
           MOVE 2 TO CINEMA-NUM
           PERFORM SEAT-RESERVE-SUCCESS.

       HOME-PARA.
      *----COUNT NUMBER OF RESERVED BY THE CUSTOMER----
           PERFORM  UNTIL IS-VALID-CHOICE-HOME EQUAL 'Y'
           MOVE 00 TO CUSTOMER-CURRENT-TICKET
           MOVE 'N' TO WS-EOF
           OPEN INPUT FD-RESERVEDSEAT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-RESERVEDSEAT NEXT RECORD INTO WS-RESERVEDSEAT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-ACCID EQUAL WS-CUSTOMER-ID
                       ADD 1 TO CUSTOMER-CURRENT-TICKET
                   END-IF
               END-READ
           END-PERFORM
           CLOSE FD-RESERVEDSEAT
           MOVE 'N' TO WS-EOF
           DISPLAY " "
           DISPLAY "******************************************"
           DISPLAY " MOVIE TICKET RESERVATION - CUSTOMER MENU"
           DISPLAY "******************************************"
           DISPLAY "WELCOME, "FUNCTION UPPER-CASE(WS-FNAME)
           DISPLAY "A - RESERVE A SEAT"
           DISPLAY "B - VIEW RESERVED SEATS"
           DISPLAY "C - CANCEL RESERVED SEAT"
           DISPLAY "D - LOGOUT"
           ACCEPT HOME-CHOICE
           EVALUATE HOME-CHOICE
                WHEN 'A'
                WHEN 'a' PERFORM RESERVE-SEAT
                WHEN 'B'
                WHEN 'b' PERFORM CHOOSE-VIEW
                WHEN 'C'
                WHEN 'c' PERFORM CANCEL-SEAT
                WHEN 'D'
                WHEN 'd' PERFORM MAIN-PARA
                WHEN OTHER PERFORM DEFAULT-PARA
           END-EVALUATE
           END-PERFORM.

       CHOOSE-VIEW.
           PERFORM UNTIL SELECT-REFRESH EQUAL 'Y'
           DISPLAY " "
           DISPLAY "************************"
           DISPLAY " VIEW MY RESERVED SEATS"
           DISPLAY "************************"
           DISPLAY "1 - VIEW PAID RESERVATIONS"
           DISPLAY "2 - VIEW UNPAID RESERVATIONS"
           DISPLAY "0 - BACK"
           ACCEPT SELECT-VIEW
           EVALUATE SELECT-VIEW
               WHEN 1 PERFORM VIEW-FINISHED-TICKET
               MOVE 'Y' TO SELECT-REFRESH
               WHEN 2 PERFORM VIEW-RESERVE-TICKET
               MOVE 'Y' TO SELECT-REFRESH
               WHEN 0 PERFORM HOME-PARA
               MOVE 'Y' TO SELECT-REFRESH
               WHEN OTHER PERFORM DEFAULT-PARA
           END-EVALUATE
           END-PERFORM
           MOVE 'N' TO SELECT-REFRESH.

       VIEW-RESERVE-TICKET-CANCEL.
           MOVE 'N' TO WS-EOF
           OPEN INPUT FD-RESERVEDSEAT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-RESERVEDSEAT NEXT RECORD INTO WS-RESERVEDSEAT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-ACCID EQUAL WS-CUSTOMER-ID
                       AND WS-RESERVE-STATUS EQUAL 'UNPAID'
                       MOVE 'Y' TO YES-UNPAID
                       DISPLAY " "
                       DISPLAY "RESERVATION ID: " WS-SEATID
                       DISPLAY "DATE OF RESERVATION: " WS-DATE-RESERVED
                       DISPLAY "TIME OF RESERVATION: " WS-TIME-RESERVED
                       DISPLAY "SEAT NUMBER: "
                       WS-LAYOUT-NUM FUNCTION
                       UPPER-CASE(WS-SEATNUMBER)
                       DISPLAY "CINEMA: " WS-CINEMA-NUM
                       DISPLAY "TITLE: " FUNCTION
                       UPPER-CASE(WS-MOVIE-TITLE)
                       DISPLAY "STATUS: " FUNCTION
                       UPPER-CASE(WS-RESERVE-STATUS)
                   END-IF
               END-READ
           END-PERFORM
           IF YES-UNPAID NOT EQUAL 'Y'
               DISPLAY "YOU HAVE NO ANY UNPAID RESERVATIONS YET."
           END-IF.
           CLOSE FD-RESERVEDSEAT.

       VIEW-RESERVE-TICKET.
           MOVE 'N' TO WS-EOF
           OPEN INPUT FD-RESERVEDSEAT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-RESERVEDSEAT NEXT RECORD INTO WS-RESERVEDSEAT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-ACCID EQUAL WS-CUSTOMER-ID
                       AND WS-RESERVE-STATUS EQUAL 'UNPAID'
                       MOVE 'Y' TO YES-UNPAID
                       DISPLAY " "
                       DISPLAY "RESERVATION ID: " WS-SEATID
                       DISPLAY "DATE OF RESERVATION: " WS-DATE-RESERVED
                       DISPLAY "TIME OF RESERVATION: " WS-TIME-RESERVED
                       DISPLAY "SEAT NUMBER: "
                       WS-LAYOUT-NUM FUNCTION
                       UPPER-CASE(WS-SEATNUMBER)
                       DISPLAY "CINEMA: " WS-CINEMA-NUM
                       DISPLAY "TITLE: " FUNCTION
                       UPPER-CASE(WS-MOVIE-TITLE)
                       DISPLAY "STATUS: " FUNCTION
                       UPPER-CASE(WS-RESERVE-STATUS)
                   END-IF
               END-READ
           END-PERFORM
           IF YES-UNPAID NOT EQUAL 'Y'
               DISPLAY "YOU HAVE NO ANY UNPAID RESERVATIONS YET."
           END-IF.
           MOVE 'N' TO YES-UNPAID
           CLOSE FD-RESERVEDSEAT.

       VIEW-FINISHED-TICKET.
           MOVE 'N' TO WS-EOF
           OPEN INPUT FD-RESERVEDSEAT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-RESERVEDSEAT NEXT RECORD INTO WS-RESERVEDSEAT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-ACCID EQUAL WS-CUSTOMER-ID
                       AND WS-RESERVE-STATUS EQUAL 'PAID'
                       MOVE 'Y' TO YES-PAID
                       DISPLAY "RESERVATION ID: " WS-SEATID
                       DISPLAY "DATE OF RESERVATION: " WS-DATE-RESERVED
                       DISPLAY "TIME OF RESERVATION: " WS-TIME-RESERVED
                       DISPLAY "SEAT NUMBER: "
                       WS-LAYOUT-NUM FUNCTION
                       UPPER-CASE(WS-SEATNUMBER)
                       DISPLAY "CINEMA: " WS-CINEMA-NUM
                       DISPLAY "TITLE: " FUNCTION
                       UPPER-CASE(WS-MOVIE-TITLE)
                       DISPLAY "STATUS: " FUNCTION
                       UPPER-CASE(WS-RESERVE-STATUS)
                       DISPLAY " "
                   END-IF
               END-READ
           END-PERFORM
           IF YES-PAID NOT EQUAL 'Y'
               DISPLAY "YOU HAVE NO ANY PAID RESERVATIONS YET."
           END-IF
           MOVE 'N' TO YES-PAID
           CLOSE FD-RESERVEDSEAT.

       SEAT-RESERVE-SUCCESS.
       MOVE 'N' TO WS-EOF
           OPEN INPUT FD-RESERVEDSEAT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-RESERVEDSEAT NEXT RECORD INTO WS-RESERVEDSEAT
                   AT END MOVE 'Y' TO WS-EOF
                   MOVE WS-SEATID TO DEFAULTID
               END-READ
           END-PERFORM
           CLOSE FD-RESERVEDSEAT

       IF DEFAULTID EQUAL 0000
           ADD 1 TO DEFAULTID
       ELSE
           ADD 1 TO DEFAULTID
       END-IF

       IF CUSTOMER-CHOOSE-MOVIE EQUAL 1
           MOVE 1 TO CINEMA-NUM
       ELSE
           MOVE 2 TO CINEMA-NUM
       END-IF

       MOVE DEFAULTID TO SEATID
       MOVE WS-ACCID TO CUSTOMER-ID
       MOVE WS-MOVIECODE TO MOVIE-ID
       MOVE SEAT-KEY-INPUT TO LAYOUT-NUM
       MOVE SEAT-NUMBER-INPUT TO SEATNUMBER
       MOVE TIME-STRINGER TO TIME-RESERVED
       MOVE DATER TO DATE-RESERVED
       MOVE DEFAULT-STATUS TO RESERVE-STATUS
       MOVE WS-TITLE TO MOVIE-TITLE

       OPEN I-O FD-RESERVEDSEAT
           IF WS-FILESTATUS = 35 THEN
              OPEN OUTPUT FD-RESERVEDSEAT
           ELSE
               WRITE C-RESERVEDSEAT
           END-IF
       MOVE 'N' TO WS-EOF
       CLOSE FD-RESERVEDSEAT.

       CANCEL-SEAT.
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO RESERVATION-VALID
           MOVE 'N' TO CORRECT-ID
           DISPLAY " "
           DISPLAY "*************************"
           DISPLAY " CANCEL SEAT RESERVATION"
           DISPLAY "*************************"
           PERFORM VIEW-RESERVE-TICKET-CANCEL
           IF YES-UNPAID NOT EQUAL 'Y'
                 PERFORM HOME-PARA
           END-IF
           MOVE 'N' TO YES-UNPAID
           PERFORM UNTIL RESERVATION-VALID EQUAL 'Y'
           DISPLAY "ENTER RESERVATION ID:"
           ACCEPT RESERVE-ID-INPUT
           MOVE 'N' TO WS-EOF
           OPEN INPUT FD-RESERVEDSEAT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-RESERVEDSEAT NEXT RECORD INTO WS-RESERVEDSEAT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-ACCID EQUAL WS-CUSTOMER-ID
                      IF RESERVE-ID-INPUT EQUAL WS-SEATID
                          MOVE 'Y' TO WS-EOF
                          MOVE 'Y' TO RESERVATION-VALID
                          MOVE 'Y' TO CORRECT-ID
                      END-IF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE FD-RESERVEDSEAT

           IF CORRECT-ID EQUAL 'Y'
               ELSE
                   DISPLAY "ERROR! INVALID RESERVATION ID!"
                   MOVE 'N' TO WS-EOF
               END-IF
           END-PERFORM

           DISPLAY "YOU ARE ABOUT TO CANCEL YOUR SEAT RESERVATION ("
           RESERVE-ID-INPUT"), DO YOU WISH TO PROCEED?"
           PERFORM UNTIL IS-PROCEED-EVAL EQUAL 'Y'
           DISPLAY "1 - YES"
           DISPLAY "2 - NO"
           DISPLAY "0 - BACK"
           ACCEPT IS-PROCEED
               EVALUATE IS-PROCEED
                WHEN 1 MOVE 'Y' TO IS-PROCEED-EVAL
                PERFORM DELETE-RESERVATION
                WHEN 2 MOVE 'Y' TO IS-PROCEED-EVAL
                PERFORM HOME-PARA
                WHEN 0 MOVE 'Y' TO IS-PROCEED-EVAL
                PERFORM CANCEL-SEAT
                WHEN OTHER PERFORM DEFAULT-PARA
           END-EVALUATE
           END-PERFORM
           MOVE 'N' TO IS-PROCEED-EVAL.

       DELETE-RESERVATION.
           MOVE RESERVE-ID-INPUT TO SEATID
           OPEN I-O FD-RESERVEDSEAT
           DELETE FD-RESERVEDSEAT RECORD
           INVALID KEY DISPLAY 'INVALID KEY!'
           NOT INVALID KEY DISPLAY 'RESERVATION SUCCESSFULLY CANCELLED!'
           END-DELETE.
           CLOSE FD-RESERVEDSEAT.
           PERFORM MAKE-AVAILABLE-SEAT.

       MAKE-AVAILABLE-SEAT.
           EVALUATE WS-CINEMA-NUM
               WHEN 1
               OPEN I-O FD-LAYOUT
               MOVE WS-LAYOUT-NUM TO SEAT-KEY
               READ FD-LAYOUT
               KEY IS SEAT-KEY
               NOT INVALID KEY
               PERFORM PUT-ASTERISK
               REWRITE C-LAYOUT
               INVALID KEY DISPLAY "KEY IS NOT EXISTING!"
               END-REWRITE
               END-READ
               CLOSE FD-LAYOUT

               WHEN 2
               OPEN I-O FD-LAYOUT1
               MOVE WS-LAYOUT-NUM TO SEAT-KEY1
               READ FD-LAYOUT1
               KEY IS SEAT-KEY1
               NOT INVALID KEY
               PERFORM PUT-ASTERISK2
               REWRITE C-LAYOUT1
               END-REWRITE
               END-READ
               CLOSE FD-LAYOUT1.

       PUT-ASTERISK.
           IF WS-SEATNUMBER EQUAL 'A' OR WS-SEATNUMBER EQUAL 'a'
               MOVE '*' TO COL0
           ELSE IF WS-SEATNUMBER EQUAL 'B' OR
               WS-SEATNUMBER EQUAL 'b'
               MOVE '*' TO COL1
           ELSE IF WS-SEATNUMBER EQUAL 'C' OR
               WS-SEATNUMBER EQUAL 'c'
               MOVE '*' TO COL2
           ELSE IF WS-SEATNUMBER EQUAL 'D' OR
               WS-SEATNUMBER EQUAL 'd'
               MOVE '*' TO COL3
           ELSE IF WS-SEATNUMBER EQUAL 'E' OR
               WS-SEATNUMBER EQUAL 'e'
               MOVE '*' TO COL4
           ELSE IF WS-SEATNUMBER EQUAL 'F' OR
               WS-SEATNUMBER EQUAL 'f'
               MOVE '*' TO COL5
           ELSE IF WS-SEATNUMBER EQUAL 'G' OR
               WS-SEATNUMBER EQUAL 'g'
               MOVE '*' TO COL6
           ELSE IF WS-SEATNUMBER EQUAL 'H' OR
               WS-SEATNUMBER EQUAL 'h'
               MOVE '*' TO COL7
           ELSE IF WS-SEATNUMBER EQUAL 'I' OR
               WS-SEATNUMBER EQUAL 'i'
               MOVE '*' TO COL8
           ELSE IF WS-SEATNUMBER EQUAL 'J' OR
               WS-SEATNUMBER EQUAL 'j'
               MOVE '*' TO COL9
           END-IF.

       PUT-ASTERISK2.
           IF WS-SEATNUMBER EQUAL 'A' OR WS-SEATNUMBER EQUAL 'a'
               MOVE '*' TO COL01
           ELSE IF WS-SEATNUMBER EQUAL 'B' OR
               WS-SEATNUMBER EQUAL 'b'
               MOVE '*' TO COL11
           ELSE IF WS-SEATNUMBER EQUAL 'C' OR
               WS-SEATNUMBER EQUAL 'c'
               MOVE '*' TO COL21
           ELSE IF WS-SEATNUMBER EQUAL 'D' OR
               WS-SEATNUMBER EQUAL 'd'
               MOVE '*' TO COL31
           ELSE IF WS-SEATNUMBER EQUAL 'E' OR
               WS-SEATNUMBER EQUAL 'e'
               MOVE '*' TO COL41
           ELSE IF WS-SEATNUMBER EQUAL 'F' OR
               WS-SEATNUMBER EQUAL 'f'
               MOVE '*' TO COL51
           ELSE IF WS-SEATNUMBER EQUAL 'G' OR
               WS-SEATNUMBER EQUAL 'g'
               MOVE '*' TO COL61
           ELSE IF WS-SEATNUMBER EQUAL 'H' OR
               WS-SEATNUMBER EQUAL 'h'
               MOVE '*' TO COL71
           ELSE IF WS-SEATNUMBER EQUAL 'I' OR
               WS-SEATNUMBER EQUAL 'i'
               MOVE '*' TO COL81
           ELSE IF WS-SEATNUMBER EQUAL 'J' OR
               WS-SEATNUMBER EQUAL 'j'
               MOVE '*' TO COL91
           END-IF.

      *----VALIDATE IF SEAT IS ALREADY TAKEN FOR MOVIE 1----
       IS-SEAT-TAKEN.
           OPEN INPUT FD-LAYOUT.
           MOVE SEAT-KEY-INPUT TO SEAT-KEY.
           READ FD-LAYOUT RECORD INTO WS-LAYOUT
           KEY IS SEAT-KEY
           INVALID KEY DISPLAY 'ERROR! INVALID SEAT!'
           END-READ.
           CLOSE FD-LAYOUT.

           IF SEAT-NUMBER-INPUT EQUAL 'A' OR SEAT-NUMBER-INPUT EQUAL 'a'
               IF WS-COL0 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
         ELSE IF SEAT-NUMBER-INPUT EQUAL 'B' or
             SEAT-NUMBER-INPUT EQUAL 'b'
               IF COL1 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'C' OR
               SEAT-NUMBER-INPUT EQUAL 'c'
               IF COL2 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'D' OR
               SEAT-NUMBER-INPUT EQUAL 'd'
               IF COL3 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'E' OR
               SEAT-NUMBER-INPUT EQUAL 'e'
               IF COL4 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'F' OR
               SEAT-NUMBER-INPUT EQUAL 'f'
               IF COL5 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'G' OR
               SEAT-NUMBER-INPUT EQUAL 'g'
               IF COL6 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'H' OR
               SEAT-NUMBER-INPUT EQUAL 'h'
               IF COL7 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'I' OR
               SEAT-NUMBER-INPUT EQUAL 'i'
               IF COL8 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'J' OR
               SEAT-NUMBER-INPUT EQUAL 'j'
               IF COL9 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE
               DISPLAY "ERROR! INVALID INPUT!"
           END-IF.

      *----VALIDATE IF SEAT IS ALREADY TAKEN FOR MOVIE 2----
       IS-SEAT-TAKEN2.
           OPEN INPUT FD-LAYOUT1.
           MOVE SEAT-KEY-INPUT TO SEAT-KEY1.
           READ FD-LAYOUT1 RECORD INTO WS-LAYOUT
           KEY IS SEAT-KEY1
           INVALID KEY DISPLAY 'ERROR! INVALID SEAT!'
           END-READ.
           CLOSE FD-LAYOUT1.

           IF SEAT-NUMBER-INPUT EQUAL 'A' OR SEAT-NUMBER-INPUT EQUAL 'a'
               IF COL01 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
                ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'B' OR
               SEAT-NUMBER-INPUT EQUAL 'b'
               IF COL11 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'C' OR
               SEAT-NUMBER-INPUT EQUAL 'c'
               IF COL21 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'D' OR
               SEAT-NUMBER-INPUT EQUAL 'd'
               IF COL31 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'E' OR
               SEAT-NUMBER-INPUT EQUAL 'e'
               IF COL41 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'F' OR
               SEAT-NUMBER-INPUT EQUAL 'f'
               IF COL51 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'G' OR
               SEAT-NUMBER-INPUT EQUAL 'g'
               IF COL61 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'H' OR
               SEAT-NUMBER-INPUT EQUAL 'h'
               IF COL71 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'I' OR
               SEAT-NUMBER-INPUT EQUAL 'i'
               IF COL81 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'J' OR
               SEAT-NUMBER-INPUT EQUAL 'j'
               IF COL91 EQUAL 'X'
                   DISPLAY "THIS SEAT IS ALREADY TAKEN. CHOOSE"
                   " ANOTHER AVAILABLE SEAT!"
               ELSE
               MOVE 'Y' TO SEAT-VALID
               END-IF
           ELSE
               DISPLAY "ERROR! INVALID INPUT!"
           END-IF.

       PUT-X.
           IF SEAT-NUMBER-INPUT EQUAL 'A' OR SEAT-NUMBER-INPUT EQUAL 'a'
               MOVE 'X' TO COL0
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'B' OR
               SEAT-NUMBER-INPUT EQUAL 'b'
               MOVE 'X' TO COL1
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'C' OR
               SEAT-NUMBER-INPUT EQUAL 'c'
               MOVE 'X' TO COL2
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'D' OR
               SEAT-NUMBER-INPUT EQUAL 'd'
               MOVE'X' TO COL3
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'E' OR
               SEAT-NUMBER-INPUT EQUAL 'e'
               MOVE 'X' TO COL4
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'F' OR
               SEAT-NUMBER-INPUT EQUAL 'f'
               MOVE 'X' TO COL5
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'G' OR
               SEAT-NUMBER-INPUT EQUAL 'g'
               MOVE 'X' TO COL6
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'H' OR
               SEAT-NUMBER-INPUT EQUAL 'h'
               MOVE 'X' TO COL7
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'I' OR
               SEAT-NUMBER-INPUT EQUAL 'i'
               MOVE 'X' TO COL8
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'J' OR
               SEAT-NUMBER-INPUT EQUAL 'j'
               MOVE 'X' TO COL9
           ELSE
               DISPLAY "ERROR! INVALID INPUT!"
           END-IF.

       PUT-X2.
           IF SEAT-NUMBER-INPUT EQUAL 'A' OR SEAT-NUMBER-INPUT EQUAL 'a'
               MOVE 'X' TO COL01
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'B' OR
               SEAT-NUMBER-INPUT EQUAL 'b'
               MOVE 'X' TO COL11
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'C' OR
               SEAT-NUMBER-INPUT EQUAL 'c'
               MOVE 'X' TO COL21
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'D' OR
               SEAT-NUMBER-INPUT EQUAL 'd'
               MOVE'X' TO COL31
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'E' OR
               SEAT-NUMBER-INPUT EQUAL 'e'
               MOVE 'X' TO COL41
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'F' OR
               SEAT-NUMBER-INPUT EQUAL 'f'
               MOVE 'X' TO COL51
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'G' OR
               SEAT-NUMBER-INPUT EQUAL 'g'
               MOVE 'X' TO COL61
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'H' OR
               SEAT-NUMBER-INPUT EQUAL 'h'
               MOVE 'X' TO COL71
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'I' OR
               SEAT-NUMBER-INPUT EQUAL 'i'
               MOVE 'X' TO COL81
           ELSE IF SEAT-NUMBER-INPUT EQUAL 'J' OR
               SEAT-NUMBER-INPUT EQUAL 'j'
               MOVE 'X' TO COL91
           ELSE
               DISPLAY "ERROR! INVALID INPUT!"
           END-IF.

       ADMIN-REG.
           DISPLAY " ".
           DISPLAY "**********************"
           DISPLAY " REGISTER A NEW ADMIN".
           DISPLAY "**********************"
           DISPLAY "INPUT FIRST NAME:".
           ACCEPT FA-FNAME.
           DISPLAY "INPUT LAST NAME:".
           ACCEPT FA-LNAME.
           DISPLAY "INPUT PASSCODE (E.G. 1234)".
           ACCEPT FA-PASSCODE.

           MOVE FUNCTION CURRENT-DATE to WS-GENERATE-DATA

           MOVE WS-TIME TO FA-ACCID.

           COMPUTE FA-ACCID = FUNCTION
           RANDOM(WS-DATE) * FA-ACCID.

           DISPLAY "ACCOUNT ID: ["FA-ACCID"]"

           OPEN I-O FD-ADMIN.

           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-ADMIN
           END-IF

           WRITE FA-ACCOUNT
           DISPLAY " ".
           DISPLAY "ADMIN ACCOUNT SUCESSFULLY CREATED. PLEASE TAKE NOTE"
           " OF YOUR ACCOUNT DETAILS:".
           DISPLAY "ACCOUNT ID     : "FA-ACCID" ".
           DISPLAY "ACCOUNT HOLDER : ADMIN "FUNCTION
           UPPER-CASE(FA-LNAME) FUNCTION UPPER-CASE(FA-FNAME)
           DISPLAY "PASSCODE       : "FA-PASSCODE" ".
           CLOSE FD-ADMIN
           GO TO ADMIN-PARA.

       ADMIN-LOGIN.
           DISPLAY " ".
           DISPLAY "*******************"
           DISPLAY " LOGIN AS AN ADMIN".
           DISPLAY "*******************"
           DISPLAY "ACCOUNT ID:"
           ACCEPT FA-ACCID
           DISPLAY "PASSCODE:"
           ACCEPT FA-PASSCODE

           MOVE FA-PASSCODE TO WS-PASSCODE-TEMP

           OPEN I-O FD-ADMIN
           IF WS-FILESTATUS IS NOT EQUAL TO 35 THEN
                READ FD-ADMIN INTO WS-ACCOUNT
                    KEY IS FA-ACCID
                    INVALID KEY MOVE 0 TO WS-FLAG
                    NOT INVALID KEY MOVE 1 TO WS-FLAG
                END-READ
           ELSE
               DISPLAY "ACCOUNT DOES NOT EXIST!"
               GO TO ADMIN-PARA
           END-IF

           IF WS-FLAG = 1 THEN
               IF FA-PASSCODE = WS-PASSCODE-TEMP THEN
                   CLOSE FD-ADMIN
                   GO TO ADMIN-MENU
               ELSE
                   DISPLAY "ACCOUNT ID AND PASSCODE DOES NOT MATCH!"
                   CLOSE FD-ADMIN
                   GO TO ADMIN-PARA
               END-IF
           ELSE IF WS-FLAG = 0 THEN
               DISPLAY "ACCOUNT DOES NOT EXIST."
               CLOSE FD-ADMIN
               GO TO ADMIN-PARA
           END-IF.

           CLOSE FD-ADMIN.

       ADMIN-MENU.
           DISPLAY " ".
           DISPLAY "***************************************"
           DISPLAY " MOVIE TICKET RESERVATION - ADMIN MENU"
           DISPLAY "***************************************"
           DISPLAY "WELCOME BACK, ADMIN "FUNCTION UPPER-CASE(FA-FNAME).
           DISPLAY "1 - MOVIE SETTINGS".
           DISPLAY "2 - EMPLOYEE SETTINGS".
           DISPLAY "3 - LOGOUT".
           ACCEPT WS-CHOICE.

           IF WS-CHOICE = 1 THEN
               DISPLAY " "
               DISPLAY "**************************"
               DISPLAY " MOVIE SETTINGS FOR ADMIN"
               DISPLAY "**************************"
               DISPLAY "1 - ADD A NOW SHOWING MOVIE"
               DISPLAY "2 - REMOVE A MOVIE"
               DISPLAY "3 - VIEW LIST OF MOVIES"
               DISPLAY "4 - BACK"
               ACCEPT WS-DECISION

               IF WS-DECISION = 1 THEN
                   GO TO NEW-MOVIES
               ELSE IF WS-DECISION = 2 THEN
                   GO TO NO-MOVIES
               ELSE IF WS-DECISION = 3 THEN
                   GO TO VIEW-MOVIES
               ELSE IF WS-DECISION = 4 THEN
                   GO TO ADMIN-MENU
               ELSE
                   DISPLAY "ERROR! INVALID OPTION!"
               END-IF

           ELSE IF WS-CHOICE = 2 THEN
               DISPLAY " "
               DISPLAY "*****************************"
               DISPLAY " EMPLOYEE SETTINGS FOR ADMIN"
               DISPLAY "*****************************"
               DISPLAY "1 - ADD EMPLOYEE"
               DISPLAY "2 - REMOVE EMPLOYEE"
               DISPLAY "3 - VIEW LIST OF EMPLOYEES"
               DISPLAY "4 - BACK"
               ACCEPT WS-DECISION

               IF WS-DECISION = 1 THEN
                   GO TO NEW-EMPLOYEE
               ELSE IF WS-DECISION = 2 THEN
                   GO TO DEL-EMPLOYEE
               ELSE IF WS-DECISION = 3 THEN
                   GO TO VIEW-EMPLOYEE
               ELSE IF WS-DECISION = 4 THEN
                   GO TO ADMIN-MENU
               ELSE
                   DISPLAY "ERROR! INVALID OPTION!"
               END-IF

           ELSE IF WS-CHOICE = 3 THEN
               DISPLAY " "
               DISPLAY "ARE YOU SURE YOU WANT TO LOGOUT?"
               DISPLAY "1 - YES"
               DISPLAY "2 - NO"
               ACCEPT WS-CHOICE

               IF WS-CHOICE = 1 THEN
                   GO TO ADMIN-PARA
               ELSE IF WS-CHOICE = 2 THEN
                   CLOSE FD-ADMIN
                   GO TO ADMIN-MENU
               ELSE
                   DISPLAY "ERROR! INVALID OPTION."
                   GO TO ADMIN-MENU
                END-IF
           END-IF.

       NEW-MOVIES.
           MOVE 0 TO WS-MOVIE-COUNTER
           OPEN INPUT FD-MOVIES.
           PERFORM UNTIL WS-EOF = 'Y'
              READ FD-MOVIES NEXT RECORD INTO WS-MOVIES
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   COMPUTE WS-MOVIE-COUNTER = WS-MOVIE-COUNTER + 1
              END-READ
           END-PERFORM.
           CLOSE FD-MOVIES
           MOVE 'N' TO WS-EOF

           IF WS-MOVIE-COUNTER IS NOT GREATER THAN 1
               DISPLAY " "
               DISPLAY "*****************************"
               DISPLAY " ADD ADD A NOW SHOWING MOVIE"
               DISPLAY "*****************************"
           *> Movie code generator.
               COMPUTE WS-CODE = FUNCTION RANDOM * (WS-MAX - WS-MIN + 1)
                   + WS-MIN
               MOVE WS-CODE TO WS-MOVIECODE
           *> Additonal info needed for movie record
               DISPLAY "MOVIE CODE: ["WS-MOVIECODE"]"
               DISPLAY "ENTER TITLE: "
               ACCEPT FM-TITLE
               DISPLAY "ENTER RELEASE DATE: "
               ACCEPT FM-RDATE
               DISPLAY "ENTER MTRCB RATING (G, PG, SPG): "
               ACCEPT FM-RATINGS
               DISPLAY "ENTER SYNOPSIS (UP TO 800 CHARACTERS): "
               ACCEPT FM-SYNOPSIS

               MOVE WS-MOVIECODE TO FM-MOVIECODE
               OPEN I-O FD-MOVIES
                   IF WS-FILESTATUS = 35 THEN
                       OPEN OUTPUT FD-MOVIES
                   END-IF

               WRITE FM-RECORD
               CLOSE FD-MOVIES

               DISPLAY " "
               DISPLAY "MOVIE SUCCESSFULLY ADDED!"

           ELSE
                DISPLAY "OPERATION FAILED! REACHED MAXIMUM NUMBER OF"
                " ALLOWABLE MOVIES TO BE ADDED!"
           END-IF
           CLOSE FD-MOVIES
           GO TO ADMIN-MENU.

       NO-MOVIES.
           DISPLAY " ".
           DISPLAY "**************************"
           DISPLAY " REMOVE AN EXISTING MOVIE".
           DISPLAY "**************************"
           DISPLAY "ENTER MOVIE CODE:"
           ACCEPT FM-MOVIECODE

           OPEN I-O FD-MOVIES.
           READ FD-MOVIES
           DELETE FD-MOVIES RECORD
               NOT INVALID KEY DISPLAY "MOVIE SUCCESFULLY REMOVED!"
               INVALID KEY DISPLAY "NO MOVIE FOUND!"
           END-DELETE
           CLOSE FD-MOVIES.
           GO TO ADMIN-MENU.

       VIEW-MOVIES.
           DISPLAY " ".
           DISPLAY "********************"
           DISPLAY " LIST OF ALL MOVIES"
           DISPLAY "********************"
           OPEN INPUT FD-MOVIES.
           PERFORM UNTIL WS-EOF = 'Y'
              READ FD-MOVIES NEXT RECORD INTO WS-MOVIES
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY "MOVIE CODE  : ["WS-MOVIECODE"]"
                       DISPLAY "MOVIE TITLE : "FUNCTION
                       UPPER-CASE(WS-TITLE)
                       DISPLAY " "
              END-READ
           END-PERFORM.
           CLOSE FD-MOVIES.

           MOVE 'N' TO WS-EOF
           GO TO ADMIN-MENU.

       NEW-EMPLOYEE.
           DISPLAY " ".
           DISPLAY "*************************"
           DISPLAY " REGISTER A NEW EMPLOYEE".
           DISPLAY "*************************"
           DISPLAY "ENTER FIRST NAME: "
           ACCEPT FE-FNAME
           DISPLAY "ENTER LAST NAME: "
           ACCEPT FE-LNAME
           DISPLAY "ENTER POST NUMBER: "
           ACCEPT FE-POSITION
           DISPLAY "ENTER PASSCODE (E.G. 1234):"
           ACCEPT FE-PASSCODE

           MOVE FUNCTION CURRENT-DATE to WS-GENERATE-DATA
           MOVE WS-TIME TO FE-ACCID.
           COMPUTE FE-ACCID = FUNCTION RANDOM(WS-DATE) * FE-ACCID.

           DISPLAY "ACCOUNT ID: ["FE-ACCID"]"

           OPEN I-O FD-EMPLOYEES
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-EMPLOYEES
           END-IF
           WRITE FE-ACCOUNT

`          DISPLAY " ".
           DISPLAY "EMPLOYEE ACCOUNT SUCESSFULLY CREATED. PLEASE TAKE"
           " NOTE OF EMPLOYEE'S ACCOUNT DETAILS: ".
           DISPLAY "ACCOUNT ID     : "FE-ACCID" ".
           DISPLAY "ACCOUNT HOLDER : "FUNCTION UPPER-CASE(FE-LNAME)
                                      FUNCTION UPPER-CASE(FE-FNAME).
           DISPLAY "POST           : "FE-POSITION" ".
           CLOSE FD-EMPLOYEES
           GO TO ADMIN-MENU.

       DEL-EMPLOYEE.
           DISPLAY " ".
           DISPLAY "*****************************"
           DISPLAY " REMOVE AN EXISTING EMPLOYEE".
           DISPLAY "*****************************"
           DISPLAY "ENTER EMPLOYEE'S ACCOUNT ID:".
           ACCEPT FE-ACCID

           OPEN I-O FD-EMPLOYEES.
           READ FD-EMPLOYEES
           DELETE FD-EMPLOYEES RECORD
               NOT INVALID KEY DISPLAY "EMPLOYEE REMOVED!"
               INVALID KEY DISPLAY "ERROR! INVALID ACCOUNT ID!"
           END-DELETE
           CLOSE FD-EMPLOYEES.
           GO TO ADMIN-MENU.

       VIEW-EMPLOYEE.
           DISPLAY " ".
           DISPLAY "***********************"
           DISPLAY " LIST OF ALL EMPLOYEES".
           DISPLAY "***********************"
           OPEN INPUT FD-EMPLOYEES.
           MOVE 'N' TO WS-EOF.
           PERFORM UNTIL WS-EOF = 'Y'
              READ FD-EMPLOYEES NEXT RECORD INTO WSA-ACCOUNT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY " "
                       DISPLAY "EMPLOYEE ACCOUNT ID   : "WSA-ACCID" "
                       DISPLAY "EMPLOYEE NAME         : "
                       FUNCTION UPPER-CASE(WSA-LNAME)
                       DISPLAY "EMPLOYEE POST NUMBER  : "WSA-POSITION" "
              END-READ
           END-PERFORM.
           CLOSE FD-EMPLOYEES.
           MOVE 'N' TO WS-EOF
           GO TO ADMIN-MENU.

       EMPLOYEE-PARA.
           DISPLAY " ".
           DISPLAY "****************************"
           DISPLAY " WELCOME TO EMPLOYEE PORTAL".
           DISPLAY "****************************"
           DISPLAY "1 - LOGIN".
           DISPLAY "2 - GO BACK TO MENU".
           ACCEPT WS-ECHOICE.

           IF WS-ECHOICE = 1 THEN
               GO TO EMPLOYEE-LOGIN
           ELSE IF WS-ECHOICE = 2 THEN
               GO TO MAIN-PARA
           ELSE
               DISPLAY "ERROR! INVALID OPTION."
               GO TO EMPLOYEE-PARA
           END-IF.

       EMPLOYEE-LOGIN.
           DISPLAY " ".
           DISPLAY "**********************"
           DISPLAY " LOGIN AS AN EMPLOYEE".
           DISPLAY "**********************"
           DISPLAY "ACCOUNT ID: "
           ACCEPT FE-ACCID
           DISPLAY "PASSCODE (E.G. 1234): "
           ACCEPT FE-PASSCODE

           MOVE FE-PASSCODE TO WS-PASSCODE-TEMP

           OPEN I-O FD-EMPLOYEES
           IF WS-FILESTATUS IS NOT EQUAL TO 35 THEN
                READ FD-EMPLOYEES INTO WSA-ACCOUNT
                    KEY IS FE-ACCID
                    INVALID KEY MOVE 0 TO WS-FLAG
                    NOT INVALID KEY MOVE 1 TO WS-FLAG
                END-READ
           ELSE
               DISPLAY "ACCOUNT DOES NOT EXIST!"
               CLOSE FD-EMPLOYEES
               GO TO EMPLOYEE-PARA
           END-IF

           IF WS-FLAG = 1 THEN
               IF FE-PASSCODE = WS-PASSCODE-TEMP THEN
                   CLOSE FD-EMPLOYEES
                   GO TO EMPLOYEE-MENU
               ELSE
                   DISPLAY "ACCOUNT ID AND PASSCODE DOES NOT MATCH!"
                   CLOSE FD-EMPLOYEES
                   GO TO EMPLOYEE-PARA
               END-IF
           ELSE IF WS-FLAG = 0 THEN
               DISPLAY "ACCOUNT DOES NOT EXIST!"
               CLOSE FD-EMPLOYEES
               GO TO EMPLOYEE-PARA
           END-IF.

           CLOSE FD-EMPLOYEES.

       EMPLOYEE-MENU.
           DISPLAY " ".
           DISPLAY "******************************************"
           DISPLAY " MOVIE TICKET RESERVATION - EMPLOYEE MENU"
           DISPLAY "******************************************"
           DISPLAY "WELCOME BACK, EMPLOYEE " FUNCTION
                    UPPER-CASE(FE-FNAME)
           DISPLAY "1 - VIEW TICKET DETAILS".
           DISPLAY "2 - CONFIRM PAYMENT".
           DISPLAY "3 - LOGOUT".
           ACCEPT WS-CHOICE.

           IF WS-CHOICE = 1 THEN
               GO TO VIEW-TICKET
           ELSE IF WS-CHOICE = 2 THEN
               GO TO PAY-CONFIRM
           ELSE IF WS-CHOICE = 3 THEN
               GO TO EMPLOYEE-PARA
           ELSE
               DISPLAY "ERROR! INVALID INPUT!"
               GO TO EMPLOYEE-MENU
           END-IF.

       VIEW-TICKET.
           OPEN INPUT FD-MOVIES.
              READ FD-MOVIES NEXT RECORD INTO WS-MOVIES
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY " "
                       DISPLAY "MOVIE CODE  : ["WS-MOVIECODE"]"
                       DISPLAY "MOVIE TITLE : "FUNCTION
                       UPPER-CASE(WS-TITLE)
              END-READ
           DISPLAY "**********************"
           DISPLAY " CINEMA 1 SEAT LAYOUT"
           DISPLAY "**********************"
           DISPLAY "  A B C D E F G H I J"
           OPEN INPUT FD-LAYOUT
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-LAYOUT NEXT RECORD INTO WS-LAYOUT
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
               DISPLAY WS-SEAT-KEY " " WS-COL0 " " WS-COL1 " " WS-COL2
               " " WS-COL3 " " WS-COL4 " " WS-COL5 " " WS-COL6 " "
               WS-COL7 " " WS-COL8 " " WS-COL9
               IF WS-SEAT-KEY EQUAL 5
                   MOVE 'Y' TO WS-EOF
               END-IF
           END-PERFORM
           CLOSE FD-LAYOUT

           READ FD-MOVIES NEXT RECORD INTO WS-MOVIES
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY " "
                       DISPLAY "MOVIE CODE  : ["WS-MOVIECODE"]"
                       DISPLAY "MOVIE TITLE : "
                       FUNCTION UPPER-CASE(WS-TITLE)
              END-READ

           MOVE 'N' TO WS-EOF

           DISPLAY "**********************"
           DISPLAY " CINEMA 2 SEAT LAYOUT"
           DISPLAY "**********************"
           DISPLAY "  A B C D E F G H I J"
           OPEN INPUT FD-LAYOUT1
           PERFORM UNTIL WS-EOF EQUAL 'Y'
               READ FD-LAYOUT1 NEXT RECORD INTO WS-LAYOUT
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
               DISPLAY WS-SEAT-KEY " " WS-COL0 " " WS-COL1 " " WS-COL2
               " " WS-COL3 " " WS-COL4 " " WS-COL5 " " WS-COL6 " "
               WS-COL7 " " WS-COL8 " " WS-COL9
               IF WS-SEAT-KEY EQUAL 5
                   MOVE 'Y' TO WS-EOF
               END-IF
           END-PERFORM
           CLOSE FD-LAYOUT1
           CLOSE FD-MOVIES
           MOVE 'N' TO WS-EOF

           DISPLAY " "
           DISPLAY "INPUT RESERVATION ID:"
           ACCEPT SEATID
           OPEN I-O FD-RESERVEDSEAT
           IF WS-FILESTATUS IS NOT EQUAL TO 35 THEN
           READ FD-RESERVEDSEAT INTO WS-RESERVEDSEAT
                   KEY IS SEATID
                   NOT INVALID KEY
                       DISPLAY " "
                       DISPLAY "CUSTOMER ID: " WS-CUSTOMER-ID
                       DISPLAY "RESERVATION ID: " WS-SEATID
                       DISPLAY "DATE OF RESERVATION: " WS-DATE-RESERVED
                       DISPLAY "TIME OF RESERVATION: " WS-TIME-RESERVED
                       DISPLAY "SEAT NUMBER: "
                       WS-LAYOUT-NUM FUNCTION UPPER-CASE(WS-SEATNUMBER)
                       DISPLAY "CINEMA: " WS-CINEMA-NUM
                       DISPLAY "TITLE: "
                       FUNCTION UPPER-CASE(WS-MOVIE-TITLE)
                       DISPLAY "STATUS: " FUNCTION
                       UPPER-CASE(WS-RESERVE-STATUS)
                   INVALID KEY DISPLAY "INVALID RESERVATION ID!"
           ElSE
               DISPLAY "NO RECORD FOUND!"
           END-IF

           CLOSE FD-RESERVEDSEAT.
           GO TO EMPLOYEE-MENU.

       PAY-CONFIRM.
           DISPLAY " "
           DISPLAY "*******************************"
           DISPLAY " CUSTOMER PAYMENT CONFIRMATION"
           DISPLAY "*******************************"
           DISPLAY "INPUT RESERVATION ID:"
           ACCEPT SEATID

           OPEN I-O FD-RESERVEDSEAT
           IF WS-FILESTATUS IS NOT EQUAL TO 35 THEN
           READ FD-RESERVEDSEAT INTO WS-RESERVEDSEAT
                   KEY IS SEATID
                   NOT INVALID KEY
                       MOVE 'PAID' TO WS-RESERVE-STATUS
                       DISPLAY" "
                       DISPLAY "PAID STATUS CHANGED SUCCESSFULLY!"
                       MOVE WS-RESERVE-STATUS TO RESERVE-STATUS
                   INVALID KEY DISPLAY "THIS USER HAS NO ACTIVE "
                   " RESERVATION YET!"
           ElSE
               DISPLAY "NO RECORD FOUND!"
           END-IF

           REWRITE C-RESERVEDSEAT.
           CLOSE FD-RESERVEDSEAT.

           GO TO EMPLOYEE-MENU
           STOP RUN.

       DEFAULT-PARA.
           DISPLAY "ERROR! INVALID INPUT!".

       EXIT-PARA.
           STOP RUN.
