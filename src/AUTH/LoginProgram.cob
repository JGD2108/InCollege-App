IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGINPROG.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT USERS-FILE ASSIGN TO "USERS.DAT"
                    ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD USERS-FILE.
           01 USER-RECORD.
             05 USERNAME PIC X(12).
             05 PASSWORD PIC X(12).

       WORKING-STORAGE SECTION.
         77 WS-I PIC 9 VALUE 1.
         77 WS-COUNT PIC 9 VALUE 0.
         77 WS-USER-EOF PIC X VALUE "N".
         77 WS-FOUND PIC X VALUE "N".
         77 WS-FOUND-INDEX PIC 9 VALUE 0.
         01 WS-USER-TABLE.
           05 WS-UT PIC X(12) OCCURS 5 TIMES.
         01 WS-PASS-TABLE.
           05 WS-PT PIC X(12) OCCURS 5 TIMES.

       LINKAGE SECTION.
         77 LK-USERNAME PIC X(12).
         77 LK-PASSWORD PIC X(12).
         77 LK-STATUS PIC X(1).
         77 LK-MESSAGE PIC X(100).

       PROCEDURE DIVISION USING LK-USERNAME LK-PASSWORD LK-STATUS LK-MESSAGE.
         *> Validate credentials against USERS.DAT
         PERFORM LOAD-USERS

         MOVE "N" TO WS-FOUND
         MOVE 0 TO WS-FOUND-INDEX
         PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT OR WS-FOUND = "Y"
           IF FUNCTION TRIM(LK-USERNAME) = FUNCTION TRIM(WS-UT(WS-I))
             MOVE "Y" TO WS-FOUND
             MOVE WS-I TO WS-FOUND-INDEX
           END-IF
         END-PERFORM

         IF WS-FOUND = "Y"
           IF FUNCTION TRIM(LK-PASSWORD) = FUNCTION TRIM(WS-PT(WS-FOUND-INDEX))
             MOVE "Y" TO LK-STATUS
             MOVE "You have successfully logged in" TO LK-MESSAGE
           ELSE
             MOVE "N" TO LK-STATUS
             MOVE "Incorrect username / password, please try again" TO LK-MESSAGE
           END-IF
         ELSE
           MOVE "N" TO LK-STATUS
           MOVE "Incorrect username / password, please try again" TO LK-MESSAGE
         END-IF

         GOBACK.

       LOAD-USERS.
         MOVE 0 TO WS-COUNT
         MOVE "N" TO WS-USER-EOF
         OPEN INPUT USERS-FILE
         PERFORM UNTIL WS-USER-EOF = "Y" OR WS-COUNT = 5
           READ USERS-FILE
             AT END
               MOVE "Y" TO WS-USER-EOF
             NOT AT END
               ADD 1 TO WS-COUNT
               MOVE FUNCTION TRIM(USERNAME) TO WS-UT(WS-COUNT)
               MOVE FUNCTION TRIM(PASSWORD) TO WS-PT(WS-COUNT)
           END-READ
         END-PERFORM
         CLOSE USERS-FILE.

       END PROGRAM LOGINPROG.
