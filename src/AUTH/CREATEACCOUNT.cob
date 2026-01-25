       IDENTIFICATION DIVISION.
         PROGRAM-ID. CREATEACCOUNT.

       ENVIRONMENT DIVISION.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.

            SELECT USERS-FILE ASSIGN TO "USERS.DAT"
                     ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
         FILE SECTION.

           FD USERS-FILE.
                01 USER-RECORD.
                     05 USERNAME        PIC X(12).
                     05 PASSWORD        PIC X(12).

          WORKING-STORAGE SECTION.
      *> WS-I VARIABLE I FOR LOOP TO CHECK ALL USERS TO SEE IF UNIQUE
           77 WS-I PIC 9 VALUE 1.
           77 WS-HAS-UPPER   PIC X VALUE "N".
           77 WS-HAS-DIGIT   PIC X VALUE "N".
           77 WS-HAS-SPECIAL PIC X VALUE "N".
           77 WS-POS         PIC 99 VALUE 1.
           77 WS-CHAR        PIC X.
           77 WS-PASS-LEN    PIC 99 VALUE 0.

           77 WS-TRIM-PASSWORD PIC X(12).



      *> WS-COUNT keeps track of number of existing accounts
           77 WS-COUNT PIC 9 VALUE 0.

           77 WS-USER-EOF PIC X VALUE "N".

      *> WS-FOUND is a flag to check if username is unique
           77 WS-FOUND PIC X VALUE "N".
      *> WS-ACCOUNTS-EXISTING is an array to hold existing usernames and passwords
           01 WS-ACCOUNTS-EXISTING.
                05 WS-USER-TABLE PIC X(12) OCCURS 5 TIMES.
                05 WS-PASS-TABLE PIC X(12) OCCURS 5 TIMES.

          LINKAGE SECTION.
              77 LK-USERNAME PIC X(12).
              77 LK-PASSWORD PIC X(12).
              77 LK-STATUS PIC X(1).
              77 LK-MESSAGE PIC X(100).
              77 LK-RET-CODE PIC 9.

           PROCEDURE DIVISION USING LK-USERNAME LK-PASSWORD LK-STATUS LK-MESSAGE LK-RET-CODE.
             *> Main sequence: load existing users, validate inputs, then save
             PERFORM LOAD-USERS

             IF WS-COUNT = 5
                MOVE "N" TO LK-STATUS
                MOVE 1 TO LK-RET-CODE
                MOVE "All permitted accounts have been created, please come back later" TO LK-MESSAGE
                GOBACK
             END-IF

             PERFORM CHECK-INPUT
             IF LK-STATUS = "N"
                GOBACK
             END-IF

             PERFORM CHECK-USERNAME-UNIQUE
             IF LK-STATUS = "N"
                GOBACK
             END-IF

             PERFORM VALIDATE-PASSWORD
             IF LK-STATUS = "N"
                GOBACK
             END-IF

             PERFORM ADD-SAVE
             GOBACK.

           *> Paragraphs
           CHECK-INPUT.
               IF FUNCTION LENGTH(FUNCTION TRIM(LK-USERNAME)) = 0
                  OR FUNCTION LENGTH(FUNCTION TRIM(LK-PASSWORD)) = 0
                   MOVE "N" TO LK-STATUS
                   MOVE 7 TO LK-RET-CODE
                   MOVE "Username or password not provided; returning to menu." TO LK-MESSAGE
               END-IF.

           LOAD-USERS.
             MOVE 0 TO WS-COUNT.
             MOVE "N" TO WS-USER-EOF.
             OPEN INPUT USERS-FILE
             PERFORM UNTIL WS-USER-EOF = "Y" OR WS-COUNT = 5
                  READ USERS-FILE
                     AT END
                         MOVE "Y" TO WS-USER-EOF
                     NOT AT END
                         ADD 1 TO WS-COUNT
                         MOVE FUNCTION TRIM(USERNAME) TO WS-USER-TABLE(WS-COUNT)
                         MOVE FUNCTION TRIM(PASSWORD) TO WS-PASS-TABLE(WS-COUNT)
                  END-READ
              END-PERFORM
             CLOSE USERS-FILE.

           CHECK-USERNAME-UNIQUE.
               MOVE "N" TO WS-FOUND
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT OR WS-FOUND = "Y"
                   IF FUNCTION TRIM(LK-USERNAME) = FUNCTION TRIM(WS-USER-TABLE(WS-I))
                       MOVE "Y" TO WS-FOUND
                   END-IF
               END-PERFORM
               IF WS-FOUND = "Y"
                   MOVE "N" TO LK-STATUS
                   MOVE 2 TO LK-RET-CODE
                   MOVE "Username already exists" TO LK-MESSAGE
               END-IF.

           VALIDATE-PASSWORD.
               MOVE "N" TO WS-HAS-UPPER
               MOVE "N" TO WS-HAS-DIGIT
               MOVE "N" TO WS-HAS-SPECIAL

               MOVE FUNCTION TRIM(LK-PASSWORD) TO WS-TRIM-PASSWORD
               COMPUTE WS-PASS-LEN = FUNCTION LENGTH(FUNCTION TRIM(LK-PASSWORD))

               IF WS-PASS-LEN < 8 OR WS-PASS-LEN > 12
                   MOVE "N" TO LK-STATUS
                   MOVE 3 TO LK-RET-CODE
                   MOVE "Password must be 8 to 12 characters." TO LK-MESSAGE
                   EXIT PARAGRAPH
               END-IF

               PERFORM VARYING WS-POS FROM 1 BY 1 UNTIL WS-POS > WS-PASS-LEN
                   MOVE WS-TRIM-PASSWORD(WS-POS:1) TO WS-CHAR
                   IF WS-CHAR >= "A" AND WS-CHAR <= "Z"
                       MOVE "Y" TO WS-HAS-UPPER
                   ELSE
                       IF WS-CHAR >= "0" AND WS-CHAR <= "9"
                           MOVE "Y" TO WS-HAS-DIGIT
                       ELSE
                           IF WS-CHAR NOT = " "
                              AND NOT (WS-CHAR >= "a" AND WS-CHAR <= "z")
                              AND NOT (WS-CHAR >= "A" AND WS-CHAR <= "Z")
                              AND NOT (WS-CHAR >= "0" AND WS-CHAR <= "9")
                               MOVE "Y" TO WS-HAS-SPECIAL
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM

               IF WS-HAS-UPPER NOT = "Y"
                   MOVE "N" TO LK-STATUS
                   MOVE 4 TO LK-RET-CODE
                   MOVE "Password must include at least one capital letter." TO LK-MESSAGE
                   EXIT PARAGRAPH
               END-IF

               IF WS-HAS-DIGIT NOT = "Y"
                   MOVE "N" TO LK-STATUS
                   MOVE 5 TO LK-RET-CODE
                   MOVE "Password must include at least one digit." TO LK-MESSAGE
                   EXIT PARAGRAPH
               END-IF

               IF WS-HAS-SPECIAL NOT = "Y"
                   MOVE "N" TO LK-STATUS
                   MOVE 6 TO LK-RET-CODE
                   MOVE "Password must include at least one special character." TO LK-MESSAGE
                   EXIT PARAGRAPH
               END-IF

               MOVE "Y" TO LK-STATUS
               MOVE 0 TO LK-RET-CODE
               MOVE "Password is valid." TO LK-MESSAGE.

           ADD-SAVE.
               ADD 1 TO WS-COUNT
               MOVE FUNCTION TRIM(LK-USERNAME) TO WS-USER-TABLE(WS-COUNT)
               MOVE FUNCTION TRIM(LK-PASSWORD) TO WS-PASS-TABLE(WS-COUNT)
               OPEN OUTPUT USERS-FILE
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT
                    MOVE WS-USER-TABLE(WS-I) TO USERNAME
                    MOVE WS-PASS-TABLE(WS-I) TO PASSWORD
                    WRITE USER-RECORD
               END-PERFORM
               CLOSE USERS-FILE
               MOVE "Y" TO LK-STATUS
               MOVE 0 TO LK-RET-CODE
               MOVE "Account created succesfully." TO LK-MESSAGE.

