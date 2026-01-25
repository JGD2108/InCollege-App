IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE-START.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUTPUT-FILE ASSIGN TO "OUTPUT.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.

             SELECT USERS-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

         FD  INPUT-FILE.
            01  INPUT-RECORD        PIC X(80).

         FD  OUTPUT-FILE.
            01  OUTPUT-RECORD       PIC X(80).

        FD USERS-FILE.
          01 USER-RECORD.
            05 USERNAME PIC X(12).
            05 PASSWORD PIC X(12).

       WORKING-STORAGE SECTION.
         77 Text-Line-to-Print-Write PIC X(80).

         77 MENU-CHOICE PIC X(1).

         77 WS-EOF PIC X VALUE "N".


         77 WS-USERNAME PIC X(12).
         77 WS-PASSWORD PIC X(12).
      *> WS-STATUS is a 1 character success/failure flag, 'Y' for success and 'N' for failure
         77 WS-STATUS PIC X(1).
      *> WS-MESSAGE is a 100 character message that CREATE-ACCOUNT prepares and MAIN PRINTS
      *> Example: "Account created successfully" or "Username already exists"
         77 WS-MESSAGE PIC X(100).

      *> WS-RET-CODE is a numeric return code from CREATE-ACCOUNT
      *> 0 = no error / not set;
      *> 1 = account limit reached
      *> 2 = username already exists
      *> 3 = password length invalid
      *> 4 = missing uppercase
      *> 5 = missing digit
      *> 6 = missing special character
         77 WS-RET-CODE PIC 9 VALUE 0.


      *> WS-TRIMMED-IN: holds trimmed input
      *> WS-IN-LEN: length trimmed input
      *> WS-OK:'N' if the username of password is too long
         77 WS-TRIMMED-IN   PIC X(80).
         77 WS-IN-LEN       PIC 99 VALUE 0.
         77 WS-OK           PIC X VALUE "Y".

          *> Variables and tables for account creation (inlined from CREATEACCOUNT)
          77 WS-I PIC 9 VALUE 1.
          77 WS-HAS-UPPER   PIC X VALUE "N".
          77 WS-HAS-DIGIT   PIC X VALUE "N".
          77 WS-HAS-SPECIAL PIC X VALUE "N".
          77 WS-POS         PIC 99 VALUE 1.
          77 WS-CHAR        PIC X.
          77 WS-PASS-LEN    PIC 99 VALUE 0.
          77 WS-TRIM-PASSWORD PIC X(12).
          77 WS-COUNT PIC 9 VALUE 0.
          77 WS-USER-EOF PIC X VALUE "N".
          77 WS-FOUND PIC X VALUE "N".
          77 WS-FOUND-INDEX PIC 9 VALUE 0.
          *> Flag set to "Y" once login succeeds; controls post-login menu flow
            77 WS-LOGGED-IN PIC X VALUE "N".
          *> Post-login menu choice and skill selection choice
            77 WS-POST-CHOICE PIC X(1).
            77 WS-SKILL-CHOICE PIC X(1).
          *> Simple list of 5 skills; populated when needed
            01 WS-SKILL-LIST.
             05 WS-SKILL PIC X(30) OCCURS 5 TIMES.
          01 WS-ACCOUNTS-EXISTING.
            05 WS-USER-TABLE PIC X(12) OCCURS 5 TIMES.
            05 WS-PASS-TABLE PIC X(12) OCCURS 5 TIMES.


       PROCEDURE DIVISION.

      *> Start main program
       PERFORM MAIN-PARA.
       GO TO PROGRAM-END.

       PROGRAM-END.
          CLOSE INPUT-FILE OUTPUT-FILE.
          STOP RUN.

      *> Paragrah (function) to print a line to output file and display
       PRINT-LINE.
           DISPLAY OUTPUT-RECORD
           WRITE OUTPUT-RECORD.

       READ-AND-LOG.
           READ INPUT-FILE
                AT END
                    MOVE "Y" TO WS-EOF
                NOT AT END
                    MOVE INPUT-RECORD TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
           END-READ.

       LOGIN-PARA.
          PERFORM LOAD-USERS-LOCAL

          *> Keep trying until successful login or EOF
          PERFORM UNTIL WS-EOF = "Y"
            MOVE "Enter username:" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for username; returning to menu." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PERFORM
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-USERNAME

            MOVE "Enter password:" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for password; returning to menu." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PERFORM
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PASSWORD

            *> Search for username
            MOVE "N" TO WS-FOUND
            MOVE 0 TO WS-FOUND-INDEX
            PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT OR WS-FOUND = "Y"
              IF FUNCTION TRIM(WS-USERNAME) = FUNCTION TRIM(WS-USER-TABLE(WS-I))
                MOVE "Y" TO WS-FOUND
                MOVE WS-I TO WS-FOUND-INDEX
              END-IF
            END-PERFORM

            *> Check if username exists and password matches
            IF WS-FOUND = "Y"
              IF FUNCTION TRIM(WS-PASSWORD) = FUNCTION TRIM(WS-PASS-TABLE(WS-FOUND-INDEX))
                *> Successful login: set flag and show post-login menu
                MOVE "You have successfully logged in" TO OUTPUT-RECORD
                PERFORM PRINT-LINE
                MOVE "Y" TO WS-LOGGED-IN
                PERFORM POST-LOGIN-MENU
                *> After returning from post-login, clear flag and exit login loop
                MOVE "N" TO WS-LOGGED-IN
                EXIT PERFORM
              ELSE
                MOVE "Incorrect username / password, please try again" TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              END-IF
            ELSE
              MOVE "Incorrect username / password, please try again" TO OUTPUT-RECORD
              PERFORM PRINT-LINE
            END-IF
          END-PERFORM.

       MAIN-PARA.
       OPEN INPUT INPUT-FILE
       OPEN OUTPUT OUTPUT-FILE

       MOVE "N" TO WS-EOF

       PERFORM UNTIL WS-EOF = "Y"
         MOVE "Welcome to InCollege!" TO OUTPUT-RECORD
         PERFORM PRINT-LINE

         MOVE "Please select an option:" TO OUTPUT-RECORD
         PERFORM PRINT-LINE

         MOVE "1. Log In" TO OUTPUT-RECORD
         PERFORM PRINT-LINE

         MOVE "2. Create a new account" TO OUTPUT-RECORD
         PERFORM PRINT-LINE

         *> Top-level logout option; terminates the program gracefully
         MOVE "3. Logout" TO OUTPUT-RECORD
         PERFORM PRINT-LINE

         PERFORM READ-AND-LOG
         IF WS-EOF = "Y"
           MOVE "No input received." TO OUTPUT-RECORD
           PERFORM PRINT-LINE
           EXIT PERFORM
         END-IF

         MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

         IF MENU-CHOICE = "1"
           PERFORM LOGIN-PARA
         ELSE
           IF MENU-CHOICE = "2"
      *> ===== USERNAME INPUT LOOP =====
             PERFORM UNTIL WS-EOF = "Y"
               MOVE "Enter desired username: " TO OUTPUT-RECORD
               PERFORM PRINT-LINE

               PERFORM READ-AND-LOG
               IF WS-EOF = "Y"
                 EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
               COMPUTE WS-IN-LEN = FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))

               IF WS-IN-LEN = 0 OR WS-IN-LEN > 12
                 MOVE "Username must be 1 to 12 characters. Try again." TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
               ELSE
                 MOVE WS-TRIMMED-IN(1:12) TO WS-USERNAME
                 EXIT PERFORM
               END-IF
             END-PERFORM

      *> ===== PASSWORD INPUT LOOP =====
             PERFORM UNTIL WS-EOF = "Y"
               MOVE "Enter desired password:" TO OUTPUT-RECORD
               PERFORM PRINT-LINE

               PERFORM READ-AND-LOG
               IF WS-EOF = "Y"
                 EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
               COMPUTE WS-IN-LEN = FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))

               IF WS-IN-LEN < 8 OR WS-IN-LEN > 12
                 MOVE "Password must be 8 to 12 characters. Try again." TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
               ELSE
                 MOVE WS-TRIMMED-IN(1:12) TO WS-PASSWORD
                 EXIT PERFORM
               END-IF
             END-PERFORM

      *> ===== CREATE THE ACCOUNT =====
             PERFORM CREATE-ACCOUNT
             MOVE WS-MESSAGE TO OUTPUT-RECORD
             PERFORM PRINT-LINE
           ELSE
             IF MENU-CHOICE = "3"
               MOVE "Logging out. Goodbye!" TO OUTPUT-RECORD
               PERFORM PRINT-LINE
               MOVE "Y" TO WS-EOF
             ELSE
      *> ===== INVALID MENU SELECTION =====
               MOVE "Invalid Selection." TO OUTPUT-RECORD
               PERFORM PRINT-LINE
             END-IF
           END-IF
         END-IF
       END-PERFORM.

      *> ------------------------------------------------------------------
      *> Post-login menu: provides options required by the assignment
      *> 1 = Search for a job (under construction)
      *> 2 = Find someone you know (under construction)
      *> 3 = Learn a new skill (lists 5 skills; each selection is under construction)
      *> 4 = Logout (terminates the program)
      *> Uses INPUT.DAT for selections; gracefully returns if EOF is reached.
      *> ------------------------------------------------------------------
        POST-LOGIN-MENU.
          PERFORM UNTIL WS-EOF = "Y"
            MOVE "--- Welcome to InCollege, select an option ---" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "1. Search for a job" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "2. Find someone you know" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "3. Learn a new skill" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "4. Logout" TO OUTPUT-RECORD
            PERFORM PRINT-LINE

            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for selection; returning to top level." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PERFORM
            END-IF

            MOVE INPUT-RECORD(1:1) TO WS-POST-CHOICE

            EVALUATE WS-POST-CHOICE
              WHEN "1"
                MOVE "Job search is under construction." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              WHEN "2"
                MOVE "Find someone you know is under construction." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              WHEN "3"
                PERFORM LEARN-SKILL-MENU
              WHEN "4"
                MOVE "Logging out. Goodbye!" TO OUTPUT-RECORD
                PERFORM PRINT-LINE
                MOVE "Y" TO WS-EOF
                EXIT PERFORM
              WHEN OTHER
                MOVE "Invalid Selection." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
            END-EVALUATE
          END-PERFORM.

      *> ------------------------------------------------------------------
      *> Learn-a-new-skill submenu:
      *> - Presents 5 skills (you can adjust the names as needed).
      *> - Selecting any skill prints an "under construction" message.
      *> - Selecting 0 returns to the post-login menu.
      *> ------------------------------------------------------------------
        LEARN-SKILL-MENU.
          *> Populate skill list (kept simple for readability)
          MOVE "Python Basics"           TO WS-SKILL(1)
          MOVE "Data Analysis"          TO WS-SKILL(2)
          MOVE "Public Speaking"        TO WS-SKILL(3)
          MOVE "Project Management"     TO WS-SKILL(4)
          MOVE "Networking Essentials"  TO WS-SKILL(5)

          PERFORM UNTIL WS-EOF = "Y"
            MOVE "Select a skill to learn (0 to return):" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "1. Python Basics" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "2. Data Analysis" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "3. Public Speaking" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "4. Project Management" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "5. Networking Essentials" TO OUTPUT-RECORD
            PERFORM PRINT-LINE

            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for skill; returning to post-login menu." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PERFORM
            END-IF

            MOVE INPUT-RECORD(1:1) TO WS-SKILL-CHOICE

            IF WS-SKILL-CHOICE = "0"
              MOVE "Returning to post-login menu." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PERFORM
            ELSE
              IF WS-SKILL-CHOICE >= "1" AND WS-SKILL-CHOICE <= "5"
                MOVE "Selected skill is under construction." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              ELSE
                MOVE "Invalid Selection." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              END-IF
            END-IF
          END-PERFORM.

         *> ------------------------------------------------------------------
         *> Inlined CREATE-ACCOUNT logic (adapted from CREATEACCOUNT.cob)
         *> ------------------------------------------------------------------
         CREATE-ACCOUNT.
           PERFORM CHECK-INPUT-LOCAL

           PERFORM LOAD-USERS-LOCAL
           IF WS-COUNT=5
             MOVE "N" TO WS-STATUS
             MOVE 1 TO WS-RET-CODE
             MOVE "All permitted accounts have been created, please come back later" TO WS-MESSAGE
             EXIT PARAGRAPH
           END-IF

           PERFORM CHECK-USERNAME-UNIQUE-LOCAL
           IF WS-FOUND = "Y"
             MOVE "N" TO WS-STATUS
             MOVE 2 TO WS-RET-CODE
             MOVE "Username already exists" TO WS-MESSAGE
             EXIT PARAGRAPH
           END-IF

           PERFORM VALIDATE-PASSWORD-LOCAL
           IF WS-STATUS = "N"
             EXIT PARAGRAPH
           END-IF

           PERFORM ADD-SAVE-LOCAL
           MOVE "Y" TO WS-STATUS
           MOVE 0 TO WS-RET-CODE
           MOVE "Account created succesfully." TO WS-MESSAGE.

         CHECK-INPUT-LOCAL.
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-USERNAME)) = 0
            OR FUNCTION LENGTH(FUNCTION TRIM(WS-PASSWORD)) = 0
             MOVE "N" TO WS-STATUS
             MOVE 7 TO WS-RET-CODE
             MOVE "Username or password not provided; returning to menu." TO WS-MESSAGE
             EXIT PARAGRAPH
           END-IF.

         LOAD-USERS-LOCAL.
           MOVE 0 TO WS-COUNT
           MOVE "N" TO WS-USER-EOF
           OPEN INPUT USERS-FILE
           PERFORM UNTIL WS-USER-EOF = "Y" OR WS-COUNT=5
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

         CHECK-USERNAME-UNIQUE-LOCAL.
           MOVE "N" TO WS-FOUND
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I>WS-COUNT OR WS-FOUND="Y"
            IF FUNCTION TRIM(WS-USERNAME) = FUNCTION TRIM(WS-USER-TABLE(WS-I))
              MOVE "Y" TO WS-FOUND
            END-IF
           END-PERFORM.

         VALIDATE-PASSWORD-LOCAL.
           MOVE "N" TO WS-HAS-UPPER
           MOVE "N" TO WS-HAS-DIGIT
           MOVE "N" TO WS-HAS-SPECIAL

           MOVE FUNCTION TRIM(WS-PASSWORD) TO WS-TRIM-PASSWORD
           COMPUTE WS-PASS-LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-PASSWORD))

           IF WS-PASS-LEN < 8 OR WS-PASS-LEN > 12
             MOVE "N" TO WS-STATUS
             MOVE 3 TO WS-RET-CODE
             MOVE "Password must be 8 to 12 characters." TO WS-MESSAGE
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
             MOVE "N" TO WS-STATUS
             MOVE 4 TO WS-RET-CODE
             MOVE "Password must include at least one capital letter." TO WS-MESSAGE
             EXIT PARAGRAPH
           END-IF

           IF WS-HAS-DIGIT NOT = "Y"
             MOVE "N" TO WS-STATUS
             MOVE 5 TO WS-RET-CODE
             MOVE "Password must include at least one digit." TO WS-MESSAGE
             EXIT PARAGRAPH
           END-IF

           IF WS-HAS-SPECIAL NOT = "Y"
             MOVE "N" TO WS-STATUS
             MOVE 6 TO WS-RET-CODE
             MOVE "Password must include at least one special character." TO WS-MESSAGE
             EXIT PARAGRAPH
           END-IF.

         ADD-SAVE-LOCAL.
           ADD 1 TO WS-COUNT
           MOVE FUNCTION TRIM(WS-USERNAME) TO WS-USER-TABLE(WS-COUNT)
           MOVE FUNCTION TRIM(WS-PASSWORD) TO WS-PASS-TABLE(WS-COUNT)
           OPEN OUTPUT USERS-FILE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I>WS-COUNT
            MOVE WS-USER-TABLE(WS-I) TO USERNAME
            MOVE WS-PASS-TABLE(WS-I) TO PASSWORD
            WRITE USER-RECORD
           END-PERFORM
           CLOSE USERS-FILE.
         END PROGRAM INCOLLEGE-START.
