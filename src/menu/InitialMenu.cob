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

      *> LOGIN handled by src/login_prog.cob (called via CALL "LOGINPROG").

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
           *> Login loop: prompt, read, call auth subprogram until success or EOF
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

             CALL "LOGINPROG" USING WS-USERNAME WS-PASSWORD WS-STATUS WS-MESSAGE
             MOVE WS-MESSAGE TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             IF WS-STATUS = "Y"
               *> Enter post-login menu
               MOVE "N" TO WS-EOF
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

                 *> Call post-login logic handler
                 MOVE SPACES TO WS-MESSAGE
                 MOVE SPACES TO WS-SKILL-CHOICE
                 CALL "POSTLOGINPROG" USING WS-POST-CHOICE WS-SKILL-CHOICE WS-POST-ACTION WS-MESSAGE
                 IF WS-POST-ACTION = 1
                   MOVE WS-MESSAGE TO OUTPUT-RECORD
                   PERFORM PRINT-LINE
                 ELSE
                   IF WS-POST-ACTION = 2
                     *> Enter skill submenu: show list and read choices, call POSTLOGINPROG with skill choice
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
                       CALL "POSTLOGINPROG" USING WS-POST-CHOICE WS-SKILL-CHOICE WS-POST-ACTION WS-MESSAGE
                       MOVE WS-MESSAGE TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       IF WS-SKILL-CHOICE = "0"
                         EXIT PERFORM
                       END-IF
                     END-PERFORM
                   ELSE
                     IF WS-POST-ACTION = 3
                       MOVE WS-MESSAGE TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       MOVE "Y" TO WS-EOF
                       EXIT PERFORM
                     END-IF
                   END-IF
                 END-IF
               END-PERFORM
               EXIT PERFORM
             END-IF
           END-PERFORM
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
              CALL "CREATEACCOUNT" USING WS-USERNAME WS-PASSWORD WS-STATUS WS-MESSAGE WS-RET-CODE
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

      *> Post-login handled by src/postlogin_prog.cob (called from login program).

         END PROGRAM INCOLLEGE-START.
