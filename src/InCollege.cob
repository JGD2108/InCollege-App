
      *> Consolidated single-file COBOL program containing all program units

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       PROCEDURE DIVISION.
           CALL "INCOLLEGE-START"
           STOP RUN.

       END PROGRAM INCOLLEGE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE-START.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUTPUT-FILE ASSIGN TO "OUTPUT.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OPTIONAL USERS-FILE
                  ASSIGN TO "USERS.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-USERS-STATUS.

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
          01 WS-USERS-STATUS PIC XX.
          77 WS-FOUND PIC X VALUE "N".
          77 WS-FOUND-INDEX PIC 9 VALUE 0.
          *> Flag set to "Y" once login succeeds; controls post-login menu flow
          77 WS-LOGGED-IN PIC X VALUE "N".
          *> Post-login menu choice and skill selection choice
          77 WS-POST-CHOICE PIC X(1).
          77 WS-SKILL-CHOICE PIC X(1).
          *> Action code set by POSTLOGINPROG: 1=print message,2=skill submenu,3=logout
          77 WS-POST-ACTION PIC 9.
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

      *> Paragraph (function) to print a line to output file and display
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

      *> LOGIN handled by embedded LOGINPROG.

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
               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN

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
               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN

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

       END PROGRAM INCOLLEGE-START.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATEACCOUNT.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.

           SELECT OPTIONAL USERS-FILE
                   ASSIGN TO "USERS.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS WS-USERS-STATUS.

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
         01 WS-USERS-STATUS PIC XX.

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
         MOVE 0 TO WS-COUNT
         MOVE "N" TO WS-USER-EOF
         OPEN INPUT USERS-FILE
         IF WS-USERS-STATUS = "35"
            OPEN OUTPUT USERS-FILE
            CLOSE USERS-FILE
            OPEN INPUT USERS-FILE
          END-IF
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
         MOVE FUNCTION LENGTH(FUNCTION TRIM(LK-PASSWORD)) TO WS-PASS-LEN

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

       END PROGRAM CREATEACCOUNT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGINPROG.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT OPTIONAL USERS-FILE
                  ASSIGN TO "USERS.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-USERS-STATUS.

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
         01 WS-USERS-STATUS PIC XX.

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
         IF WS-USERS-STATUS = "35"
           OPEN OUTPUT USERS-FILE
           CLOSE USERS-FILE
           OPEN INPUT USERS-FILE
         END-IF
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

       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSTLOGINPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01 WS-SKILL-LIST.
           05 WS-SKILL PIC X(30) OCCURS 5 TIMES.

       LINKAGE SECTION.
         77 LK-POST-CHOICE PIC X(1).
         77 LK-SKILL-CHOICE PIC X(1).
         77 LK-ACTION PIC 9.
         77 LK-MESSAGE PIC X(100).

       PROCEDURE DIVISION USING LK-POST-CHOICE LK-SKILL-CHOICE LK-ACTION LK-MESSAGE.
         *> Action codes returned in LK-ACTION:
         *> 1 = print LK-MESSAGE
         *> 2 = caller should show skill submenu and pass skill choice back
         *> 3 = logout

         EVALUATE LK-POST-CHOICE
           WHEN "1"
             MOVE "Job search is under construction." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
           WHEN "2"
             MOVE "Find someone you know is under construction." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
           WHEN "3"
             MOVE 2 TO LK-ACTION
           WHEN "4"
             MOVE "Logging out. Goodbye!" TO LK-MESSAGE
             MOVE 3 TO LK-ACTION
           WHEN OTHER
             MOVE "Invalid Selection." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
         END-EVALUATE

         *> Skill handling: if caller provided LK-SKILL-CHOICE, interpret it
         IF LK-ACTION = 2 AND FUNCTION LENGTH(FUNCTION TRIM(LK-SKILL-CHOICE)) > 0
           IF LK-SKILL-CHOICE = "0"
             MOVE "Returning to post-login menu." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
           ELSE
             IF LK-SKILL-CHOICE >= "1" AND LK-SKILL-CHOICE <= "5"
               MOVE "Selected skill is under construction." TO LK-MESSAGE
               MOVE 1 TO LK-ACTION
             ELSE
               MOVE "Invalid Selection." TO LK-MESSAGE
               MOVE 1 TO LK-ACTION
             END-IF
           END-IF
         END-IF

         GOBACK.

      END PROGRAM POSTLOGINPROG.
