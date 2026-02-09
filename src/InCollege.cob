
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
              SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.

              SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.

              SELECT OPTIONAL USERS-FILE
                ASSIGN TO "USERS.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-USERS-STATUS.

              SELECT OPTIONAL PROFILES-FILE
                ASSIGN TO "PROFILES.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-PROFILES-STATUS.

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

         FD PROFILES-FILE.
           01 PROFILE-RECORD.
             05 PROFILE-USERNAME PIC X(12).
             05 PROFILE-FIRST-NAME PIC X(20).
             05 PROFILE-LAST-NAME PIC X(20).
             05 PROFILE-COLLEGE PIC X(30).
             05 PROFILE-MAJOR PIC X(30).
             05 PROFILE-GRAD-YEAR PIC 9(4).
             05 PROFILE-ABOUT-ME PIC X(100).

       WORKING-STORAGE SECTION.

         77 WS-EXIT-PROGRAM PIC X VALUE "N".

         77 WS-ACCT-DONE PIC X VALUE "N".
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

      *> Profile creation/editing variables
         01 WS-PROFILE-DATA.
           05 WS-FIRST-NAME PIC X(20).
           05 WS-LAST-NAME PIC X(20).
           05 WS-COLLEGE PIC X(30).
           05 WS-MAJOR PIC X(30).
           05 WS-GRAD-YEAR PIC 9(4).
           05 WS-ABOUT-ME PIC X(100).
      *> Profile viewing buffers
        01 WS-VIEW-PROFILE-DATA.
          05 WS-VIEW-FIRST-NAME PIC X(20).
          05 WS-VIEW-LAST-NAME PIC X(20).
          05 WS-VIEW-COLLEGE PIC X(30).
          05 WS-VIEW-MAJOR PIC X(30).
          05 WS-VIEW-GRAD-YEAR PIC 9(4).
          05 WS-VIEW-ABOUT-ME PIC X(100).
        01 WS-VIEW-EXPERIENCE-LIST.
          05 WS-VIEW-EXP-ENTRY OCCURS 3 TIMES.
            10 WS-VIEW-EXP-TITLE PIC X(30).
            10 WS-VIEW-EXP-COMPANY PIC X(30).
            10 WS-VIEW-EXP-START-DATE PIC X(10).
            10 WS-VIEW-EXP-END-DATE PIC X(10).
            10 WS-VIEW-EXP-DESC PIC X(100).
        01 WS-VIEW-EDUCATION-LIST.
          05 WS-VIEW-EDU-ENTRY OCCURS 3 TIMES.
            10 WS-VIEW-EDU-DEGREE PIC X(30).
            10 WS-VIEW-EDU-UNI PIC X(30).
            10 WS-VIEW-EDU-START-YEAR PIC 9(4).
            10 WS-VIEW-EDU-END-YEAR PIC 9(4).
        77 WS-VIEW-EXP-COUNT PIC 9 VALUE 0.
        77 WS-VIEW-EDU-COUNT PIC 9 VALUE 0.
        77 WS-PROFILE-FOUND PIC X VALUE "N".
        77 WS-VIEW-INDEX PIC 9 VALUE 0.
        77 WS-YEAR-TEXT PIC X(4).
        77 WS-DESC-TEMP PIC X(100).
        77 WS-IN-LEN-3 PIC 999 VALUE 0.
      *> Search functionality variables
        77 WS-SEARCH-NAME PIC X(50).
        77 WS-SEARCH-RESULT-USERNAME PIC X(12).
        77 WS-PROFILES-STATUS PIC XX.
        77 WS-PROFILE-EOF PIC X VALUE "N".
      *> WS-PROFILE-CHOICE Indicates if user wants to add optional info or not
         77 WS-PROFILE-CHOICE PIC X(1).
      *> WS-PROFILE-ACTION Indicates to EDITPROFILE what part of profile to edit: 1=basic,2=experience,3=education
         77 WS-PROFILE-ACTION PIC 9(1).
      *> Profile input control flags and limits
        77 WS-VALID-INPUT PIC X VALUE "N".
        77 WS-PROFILE-CANCEL PIC X VALUE "N".
        77 WS-EXPERIENCE-LIMIT PIC 9 VALUE 3.
        77 WS-EDUCATION-LIMIT PIC 9 VALUE 3.
        77 WS-EXP-ENTRY-COUNT PIC 9 VALUE 0.
        77 WS-EDU-ENTRY-COUNT PIC 9 VALUE 0.
      *> Optional experience entries
         01 WS-EXPERIENCE.
           05 WS-EXP-TITLE PIC X(30).
           05 WS-EXP-COMPANY PIC X(30).
           05 WS-EXP-START-DATE PIC X(10).
           05 WS-EXP-END-DATE PIC X(10).
           05 WS-EXP-DESC PIC X(100).
      *> Optional education entries
         01 WS-EDUCATION.
           05 WS-EDU-DEGREE PIC X(30).
           05 WS-EDU-UNI PIC X(30).
           05 WS-EDU-START-YEAR PIC 9(4).
           05 WS-EDU-END-YEAR PIC 9(4).

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
        77 WS-POST-EXIT PIC X VALUE "N".
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

       HANDLE-VIEW-PROFILE.
          MOVE SPACES TO WS-VIEW-PROFILE-DATA
          MOVE SPACES TO WS-VIEW-EXPERIENCE-LIST
          MOVE SPACES TO WS-VIEW-EDUCATION-LIST
          MOVE 0 TO WS-VIEW-EXP-COUNT
          MOVE 0 TO WS-VIEW-EDU-COUNT
          MOVE "N" TO WS-PROFILE-FOUND

          CALL "VIEWPROFILE" USING WS-USERNAME WS-VIEW-PROFILE-DATA
                               WS-VIEW-EXPERIENCE-LIST WS-VIEW-EDUCATION-LIST
                               WS-VIEW-EXP-COUNT WS-VIEW-EDU-COUNT
                               WS-PROFILE-FOUND WS-MESSAGE

          IF WS-PROFILE-FOUND = "N"
            MOVE WS-MESSAGE TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            EXIT PARAGRAPH
          END-IF

          MOVE "--- My Profile ---" TO OUTPUT-RECORD
          PERFORM PRINT-LINE

          MOVE SPACES TO OUTPUT-RECORD
          STRING "Name: " DELIMITED BY SIZE
                 WS-VIEW-FIRST-NAME DELIMITED BY SIZE
                 " " DELIMITED BY SIZE
                 WS-VIEW-LAST-NAME DELIMITED BY SIZE
            INTO OUTPUT-RECORD
          END-STRING
          PERFORM PRINT-LINE

          MOVE SPACES TO OUTPUT-RECORD
          STRING "College: " DELIMITED BY SIZE
                 WS-VIEW-COLLEGE DELIMITED BY SIZE
            INTO OUTPUT-RECORD
          END-STRING
          PERFORM PRINT-LINE

          MOVE SPACES TO OUTPUT-RECORD
          STRING "Major: " DELIMITED BY SIZE
                 WS-VIEW-MAJOR DELIMITED BY SIZE
            INTO OUTPUT-RECORD
          END-STRING
          PERFORM PRINT-LINE

          MOVE WS-VIEW-GRAD-YEAR TO WS-YEAR-TEXT
          MOVE SPACES TO OUTPUT-RECORD
          STRING "Graduation Year: " DELIMITED BY SIZE
                 WS-YEAR-TEXT DELIMITED BY SIZE
            INTO OUTPUT-RECORD
          END-STRING
          PERFORM PRINT-LINE

          MOVE SPACES TO OUTPUT-RECORD
          STRING "About Me: " DELIMITED BY SIZE
                 WS-VIEW-ABOUT-ME DELIMITED BY SIZE
            INTO OUTPUT-RECORD
          END-STRING
          PERFORM PRINT-LINE

          MOVE "Experience:" TO OUTPUT-RECORD
          PERFORM PRINT-LINE
          IF WS-VIEW-EXP-COUNT = 0
            MOVE "  None" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
          ELSE
            PERFORM VARYING WS-VIEW-INDEX FROM 1 BY 1
              UNTIL WS-VIEW-INDEX > WS-VIEW-EXP-COUNT
              MOVE SPACES TO OUTPUT-RECORD
              STRING "  " DELIMITED BY SIZE
                     WS-VIEW-INDEX DELIMITED BY SIZE
                     ". " DELIMITED BY SIZE
                     WS-VIEW-EXP-TITLE(WS-VIEW-INDEX) DELIMITED BY SIZE
                     " - " DELIMITED BY SIZE
                     WS-VIEW-EXP-COMPANY(WS-VIEW-INDEX) DELIMITED BY SIZE
                INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              MOVE SPACES TO OUTPUT-RECORD
              STRING "     Dates: " DELIMITED BY SIZE
                     WS-VIEW-EXP-START-DATE(WS-VIEW-INDEX) DELIMITED BY SIZE
                     " - " DELIMITED BY SIZE
                     WS-VIEW-EXP-END-DATE(WS-VIEW-INDEX) DELIMITED BY SIZE
                INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              MOVE FUNCTION TRIM(WS-VIEW-EXP-DESC(WS-VIEW-INDEX)) TO WS-DESC-TEMP
              MOVE FUNCTION LENGTH(WS-DESC-TEMP) TO WS-IN-LEN-3
              IF WS-IN-LEN-3 > 0
                MOVE SPACES TO OUTPUT-RECORD
                STRING "     Description: " DELIMITED BY SIZE
                       WS-DESC-TEMP DELIMITED BY SIZE
                  INTO OUTPUT-RECORD
                END-STRING
                PERFORM PRINT-LINE
              END-IF
            END-PERFORM
          END-IF

          MOVE "Education:" TO OUTPUT-RECORD
          PERFORM PRINT-LINE
          IF WS-VIEW-EDU-COUNT = 0
            MOVE "  None" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
          ELSE
            PERFORM VARYING WS-VIEW-INDEX FROM 1 BY 1
              UNTIL WS-VIEW-INDEX > WS-VIEW-EDU-COUNT
              MOVE SPACES TO OUTPUT-RECORD
              STRING "  " DELIMITED BY SIZE
                     WS-VIEW-INDEX DELIMITED BY SIZE
                     ". " DELIMITED BY SIZE
                     WS-VIEW-EDU-DEGREE(WS-VIEW-INDEX) DELIMITED BY SIZE
                     " - " DELIMITED BY SIZE
                     WS-VIEW-EDU-UNI(WS-VIEW-INDEX) DELIMITED BY SIZE
                INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              MOVE WS-VIEW-EDU-START-YEAR(WS-VIEW-INDEX) TO WS-YEAR-TEXT
              MOVE SPACES TO OUTPUT-RECORD
              STRING "     Years: " DELIMITED BY SIZE
                     WS-YEAR-TEXT DELIMITED BY SIZE
                     " - " DELIMITED BY SIZE
                     WS-VIEW-EDU-END-YEAR(WS-VIEW-INDEX) DELIMITED BY SIZE
                INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE
            END-PERFORM
          END-IF.

       HANDLE-SEARCH-USER.
           MOVE "Enter the full name of the person you are looking for:" TO OUTPUT-RECORD
           PERFORM PRINT-LINE
           PERFORM READ-AND-LOG
           IF WS-EOF = "Y"
             MOVE "No input for search; returning to post-login menu." TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-SEARCH-NAME
           PERFORM SEARCH-FOR-USER-PROFILE.

       SEARCH-FOR-USER-PROFILE.
           MOVE "N" TO WS-PROFILE-FOUND
           MOVE SPACES TO WS-SEARCH-RESULT-USERNAME

           *> Open PROFILES.DAT and search
           OPEN INPUT PROFILES-FILE
           IF WS-PROFILES-STATUS = "35"
             MOVE "No profiles exist yet." TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             EXIT PARAGRAPH
           END-IF

           IF WS-PROFILES-STATUS NOT = "00"
             MOVE "Unable to search profiles." TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             CLOSE PROFILES-FILE
             EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-PROFILE-EOF
           PERFORM UNTIL WS-PROFILE-EOF = "Y"
             READ PROFILES-FILE
               AT END
                 MOVE "Y" TO WS-PROFILE-EOF
               NOT AT END
                 *> Build full name from profile
                 MOVE SPACES TO WS-TRIMMED-IN
                 STRING FUNCTION TRIM(PROFILE-FIRST-NAME) DELIMITED BY SIZE
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(PROFILE-LAST-NAME) DELIMITED BY SIZE
                   INTO WS-TRIMMED-IN
                 END-STRING

                 *> Check for exact match
                 IF FUNCTION TRIM(WS-TRIMMED-IN) = FUNCTION TRIM(WS-SEARCH-NAME)
                   MOVE "Y" TO WS-PROFILE-FOUND
                   MOVE PROFILE-USERNAME TO WS-SEARCH-RESULT-USERNAME
                   MOVE "Y" TO WS-PROFILE-EOF
                 END-IF
             END-READ
           END-PERFORM

           CLOSE PROFILES-FILE

           *> Handle search results
           IF WS-PROFILE-FOUND = "N"
             MOVE "No one by that name could be found." TO OUTPUT-RECORD
             PERFORM PRINT-LINE
           ELSE
             PERFORM DISPLAY-FOUND-USER-PROFILE
           END-IF.

       DISPLAY-FOUND-USER-PROFILE.
           *> Initialize view structures
           MOVE SPACES TO WS-VIEW-PROFILE-DATA
           MOVE SPACES TO WS-VIEW-EXPERIENCE-LIST
           MOVE SPACES TO WS-VIEW-EDUCATION-LIST
           MOVE 0 TO WS-VIEW-EXP-COUNT
           MOVE 0 TO WS-VIEW-EDU-COUNT
           MOVE "N" TO WS-PROFILE-FOUND

           *> Load the found user's complete profile
           CALL "VIEWPROFILE" USING WS-SEARCH-RESULT-USERNAME WS-VIEW-PROFILE-DATA
                                    WS-VIEW-EXPERIENCE-LIST WS-VIEW-EDUCATION-LIST
                                    WS-VIEW-EXP-COUNT WS-VIEW-EDU-COUNT
                                    WS-PROFILE-FOUND WS-MESSAGE

           IF WS-PROFILE-FOUND = "N"
             MOVE WS-MESSAGE TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             EXIT PARAGRAPH
           END-IF

           *> Display header
           MOVE "--- Found User Profile ---" TO OUTPUT-RECORD
           PERFORM PRINT-LINE

           *> Display basic info
           MOVE SPACES TO OUTPUT-RECORD
           STRING "Name: " DELIMITED BY SIZE
                  WS-VIEW-FIRST-NAME DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-VIEW-LAST-NAME DELIMITED BY SIZE
             INTO OUTPUT-RECORD
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO OUTPUT-RECORD
           STRING "University: " DELIMITED BY SIZE
                  WS-VIEW-COLLEGE DELIMITED BY SIZE
             INTO OUTPUT-RECORD
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO OUTPUT-RECORD
           STRING "Major: " DELIMITED BY SIZE
                  WS-VIEW-MAJOR DELIMITED BY SIZE
             INTO OUTPUT-RECORD
           END-STRING
           PERFORM PRINT-LINE

           MOVE WS-VIEW-GRAD-YEAR TO WS-YEAR-TEXT
           MOVE SPACES TO OUTPUT-RECORD
           STRING "Graduation Year: " DELIMITED BY SIZE
                  WS-YEAR-TEXT DELIMITED BY SIZE
             INTO OUTPUT-RECORD
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO OUTPUT-RECORD
           STRING "About Me: " DELIMITED BY SIZE
                  WS-VIEW-ABOUT-ME DELIMITED BY SIZE
             INTO OUTPUT-RECORD
           END-STRING
           PERFORM PRINT-LINE

           *> Display Experience
           MOVE "Experience:" TO OUTPUT-RECORD
           PERFORM PRINT-LINE
           IF WS-VIEW-EXP-COUNT = 0
             MOVE "  None" TO OUTPUT-RECORD
             PERFORM PRINT-LINE
           ELSE
             PERFORM VARYING WS-VIEW-INDEX FROM 1 BY 1
               UNTIL WS-VIEW-INDEX > WS-VIEW-EXP-COUNT
               MOVE SPACES TO OUTPUT-RECORD
               STRING "  " DELIMITED BY SIZE
                      WS-VIEW-INDEX DELIMITED BY SIZE
                      ". " DELIMITED BY SIZE
                      WS-VIEW-EXP-TITLE(WS-VIEW-INDEX) DELIMITED BY SIZE
                      " - " DELIMITED BY SIZE
                      WS-VIEW-EXP-COMPANY(WS-VIEW-INDEX) DELIMITED BY SIZE
                 INTO OUTPUT-RECORD
               END-STRING
               PERFORM PRINT-LINE

               MOVE SPACES TO OUTPUT-RECORD
               STRING "     Dates: " DELIMITED BY SIZE
                      WS-VIEW-EXP-START-DATE(WS-VIEW-INDEX) DELIMITED BY SIZE
                      " - " DELIMITED BY SIZE
                      WS-VIEW-EXP-END-DATE(WS-VIEW-INDEX) DELIMITED BY SIZE
                 INTO OUTPUT-RECORD
               END-STRING
               PERFORM PRINT-LINE

               MOVE FUNCTION TRIM(WS-VIEW-EXP-DESC(WS-VIEW-INDEX)) TO WS-DESC-TEMP
               MOVE FUNCTION LENGTH(WS-DESC-TEMP) TO WS-IN-LEN-3
               IF WS-IN-LEN-3 > 0
                 MOVE SPACES TO OUTPUT-RECORD
                 STRING "     Description: " DELIMITED BY SIZE
                        WS-DESC-TEMP DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
                 END-STRING
                 PERFORM PRINT-LINE
               END-IF
             END-PERFORM
           END-IF

           *> Display Education
           MOVE "Education:" TO OUTPUT-RECORD
           PERFORM PRINT-LINE
           IF WS-VIEW-EDU-COUNT = 0
             MOVE "  None" TO OUTPUT-RECORD
             PERFORM PRINT-LINE
           ELSE
             PERFORM VARYING WS-VIEW-INDEX FROM 1 BY 1
               UNTIL WS-VIEW-INDEX > WS-VIEW-EDU-COUNT
               MOVE SPACES TO OUTPUT-RECORD
               STRING "  " DELIMITED BY SIZE
                      WS-VIEW-INDEX DELIMITED BY SIZE
                      ". " DELIMITED BY SIZE
                      WS-VIEW-EDU-DEGREE(WS-VIEW-INDEX) DELIMITED BY SIZE
                      " - " DELIMITED BY SIZE
                      WS-VIEW-EDU-UNI(WS-VIEW-INDEX) DELIMITED BY SIZE
                 INTO OUTPUT-RECORD
               END-STRING
               PERFORM PRINT-LINE

               MOVE WS-VIEW-EDU-START-YEAR(WS-VIEW-INDEX) TO WS-YEAR-TEXT
               MOVE SPACES TO OUTPUT-RECORD
               STRING "     Years: " DELIMITED BY SIZE
                      WS-YEAR-TEXT DELIMITED BY SIZE
                      " - " DELIMITED BY SIZE
                      WS-VIEW-EDU-END-YEAR(WS-VIEW-INDEX) DELIMITED BY SIZE
                 INTO OUTPUT-RECORD
               END-STRING
               PERFORM PRINT-LINE
             END-PERFORM
           END-IF

           MOVE "-------------------------" TO OUTPUT-RECORD
           PERFORM PRINT-LINE.

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

       PERFORM UNTIL WS-EOF = "Y" OR WS-EXIT-PROGRAM = "Y"
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
              MOVE "N" TO WS-POST-EXIT
              CALL "EDITPROFILE"
              PERFORM UNTIL WS-EOF = "Y" OR WS-POST-EXIT = "Y"
                 MOVE "--- Welcome to InCollege, select an option ---" TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
                 MOVE "0. Create/Edit your profile" TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
                 MOVE "1. Search for a job" TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
                 MOVE "2. Find someone you know" TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
                 MOVE "3. Learn a new skill" TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
                 MOVE "4. Logout" TO OUTPUT-RECORD
                PERFORM PRINT-LINE
                MOVE "5. View My Profile" TO OUTPUT-RECORD
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
                 EVALUATE WS-POST-ACTION
                  WHEN 1
                   MOVE WS-MESSAGE TO OUTPUT-RECORD
                   PERFORM PRINT-LINE
                  WHEN 2
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
                  WHEN 3
                     *> Logout action
                       MOVE WS-MESSAGE TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       MOVE "Y" TO WS-POST-EXIT
                       EXIT PERFORM
                 WHEN 4
                     *> Profile creation/editing
                     MOVE "1" TO WS-PROFILE-ACTION
                     MOVE "N" TO WS-PROFILE-CANCEL
                     MOVE SPACES TO WS-PROFILE-DATA

                     MOVE "N" TO WS-VALID-INPUT
                     PERFORM UNTIL WS-VALID-INPUT = "Y"
                       MOVE "Enter your First Name:" TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       PERFORM READ-AND-LOG
                       IF WS-EOF = "Y"
                         MOVE "No input for profile; returning to post-login menu." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                         MOVE "Y" TO WS-PROFILE-CANCEL
                         EXIT PERFORM
                       END-IF
                       MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                       MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                       IF WS-IN-LEN = 0
                         MOVE "First Name is required." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                       ELSE
                         MOVE WS-TRIMMED-IN TO WS-FIRST-NAME
                         MOVE "Y" TO WS-VALID-INPUT
                       END-IF
                     END-PERFORM
                     IF WS-PROFILE-CANCEL = "Y"
                       EXIT PERFORM
                     END-IF

                     MOVE "N" TO WS-VALID-INPUT
                     PERFORM UNTIL WS-VALID-INPUT = "Y"
                       MOVE "Enter your Last Name:" TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       PERFORM READ-AND-LOG
                       IF WS-EOF = "Y"
                         MOVE "No input for profile; returning to post-login menu." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                         MOVE "Y" TO WS-PROFILE-CANCEL
                         EXIT PERFORM
                       END-IF
                       MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                       MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                       IF WS-IN-LEN = 0
                         MOVE "Last Name is required." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                       ELSE
                         MOVE WS-TRIMMED-IN TO WS-LAST-NAME
                         MOVE "Y" TO WS-VALID-INPUT
                       END-IF
                     END-PERFORM
                     IF WS-PROFILE-CANCEL = "Y"
                       EXIT PERFORM
                     END-IF

                     MOVE "N" TO WS-VALID-INPUT
                     PERFORM UNTIL WS-VALID-INPUT = "Y"
                       MOVE "Enter your College's Name:" TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       PERFORM READ-AND-LOG
                       IF WS-EOF = "Y"
                         MOVE "No input for profile; returning to post-login menu." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                         MOVE "Y" TO WS-PROFILE-CANCEL
                         EXIT PERFORM
                       END-IF
                       MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                       MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                       IF WS-IN-LEN = 0
                         MOVE "University/College is required." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                       ELSE
                         MOVE WS-TRIMMED-IN TO WS-COLLEGE
                         MOVE "Y" TO WS-VALID-INPUT
                       END-IF
                     END-PERFORM
                     IF WS-PROFILE-CANCEL = "Y"
                       EXIT PERFORM
                     END-IF

                     MOVE "N" TO WS-VALID-INPUT
                     PERFORM UNTIL WS-VALID-INPUT = "Y"
                       MOVE "Enter your Major:" TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       PERFORM READ-AND-LOG
                       IF WS-EOF = "Y"
                         MOVE "No input for profile; returning to post-login menu." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                         MOVE "Y" TO WS-PROFILE-CANCEL
                         EXIT PERFORM
                       END-IF
                       MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                       MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                       IF WS-IN-LEN = 0
                         MOVE "Major is required." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                       ELSE
                         MOVE WS-TRIMMED-IN TO WS-MAJOR
                         MOVE "Y" TO WS-VALID-INPUT
                       END-IF
                     END-PERFORM
                     IF WS-PROFILE-CANCEL = "Y"
                       EXIT PERFORM
                     END-IF

                     MOVE "N" TO WS-VALID-INPUT
                     PERFORM UNTIL WS-VALID-INPUT = "Y"
                       MOVE "Enter your Graduation Year:" TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       PERFORM READ-AND-LOG
                       IF WS-EOF = "Y"
                         MOVE "No input for profile; returning to post-login menu." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                         MOVE "Y" TO WS-PROFILE-CANCEL
                         EXIT PERFORM
                       END-IF
                       MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                       MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                       IF WS-IN-LEN NOT = 4 OR WS-TRIMMED-IN(1:4) IS NOT NUMERIC
                         MOVE "Graduation Year must be a valid 4-digit year." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                       ELSE
                         MOVE WS-TRIMMED-IN TO WS-GRAD-YEAR
                         MOVE "Y" TO WS-VALID-INPUT
                       END-IF
                     END-PERFORM
                     IF WS-PROFILE-CANCEL = "Y"
                       EXIT PERFORM
                     END-IF

                     MOVE SPACES TO WS-ABOUT-ME
                     MOVE "About Me (Optional, type N to skip):" TO OUTPUT-RECORD
                     PERFORM PRINT-LINE
                     PERFORM READ-AND-LOG
                     IF WS-EOF = "Y"
                       MOVE "No input for profile; returning to post-login menu." TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                       MOVE "Y" TO WS-PROFILE-CANCEL
                       EXIT PERFORM
                     END-IF
                     *> Check for 'N' to skip About Me
                     IF INPUT-RECORD(1:1) = "N" OR INPUT-RECORD(1:1) = "n"
                       MOVE "Skipping About Me entry." TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                     ELSE
                       MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-ABOUT-ME
                     END-IF
                     IF WS-PROFILE-CANCEL = "Y"
                       EXIT PERFORM
                     END-IF

                     CALL "BASIC" USING WS-USERNAME WS-PROFILE-DATA WS-PROFILE-ACTION WS-MESSAGE
                     MOVE WS-MESSAGE TO OUTPUT-RECORD
                     PERFORM PRINT-LINE

      *> ===== OPTIONAL EXPERIENCE AND EDUCATION ENTRY =====
                     MOVE "Y" TO WS-PROFILE-CHOICE
                     MOVE 0 TO WS-EXP-ENTRY-COUNT
                     PERFORM UNTIL WS-PROFILE-CHOICE = "N" OR WS-PROFILE-CHOICE = "n"
                         MOVE "Would you like to add experience? (Y/N)" TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                         PERFORM READ-AND-LOG
                         IF WS-EOF = "Y"
                           MOVE "No input for experiences; returning to post-login menu." TO OUTPUT-RECORD
                           PERFORM PRINT-LINE
                           EXIT PERFORM
                         END-IF
                         IF INPUT-RECORD(1:1) = "Y" OR INPUT-RECORD(1:1) = "y"
                           IF WS-EXP-ENTRY-COUNT >= WS-EXPERIENCE-LIMIT
                             MOVE "Experience entry limit reached (max 3)." TO OUTPUT-RECORD
                             PERFORM PRINT-LINE
                             MOVE "N" TO WS-PROFILE-CHOICE
                           ELSE
                             MOVE SPACES TO WS-EXPERIENCE

                             MOVE "N" TO WS-VALID-INPUT
                             PERFORM UNTIL WS-VALID-INPUT = "Y"
                               MOVE "Your Title:" TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               PERFORM READ-AND-LOG
                               IF WS-EOF = "Y"
                                 MOVE "No input for experience; returning to post-login menu." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                                 MOVE "Y" TO WS-PROFILE-CANCEL
                                 EXIT PERFORM
                               END-IF
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                               IF WS-IN-LEN = 0
                                 MOVE "Title is required." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                               ELSE
                                 MOVE WS-TRIMMED-IN TO WS-EXP-TITLE
                                 MOVE "Y" TO WS-VALID-INPUT
                               END-IF
                             END-PERFORM
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE "N" TO WS-VALID-INPUT
                             PERFORM UNTIL WS-VALID-INPUT = "Y"
                               MOVE "Company Name:" TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               PERFORM READ-AND-LOG
                               IF WS-EOF = "Y"
                                 MOVE "No input for experience; returning to post-login menu." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                                 MOVE "Y" TO WS-PROFILE-CANCEL
                                 EXIT PERFORM
                               END-IF
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                               IF WS-IN-LEN = 0
                                 MOVE "Company/Organization is required." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                               ELSE
                                 MOVE WS-TRIMMED-IN TO WS-EXP-COMPANY
                                 MOVE "Y" TO WS-VALID-INPUT
                               END-IF
                             END-PERFORM
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE "N" TO WS-VALID-INPUT
                             PERFORM UNTIL WS-VALID-INPUT = "Y"
                               MOVE "Start Date:" TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               PERFORM READ-AND-LOG
                               IF WS-EOF = "Y"
                                 MOVE "No input for experience; returning to post-login menu." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                                 MOVE "Y" TO WS-PROFILE-CANCEL
                                 EXIT PERFORM
                               END-IF
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                               IF WS-IN-LEN = 0
                                 MOVE "Dates are required." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                               ELSE
                                 MOVE WS-TRIMMED-IN TO WS-EXP-START-DATE
                                 MOVE "Y" TO WS-VALID-INPUT
                               END-IF
                             END-PERFORM
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE "N" TO WS-VALID-INPUT
                             PERFORM UNTIL WS-VALID-INPUT = "Y"
                               MOVE "End Date:" TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               PERFORM READ-AND-LOG
                               IF WS-EOF = "Y"
                                 MOVE "No input for experience; returning to post-login menu." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                                 MOVE "Y" TO WS-PROFILE-CANCEL
                                 EXIT PERFORM
                               END-IF
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                               IF WS-IN-LEN = 0
                                 MOVE "Dates are required." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                               ELSE
                                 MOVE WS-TRIMMED-IN TO WS-EXP-END-DATE
                                 MOVE "Y" TO WS-VALID-INPUT
                               END-IF
                             END-PERFORM
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE SPACES TO WS-EXP-DESC
                             MOVE "Description (Optional, type N to skip):" TO OUTPUT-RECORD
                             PERFORM PRINT-LINE
                             PERFORM READ-AND-LOG
                             IF WS-EOF = "Y"
                               MOVE "No input for experience; returning to post-login menu." TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               MOVE "Y" TO WS-PROFILE-CANCEL
                               EXIT PERFORM
                             END-IF
                             IF INPUT-RECORD(1:1) = "N" OR INPUT-RECORD(1:1) = "n"
                               MOVE "Skipping Description entry." TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                             ELSE
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-EXP-DESC
                             END-IF
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE "2" TO WS-PROFILE-ACTION
                             CALL "EXPERIENCE" USING WS-USERNAME WS-EXPERIENCE WS-PROFILE-ACTION WS-MESSAGE
                             MOVE WS-MESSAGE TO OUTPUT-RECORD
                             PERFORM PRINT-LINE
                             IF WS-MESSAGE(1:23) = "Experience limit reached"
                               MOVE "N" TO WS-PROFILE-CHOICE
                             ELSE
                               IF WS-MESSAGE(1:16) = "Experience saved"
                                 ADD 1 TO WS-EXP-ENTRY-COUNT
                               END-IF
                             END-IF
                           END-IF
                         ELSE
                           MOVE "N" TO WS-PROFILE-CHOICE
                         END-IF
                     END-PERFORM
                     MOVE "Y" TO WS-PROFILE-CHOICE
                     MOVE 0 TO WS-EDU-ENTRY-COUNT

                     PERFORM UNTIL WS-PROFILE-CHOICE = "N" OR WS-PROFILE-CHOICE = "n"
                         MOVE "Would you like to add education? (Y/N)" TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                         PERFORM READ-AND-LOG
                         IF WS-EOF = "Y"
                           MOVE "No input for education; returning to post-login menu." TO OUTPUT-RECORD
                           PERFORM PRINT-LINE
                           EXIT PERFORM
                         END-IF
                         IF INPUT-RECORD(1:1) = "Y" OR INPUT-RECORD(1:1) = "y"
                           IF WS-EDU-ENTRY-COUNT >= WS-EDUCATION-LIMIT
                             MOVE "Education entry limit reached (max 3)." TO OUTPUT-RECORD
                             PERFORM PRINT-LINE
                             MOVE "N" TO WS-PROFILE-CHOICE
                           ELSE
                             MOVE SPACES TO WS-EDUCATION

                             MOVE "N" TO WS-VALID-INPUT
                             PERFORM UNTIL WS-VALID-INPUT = "Y"
                               MOVE "Degree:" TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               PERFORM READ-AND-LOG
                               IF WS-EOF = "Y"
                                 MOVE "No input for education; returning to post-login menu." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                                 MOVE "Y" TO WS-PROFILE-CANCEL
                                 EXIT PERFORM
                               END-IF
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                               IF WS-IN-LEN = 0
                                 MOVE "Degree is required." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                               ELSE
                                 MOVE WS-TRIMMED-IN TO WS-EDU-DEGREE
                                 MOVE "Y" TO WS-VALID-INPUT
                               END-IF
                             END-PERFORM
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE "N" TO WS-VALID-INPUT
                             PERFORM UNTIL WS-VALID-INPUT = "Y"
                               MOVE "University Name:" TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               PERFORM READ-AND-LOG
                               IF WS-EOF = "Y"
                                 MOVE "No input for education; returning to post-login menu." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                                 MOVE "Y" TO WS-PROFILE-CANCEL
                                 EXIT PERFORM
                               END-IF
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                               IF WS-IN-LEN = 0
                                 MOVE "University/College is required." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                               ELSE
                                 MOVE WS-TRIMMED-IN TO WS-EDU-UNI
                                 MOVE "Y" TO WS-VALID-INPUT
                               END-IF
                             END-PERFORM
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE "N" TO WS-VALID-INPUT
                             PERFORM UNTIL WS-VALID-INPUT = "Y"
                               MOVE "Start Year:" TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               PERFORM READ-AND-LOG
                               IF WS-EOF = "Y"
                                 MOVE "No input for education; returning to post-login menu." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                                 MOVE "Y" TO WS-PROFILE-CANCEL
                                 EXIT PERFORM
                               END-IF
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                               IF WS-IN-LEN NOT = 4 OR WS-TRIMMED-IN(1:4) IS NOT NUMERIC
                                 MOVE "Start Year must be a valid 4-digit year." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                               ELSE
                                 MOVE WS-TRIMMED-IN TO WS-EDU-START-YEAR
                                 MOVE "Y" TO WS-VALID-INPUT
                               END-IF
                             END-PERFORM
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE "N" TO WS-VALID-INPUT
                             PERFORM UNTIL WS-VALID-INPUT = "Y"
                               MOVE "End Year:" TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               PERFORM READ-AND-LOG
                               IF WS-EOF = "Y"
                                 MOVE "No input for education; returning to post-login menu." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                                 MOVE "Y" TO WS-PROFILE-CANCEL
                                 EXIT PERFORM
                               END-IF
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN
                               IF WS-IN-LEN NOT = 4 OR WS-TRIMMED-IN(1:4) IS NOT NUMERIC
                                 MOVE "End Year must be a valid 4-digit year." TO OUTPUT-RECORD
                                 PERFORM PRINT-LINE
                               ELSE
                                 MOVE WS-TRIMMED-IN TO WS-EDU-END-YEAR
                                 MOVE "Y" TO WS-VALID-INPUT
                               END-IF
                             END-PERFORM
                             IF WS-PROFILE-CANCEL = "Y"
                               EXIT PERFORM
                             END-IF

                             MOVE "3" TO WS-PROFILE-ACTION
                             CALL "EDUCATION" USING WS-USERNAME WS-EDUCATION WS-PROFILE-ACTION WS-MESSAGE
                             MOVE WS-MESSAGE TO OUTPUT-RECORD
                             PERFORM PRINT-LINE
                             IF WS-MESSAGE(1:23) = "Education limit reached"
                               MOVE "N" TO WS-PROFILE-CHOICE
                             ELSE
                               IF WS-MESSAGE(1:15) = "Education saved"
                                 ADD 1 TO WS-EDU-ENTRY-COUNT
                               END-IF
                             END-IF
                           END-IF
                          ELSE
                           MOVE "N" TO WS-PROFILE-CHOICE
                         END-IF
                     END-PERFORM

                     MOVE "Return to the top-level menu? (Y/N)" TO OUTPUT-RECORD
                     PERFORM PRINT-LINE
                     PERFORM READ-AND-LOG
                     IF WS-EOF = "Y"
                       MOVE "No input for selection; returning to post-login menu." TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                     ELSE
                       IF INPUT-RECORD(1:1) = "Y" OR INPUT-RECORD(1:1) = "y"
                         MOVE "Y" TO WS-POST-EXIT
                         EXIT PERFORM
                       END-IF
                     END-IF

                 WHEN 5
                    PERFORM HANDLE-VIEW-PROFILE
                 WHEN 6
                    *> User search functionality
                    PERFORM HANDLE-SEARCH-USER
                 END-EVALUATE
               END-PERFORM
               EXIT PERFORM
             END-IF
           END-PERFORM
         ELSE
           IF MENU-CHOICE = "2"

          *> Keep creating until success, EOF, or fatal condition


          MOVE "N" TO WS-ACCT-DONE

          PERFORM UNTIL WS-EOF = "Y" OR WS-ACCT-DONE = "Y"

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

             IF WS-EOF = "Y"
               EXIT PERFORM
             END-IF

             *> ===== PASSWORD + CREATE LOOP =====
             PERFORM UNTIL WS-EOF = "Y" OR WS-ACCT-DONE = "Y"

               MOVE "Enter desired password:" TO OUTPUT-RECORD
               PERFORM PRINT-LINE

               PERFORM READ-AND-LOG
               IF WS-EOF = "Y"
                 EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
               MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) TO WS-IN-LEN

               *> Keep your local length check (this already reprompts)
               IF WS-IN-LEN < 8 OR WS-IN-LEN > 12
                 MOVE "Password must be 8 to 12 characters. Try again." TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
               ELSE
                 MOVE WS-TRIMMED-IN(1:12) TO WS-PASSWORD

                 *> Call CREATEACCOUNT and handle return code
                 CALL "CREATEACCOUNT"
                   USING WS-USERNAME WS-PASSWORD WS-STATUS WS-MESSAGE WS-RET-CODE

                 MOVE WS-MESSAGE TO OUTPUT-RECORD
                 PERFORM PRINT-LINE

                 IF WS-STATUS = "Y"
                   MOVE "Y" TO WS-ACCT-DONE
                   EXIT PERFORM
                 ELSE
                   EVALUATE WS-RET-CODE
                     WHEN 2
                       *> Username already exists -> go back and ask username again
                       EXIT PERFORM

                     WHEN 3
                       *> length issue (should be caught above, but keep safe) -> reprompt password
                       CONTINUE
                     WHEN 4
                       *> missing uppercase -> reprompt password
                       CONTINUE
                     WHEN 5
                       *> missing digit -> reprompt password
                       CONTINUE
                     WHEN 6
                       *> missing special -> reprompt password
                       CONTINUE

                     WHEN 1
                       *> max accounts -> stop this create attempt, return to main menu
                       MOVE "Y" TO WS-ACCT-DONE
                       EXIT PERFORM

                     WHEN OTHER
                       *> any other failure -> reprompt password by default
                       CONTINUE
                   END-EVALUATE
                 END-IF
               END-IF

             END-PERFORM

          END-PERFORM

       ELSE
             IF MENU-CHOICE = "3"
               MOVE "Logging out. Goodbye!" TO OUTPUT-RECORD
               PERFORM PRINT-LINE
               MOVE "Y" TO WS-EXIT-PROGRAM
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
       PROGRAM-ID. EDITPROFILE.

      ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          SELECT OPTIONAL PROFILES-FILE
             ASSIGN TO "PROFILES.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-PROFILES-STATUS.
          SELECT OPTIONAL EXPERIENCE-FILE
             ASSIGN TO "EXPERIENCE.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-EXPERIENCE-STATUS.
          SELECT OPTIONAL EDUCATION-FILE
             ASSIGN TO "EDUCATION.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-EDUCATION-STATUS.

      DATA DIVISION.
      FILE SECTION.
        FD PROFILES-FILE.
          01 PROFILE-RECORD.
            05 PROFILE-USERNAME PIC X(12).
            05 PROFILE-FIRST-NAME PIC X(20).
            05 PROFILE-LAST-NAME PIC X(20).
            05 PROFILE-COLLEGE PIC X(30).
            05 PROFILE-MAJOR PIC X(30).
            05 PROFILE-GRAD-YEAR PIC 9(4).
            05 PROFILE-ABOUT-ME PIC X(100).

        FD EXPERIENCE-FILE.
          01 EXPERIENCE-RECORD.
            05 EXPERIENCE-USERNAME PIC X(12).
            05 EXPERIENCE-INDEX PIC 9.
            05 EXPERIENCE-TITLE PIC X(30).
            05 EXPERIENCE-COMPANY PIC X(30).
            05 EXPERIENCE-START-DATE PIC X(10).
            05 EXPERIENCE-END-DATE PIC X(10).
            05 EXPERIENCE-DESC PIC X(100).

        FD EDUCATION-FILE.
          01 EDUCATION-RECORD.
            05 EDUCATION-USERNAME PIC X(12).
            05 EDUCATION-INDEX PIC 9.
            05 EDUCATION-DEGREE PIC X(30).
            05 EDUCATION-UNI PIC X(30).
            05 EDUCATION-START-YEAR PIC 9(4).
            05 EDUCATION-END-YEAR PIC 9(4).

      WORKING-STORAGE SECTION.
        77 WS-PROFILES-STATUS PIC XX.
        77 WS-EXPERIENCE-STATUS PIC XX.
        77 WS-EDUCATION-STATUS PIC XX.
        77 WS-PROFILE-EOF PIC X VALUE "N".
        77 WS-EXPERIENCE-EOF PIC X VALUE "N".
        77 WS-EDUCATION-EOF PIC X VALUE "N".
        77 WS-PROFILE-COUNT PIC 9 VALUE 0.
        77 WS-EXPERIENCE-COUNT PIC 99 VALUE 0.
        77 WS-EDUCATION-COUNT PIC 99 VALUE 0.
        77 WS-PROFILE-FOUND PIC X VALUE "N".
        77 WS-ENTRY-INDEX PIC 99 VALUE 0.
        77 WS-USER-EXPERIENCE-COUNT PIC 9 VALUE 0.
        77 WS-USER-EDUCATION-COUNT PIC 9 VALUE 0.
        77 WS-MAX-PROFILES PIC 9 VALUE 5.
        77 WS-MAX-EXPERIENCE-ENTRIES PIC 9 VALUE 3.
        77 WS-MAX-EDUCATION-ENTRIES PIC 9 VALUE 3.
        77 WS-MAX-EXPERIENCE-ROWS PIC 99 VALUE 15.
        77 WS-MAX-EDUCATION-ROWS PIC 99 VALUE 15.

        01 WS-PROFILE-TABLE.
          05 WS-PROFILE-ENTRY OCCURS 5 TIMES.
            10 WS-PROFILE-USERNAME PIC X(12).
            10 WS-PROFILE-FIRST-NAME PIC X(20).
            10 WS-PROFILE-LAST-NAME PIC X(20).
            10 WS-PROFILE-COLLEGE PIC X(30).
            10 WS-PROFILE-MAJOR PIC X(30).
            10 WS-PROFILE-GRAD-YEAR PIC 9(4).
            10 WS-PROFILE-ABOUT-ME PIC X(100).

        01 WS-EXPERIENCE-TABLE.
          05 WS-EXPERIENCE-ENTRY OCCURS 15 TIMES.
            10 WS-EXPERIENCE-USERNAME PIC X(12).
            10 WS-EXPERIENCE-INDEX PIC 9.
            10 WS-EXPERIENCE-TITLE PIC X(30).
            10 WS-EXPERIENCE-COMPANY PIC X(30).
            10 WS-EXPERIENCE-START-DATE PIC X(10).
            10 WS-EXPERIENCE-END-DATE PIC X(10).
            10 WS-EXPERIENCE-DESC PIC X(100).

        01 WS-EDUCATION-TABLE.
          05 WS-EDUCATION-ENTRY OCCURS 15 TIMES.
            10 WS-EDUCATION-USERNAME PIC X(12).
            10 WS-EDUCATION-INDEX PIC 9.
            10 WS-EDUCATION-DEGREE PIC X(30).
            10 WS-EDUCATION-UNI PIC X(30).
            10 WS-EDUCATION-START-YEAR PIC 9(4).
            10 WS-EDUCATION-END-YEAR PIC 9(4).

      LINKAGE SECTION.
        77 LK-USERNAME PIC X(12).
        01 LK-PROFILE-DATA.
          05 LK-FIRST-NAME PIC X(20).
          05 LK-LAST-NAME PIC X(20).
          05 LK-COLLEGE PIC X(30).
          05 LK-MAJOR PIC X(30).
          05 LK-GRAD-YEAR PIC 9(4).
          05 LK-ABOUT-ME PIC X(100).
        77 LK-PROFILE-ACTION PIC X(1).
        01 LK-EXPERIENCE.
          05 LK-EXP-TITLE PIC X(30).
          05 LK-EXP-COMPANY PIC X(30).
          05 LK-EXP-START-DATE PIC X(10).
          05 LK-EXP-END-DATE PIC X(10).
          05 LK-EXP-DESC PIC X(100).
        01 LK-EDUCATION.
          05 LK-EDU-DEGREE PIC X(30).
          05 LK-EDU-UNI PIC X(30).
          05 LK-EDU-START-YEAR PIC 9(4).
          05 LK-EDU-END-YEAR PIC 9(4).
        01 LK-VIEW-EXPERIENCE-LIST.
          05 LK-VIEW-EXP-ENTRY OCCURS 3 TIMES.
            10 LK-VIEW-EXP-TITLE PIC X(30).
            10 LK-VIEW-EXP-COMPANY PIC X(30).
            10 LK-VIEW-EXP-START-DATE PIC X(10).
            10 LK-VIEW-EXP-END-DATE PIC X(10).
            10 LK-VIEW-EXP-DESC PIC X(100).
        01 LK-VIEW-EDUCATION-LIST.
          05 LK-VIEW-EDU-ENTRY OCCURS 3 TIMES.
            10 LK-VIEW-EDU-DEGREE PIC X(30).
            10 LK-VIEW-EDU-UNI PIC X(30).
            10 LK-VIEW-EDU-START-YEAR PIC 9(4).
            10 LK-VIEW-EDU-END-YEAR PIC 9(4).
        77 LK-VIEW-EXP-COUNT PIC 9.
        77 LK-VIEW-EDU-COUNT PIC 9.
        77 LK-PROFILE-FOUND PIC X.
        77 LK-MESSAGE PIC X(100).

      PROCEDURE DIVISION.
      MAIN-ENTRY.
        GOBACK.

      ENTRY "BASIC" USING LK-USERNAME LK-PROFILE-DATA LK-PROFILE-ACTION LK-MESSAGE.
        PERFORM UPSERT-PROFILE
        GOBACK.

      ENTRY "EXPERIENCE" USING LK-USERNAME LK-EXPERIENCE LK-PROFILE-ACTION LK-MESSAGE.
        PERFORM ADD-EXPERIENCE
        GOBACK.

      ENTRY "EDUCATION" USING LK-USERNAME LK-EDUCATION LK-PROFILE-ACTION LK-MESSAGE.
        PERFORM ADD-EDUCATION
        GOBACK.

      ENTRY "VIEWPROFILE" USING LK-USERNAME LK-PROFILE-DATA
                                LK-VIEW-EXPERIENCE-LIST LK-VIEW-EDUCATION-LIST
                                LK-VIEW-EXP-COUNT LK-VIEW-EDU-COUNT
                                LK-PROFILE-FOUND LK-MESSAGE.
        PERFORM VIEW-PROFILE
        GOBACK.

      LOAD-PROFILES.
        MOVE 0 TO WS-PROFILE-COUNT
        MOVE "N" TO WS-PROFILE-EOF
        OPEN INPUT PROFILES-FILE
        IF WS-PROFILES-STATUS = "35"
          OPEN OUTPUT PROFILES-FILE
          CLOSE PROFILES-FILE
          OPEN INPUT PROFILES-FILE
        END-IF
        IF WS-PROFILES-STATUS NOT = "00"
          MOVE "Unable to open PROFILES.DAT." TO LK-MESSAGE
          CLOSE PROFILES-FILE
          GOBACK
        END-IF
        PERFORM UNTIL WS-PROFILE-EOF = "Y" OR WS-PROFILE-COUNT >= WS-MAX-PROFILES
          READ PROFILES-FILE
            AT END
              MOVE "Y" TO WS-PROFILE-EOF
            NOT AT END
              ADD 1 TO WS-PROFILE-COUNT
              MOVE PROFILE-USERNAME TO WS-PROFILE-USERNAME(WS-PROFILE-COUNT)
              MOVE PROFILE-FIRST-NAME TO WS-PROFILE-FIRST-NAME(WS-PROFILE-COUNT)
              MOVE PROFILE-LAST-NAME TO WS-PROFILE-LAST-NAME(WS-PROFILE-COUNT)
              MOVE PROFILE-COLLEGE TO WS-PROFILE-COLLEGE(WS-PROFILE-COUNT)
              MOVE PROFILE-MAJOR TO WS-PROFILE-MAJOR(WS-PROFILE-COUNT)
              MOVE PROFILE-GRAD-YEAR TO WS-PROFILE-GRAD-YEAR(WS-PROFILE-COUNT)
              MOVE PROFILE-ABOUT-ME TO WS-PROFILE-ABOUT-ME(WS-PROFILE-COUNT)
          END-READ
        END-PERFORM
        CLOSE PROFILES-FILE.

      SAVE-PROFILES.
        OPEN OUTPUT PROFILES-FILE
        IF WS-PROFILES-STATUS NOT = "00"
          MOVE "Unable to write PROFILES.DAT." TO LK-MESSAGE
          CLOSE PROFILES-FILE
          GOBACK
        END-IF
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1 UNTIL WS-ENTRY-INDEX > WS-PROFILE-COUNT
          MOVE WS-PROFILE-USERNAME(WS-ENTRY-INDEX) TO PROFILE-USERNAME
          MOVE WS-PROFILE-FIRST-NAME(WS-ENTRY-INDEX) TO PROFILE-FIRST-NAME
          MOVE WS-PROFILE-LAST-NAME(WS-ENTRY-INDEX) TO PROFILE-LAST-NAME
          MOVE WS-PROFILE-COLLEGE(WS-ENTRY-INDEX) TO PROFILE-COLLEGE
          MOVE WS-PROFILE-MAJOR(WS-ENTRY-INDEX) TO PROFILE-MAJOR
          MOVE WS-PROFILE-GRAD-YEAR(WS-ENTRY-INDEX) TO PROFILE-GRAD-YEAR
          MOVE WS-PROFILE-ABOUT-ME(WS-ENTRY-INDEX) TO PROFILE-ABOUT-ME
          WRITE PROFILE-RECORD
        END-PERFORM
        CLOSE PROFILES-FILE.

      UPSERT-PROFILE.
        PERFORM LOAD-PROFILES
        MOVE "N" TO WS-PROFILE-FOUND
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1
          UNTIL WS-ENTRY-INDEX > WS-PROFILE-COUNT OR WS-PROFILE-FOUND = "Y"
          IF FUNCTION TRIM(WS-PROFILE-USERNAME(WS-ENTRY-INDEX))
            = FUNCTION TRIM(LK-USERNAME)
            MOVE "Y" TO WS-PROFILE-FOUND
            MOVE FUNCTION TRIM(LK-USERNAME) TO WS-PROFILE-USERNAME(WS-ENTRY-INDEX)
            MOVE FUNCTION TRIM(LK-FIRST-NAME) TO WS-PROFILE-FIRST-NAME(WS-ENTRY-INDEX)
            MOVE FUNCTION TRIM(LK-LAST-NAME) TO WS-PROFILE-LAST-NAME(WS-ENTRY-INDEX)
            MOVE FUNCTION TRIM(LK-COLLEGE) TO WS-PROFILE-COLLEGE(WS-ENTRY-INDEX)
            MOVE FUNCTION TRIM(LK-MAJOR) TO WS-PROFILE-MAJOR(WS-ENTRY-INDEX)
            MOVE LK-GRAD-YEAR TO WS-PROFILE-GRAD-YEAR(WS-ENTRY-INDEX)
            MOVE FUNCTION TRIM(LK-ABOUT-ME) TO WS-PROFILE-ABOUT-ME(WS-ENTRY-INDEX)
          END-IF
        END-PERFORM
        IF WS-PROFILE-FOUND = "N"
          IF WS-PROFILE-COUNT >= WS-MAX-PROFILES
            MOVE "Profile limit reached." TO LK-MESSAGE
            GOBACK
          END-IF
          ADD 1 TO WS-PROFILE-COUNT
          MOVE FUNCTION TRIM(LK-USERNAME) TO WS-PROFILE-USERNAME(WS-PROFILE-COUNT)
          MOVE FUNCTION TRIM(LK-FIRST-NAME) TO WS-PROFILE-FIRST-NAME(WS-PROFILE-COUNT)
          MOVE FUNCTION TRIM(LK-LAST-NAME) TO WS-PROFILE-LAST-NAME(WS-PROFILE-COUNT)
          MOVE FUNCTION TRIM(LK-COLLEGE) TO WS-PROFILE-COLLEGE(WS-PROFILE-COUNT)
          MOVE FUNCTION TRIM(LK-MAJOR) TO WS-PROFILE-MAJOR(WS-PROFILE-COUNT)
          MOVE LK-GRAD-YEAR TO WS-PROFILE-GRAD-YEAR(WS-PROFILE-COUNT)
          MOVE FUNCTION TRIM(LK-ABOUT-ME) TO WS-PROFILE-ABOUT-ME(WS-PROFILE-COUNT)
        END-IF
        PERFORM SAVE-PROFILES
        IF WS-PROFILE-FOUND = "Y"
          MOVE "Profile updated." TO LK-MESSAGE
        ELSE
          MOVE "Profile created." TO LK-MESSAGE
        END-IF.

      LOAD-EXPERIENCE.
        MOVE 0 TO WS-EXPERIENCE-COUNT
        MOVE "N" TO WS-EXPERIENCE-EOF
        OPEN INPUT EXPERIENCE-FILE
        IF WS-EXPERIENCE-STATUS = "35"
          OPEN OUTPUT EXPERIENCE-FILE
          CLOSE EXPERIENCE-FILE
          OPEN INPUT EXPERIENCE-FILE
        END-IF
        IF WS-EXPERIENCE-STATUS NOT = "00"
          MOVE "Unable to open EXPERIENCE.DAT." TO LK-MESSAGE
          CLOSE EXPERIENCE-FILE
          GOBACK
        END-IF
        PERFORM UNTIL WS-EXPERIENCE-EOF = "Y" OR WS-EXPERIENCE-COUNT >= WS-MAX-EXPERIENCE-ROWS
          READ EXPERIENCE-FILE
            AT END
              MOVE "Y" TO WS-EXPERIENCE-EOF
            NOT AT END
              ADD 1 TO WS-EXPERIENCE-COUNT
              MOVE EXPERIENCE-USERNAME TO WS-EXPERIENCE-USERNAME(WS-EXPERIENCE-COUNT)
              MOVE EXPERIENCE-INDEX TO WS-EXPERIENCE-INDEX(WS-EXPERIENCE-COUNT)
              MOVE EXPERIENCE-TITLE TO WS-EXPERIENCE-TITLE(WS-EXPERIENCE-COUNT)
              MOVE EXPERIENCE-COMPANY TO WS-EXPERIENCE-COMPANY(WS-EXPERIENCE-COUNT)
              MOVE EXPERIENCE-START-DATE TO WS-EXPERIENCE-START-DATE(WS-EXPERIENCE-COUNT)
              MOVE EXPERIENCE-END-DATE TO WS-EXPERIENCE-END-DATE(WS-EXPERIENCE-COUNT)
              MOVE EXPERIENCE-DESC TO WS-EXPERIENCE-DESC(WS-EXPERIENCE-COUNT)
          END-READ
        END-PERFORM
        CLOSE EXPERIENCE-FILE.

      SAVE-EXPERIENCE.
        OPEN OUTPUT EXPERIENCE-FILE
        IF WS-EXPERIENCE-STATUS NOT = "00"
          MOVE "Unable to write EXPERIENCE.DAT." TO LK-MESSAGE
          CLOSE EXPERIENCE-FILE
          GOBACK
        END-IF
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1
          UNTIL WS-ENTRY-INDEX > WS-EXPERIENCE-COUNT
          MOVE WS-EXPERIENCE-USERNAME(WS-ENTRY-INDEX) TO EXPERIENCE-USERNAME
          MOVE WS-EXPERIENCE-INDEX(WS-ENTRY-INDEX) TO EXPERIENCE-INDEX
          MOVE WS-EXPERIENCE-TITLE(WS-ENTRY-INDEX) TO EXPERIENCE-TITLE
          MOVE WS-EXPERIENCE-COMPANY(WS-ENTRY-INDEX) TO EXPERIENCE-COMPANY
          MOVE WS-EXPERIENCE-START-DATE(WS-ENTRY-INDEX) TO EXPERIENCE-START-DATE
          MOVE WS-EXPERIENCE-END-DATE(WS-ENTRY-INDEX) TO EXPERIENCE-END-DATE
          MOVE WS-EXPERIENCE-DESC(WS-ENTRY-INDEX) TO EXPERIENCE-DESC
          WRITE EXPERIENCE-RECORD
        END-PERFORM
        CLOSE EXPERIENCE-FILE.

      ADD-EXPERIENCE.
        PERFORM LOAD-EXPERIENCE
        MOVE 0 TO WS-USER-EXPERIENCE-COUNT
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1
          UNTIL WS-ENTRY-INDEX > WS-EXPERIENCE-COUNT
          IF FUNCTION TRIM(WS-EXPERIENCE-USERNAME(WS-ENTRY-INDEX))
            = FUNCTION TRIM(LK-USERNAME)
            ADD 1 TO WS-USER-EXPERIENCE-COUNT
          END-IF
        END-PERFORM
        IF WS-USER-EXPERIENCE-COUNT >= WS-MAX-EXPERIENCE-ENTRIES
          MOVE "Experience limit reached (max 3)." TO LK-MESSAGE
          GOBACK
        END-IF
        IF WS-EXPERIENCE-COUNT >= WS-MAX-EXPERIENCE-ROWS
          MOVE "Experience storage is full." TO LK-MESSAGE
          GOBACK
        END-IF
        ADD 1 TO WS-EXPERIENCE-COUNT
        MOVE FUNCTION TRIM(LK-USERNAME) TO WS-EXPERIENCE-USERNAME(WS-EXPERIENCE-COUNT)
        COMPUTE WS-EXPERIENCE-INDEX(WS-EXPERIENCE-COUNT)
          = WS-USER-EXPERIENCE-COUNT + 1
        MOVE FUNCTION TRIM(LK-EXP-TITLE) TO WS-EXPERIENCE-TITLE(WS-EXPERIENCE-COUNT)
        MOVE FUNCTION TRIM(LK-EXP-COMPANY) TO WS-EXPERIENCE-COMPANY(WS-EXPERIENCE-COUNT)
        MOVE FUNCTION TRIM(LK-EXP-START-DATE) TO WS-EXPERIENCE-START-DATE(WS-EXPERIENCE-COUNT)
        MOVE FUNCTION TRIM(LK-EXP-END-DATE) TO WS-EXPERIENCE-END-DATE(WS-EXPERIENCE-COUNT)
        MOVE FUNCTION TRIM(LK-EXP-DESC) TO WS-EXPERIENCE-DESC(WS-EXPERIENCE-COUNT)
        PERFORM SAVE-EXPERIENCE
        MOVE "Experience saved." TO LK-MESSAGE.

      LOAD-EDUCATION.
        MOVE 0 TO WS-EDUCATION-COUNT
        MOVE "N" TO WS-EDUCATION-EOF
        OPEN INPUT EDUCATION-FILE
        IF WS-EDUCATION-STATUS = "35"
          OPEN OUTPUT EDUCATION-FILE
          CLOSE EDUCATION-FILE
          OPEN INPUT EDUCATION-FILE
        END-IF
        IF WS-EDUCATION-STATUS NOT = "00"
          MOVE "Unable to open EDUCATION.DAT." TO LK-MESSAGE
          CLOSE EDUCATION-FILE
          GOBACK
        END-IF
        PERFORM UNTIL WS-EDUCATION-EOF = "Y" OR WS-EDUCATION-COUNT >= WS-MAX-EDUCATION-ROWS
          READ EDUCATION-FILE
            AT END
              MOVE "Y" TO WS-EDUCATION-EOF
            NOT AT END
              ADD 1 TO WS-EDUCATION-COUNT
              MOVE EDUCATION-USERNAME TO WS-EDUCATION-USERNAME(WS-EDUCATION-COUNT)
              MOVE EDUCATION-INDEX TO WS-EDUCATION-INDEX(WS-EDUCATION-COUNT)
              MOVE EDUCATION-DEGREE TO WS-EDUCATION-DEGREE(WS-EDUCATION-COUNT)
              MOVE EDUCATION-UNI TO WS-EDUCATION-UNI(WS-EDUCATION-COUNT)
              MOVE EDUCATION-START-YEAR TO WS-EDUCATION-START-YEAR(WS-EDUCATION-COUNT)
              MOVE EDUCATION-END-YEAR TO WS-EDUCATION-END-YEAR(WS-EDUCATION-COUNT)
          END-READ
        END-PERFORM
        CLOSE EDUCATION-FILE.

      SAVE-EDUCATION.
        OPEN OUTPUT EDUCATION-FILE
        IF WS-EDUCATION-STATUS NOT = "00"
          MOVE "Unable to write EDUCATION.DAT." TO LK-MESSAGE
          CLOSE EDUCATION-FILE
          GOBACK
        END-IF
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1
          UNTIL WS-ENTRY-INDEX > WS-EDUCATION-COUNT
          MOVE WS-EDUCATION-USERNAME(WS-ENTRY-INDEX) TO EDUCATION-USERNAME
          MOVE WS-EDUCATION-INDEX(WS-ENTRY-INDEX) TO EDUCATION-INDEX
          MOVE WS-EDUCATION-DEGREE(WS-ENTRY-INDEX) TO EDUCATION-DEGREE
          MOVE WS-EDUCATION-UNI(WS-ENTRY-INDEX) TO EDUCATION-UNI
          MOVE WS-EDUCATION-START-YEAR(WS-ENTRY-INDEX) TO EDUCATION-START-YEAR
          MOVE WS-EDUCATION-END-YEAR(WS-ENTRY-INDEX) TO EDUCATION-END-YEAR
          WRITE EDUCATION-RECORD
        END-PERFORM
        CLOSE EDUCATION-FILE.

      ADD-EDUCATION.
        PERFORM LOAD-EDUCATION
        MOVE 0 TO WS-USER-EDUCATION-COUNT
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1
          UNTIL WS-ENTRY-INDEX > WS-EDUCATION-COUNT
          IF FUNCTION TRIM(WS-EDUCATION-USERNAME(WS-ENTRY-INDEX))
            = FUNCTION TRIM(LK-USERNAME)
            ADD 1 TO WS-USER-EDUCATION-COUNT
          END-IF
        END-PERFORM
        IF WS-USER-EDUCATION-COUNT >= WS-MAX-EDUCATION-ENTRIES
          MOVE "Education limit reached (max 3)." TO LK-MESSAGE
          GOBACK
        END-IF
        IF WS-EDUCATION-COUNT >= WS-MAX-EDUCATION-ROWS
          MOVE "Education storage is full." TO LK-MESSAGE
          GOBACK
        END-IF
        ADD 1 TO WS-EDUCATION-COUNT
        MOVE FUNCTION TRIM(LK-USERNAME) TO WS-EDUCATION-USERNAME(WS-EDUCATION-COUNT)
        COMPUTE WS-EDUCATION-INDEX(WS-EDUCATION-COUNT)
          = WS-USER-EDUCATION-COUNT + 1
        MOVE FUNCTION TRIM(LK-EDU-DEGREE) TO WS-EDUCATION-DEGREE(WS-EDUCATION-COUNT)
        MOVE FUNCTION TRIM(LK-EDU-UNI) TO WS-EDUCATION-UNI(WS-EDUCATION-COUNT)
        MOVE LK-EDU-START-YEAR TO WS-EDUCATION-START-YEAR(WS-EDUCATION-COUNT)
        MOVE LK-EDU-END-YEAR TO WS-EDUCATION-END-YEAR(WS-EDUCATION-COUNT)
        PERFORM SAVE-EDUCATION
        MOVE "Education saved." TO LK-MESSAGE.

      VIEW-PROFILE.
        MOVE "N" TO LK-PROFILE-FOUND
        MOVE 0 TO LK-VIEW-EXP-COUNT
        MOVE 0 TO LK-VIEW-EDU-COUNT
        MOVE SPACES TO LK-PROFILE-DATA
        MOVE SPACES TO LK-VIEW-EXPERIENCE-LIST
        MOVE SPACES TO LK-VIEW-EDUCATION-LIST

        PERFORM LOAD-PROFILES
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1
          UNTIL WS-ENTRY-INDEX > WS-PROFILE-COUNT OR LK-PROFILE-FOUND = "Y"
          IF FUNCTION TRIM(WS-PROFILE-USERNAME(WS-ENTRY-INDEX))
            = FUNCTION TRIM(LK-USERNAME)
            MOVE "Y" TO LK-PROFILE-FOUND
            MOVE WS-PROFILE-FIRST-NAME(WS-ENTRY-INDEX) TO LK-FIRST-NAME
            MOVE WS-PROFILE-LAST-NAME(WS-ENTRY-INDEX) TO LK-LAST-NAME
            MOVE WS-PROFILE-COLLEGE(WS-ENTRY-INDEX) TO LK-COLLEGE
            MOVE WS-PROFILE-MAJOR(WS-ENTRY-INDEX) TO LK-MAJOR
            MOVE WS-PROFILE-GRAD-YEAR(WS-ENTRY-INDEX) TO LK-GRAD-YEAR
            MOVE WS-PROFILE-ABOUT-ME(WS-ENTRY-INDEX) TO LK-ABOUT-ME
          END-IF
        END-PERFORM

        IF LK-PROFILE-FOUND = "N"
          MOVE "Profile not found." TO LK-MESSAGE
          GOBACK
        END-IF

        PERFORM LOAD-EXPERIENCE
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1
          UNTIL WS-ENTRY-INDEX > WS-EXPERIENCE-COUNT
            OR LK-VIEW-EXP-COUNT >= WS-MAX-EXPERIENCE-ENTRIES
          IF FUNCTION TRIM(WS-EXPERIENCE-USERNAME(WS-ENTRY-INDEX))
            = FUNCTION TRIM(LK-USERNAME)
            ADD 1 TO LK-VIEW-EXP-COUNT
            MOVE WS-EXPERIENCE-TITLE(WS-ENTRY-INDEX)
              TO LK-VIEW-EXP-TITLE(LK-VIEW-EXP-COUNT)
            MOVE WS-EXPERIENCE-COMPANY(WS-ENTRY-INDEX)
              TO LK-VIEW-EXP-COMPANY(LK-VIEW-EXP-COUNT)
            MOVE WS-EXPERIENCE-START-DATE(WS-ENTRY-INDEX)
              TO LK-VIEW-EXP-START-DATE(LK-VIEW-EXP-COUNT)
            MOVE WS-EXPERIENCE-END-DATE(WS-ENTRY-INDEX)
              TO LK-VIEW-EXP-END-DATE(LK-VIEW-EXP-COUNT)
            MOVE WS-EXPERIENCE-DESC(WS-ENTRY-INDEX)
              TO LK-VIEW-EXP-DESC(LK-VIEW-EXP-COUNT)
          END-IF
        END-PERFORM

        PERFORM LOAD-EDUCATION
        PERFORM VARYING WS-ENTRY-INDEX FROM 1 BY 1
          UNTIL WS-ENTRY-INDEX > WS-EDUCATION-COUNT
            OR LK-VIEW-EDU-COUNT >= WS-MAX-EDUCATION-ENTRIES
          IF FUNCTION TRIM(WS-EDUCATION-USERNAME(WS-ENTRY-INDEX))
            = FUNCTION TRIM(LK-USERNAME)
            ADD 1 TO LK-VIEW-EDU-COUNT
            MOVE WS-EDUCATION-DEGREE(WS-ENTRY-INDEX)
              TO LK-VIEW-EDU-DEGREE(LK-VIEW-EDU-COUNT)
            MOVE WS-EDUCATION-UNI(WS-ENTRY-INDEX)
              TO LK-VIEW-EDU-UNI(LK-VIEW-EDU-COUNT)
            MOVE WS-EDUCATION-START-YEAR(WS-ENTRY-INDEX)
              TO LK-VIEW-EDU-START-YEAR(LK-VIEW-EDU-COUNT)
            MOVE WS-EDUCATION-END-YEAR(WS-ENTRY-INDEX)
              TO LK-VIEW-EDU-END-YEAR(LK-VIEW-EDU-COUNT)
          END-IF
        END-PERFORM

        MOVE "Profile loaded." TO LK-MESSAGE.

      END PROGRAM EDITPROFILE.




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
         MOVE "Y" TO LK-STATUS
         MOVE 0   TO LK-RET-CODE
         MOVE SPACES TO LK-MESSAGE
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
          WHEN "0"
             MOVE 4 TO LK-ACTION
           WHEN "1"
             MOVE "Job search is under construction." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
          WHEN "2"
            MOVE 6 TO LK-ACTION
           WHEN "3"
             MOVE 2 TO LK-ACTION
           WHEN "4"
             MOVE "Logging out. Goodbye!" TO LK-MESSAGE
             MOVE 3 TO LK-ACTION
          WHEN "5"
            MOVE 5 TO LK-ACTION
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
