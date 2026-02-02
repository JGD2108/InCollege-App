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

      *> Profile creation/editing variables
        01 WS-PROFILE-DATA.
          05 WS-FIRST-NAME PIC X(20).
          05 WS-LAST-NAME PIC X(20).
          05 WS-COLLEGE PIC X(30).
          05 WS-MAJOR PIC X(30).
          05 WS-GRAD-YEAR PIC 9(4).
          05 WS-ABOUT-ME PIC X(100).
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
        77 WS-POST-EXIT PIC X VALUE "N".
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

      *> Paragrah (function) to print a line to output file and display
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

      HANDLE-PROFILE-CREATE-UPDATE.
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
            EXIT PARAGRAPH
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
            EXIT PARAGRAPH
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
            EXIT PARAGRAPH
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
            EXIT PARAGRAPH
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
            EXIT PARAGRAPH
          END-IF

          MOVE SPACES TO WS-ABOUT-ME
          MOVE "About Me (Optional, type N to skip):" TO OUTPUT-RECORD
          PERFORM PRINT-LINE
          PERFORM READ-AND-LOG
          IF WS-EOF = "Y"
            MOVE "No input for profile; returning to post-login menu." TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "Y" TO WS-PROFILE-CANCEL
            EXIT PARAGRAPH
          END-IF
          IF INPUT-RECORD(1:1) = "N" OR INPUT-RECORD(1:1) = "n"
            MOVE "Skipping About Me entry." TO OUTPUT-RECORD
            PERFORM PRINT-LINE
          ELSE
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-ABOUT-ME
          END-IF

          CALL "BASIC" USING WS-USERNAME WS-PROFILE-DATA WS-PROFILE-ACTION WS-MESSAGE
          MOVE WS-MESSAGE TO OUTPUT-RECORD
          PERFORM PRINT-LINE

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
            END-IF
          END-IF.

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
                MOVE WS-MESSAGE TO OUTPUT-RECORD
                PERFORM PRINT-LINE
                MOVE "Y" TO WS-EOF
                EXIT PERFORM
              WHEN 4
                PERFORM HANDLE-PROFILE-CREATE-UPDATE
              WHEN 5
                PERFORM HANDLE-VIEW-PROFILE
              WHEN OTHER
                MOVE "Invalid Selection." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
            END-EVALUATE
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
