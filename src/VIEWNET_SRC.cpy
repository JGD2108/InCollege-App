      *> VIEWNET_SRC.cpy - View My Network logic (established connections)
      *> Copybook for: As a logged-in user, I want to view all users I am connected with.
      *> Requires in main program: CONNECTED-FILE, WS-USERNAME, WS-CONNECTED-OTHER,
      *>   WS-CONNECTED-STATUS, WS-CONN-EOF, WS-VIEW-PROFILE-DATA, VIEWPROFILE, PRINT-LINE.

      HANDLE-VIEW-NETWORK.
          MOVE "--- Your Network ---" TO OUTPUT-RECORD
          PERFORM PRINT-LINE
          OPEN INPUT CONNECTED-FILE
          IF WS-CONNECTED-STATUS = "35"
            MOVE "You have no connections yet." TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "--------------------" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            EXIT PARAGRAPH
          END-IF
          IF WS-CONNECTED-STATUS NOT = "00"
            CLOSE CONNECTED-FILE
            MOVE "You have no connections yet." TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "--------------------" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            EXIT PARAGRAPH
          END-IF
          MOVE "N" TO WS-CONN-EOF
          PERFORM UNTIL WS-CONN-EOF = "Y"
            READ CONNECTED-FILE
              AT END
                MOVE "Y" TO WS-CONN-EOF
              NOT AT END
                IF FUNCTION TRIM(CONNECTED-USER1) =
                   FUNCTION TRIM(WS-USERNAME)
                  MOVE FUNCTION TRIM(CONNECTED-USER2) TO WS-CONNECTED-OTHER
                ELSE
                  IF FUNCTION TRIM(CONNECTED-USER2) =
                     FUNCTION TRIM(WS-USERNAME)
                    MOVE FUNCTION TRIM(CONNECTED-USER1) TO WS-CONNECTED-OTHER
                  END-IF
                END-IF
                IF FUNCTION TRIM(CONNECTED-USER1) =
                   FUNCTION TRIM(WS-USERNAME)
                   OR FUNCTION TRIM(CONNECTED-USER2) =
                      FUNCTION TRIM(WS-USERNAME)
                  MOVE "N" TO WS-PROFILE-FOUND
                  MOVE SPACES TO WS-VIEW-PROFILE-DATA
                  MOVE SPACES TO WS-VIEW-EXPERIENCE-LIST
                  MOVE SPACES TO WS-VIEW-EDUCATION-LIST
                  MOVE 0 TO WS-VIEW-EXP-COUNT
                  MOVE 0 TO WS-VIEW-EDU-COUNT
                  CALL "VIEWPROFILE" USING WS-CONNECTED-OTHER
                       WS-VIEW-PROFILE-DATA WS-VIEW-EXPERIENCE-LIST
                       WS-VIEW-EDUCATION-LIST WS-VIEW-EXP-COUNT
                       WS-VIEW-EDU-COUNT WS-PROFILE-FOUND WS-MESSAGE
                  MOVE SPACES TO OUTPUT-RECORD
                  STRING "Connected with: " DELIMITED BY SIZE
                         FUNCTION TRIM(WS-VIEW-FIRST-NAME) DELIMITED BY SIZE
                         " " DELIMITED BY SIZE
                         FUNCTION TRIM(WS-VIEW-LAST-NAME) DELIMITED BY SIZE
                         " (University: " DELIMITED BY SIZE
                         FUNCTION TRIM(WS-VIEW-COLLEGE) DELIMITED BY SIZE
                         ", Major: " DELIMITED BY SIZE
                         FUNCTION TRIM(WS-VIEW-MAJOR) DELIMITED BY SIZE
                         ")" DELIMITED BY SIZE
                    INTO OUTPUT-RECORD
                  END-STRING
                  PERFORM PRINT-LINE
                END-IF
            END-READ
          END-PERFORM
          CLOSE CONNECTED-FILE
          MOVE "--------------------" TO OUTPUT-RECORD
          PERFORM PRINT-LINE.
