      HANDLE-VIEW-NETWORK.
          MOVE "--- My Network ---" TO OUTPUT-RECORD
          PERFORM PRINT-LINE

          MOVE 0 TO WS-NETWORK-COUNT
          MOVE "N" TO WS-EST-EOF

          OPEN INPUT ESTABLISHED-FILE
          IF WS-EST-FILE-STATUS = "35"
            MOVE "You have no established connections at this time."
              TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "--------------------" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM PROMPT-RETURN-FROM-NETWORK
            EXIT PARAGRAPH
          END-IF

          IF WS-EST-FILE-STATUS NOT = "00"
            MOVE "Unable to access connection data." TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "--------------------" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM PROMPT-RETURN-FROM-NETWORK
            EXIT PARAGRAPH
          END-IF

          PERFORM UNTIL WS-EST-EOF = "Y"
            READ ESTABLISHED-FILE
              AT END
                MOVE "Y" TO WS-EST-EOF
              NOT AT END
                IF FUNCTION TRIM(EST-USER1) = FUNCTION TRIM(WS-USERNAME)
                  MOVE FUNCTION TRIM(EST-USER2) TO WS-CONNECTED-OTHER
                  PERFORM DISPLAY-NETWORK-CONNECTION
                ELSE
                  IF FUNCTION TRIM(EST-USER2) = FUNCTION TRIM(WS-USERNAME)
                    MOVE FUNCTION TRIM(EST-USER1) TO WS-CONNECTED-OTHER
                    PERFORM DISPLAY-NETWORK-CONNECTION
                  END-IF
                END-IF
            END-READ
          END-PERFORM

          CLOSE ESTABLISHED-FILE

          IF WS-NETWORK-COUNT = 0
            MOVE "You have no established connections at this time."
              TO OUTPUT-RECORD
            PERFORM PRINT-LINE
          END-IF

          MOVE "--------------------" TO OUTPUT-RECORD
          PERFORM PRINT-LINE

          PERFORM PROMPT-RETURN-FROM-NETWORK.

      DISPLAY-NETWORK-CONNECTION.
          ADD 1 TO WS-NETWORK-COUNT
          MOVE "N" TO WS-PROFILE-FOUND
          MOVE SPACES TO WS-VIEW-PROFILE-DATA
          MOVE SPACES TO WS-VIEW-EXPERIENCE-LIST
          MOVE SPACES TO WS-VIEW-EDUCATION-LIST
          MOVE 0 TO WS-VIEW-EXP-COUNT
          MOVE 0 TO WS-VIEW-EDU-COUNT

          CALL "VIEWPROFILE" USING WS-CONNECTED-OTHER WS-VIEW-PROFILE-DATA
                               WS-VIEW-EXPERIENCE-LIST WS-VIEW-EDUCATION-LIST
                               WS-VIEW-EXP-COUNT WS-VIEW-EDU-COUNT
                               WS-PROFILE-FOUND WS-MESSAGE

          IF WS-PROFILE-FOUND = "Y"
            MOVE SPACES TO OUTPUT-RECORD
            STRING "  " DELIMITED BY SIZE
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
          ELSE
            MOVE SPACES TO OUTPUT-RECORD
            STRING "  " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-CONNECTED-OTHER) DELIMITED BY SIZE
              INTO OUTPUT-RECORD
            END-STRING
            PERFORM PRINT-LINE
          END-IF.

      PROMPT-RETURN-FROM-NETWORK.
          MOVE "Enter 0 to return to post-login menu." TO OUTPUT-RECORD
          PERFORM PRINT-LINE

          PERFORM UNTIL WS-EOF = "Y"
            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input received; returning to post-login menu."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PERFORM
            END-IF

            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
            IF WS-TRIMMED-IN = "0"
              EXIT PERFORM
            ELSE
              MOVE "Invalid selection. Please enter 0." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
            END-IF
          END-PERFORM.
