      HANDLE-MESSAGING-MENU.
          MOVE "N" TO WS-MESSAGE-EXIT

          PERFORM UNTIL WS-MESSAGE-EXIT = "Y" OR WS-EOF = "Y"
              MOVE "--- Messages Menu ---" TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "1. Send a New Message" TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "2. View My Messages" TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "3. Back to Main Menu" TO OUTPUT-RECORD
              PERFORM PRINT-LINE

              PERFORM READ-AND-LOG
              IF WS-EOF = "Y"
                 MOVE "No input received; returning to post-login menu." TO OUTPUT-RECORD
                 PERFORM PRINT-LINE
                 EXIT PERFORM
              END-IF

              MOVE INPUT-RECORD(1:1) TO WS-MESSAGE-CHOICE

              EVALUATE WS-MESSAGE-CHOICE
                 WHEN "1"
                    PERFORM SEND-MESSAGE
                 WHEN "2"
                    MOVE "View My Messages is under construction." TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
                    *> Later:
                    *> CALL "REVIEWMESSAGES" USING ...
                 WHEN "3"
                    MOVE "Returning to post-login menu." TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
                    MOVE "Y" TO WS-MESSAGE-EXIT
                 WHEN OTHER
                    MOVE "Invalid selection." TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
              END-EVALUATE
          END-PERFORM.

      SEND-MESSAGE.
           MOVE SPACES TO WS-MSG-RECIPIENT
           MOVE SPACES TO WS-MSG-TEXT
           MOVE "N" TO WS-CAN-MESSAGE

           MOVE "N" TO WS-VALID-INPUT
           PERFORM UNTIL WS-VALID-INPUT = "Y" OR WS-EOF = "Y"
              MOVE "Enter recipient's username (must be a connection):"
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE

              PERFORM READ-AND-LOG
              IF WS-EOF = "Y"
                  MOVE "No input received. Returning to menu."
                    TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
                  EXIT PERFORM
              END-IF

              MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
              MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
                TO WS-IN-LEN

              IF WS-IN-LEN = 0
                  MOVE "Recipient cannot be blank." TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
              ELSE
                  IF WS-IN-LEN > 12
                      MOVE "Recipient username must be 1 to 12 characters."
                        TO OUTPUT-RECORD
                      PERFORM PRINT-LINE
                  ELSE
                      MOVE WS-TRIMMED-IN(1:12) TO WS-MSG-RECIPIENT

                      IF FUNCTION TRIM(WS-MSG-RECIPIENT) =
                         FUNCTION TRIM(WS-USERNAME)
                          MOVE "You cannot send a message to yourself."
                            TO OUTPUT-RECORD
                          PERFORM PRINT-LINE
                      ELSE
                          MOVE "Y" TO WS-VALID-INPUT
                      END-IF
                  END-IF
              END-IF
           END-PERFORM

           IF WS-EOF = "Y"
              EXIT PARAGRAPH
           END-IF

           PERFORM VERIFY-MESSAGE-NETWORK

           IF WS-CAN-MESSAGE NOT = "Y"
              EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-VALID-INPUT
           PERFORM UNTIL WS-VALID-INPUT = "Y" OR WS-EOF = "Y"
              MOVE "Enter your message (max 200 chars):" TO OUTPUT-RECORD
              PERFORM PRINT-LINE

              PERFORM READ-AND-LOG
              IF WS-EOF = "Y"
                  MOVE "No message entered. Returning to menu."
                    TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
                  EXIT PERFORM
              END-IF

              MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
              MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
                TO WS-IN-LEN

              IF WS-IN-LEN = 0
                  MOVE "Message cannot be blank." TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
              ELSE
                  IF WS-IN-LEN > 200
                      MOVE "Message too long. Max 200 characters."
                        TO OUTPUT-RECORD
                      PERFORM PRINT-LINE
                  ELSE
                      MOVE INPUT-RECORD TO WS-MSG-TEXT
                      MOVE "Y" TO WS-VALID-INPUT
                  END-IF
              END-IF
           END-PERFORM

           IF WS-EOF = "Y"
              EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-MSG-TIMESTAMP
           MOVE WS-USERNAME      TO MSG-SENDER
           MOVE WS-MSG-RECIPIENT TO MSG-RECIPIENT
           MOVE WS-MSG-TEXT      TO MSG-CONTENT
           OPEN EXTEND MESSAGES-FILE
           WRITE MESSAGE-RECORD
           CLOSE MESSAGES-FILE

           STRING "Message sent to " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-MSG-RECIPIENT) DELIMITED BY SPACE
                  " successfully!" DELIMITED BY SIZE
             INTO OUTPUT-RECORD
           PERFORM PRINT-LINE.

      VERIFY-MESSAGE-NETWORK.
           MOVE "N" TO WS-CAN-MESSAGE
           MOVE "N" TO WS-EST-EOF

           OPEN INPUT ESTABLISHED-FILE

           IF WS-EST-FILE-STATUS = "35"
              MOVE "You can only message users in your network."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PARAGRAPH
           END-IF

           IF WS-EST-FILE-STATUS NOT = "00"
              MOVE "Error opening established connections file."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EST-EOF = "Y" OR WS-CAN-MESSAGE = "Y"
              READ ESTABLISHED-FILE
                  AT END
                      MOVE "Y" TO WS-EST-EOF
                  NOT AT END
                      IF (FUNCTION TRIM(EST-USER1) =
                            FUNCTION TRIM(WS-USERNAME)
                          AND FUNCTION TRIM(EST-USER2) =
                            FUNCTION TRIM(WS-MSG-RECIPIENT))
                         OR
                         (FUNCTION TRIM(EST-USER2) =
                            FUNCTION TRIM(WS-USERNAME)
                          AND FUNCTION TRIM(EST-USER1) =
                            FUNCTION TRIM(WS-MSG-RECIPIENT))
                          MOVE "Y" TO WS-CAN-MESSAGE
                      END-IF
              END-READ
           END-PERFORM

           CLOSE ESTABLISHED-FILE

           IF WS-CAN-MESSAGE NOT = "Y"
              MOVE "You may only send messages to users in your network."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
           END-IF.
