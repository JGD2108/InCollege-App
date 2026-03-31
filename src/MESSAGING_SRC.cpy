      HANDLE-MESSAGE-MENU.
          MOVE "N" TO WS-MESSAGE-EXIT

          PERFORM UNTIL WS-MESSAGE-EXIT = "Y" OR WS-EOF = "Y"
              MOVE "--- Messages Menu ---" TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "1. Send a Message" TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "2. Review My Messages" TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "0. Return to Previous Menu" TO OUTPUT-RECORD
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
                    MOVE "Send Message is under construction." TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
                    *> Later:
                    *> CALL "SENDMESSAGE" USING ...
                 WHEN "2"
                    MOVE "Review Messages is under construction." TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
                    *> Later:
                    *> CALL "REVIEWMESSAGES" USING ...
                 WHEN "0"
                    MOVE "Returning to post-login menu." TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
                    MOVE "Y" TO WS-MESSAGE-EXIT
                 WHEN OTHER
                    MOVE "Invalid selection." TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
              END-EVALUATE
          END-PERFORM.
          