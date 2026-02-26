      HANDLE-VIEW-PENDING-REQUESTS.
          MOVE "--- Pending Connection Requests ---" TO OUTPUT-RECORD
          PERFORM PRINT-LINE
          MOVE 0 TO WS-PENDING-COUNT
          MOVE "N" TO WS-CONN-EOF

          OPEN INPUT CONNECTIONS-FILE
          IF WS-CONN-FILE-STATUS = "00"
            PERFORM UNTIL WS-CONN-EOF = "Y"
              READ CONNECTIONS-FILE
                AT END
                  MOVE "Y" TO WS-CONN-EOF
                NOT AT END
                  IF FUNCTION TRIM(RECIPIENT-USERNAME) =
                     FUNCTION TRIM(WS-USERNAME)
                    AND (REQUEST-STATUS = "P" OR REQUEST-STATUS = " ")
                    ADD 1 TO WS-PENDING-COUNT
                    MOVE SPACES TO OUTPUT-RECORD
                    STRING "  " DELIMITED BY SIZE
                           FUNCTION TRIM(REQUESTER-USERNAME) DELIMITED BY SIZE
                      INTO OUTPUT-RECORD
                    END-STRING
                    PERFORM PRINT-LINE
                  END-IF
              END-READ
            END-PERFORM
            CLOSE CONNECTIONS-FILE
          END-IF

          IF WS-PENDING-COUNT = 0
            MOVE "You have no pending connection requests at this time."
              TO OUTPUT-RECORD
            PERFORM PRINT-LINE
          END-IF

          MOVE "-----------------------------------" TO OUTPUT-RECORD
          PERFORM PRINT-LINE.
