      HANDLE-VIEW-PENDING-REQUESTS.
          MOVE "--- Pending Connection Requests ---" TO OUTPUT-RECORD
          PERFORM PRINT-LINE
          PERFORM LOAD-CONNECTIONS-FOR-PENDING

          IF WS-CONN-FILE-STATUS NOT = "00" AND WS-CONN-FILE-STATUS NOT = "35"
            MOVE "Unable to access connection request data." TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "-----------------------------------" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            EXIT PARAGRAPH
          END-IF

          PERFORM BUILD-PENDING-CONNECTION-LIST
          IF WS-PENDING-COUNT = 0
            MOVE "You have no pending connection requests at this time."
              TO OUTPUT-RECORD
            PERFORM PRINT-LINE
          ELSE
            PERFORM PROCESS-PENDING-CONNECTION-LIST
            PERFORM SAVE-CONNECTIONS-AFTER-PENDING

            IF WS-CONN-FILE-STATUS = "00"
              MOVE SPACES TO OUTPUT-RECORD
              STRING "Processed " DELIMITED BY SIZE
                     WS-PROCESSED-COUNT DELIMITED BY SIZE
                     " pending request(s)." DELIMITED BY SIZE
                INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              MOVE SPACES TO OUTPUT-RECORD
              STRING WS-ACCEPTED-COUNT DELIMITED BY SIZE
                     " accepted, " DELIMITED BY SIZE
                     WS-REJECTED-COUNT DELIMITED BY SIZE
                     " rejected." DELIMITED BY SIZE
                INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE
            ELSE
              MOVE "Unable to save connection request updates." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
            END-IF
          END-IF

          MOVE "-----------------------------------" TO OUTPUT-RECORD
          PERFORM PRINT-LINE.

      LOAD-CONNECTIONS-FOR-PENDING.
          MOVE 0 TO WS-CONN-COUNT
          MOVE 0 TO WS-PENDING-COUNT
          MOVE 0 TO WS-ACCEPTED-COUNT
          MOVE 0 TO WS-REJECTED-COUNT
          MOVE 0 TO WS-PROCESSED-COUNT
          MOVE "N" TO WS-CONN-EOF
          MOVE ZEROES TO WS-PENDING-INDEX-TABLE

          OPEN INPUT CONNECTIONS-FILE

          IF WS-CONN-FILE-STATUS = "35"
            EXIT PARAGRAPH
          END-IF

          IF WS-CONN-FILE-STATUS NOT = "00"
            EXIT PARAGRAPH
          END-IF

          PERFORM UNTIL WS-CONN-EOF = "Y"
            READ CONNECTIONS-FILE
              AT END
                MOVE "Y" TO WS-CONN-EOF
              NOT AT END
                IF WS-CONN-COUNT < 25
                  ADD 1 TO WS-CONN-COUNT
                  MOVE REQUESTER-USERNAME
                    TO WS-CONN-REQUESTER(WS-CONN-COUNT)
                  MOVE RECIPIENT-USERNAME
                    TO WS-CONN-RECIPIENT(WS-CONN-COUNT)
                  MOVE REQUEST-STATUS
                    TO WS-CONN-STATUS(WS-CONN-COUNT)
                END-IF
            END-READ
          END-PERFORM

          CLOSE CONNECTIONS-FILE.

      BUILD-PENDING-CONNECTION-LIST.
          MOVE 0 TO WS-PENDING-COUNT

          PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
            UNTIL WS-CONN-IDX > WS-CONN-COUNT
            IF FUNCTION TRIM(WS-CONN-RECIPIENT(WS-CONN-IDX)) =
               FUNCTION TRIM(WS-USERNAME)
              AND (WS-CONN-STATUS(WS-CONN-IDX) = "P"
               OR WS-CONN-STATUS(WS-CONN-IDX) = " ")
              ADD 1 TO WS-PENDING-COUNT
              MOVE WS-CONN-IDX TO WS-PENDING-ENTRY(WS-PENDING-COUNT)

              MOVE SPACES TO OUTPUT-RECORD
              STRING "  " DELIMITED BY SIZE
                     FUNCTION TRIM(WS-CONN-REQUESTER(WS-CONN-IDX))
                       DELIMITED BY SIZE
                INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE
            END-IF
          END-PERFORM.

      PROCESS-PENDING-CONNECTION-LIST.
          PERFORM VARYING WS-PENDING-IDX FROM 1 BY 1
            UNTIL WS-PENDING-IDX > WS-PENDING-COUNT OR WS-EOF = "Y"
            MOVE WS-PENDING-ENTRY(WS-PENDING-IDX) TO WS-SELECTED-CONN-IDX
            MOVE "N" TO WS-ACTION-VALID

            PERFORM UNTIL WS-ACTION-VALID = "Y" OR WS-EOF = "Y"
              MOVE SPACES TO OUTPUT-RECORD
              STRING "Request from "
                     DELIMITED BY SIZE
                     FUNCTION TRIM(WS-CONN-REQUESTER(WS-SELECTED-CONN-IDX))
                     DELIMITED BY SIZE
                     ":" DELIMITED BY SIZE
                INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              MOVE "Enter A to accept or R to reject:" TO OUTPUT-RECORD
              PERFORM PRINT-LINE

              PERFORM READ-AND-LOG
              IF WS-EOF = "Y"
                MOVE "No input for request action; returning to menu."
                  TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              ELSE
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                MOVE SPACE TO WS-REQUEST-ACTION
                IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
                  MOVE WS-TRIMMED-IN(1:1) TO WS-REQUEST-ACTION
                END-IF

                EVALUATE WS-REQUEST-ACTION
                  WHEN "A"
                  WHEN "a"
                    MOVE "A" TO WS-CONN-STATUS(WS-SELECTED-CONN-IDX)
                    ADD 1 TO WS-ACCEPTED-COUNT
                    ADD 1 TO WS-PROCESSED-COUNT
                    MOVE "Y" TO WS-ACTION-VALID
                    MOVE SPACES TO OUTPUT-RECORD
                    STRING "Connection request from " DELIMITED BY SIZE
                           FUNCTION TRIM(
                             WS-CONN-REQUESTER(WS-SELECTED-CONN-IDX))
                             DELIMITED BY SIZE
                           " accepted." DELIMITED BY SIZE
                      INTO OUTPUT-RECORD
                    END-STRING
                    PERFORM PRINT-LINE
                  WHEN "R"
                  WHEN "r"
                    MOVE "D" TO WS-CONN-STATUS(WS-SELECTED-CONN-IDX)
                    ADD 1 TO WS-REJECTED-COUNT
                    ADD 1 TO WS-PROCESSED-COUNT
                    MOVE "Y" TO WS-ACTION-VALID
                    MOVE SPACES TO OUTPUT-RECORD
                    STRING "Connection request from " DELIMITED BY SIZE
                           FUNCTION TRIM(
                             WS-CONN-REQUESTER(WS-SELECTED-CONN-IDX))
                             DELIMITED BY SIZE
                           " rejected." DELIMITED BY SIZE
                      INTO OUTPUT-RECORD
                    END-STRING
                    PERFORM PRINT-LINE
                  WHEN OTHER
                    MOVE "Invalid selection. Please enter A or R."
                      TO OUTPUT-RECORD
                    PERFORM PRINT-LINE
                END-EVALUATE
              END-IF
            END-PERFORM
          END-PERFORM.

      SAVE-CONNECTIONS-AFTER-PENDING.
          OPEN OUTPUT CONNECTIONS-FILE
          IF WS-CONN-FILE-STATUS NOT = "00"
            EXIT PARAGRAPH
          END-IF

          PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
            UNTIL WS-CONN-IDX > WS-CONN-COUNT
            IF WS-CONN-STATUS(WS-CONN-IDX) NOT = "D"
              MOVE WS-CONN-REQUESTER(WS-CONN-IDX) TO REQUESTER-USERNAME
              MOVE WS-CONN-RECIPIENT(WS-CONN-IDX) TO RECIPIENT-USERNAME
              MOVE WS-CONN-STATUS(WS-CONN-IDX) TO REQUEST-STATUS
              WRITE CONNECTION-RECORD
            END-IF
          END-PERFORM

          CLOSE CONNECTIONS-FILE.
