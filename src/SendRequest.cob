       IDENTIFICATION DIVISION.
       PROGRAM-ID. SENDREQUEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CONNECTIONS-FILE
               ASSIGN TO "CONNECTIONS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONN-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CONNECTIONS-FILE.
       01 CONNECTION-RECORD.
          05 REQUESTER-USERNAME    PIC X(12).
          05 RECIPIENT-USERNAME    PIC X(12).
          05 REQUEST-STATUS        PIC X.

       WORKING-STORAGE SECTION.
       77 WS-CONN-FILE-STATUS      PIC XX.
       01 WS-CONNECTION-TABLE.
          05 WS-CONN-ENTRY OCCURS 25 TIMES.
             10 WS-CONN-REQUESTER     PIC X(12).
             10 WS-CONN-RECIPIENT     PIC X(12).
             10 WS-CONN-STATUS        PIC X.
       77 WS-CONN-COUNT            PIC 99 VALUE 0.
       77 WS-CONN-IDX              PIC 99.
       77 WS-CONN-EOF              PIC X VALUE "N".
       77 WS-VALID-REQUEST         PIC X VALUE "Y".
       77 WS-NORMALIZED-REQUESTER  PIC X(12).
       77 WS-NORMALIZED-RECIPIENT  PIC X(12).

       LINKAGE SECTION.
       01 LK-REQUESTER         PIC X(12).
       01 LK-RECIPIENT         PIC X(12).
       01 LK-STATUS            PIC X.
       01 LK-MESSAGE           PIC X(80).

       PROCEDURE DIVISION USING LK-REQUESTER LK-RECIPIENT LK-STATUS LK-MESSAGE.

           MOVE "Y" TO WS-VALID-REQUEST
           MOVE "Y" TO LK-STATUS
           MOVE SPACES TO LK-MESSAGE
           MOVE FUNCTION TRIM(LK-REQUESTER) TO WS-NORMALIZED-REQUESTER
           MOVE FUNCTION TRIM(LK-RECIPIENT) TO WS-NORMALIZED-RECIPIENT

           IF FUNCTION LENGTH(WS-NORMALIZED-REQUESTER) = 0
               OR FUNCTION LENGTH(WS-NORMALIZED-RECIPIENT) = 0
               MOVE "N" TO WS-VALID-REQUEST
               MOVE "Invalid connection request." TO LK-MESSAGE
           END-IF

           IF WS-VALID-REQUEST = "Y"
               IF WS-NORMALIZED-REQUESTER = WS-NORMALIZED-RECIPIENT
                   MOVE "N" TO WS-VALID-REQUEST
                   MOVE "You cannot send a connection request to yourself."
                     TO LK-MESSAGE
               END-IF
           END-IF

           *> Load existing connections
           IF WS-VALID-REQUEST = "Y"
               PERFORM LOAD-CONNECTIONS
           END-IF

           *> Validate the connection request
           IF WS-VALID-REQUEST = "Y"
               PERFORM VALIDATE-REQUEST
           END-IF

           *> If valid, add the connection
           IF WS-VALID-REQUEST = "Y"
               PERFORM ADD-CONNECTION
               PERFORM SAVE-CONNECTIONS
               IF WS-VALID-REQUEST = "Y"
                   STRING "Connection request sent to " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-NORMALIZED-RECIPIENT)
                            DELIMITED BY SIZE
                          "." DELIMITED BY SIZE
                     INTO LK-MESSAGE
                   END-STRING
                   MOVE "Y" TO LK-STATUS
               ELSE
                   MOVE "N" TO LK-STATUS
               END-IF
           ELSE
               MOVE "N" TO LK-STATUS
           END-IF

           GOBACK.

       LOAD-CONNECTIONS.
           MOVE 0 TO WS-CONN-COUNT
           MOVE "N" TO WS-CONN-EOF

           OPEN INPUT CONNECTIONS-FILE
           
           *> If file doesn't exist, create it
           IF WS-CONN-FILE-STATUS = "35"
               CLOSE CONNECTIONS-FILE
               OPEN OUTPUT CONNECTIONS-FILE
               CLOSE CONNECTIONS-FILE
               OPEN INPUT CONNECTIONS-FILE
           END-IF

           IF WS-CONN-FILE-STATUS NOT = "00"
               MOVE "N" TO WS-VALID-REQUEST
               MOVE "Unable to access connection data." TO LK-MESSAGE
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

       VALIDATE-REQUEST.
           *> Check if already connected, or if duplicate pending request exists
           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONN-COUNT OR WS-VALID-REQUEST = "N"
               
               *> Check if already connected (status = 'A')
               IF (FUNCTION TRIM(WS-CONN-REQUESTER(WS-CONN-IDX)) =
                     WS-NORMALIZED-REQUESTER
                   AND FUNCTION TRIM(WS-CONN-RECIPIENT(WS-CONN-IDX)) =
                     WS-NORMALIZED-RECIPIENT
                   AND WS-CONN-STATUS(WS-CONN-IDX) = "A")
                 OR (FUNCTION TRIM(WS-CONN-REQUESTER(WS-CONN-IDX)) =
                       WS-NORMALIZED-RECIPIENT
                   AND FUNCTION TRIM(WS-CONN-RECIPIENT(WS-CONN-IDX)) =
                       WS-NORMALIZED-REQUESTER
                   AND WS-CONN-STATUS(WS-CONN-IDX) = "A")
                   MOVE "N" TO WS-VALID-REQUEST
                   MOVE "You are already connected with this user." TO LK-MESSAGE
               END-IF

               *> Check if recipient already sent requester a pending request
               IF FUNCTION TRIM(WS-CONN-REQUESTER(WS-CONN-IDX)) =
                    WS-NORMALIZED-RECIPIENT
                   AND FUNCTION TRIM(WS-CONN-RECIPIENT(WS-CONN-IDX)) =
                    WS-NORMALIZED-REQUESTER
                   AND (WS-CONN-STATUS(WS-CONN-IDX) = "P"
                     OR WS-CONN-STATUS(WS-CONN-IDX) = " ")
                   MOVE "N" TO WS-VALID-REQUEST
                   MOVE "This user has already sent you a connection request." TO LK-MESSAGE
               END-IF

               *> Check if requester already sent recipient a pending request
               IF FUNCTION TRIM(WS-CONN-REQUESTER(WS-CONN-IDX)) =
                    WS-NORMALIZED-REQUESTER
                   AND FUNCTION TRIM(WS-CONN-RECIPIENT(WS-CONN-IDX)) =
                    WS-NORMALIZED-RECIPIENT
                   AND (WS-CONN-STATUS(WS-CONN-IDX) = "P"
                     OR WS-CONN-STATUS(WS-CONN-IDX) = " ")
                   MOVE "N" TO WS-VALID-REQUEST
                   MOVE "You have already sent a connection request to this user." TO LK-MESSAGE
               END-IF
           END-PERFORM

           *> Check if table is full
           IF WS-CONN-COUNT >= 25 AND WS-VALID-REQUEST = "Y"
               MOVE "N" TO WS-VALID-REQUEST
               MOVE "Connection limit reached. Cannot send request." TO LK-MESSAGE
           END-IF.

       ADD-CONNECTION.
           ADD 1 TO WS-CONN-COUNT
           MOVE WS-NORMALIZED-REQUESTER TO WS-CONN-REQUESTER(WS-CONN-COUNT)
           MOVE WS-NORMALIZED-RECIPIENT TO WS-CONN-RECIPIENT(WS-CONN-COUNT)
           MOVE "P" TO WS-CONN-STATUS(WS-CONN-COUNT).

       SAVE-CONNECTIONS.
           OPEN OUTPUT CONNECTIONS-FILE
           IF WS-CONN-FILE-STATUS NOT = "00"
               MOVE "N" TO WS-VALID-REQUEST
               MOVE "Unable to save connection request." TO LK-MESSAGE
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONN-COUNT
               MOVE WS-CONN-REQUESTER(WS-CONN-IDX) TO REQUESTER-USERNAME
               MOVE WS-CONN-RECIPIENT(WS-CONN-IDX) TO RECIPIENT-USERNAME
               MOVE WS-CONN-STATUS(WS-CONN-IDX) TO REQUEST-STATUS
               WRITE CONNECTION-RECORD
           END-PERFORM

           CLOSE CONNECTIONS-FILE.

       END PROGRAM SENDREQUEST.
