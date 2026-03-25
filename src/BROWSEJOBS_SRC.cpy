       HANDLE-BROWSE-JOBS.
          PERFORM UNTIL WS-EOF = "Y"
             PERFORM DISPLAY-JOB-LIST

             IF WS-EOF = "Y" OR WS-JOBS-FOUND = "E"
                EXIT PERFORM
             END-IF

             IF WS-JOBS-FOUND = "N"
                MOVE "Enter 0 to return to the job menu:"
                  TO OUTPUT-RECORD
             ELSE
                MOVE "Enter job number to view details, or 0 to go back:"
                  TO OUTPUT-RECORD
             END-IF
             PERFORM PRINT-LINE

             PERFORM READ-AND-LOG
             IF WS-EOF = "Y"
                MOVE "No input received; returning to the job menu."
                  TO OUTPUT-RECORD
                PERFORM PRINT-LINE
                EXIT PERFORM
             END-IF

             MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
             MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
               TO WS-IN-LEN

             IF WS-IN-LEN = 0
                MOVE "Invalid job number." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
             ELSE
                IF WS-TRIMMED-IN(1:WS-IN-LEN) IS NUMERIC
                   COMPUTE WS-JOB-INDEX =
                     FUNCTION NUMVAL(WS-TRIMMED-IN(1:WS-IN-LEN))

                   IF WS-JOBS-FOUND = "N"
                      IF WS-JOB-INDEX = 0
                         EXIT PERFORM
                      ELSE
                         MOVE "Invalid job number." TO OUTPUT-RECORD
                         PERFORM PRINT-LINE
                      END-IF
                   ELSE
                      IF WS-JOB-INDEX = 0
                         EXIT PERFORM
                      ELSE
                         IF WS-JOB-INDEX > WS-JOBS-COUNT
                            MOVE "Invalid job number." TO OUTPUT-RECORD
                            PERFORM PRINT-LINE
                         ELSE
                            PERFORM VIEW-JOB-DETAILS
                         END-IF
                      END-IF
                   END-IF
                ELSE
                   MOVE "Invalid job number." TO OUTPUT-RECORD
                   PERFORM PRINT-LINE
                END-IF
             END-IF
          END-PERFORM.

       DISPLAY-JOB-LIST.
          MOVE 0 TO WS-JOBS-COUNT
          MOVE "N" TO WS-JOBS-EOF
          MOVE "N" TO WS-JOBS-FOUND

          MOVE "--- Available Job Listings ---" TO OUTPUT-RECORD
          PERFORM PRINT-LINE

          OPEN INPUT JOBS-FILE
          IF WS-JOBS-STATUS = "35" OR WS-JOBS-STATUS = "05"
             MOVE "No job/internship postings are currently available."
               TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             MOVE "-----------------------------" TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             EXIT PARAGRAPH
          END-IF

          IF WS-JOBS-STATUS NOT = "00"
             MOVE "Unable to open jobs file." TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             MOVE "E" TO WS-JOBS-FOUND
             EXIT PARAGRAPH
          END-IF

          PERFORM UNTIL WS-JOBS-EOF = "Y"
             READ JOBS-FILE
                AT END
                   MOVE "Y" TO WS-JOBS-EOF
                NOT AT END
                   ADD 1 TO WS-JOBS-COUNT
                   MOVE "Y" TO WS-JOBS-FOUND

                   MOVE WS-JOBS-COUNT TO WS-JOB-NUMBER-TEXT
                   MOVE SPACES TO OUTPUT-RECORD
                   STRING
                      FUNCTION TRIM(WS-JOB-NUMBER-TEXT) DELIMITED BY SIZE
                      ". " DELIMITED BY SIZE
                      FUNCTION TRIM(JOB-TITLE) DELIMITED BY SIZE
                      " at " DELIMITED BY SIZE
                      FUNCTION TRIM(JOB-EMPLOYER) DELIMITED BY SIZE
                      INTO OUTPUT-RECORD
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE SPACES TO OUTPUT-RECORD
                   STRING
                      "   (" DELIMITED BY SIZE
                      FUNCTION TRIM(JOB-LOCATION) DELIMITED BY SIZE
                      ")" DELIMITED BY SIZE
                      INTO OUTPUT-RECORD
                   END-STRING
                   PERFORM PRINT-LINE
             END-READ
          END-PERFORM

          CLOSE JOBS-FILE

          IF WS-JOBS-FOUND = "N"
             MOVE "No job/internship postings are currently available."
               TO OUTPUT-RECORD
             PERFORM PRINT-LINE
          END-IF

          MOVE "-----------------------------" TO OUTPUT-RECORD
          PERFORM PRINT-LINE.

       VIEW-JOB-DETAILS.
          MOVE 0 TO WS-JOBS-COUNT
          MOVE "N" TO WS-JOBS-EOF
          MOVE "N" TO WS-JOBS-FOUND

          OPEN INPUT JOBS-FILE
          IF WS-JOBS-STATUS NOT = "00"
             MOVE "Unable to open jobs file." TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             EXIT PARAGRAPH
          END-IF

          PERFORM UNTIL WS-JOBS-EOF = "Y"
             READ JOBS-FILE
                AT END
                   MOVE "Y" TO WS-JOBS-EOF
                NOT AT END
                   ADD 1 TO WS-JOBS-COUNT
                   IF WS-JOBS-COUNT = WS-JOB-INDEX
                      MOVE "Y" TO WS-JOBS-FOUND
                      PERFORM DISPLAY-JOB-FULL
                      EXIT PERFORM
                   END-IF
             END-READ
          END-PERFORM

          CLOSE JOBS-FILE

          IF WS-JOBS-FOUND NOT = "Y"
             MOVE "Invalid job number." TO OUTPUT-RECORD
             PERFORM PRINT-LINE
          END-IF.

       DISPLAY-JOB-FULL.
          MOVE "--- Job Details ---" TO OUTPUT-RECORD
          PERFORM PRINT-LINE

          MOVE SPACES TO OUTPUT-RECORD
          STRING
             "Title: " DELIMITED BY SIZE
             FUNCTION TRIM(JOB-TITLE) DELIMITED BY SIZE
             INTO OUTPUT-RECORD
          END-STRING
          PERFORM PRINT-LINE

          MOVE FUNCTION TRIM(JOB-DESCRIPTION) TO WS-DESC-TEMP
          MOVE FUNCTION LENGTH(FUNCTION TRIM(JOB-DESCRIPTION))
            TO WS-IN-LEN-3

          IF WS-IN-LEN-3 <= 67
             MOVE SPACES TO OUTPUT-RECORD
             STRING
                "Description: " DELIMITED BY SIZE
                WS-DESC-TEMP(1:WS-IN-LEN-3) DELIMITED BY SIZE
                INTO OUTPUT-RECORD
             END-STRING
             PERFORM PRINT-LINE
          ELSE
             MOVE SPACES TO OUTPUT-RECORD
             STRING
                "Description: " DELIMITED BY SIZE
                WS-DESC-TEMP(1:67) DELIMITED BY SIZE
                INTO OUTPUT-RECORD
             END-STRING
             PERFORM PRINT-LINE

             MOVE SPACES TO OUTPUT-RECORD
             STRING
                "             " DELIMITED BY SIZE
                WS-DESC-TEMP(68:WS-IN-LEN-3 - 67) DELIMITED BY SIZE
                INTO OUTPUT-RECORD
             END-STRING
             PERFORM PRINT-LINE
          END-IF

          MOVE SPACES TO OUTPUT-RECORD
          STRING
             "Employer: " DELIMITED BY SIZE
             FUNCTION TRIM(JOB-EMPLOYER) DELIMITED BY SIZE
             INTO OUTPUT-RECORD
          END-STRING
          PERFORM PRINT-LINE

          MOVE SPACES TO OUTPUT-RECORD
          STRING
             "Location: " DELIMITED BY SIZE
             FUNCTION TRIM(JOB-LOCATION) DELIMITED BY SIZE
             INTO OUTPUT-RECORD
          END-STRING
          PERFORM PRINT-LINE

          MOVE SPACES TO OUTPUT-RECORD
          IF FUNCTION LENGTH(FUNCTION TRIM(JOB-SALARY)) = 0
             MOVE "Salary: Not provided" TO OUTPUT-RECORD
          ELSE
             STRING
                "Salary: " DELIMITED BY SIZE
                FUNCTION TRIM(JOB-SALARY) DELIMITED BY SIZE
                INTO OUTPUT-RECORD
             END-STRING
          END-IF
          PERFORM PRINT-LINE

          MOVE "-------------------" TO OUTPUT-RECORD
          PERFORM PRINT-LINE

          MOVE "N" TO WS-VALID-INPUT
          PERFORM UNTIL WS-EOF = "Y" OR WS-VALID-INPUT = "Y"
             MOVE "0. Back to Job List" TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             MOVE "1. Apply for this job" TO OUTPUT-RECORD
             PERFORM PRINT-LINE
             MOVE "Enter your choice:" TO OUTPUT-RECORD
             PERFORM PRINT-LINE

             PERFORM READ-AND-LOG
             IF WS-EOF = "Y"
                MOVE "No input received; returning to the job list."
                  TO OUTPUT-RECORD
                PERFORM PRINT-LINE
             ELSE
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
                MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
                  TO WS-IN-LEN

                IF WS-IN-LEN = 1 AND WS-TRIMMED-IN(1:1) = "0"
                   MOVE "Y" TO WS-VALID-INPUT
                ELSE
                   IF WS-IN-LEN = 1 AND WS-TRIMMED-IN(1:1) = "1"
                      MOVE "Applying to job!" TO OUTPUT-RECORD
                      PERFORM PRINT-LINE
                      *> This is where you would call the application handling code
                      MOVE "Y" TO WS-VALID-INPUT
                   ELSE
                       MOVE "Invalid selection." TO OUTPUT-RECORD
                       PERFORM PRINT-LINE
                END-IF
             END-IF
          END-PERFORM.
