       HANDLE-BROWSE-JOBS.
       *> Still under work, not functional
          MOVE "--- Available Jobs/Internships ---" TO OUTPUT-RECORD
          PERFORM PRINT-LINE

          PERFORM VIEW-JOB.
          GOBACK.


       VIEW-JOB.
           MOVE "N" TO WS-JOBS-EOF
           MOVE 0 TO WS-PAGE-COUNT
           MOVE SPACES TO WS-SELECTED-JOB-ID

           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STATUS NOT = "00"
               MOVE "Unable to open jobs file." TO OUTPUT-RECORD
               PERFORM PRINT-LINE
               GOBACK
           END-IF

           PERFORM UNTIL WS-JOBS-EOF = "Y"
               READ JOBS-FILE
                   AT END
                       MOVE "Y" TO WS-JOBS-EOF

                   NOT AT END
                       ADD 1 TO WS-PAGE-COUNT

                       STRING
                           "ID: "
                           FUNCTION TRIM(JOB-ID)
                           " | "
                           FUNCTION TRIM(JOB-TITLE)
                           " | "
                           FUNCTION TRIM(JOB-EMPLOYER)
                           " | "
                           FUNCTION TRIM(JOB-LOCATION)
                           INTO OUTPUT-RECORD
                       END-STRING
                       PERFORM PRINT-LINE

                       *> FULL PAGE HANDLING
                       IF WS-PAGE-COUNT >= WS-JOBS-PER-PAGE
                           MOVE "Enter a Job ID to view details, Q to quit, or any key for next page:"
                               TO OUTPUT-RECORD
                           PERFORM PRINT-LINE

                           PERFORM READ-AND-LOG
                           IF WS-EOF = "Y"
                               MOVE "No input received; returning to menu." TO OUTPUT-RECORD
                               PERFORM PRINT-LINE
                               CLOSE JOBS-FILE
                               EXIT PERFORM
                           END-IF

                           MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN

                           IF WS-TRIMMED-IN(1:1) = "Q" OR WS-TRIMMED-IN(1:1) = "q"
                               CLOSE JOBS-FILE
                               EXIT PERFORM

                           ELSE
                               MOVE WS-TRIMMED-IN TO WS-SELECTED-JOB-ID

                               CLOSE JOBS-FILE
                               PERFORM VIEW-JOB-DETAILS
                               EXIT PERFORM
                           END-IF

                           MOVE 0 TO WS-PAGE-COUNT
                       END-IF
               END-READ
           END-PERFORM

           *> FINAL (PARTIAL PAGE) HANDLING
           IF WS-PAGE-COUNT > 0 AND NOT (WS-TRIMMED-IN(1:1) = "Q" OR WS-TRIMMED-IN(1:1) = "q")
               MOVE "Enter a Job ID to view details, Q to quit:"
                   TO OUTPUT-RECORD
               PERFORM PRINT-LINE

               PERFORM READ-AND-LOG
               IF WS-EOF = "Y"
                   MOVE "No input received; returning to menu." TO OUTPUT-RECORD
                   PERFORM PRINT-LINE
               ELSE
                   MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN

                   IF WS-TRIMMED-IN(1:1) NOT = "Q"
                      AND WS-TRIMMED-IN(1:1) NOT = "q"

                       MOVE WS-TRIMMED-IN TO WS-SELECTED-JOB-ID

                       CLOSE JOBS-FILE
                       PERFORM VIEW-JOB-DETAILS
                       GOBACK
                   END-IF
               END-IF
           END-IF

           MOVE "--- End of Available Jobs/Internships ---"
               TO OUTPUT-RECORD
           PERFORM PRINT-LINE

           CLOSE JOBS-FILE
           GOBACK.

       VIEW-JOB-DETAILS.
           MOVE "N" TO WS-JOBS-EOF

           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STATUS NOT = "00"
               MOVE "Unable to open jobs file." TO OUTPUT-RECORD
               PERFORM PRINT-LINE
               GOBACK
           END-IF

           PERFORM UNTIL WS-JOBS-EOF = "Y"
               READ JOBS-FILE
                   AT END
                       MOVE "Y" TO WS-JOBS-EOF
                       MOVE "Job ID not found." TO OUTPUT-RECORD
                       PERFORM PRINT-LINE

                   NOT AT END
                       IF FUNCTION TRIM(JOB-ID) = FUNCTION TRIM(WS-SELECTED-JOB-ID)
                           PERFORM DISPLAY-JOB-FULL
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM

           CLOSE JOBS-FILE
           GOBACK.

       DISPLAY-JOB-FULL.

              MOVE "----------------------------------------" TO OUTPUT-RECORD
              PERFORM PRINT-LINE

              STRING
                  "Job ID: " FUNCTION TRIM(JOB-ID)
                  INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              STRING
                  "Title: " FUNCTION TRIM(JOB-TITLE)
                  INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              STRING
                  "Description: " FUNCTION TRIM(JOB-DESCRIPTION)
                  INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              STRING
                  "Employer: " FUNCTION TRIM(JOB-EMPLOYER)
                  INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              STRING
                  "Location: " FUNCTION TRIM(JOB-LOCATION)
                  INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              STRING
                  "Salary: " FUNCTION TRIM(JOB-SALARY)
                  INTO OUTPUT-RECORD
              END-STRING
              PERFORM PRINT-LINE

              MOVE "----------------------------------------" TO OUTPUT-RECORD
              PERFORM PRINT-LINE

              *> Prompt user to save job
              MOVE "Would you like to apply for this job/internship? (Y/N): "
                  TO OUTPUT-RECORD
              PERFORM PRINT-LINE

              PERFORM READ-AND-LOG

              IF WS-EOF = "Y"
                  MOVE "No input received. Returning to job list."
                      TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
                  EXIT PARAGRAPH
              END-IF

              MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN

              IF WS-TRIMMED-IN(1:1) = "Y" OR WS-TRIMMED-IN(1:1) = "y"
                  MOVE JOB-ID TO WS-SAVED-JOB-ID
                  *> Here is where we would actually record the application.
                  MOVE "Successfully applied!" TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
              ELSE
                  MOVE "Returning to job menu." TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
              END-IF
              GOBACK.
