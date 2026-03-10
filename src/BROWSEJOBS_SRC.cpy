       HANDLE-BROWSE-JOBS.
               MOVE "N" TO WS-JOBS-EOF
               MOVE "N" TO WS-JOBS-FOUND

               OPEN INPUT JOBS-FILE
               IF WS-JOBS-STATUS = "35"
                  MOVE "No jobs/internships have been posted yet." TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
                  CLOSE JOBS-FILE
                  EXIT PARAGRAPH
               END-IF

               IF WS-JOBS-STATUS NOT = "00"
                  MOVE "Unable to load jobs/internships right now." TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
                  CLOSE JOBS-FILE
                  EXIT PARAGRAPH
               END-IF

               MOVE "--- Available Jobs/Internships ---" TO OUTPUT-RECORD
               PERFORM PRINT-LINE

               PERFORM UNTIL WS-JOBS-EOF = "Y"
                  READ JOBS-FILE
                     AT END
                        MOVE "Y" TO WS-JOBS-EOF
                     NOT AT END
                        MOVE "Y" TO WS-JOBS-FOUND
                        MOVE SPACES TO OUTPUT-RECORD
                        STRING "Title: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-TITLE) DELIMITED BY SIZE
                           INTO OUTPUT-RECORD
                        END-STRING
                        PERFORM PRINT-LINE

                        MOVE SPACES TO OUTPUT-RECORD
                        STRING "Employer: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-EMPLOYER) DELIMITED BY SIZE
                           INTO OUTPUT-RECORD
                        END-STRING
                        PERFORM PRINT-LINE

                        MOVE SPACES TO OUTPUT-RECORD
                        STRING "Location: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-LOCATION) DELIMITED BY SIZE
                           INTO OUTPUT-RECORD
                        END-STRING
                        PERFORM PRINT-LINE

                        MOVE SPACES TO OUTPUT-RECORD
                        STRING "Description: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-DESCRIPTION) DELIMITED BY SIZE
                           INTO OUTPUT-RECORD
                        END-STRING
                        PERFORM PRINT-LINE

                        IF FUNCTION LENGTH(FUNCTION TRIM(JOB-SALARY)) > 0
                           MOVE SPACES TO OUTPUT-RECORD
                           STRING "Salary: " DELIMITED BY SIZE
                                     FUNCTION TRIM(JOB-SALARY) DELIMITED BY SIZE
                              INTO OUTPUT-RECORD
                           END-STRING
                           PERFORM PRINT-LINE
                        END-IF

                        MOVE SPACES TO OUTPUT-RECORD
                        STRING "Posted By: " DELIMITED BY SIZE
                                  FUNCTION TRIM(JOB-POSTED-BY) DELIMITED BY SIZE
                           INTO OUTPUT-RECORD
                        END-STRING
                        PERFORM PRINT-LINE

                        MOVE "----------------------------------------" TO OUTPUT-RECORD
                        PERFORM PRINT-LINE
                  END-READ
               END-PERFORM

               IF WS-JOBS-FOUND = "N"
                  MOVE "No jobs/internships have been posted yet." TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
               END-IF

               CLOSE JOBS-FILE.
