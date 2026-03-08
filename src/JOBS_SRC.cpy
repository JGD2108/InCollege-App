       HANDLE-JOB-MENU.
          PERFORM UNTIL WS-EOF = "Y"
            MOVE "--- Job Search/Internship Menu ---" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "1. Post a Job/Internship" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "2. Browse Jobs/Internships" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            MOVE "3. Back to Main Menu" TO OUTPUT-RECORD
            PERFORM PRINT-LINE

            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input received; returning to post-login menu."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              EXIT PERFORM
            END-IF

            MOVE INPUT-RECORD(1:1) TO WS-JOB-CHOICE
            EVALUATE WS-JOB-CHOICE
              WHEN "1"
                PERFORM HANDLE-JOB-POST
              WHEN "2"
                PERFORM HANDLE-BROWSE-JOBS
              WHEN "3"
                MOVE "Returning to post-login menu." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
                EXIT PERFORM
              WHEN OTHER
                MOVE "Invalid Selection." TO OUTPUT-RECORD
                PERFORM PRINT-LINE
            END-EVALUATE
          END-PERFORM.

       HANDLE-JOB-POST.
          MOVE SPACES TO WS-JOB-DATA
          MOVE "N" TO WS-JOB-CANCEL

          MOVE "N" TO WS-VALID-INPUT
          PERFORM UNTIL WS-VALID-INPUT = "Y"
            MOVE "Job Title (Required):" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for job title; returning to job menu."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "Y" TO WS-JOB-CANCEL
              EXIT PERFORM
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
            MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
              TO WS-IN-LEN
            IF WS-IN-LEN = 0
              MOVE "Job Title is required." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
            ELSE
              IF WS-IN-LEN > 30
                MOVE "Job Title must be 30 characters or fewer."
                  TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              ELSE
                MOVE WS-TRIMMED-IN TO WS-JOB-TITLE
                MOVE "Y" TO WS-VALID-INPUT
              END-IF
            END-IF
          END-PERFORM
          IF WS-JOB-CANCEL = "Y"
            EXIT PARAGRAPH
          END-IF

          MOVE "N" TO WS-VALID-INPUT
          PERFORM UNTIL WS-VALID-INPUT = "Y"
            MOVE "Description (Required):" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for description; returning to job menu."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "Y" TO WS-JOB-CANCEL
              EXIT PERFORM
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
            MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
              TO WS-IN-LEN
            IF WS-IN-LEN = 0
              MOVE "Description is required." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
            ELSE
              IF WS-IN-LEN > 100
                MOVE "Description must be 100 characters or fewer."
                  TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              ELSE
                MOVE WS-TRIMMED-IN TO WS-JOB-DESCRIPTION
                MOVE "Y" TO WS-VALID-INPUT
              END-IF
            END-IF
          END-PERFORM
          IF WS-JOB-CANCEL = "Y"
            EXIT PARAGRAPH
          END-IF

          MOVE "N" TO WS-VALID-INPUT
          PERFORM UNTIL WS-VALID-INPUT = "Y"
            MOVE "Employer (Required):" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for employer; returning to job menu."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "Y" TO WS-JOB-CANCEL
              EXIT PERFORM
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
            MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
              TO WS-IN-LEN
            IF WS-IN-LEN = 0
              MOVE "Employer is required." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
            ELSE
              IF WS-IN-LEN > 30
                MOVE "Employer must be 30 characters or fewer."
                  TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              ELSE
                MOVE WS-TRIMMED-IN TO WS-JOB-EMPLOYER
                MOVE "Y" TO WS-VALID-INPUT
              END-IF
            END-IF
          END-PERFORM
          IF WS-JOB-CANCEL = "Y"
            EXIT PARAGRAPH
          END-IF

          MOVE "N" TO WS-VALID-INPUT
          PERFORM UNTIL WS-VALID-INPUT = "Y"
            MOVE "Location (Required):" TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for location; returning to job menu."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "Y" TO WS-JOB-CANCEL
              EXIT PERFORM
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
            MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
              TO WS-IN-LEN
            IF WS-IN-LEN = 0
              MOVE "Location is required." TO OUTPUT-RECORD
              PERFORM PRINT-LINE
            ELSE
              IF WS-IN-LEN > 30
                MOVE "Location must be 30 characters or fewer."
                  TO OUTPUT-RECORD
                PERFORM PRINT-LINE
              ELSE
                MOVE WS-TRIMMED-IN TO WS-JOB-LOCATION
                MOVE "Y" TO WS-VALID-INPUT
              END-IF
            END-IF
          END-PERFORM
          IF WS-JOB-CANCEL = "Y"
            EXIT PARAGRAPH
          END-IF

          MOVE "N" TO WS-VALID-INPUT
          PERFORM UNTIL WS-VALID-INPUT = "Y"
            MOVE "Salary (Optional, format $50,000/year or $25/hour, N to skip):"
              TO OUTPUT-RECORD
            PERFORM PRINT-LINE
            PERFORM READ-AND-LOG
            IF WS-EOF = "Y"
              MOVE "No input for salary; returning to job menu."
                TO OUTPUT-RECORD
              PERFORM PRINT-LINE
              MOVE "Y" TO WS-JOB-CANCEL
              EXIT PERFORM
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TRIMMED-IN
            MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
              TO WS-IN-LEN
            IF WS-IN-LEN = 0
              MOVE SPACES TO WS-JOB-SALARY
              MOVE "Y" TO WS-VALID-INPUT
            ELSE
              IF WS-TRIMMED-IN(1:1) = "N" OR WS-TRIMMED-IN(1:1) = "n"
                MOVE SPACES TO WS-JOB-SALARY
                MOVE "Y" TO WS-VALID-INPUT
              ELSE
                PERFORM VALIDATE-SALARY-FORMAT
                IF WS-VALID-INPUT = "Y"
                  MOVE WS-TRIMMED-IN TO WS-JOB-SALARY
                ELSE
                  MOVE "Salary format must look like $50,000/year or $25/hour."
                    TO OUTPUT-RECORD
                  PERFORM PRINT-LINE
                END-IF
              END-IF
            END-IF
          END-PERFORM
          IF WS-JOB-CANCEL = "Y"
            EXIT PARAGRAPH
          END-IF

          MOVE SPACES TO WS-MESSAGE
          CALL "JOBPOSTPROG" USING WS-JOB-DATA WS-STATUS WS-MESSAGE
          MOVE WS-MESSAGE TO OUTPUT-RECORD
          PERFORM PRINT-LINE.

       VALIDATE-SALARY-FORMAT.
          MOVE "N" TO WS-VALID-INPUT
          IF WS-IN-LEN < 8
            EXIT PARAGRAPH
          END-IF
          IF WS-TRIMMED-IN(1:1) NOT = "$"
            EXIT PARAGRAPH
          END-IF

          MOVE 0 TO WS-SALARY-SLASH-POS
          PERFORM VARYING WS-SALARY-IDX FROM 1 BY 1
            UNTIL WS-SALARY-IDX > WS-IN-LEN OR WS-SALARY-SLASH-POS > 0
            IF WS-TRIMMED-IN(WS-SALARY-IDX:1) = "/"
              MOVE WS-SALARY-IDX TO WS-SALARY-SLASH-POS
            END-IF
          END-PERFORM
          IF WS-SALARY-SLASH-POS = 0
            EXIT PARAGRAPH
          END-IF
          IF WS-SALARY-SLASH-POS < 4
            EXIT PARAGRAPH
          END-IF

          MOVE "N" TO WS-SALARY-HAS-DIGIT
          MOVE "Y" TO WS-SALARY-CHARS-OK
          PERFORM VARYING WS-SALARY-IDX FROM 2 BY 1
            UNTIL WS-SALARY-IDX >= WS-SALARY-SLASH-POS
            IF WS-TRIMMED-IN(WS-SALARY-IDX:1) IS NUMERIC
              MOVE "Y" TO WS-SALARY-HAS-DIGIT
            ELSE
              IF WS-TRIMMED-IN(WS-SALARY-IDX:1) NOT = ","
                MOVE "N" TO WS-SALARY-CHARS-OK
                EXIT PERFORM
              END-IF
            END-IF
          END-PERFORM
          IF WS-SALARY-CHARS-OK NOT = "Y"
            EXIT PARAGRAPH
          END-IF
          IF WS-SALARY-HAS-DIGIT NOT = "Y"
            EXIT PARAGRAPH
          END-IF

          MOVE SPACES TO WS-SALARY-RATE
          MOVE FUNCTION LOWER-CASE(
            FUNCTION TRIM(
              WS-TRIMMED-IN(WS-SALARY-SLASH-POS + 1:
                WS-IN-LEN - WS-SALARY-SLASH-POS)
            )
          ) TO WS-SALARY-RATE
          IF FUNCTION TRIM(WS-SALARY-RATE) = "year"
             OR FUNCTION TRIM(WS-SALARY-RATE) = "hour"
            MOVE "Y" TO WS-VALID-INPUT
          END-IF.
