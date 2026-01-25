IDENTIFICATION DIVISION.
       PROGRAM-ID. POSTLOGINPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01 WS-SKILL-LIST.
           05 WS-SKILL PIC X(30) OCCURS 5 TIMES.

       LINKAGE SECTION.
         77 LK-POST-CHOICE PIC X(1).
         77 LK-SKILL-CHOICE PIC X(1).
         77 LK-ACTION PIC 9.
         77 LK-MESSAGE PIC X(100).

       PROCEDURE DIVISION USING LK-POST-CHOICE LK-SKILL-CHOICE LK-ACTION LK-MESSAGE.
         *> Action codes returned in LK-ACTION:
         *> 1 = print LK-MESSAGE
         *> 2 = caller should show skill submenu and pass skill choice back
         *> 3 = logout

         EVALUATE LK-POST-CHOICE
           WHEN "1"
             MOVE "Job search is under construction." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
           WHEN "2"
             MOVE "Find someone you know is under construction." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
           WHEN "3"
             MOVE 2 TO LK-ACTION
           WHEN "4"
             MOVE "Logging out. Goodbye!" TO LK-MESSAGE
             MOVE 3 TO LK-ACTION
           WHEN OTHER
             MOVE "Invalid Selection." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
         END-EVALUATE

         *> Skill handling: if caller provided LK-SKILL-CHOICE, interpret it
         IF LK-ACTION = 2 AND FUNCTION LENGTH(FUNCTION TRIM(LK-SKILL-CHOICE)) > 0
           IF LK-SKILL-CHOICE = "0"
             MOVE "Returning to post-login menu." TO LK-MESSAGE
             MOVE 1 TO LK-ACTION
           ELSE
             IF LK-SKILL-CHOICE >= "1" AND LK-SKILL-CHOICE <= "5"
               MOVE "Selected skill is under construction." TO LK-MESSAGE
               MOVE 1 TO LK-ACTION
             ELSE
               MOVE "Invalid Selection." TO LK-MESSAGE
               MOVE 1 TO LK-ACTION
             END-IF
           END-IF
         END-IF

         GOBACK.

       END PROGRAM POSTLOGINPROG.

