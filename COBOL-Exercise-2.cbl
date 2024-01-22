      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
           01 NUM pic 99 VALUE 00.
           01 ARRAY.
              02 WS-Unsorted PIC S9(2) OCCURS 100 TIMES.
              02 WS-UnsortTemp PIC S9(2) OCCURS 100 TIMES.
           01 TEMP PIC S9(3).
           01 i PIC 99.
           01 j PIC 99.
           01 jp1 PIC 99.
           01 WS-Input PIC A.
           *> 01 Num2 PIC 9(2).
      *-----------------------
       PROCEDURE DIVISION.
           bbl-sort-para.
              MOVE 1 TO i.
              DISPLAY "Enter 10 numbers: "
              PERFORM UNTIL i > 10
                 DISPLAY "Number " i
                 ACCEPT WS-UnsortTemp(i)
                 IF WS-UnsortTemp(i) >= 0 THEN
                     ADD 1 TO NUM
                     MOVE WS-UnsortTemp(i) TO WS-Unsorted(NUM)
                 END-IF
                 ADD 1 TO i
              END-PERFORM.

              COMPUTE i = NUM - 1.
              PERFORM UNTIL i < 1
                 MOVE 1 TO j
                 PERFORM UNTIL j > i
                 COMPUTE jp1 = j + 1
                 IF (WS-Unsorted(j) > WS-Unsorted(jp1))
                    MOVE WS-Unsorted(j) TO TEMP
                    MOVE WS-Unsorted(jp1) TO WS-Unsorted(j)
                    MOVE TEMP TO WS-Unsorted(jp1)
                 END-IF
                 ADD 1 TO j GIVING j
                 END-PERFORM
                 SUBTRACT 1 FROM i GIVING i
              END-PERFORM.

              DISPLAY "Sorted:"
              MOVE 1 TO i.
              PERFORM UNTIL i > NUM
                 DISPLAY i ": " WS-Unsorted(i)
                 ADD 1 TO i
              END-PERFORM.

              DISPLAY "Perform Bubble Sort? Y/N"
              ACCEPT WS-Input
              MOVE 0 TO NUM.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           PERFORM bbl-sort-para WITH TEST BEFORE UNTIL WS-Input="N"
           STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
