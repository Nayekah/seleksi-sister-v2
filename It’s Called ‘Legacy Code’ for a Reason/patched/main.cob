IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "input.txt".
           SELECT ACC-FILE ASSIGN TO "accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TMP-FILE ASSIGN TO "temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD IN-FILE.
       01 IN-RECORD             PIC X(18).

       FD ACC-FILE.
       01 ACC-RECORD-RAW        PIC X(18).

       FD TMP-FILE.
       01 TMP-RECORD            PIC X(18).

       FD OUT-FILE.
       01 OUT-RECORD            PIC X(120).

       WORKING-STORAGE SECTION.
       77 IN-ACCOUNT            PIC 9(6).
       77 IN-ACTION             PIC X(3).
       77 IN-AMOUNT             PIC 9(6)V99.

       77 ACC-ACCOUNT           PIC 9(6).
       77 ACC-BALANCE           PIC 9(6)V99.

       77 TMP-BALANCE           PIC 9(6)V99.
       77 MATCH-FOUND           PIC X VALUE "N".
       77 UPDATED               PIC X VALUE "N".

       77 FORMATTED-AMOUNT      PIC 9(6).99.
       
       77 RAI-TO-IDR            PIC 9(9) VALUE 120000000.
       77 IDR-VALUE             PIC 9(12)V99.
       77 IDR-FORMATTED         PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       77 RAI-FORMATTED         PIC ZZZ,ZZ9.99.

       77 INTEREST-RATE         PIC 9V999 VALUE 0.010.
       77 INTEREST-AMOUNT       PIC 9(6)V99.
       77 EOF-FLAG              PIC X VALUE "N".
       77 CMD-ARG               PIC X(20).
       77 FIRST-RUN             PIC X VALUE "Y".
       77 WS-DATE               PIC X(8).
       77 WS-TIME               PIC X(8).

       PROCEDURE DIVISION.

       MAIN.
           ACCEPT CMD-ARG FROM COMMAND-LINE
           
           IF CMD-ARG = "--apply-interest"
               PERFORM INTEREST-LOOP
           ELSE
               PERFORM READ-INPUT
               PERFORM NORMAL-BANKING
           END-IF
           STOP RUN.

       INTEREST-LOOP.
           DISPLAY "Interest daemon started (1% every 23 seconds)"
           
           PERFORM UNTIL 1 = 0
               IF FIRST-RUN = "Y"
                   DISPLAY "Waiting 23 seconds before first calculation..."
                   MOVE "N" TO FIRST-RUN
               ELSE
                   DISPLAY "Sleeping 23 seconds..."
               END-IF
               
               CALL "SYSTEM" USING "sleep 23"
               
               ACCEPT WS-DATE FROM DATE YYYYMMDD
               ACCEPT WS-TIME FROM TIME
               DISPLAY " "
               DISPLAY "=== " WS-DATE " " WS-TIME " APPLYING INTEREST ==="
               
               CALL "SYSTEM" USING "sleep 1"
               PERFORM INTEREST-CALCULATION
               
               CALL "SYSTEM" USING "cat accounts.txt"
               DISPLAY "---"
           END-PERFORM.

       READ-INPUT.
           OPEN INPUT IN-FILE
           READ IN-FILE AT END
               DISPLAY "NO INPUT"
               STOP RUN
           END-READ
           CLOSE IN-FILE

           MOVE IN-RECORD(1:6) TO IN-ACCOUNT
           MOVE IN-RECORD(7:3) TO IN-ACTION
           MOVE FUNCTION NUMVAL(IN-RECORD(10:9)) TO IN-AMOUNT.

       INTEREST-CALCULATION.
           OPEN INPUT ACC-FILE
           OPEN OUTPUT TMP-FILE
           MOVE "N" TO EOF-FLAG
           
           PERFORM UNTIL EOF-FLAG = "Y"
               READ ACC-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       MOVE ACC-RECORD-RAW(1:6) TO ACC-ACCOUNT
                       MOVE FUNCTION NUMVAL(ACC-RECORD-RAW(10:9))
                           TO ACC-BALANCE
                       
                       DISPLAY "Account: " ACC-ACCOUNT 
                               " Balance: " ACC-BALANCE " RAI"
                       
                       IF ACC-BALANCE > 0
                           MULTIPLY ACC-BALANCE BY INTEREST-RATE
                               GIVING INTEREST-AMOUNT
                           ADD INTEREST-AMOUNT TO ACC-BALANCE
                           DISPLAY "Interest: +" INTEREST-AMOUNT 
                                   " New: " ACC-BALANCE " RAI"
                       END-IF
                       
                       MOVE ACC-ACCOUNT TO TMP-RECORD(1:6)
                       MOVE "BAL" TO TMP-RECORD(7:3)
                       MOVE ACC-BALANCE TO FORMATTED-AMOUNT
                       MOVE FORMATTED-AMOUNT TO TMP-RECORD(10:9)
                       WRITE TMP-RECORD
           END-PERFORM
           
           CLOSE ACC-FILE
           CLOSE TMP-FILE
           
           CALL "SYSTEM" USING "mv temp.txt accounts.txt"
           
           OPEN OUTPUT OUT-FILE
           MOVE "INTEREST APPLIED: 1% TO ALL ACCOUNTS" TO OUT-RECORD
           WRITE OUT-RECORD
           CLOSE OUT-FILE.

       NORMAL-BANKING.
           MOVE "N" TO MATCH-FOUND
           MOVE "N" TO UPDATED
           MOVE SPACES TO OUT-RECORD
           
           IF IN-ACTION = "NEW" AND IN-AMOUNT < 0
               MOVE "INVALID AMOUNT: CANNOT BE NEGATIVE" TO OUT-RECORD
               PERFORM FINALIZE
           ELSE
               PERFORM PROCESS-RECORDS
               IF MATCH-FOUND = "N"
                   IF IN-ACTION = "NEW"
                       PERFORM APPEND-ACCOUNT
                       IF IN-AMOUNT = 0
                           MOVE "ACCOUNT CREATED (0.00 RAI = IDR 0.00)" 
                               TO OUT-RECORD
                       ELSE
                           PERFORM CALCULATE-IDR-VALUE-FOR-NEW
                           MOVE IN-AMOUNT TO RAI-FORMATTED
                           STRING "ACCOUNT CREATED | BAL: " DELIMITED SIZE
                                  RAI-FORMATTED DELIMITED SIZE
                                  " RAI (IDR " DELIMITED SIZE
                                  IDR-FORMATTED DELIMITED SIZE
                                  ")" DELIMITED SIZE
                                  INTO OUT-RECORD
                       END-IF
                   ELSE
                       MOVE "ACCOUNT NOT FOUND" TO OUT-RECORD
                   END-IF
               END-IF
               PERFORM FINALIZE
           END-IF.

       PROCESS-RECORDS.
           OPEN INPUT ACC-FILE
           OPEN OUTPUT TMP-FILE
           MOVE "N" TO EOF-FLAG
           
           PERFORM UNTIL EOF-FLAG = "Y" OR MATCH-FOUND = "Y"
               READ ACC-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       MOVE ACC-RECORD-RAW(1:6) TO ACC-ACCOUNT
                       MOVE FUNCTION NUMVAL(ACC-RECORD-RAW(10:9))
                           TO ACC-BALANCE
                       IF ACC-ACCOUNT = IN-ACCOUNT
                           MOVE "Y" TO MATCH-FOUND
                           PERFORM APPLY-ACTION
                       ELSE
                           WRITE TMP-RECORD FROM ACC-RECORD-RAW
                       END-IF
               END-READ
           END-PERFORM
           
           PERFORM UNTIL EOF-FLAG = "Y"
               READ ACC-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       WRITE TMP-RECORD FROM ACC-RECORD-RAW
               END-READ
           END-PERFORM
           
           CLOSE ACC-FILE
           CLOSE TMP-FILE.

       APPLY-ACTION.
           MOVE ACC-BALANCE TO TMP-BALANCE
           MOVE "N" TO UPDATED
           
           EVALUATE IN-ACTION
               WHEN "DEP"
                   IF IN-AMOUNT <= 0
                       MOVE "INVALID AMOUNT: MUST BE POSITIVE NUMBER" 
                           TO OUT-RECORD
                   ELSE IF TMP-BALANCE + IN-AMOUNT > 999999.99
                       MOVE "DEPOSIT REJECTED: BALANCE WOULD EXCEED MAXIMUM" 
                           TO OUT-RECORD
                   ELSE
                       ADD IN-AMOUNT TO TMP-BALANCE
                       PERFORM CALCULATE-IDR-VALUE
                       MOVE TMP-BALANCE TO RAI-FORMATTED
                       STRING "DEPOSITED | BAL: " DELIMITED SIZE
                              RAI-FORMATTED DELIMITED SIZE
                              " RAI (IDR " DELIMITED SIZE
                              IDR-FORMATTED DELIMITED SIZE
                              ")" DELIMITED SIZE
                              INTO OUT-RECORD
                       MOVE "Y" TO UPDATED
                   END-IF
               WHEN "WDR"
                   IF IN-AMOUNT <= 0
                       MOVE "INVALID AMOUNT: MUST BE POSITIVE NUMBER" 
                           TO OUT-RECORD
                   ELSE IF IN-AMOUNT > TMP-BALANCE
                       MOVE "INSUFFICIENT BALANCE" TO OUT-RECORD
                   ELSE
                       SUBTRACT IN-AMOUNT FROM TMP-BALANCE
                       PERFORM CALCULATE-IDR-VALUE
                       MOVE TMP-BALANCE TO RAI-FORMATTED
                       STRING "WITHDREW | BAL: " DELIMITED SIZE
                              RAI-FORMATTED DELIMITED SIZE
                              " RAI (IDR " DELIMITED SIZE
                              IDR-FORMATTED DELIMITED SIZE
                              ")" DELIMITED SIZE
                              INTO OUT-RECORD
                       MOVE "Y" TO UPDATED
                   END-IF
               WHEN "BAL"
                   PERFORM CALCULATE-IDR-VALUE
                   MOVE SPACES TO OUT-RECORD
                   MOVE TMP-BALANCE TO RAI-FORMATTED
                   STRING "BALANCE: " DELIMITED SIZE
                          RAI-FORMATTED DELIMITED SIZE
                          " RAI | IDR " DELIMITED SIZE
                          IDR-FORMATTED DELIMITED SIZE
                          INTO OUT-RECORD
                   MOVE "Y" TO UPDATED
               WHEN OTHER
                   MOVE "UNKNOWN ACTION" TO OUT-RECORD
           END-EVALUATE

           IF UPDATED = "Y"
               MOVE IN-ACCOUNT TO TMP-RECORD(1:6)
               MOVE "BAL" TO TMP-RECORD(7:3)
               MOVE TMP-BALANCE TO FORMATTED-AMOUNT
               MOVE FORMATTED-AMOUNT TO TMP-RECORD(10:9)
               WRITE TMP-RECORD
           END-IF.

       APPEND-ACCOUNT.
           OPEN EXTEND ACC-FILE
           MOVE IN-ACCOUNT TO ACC-RECORD-RAW(1:6)
           MOVE "BAL" TO ACC-RECORD-RAW(7:3)
           MOVE IN-AMOUNT TO FORMATTED-AMOUNT
           MOVE FORMATTED-AMOUNT TO ACC-RECORD-RAW(10:9)

           WRITE ACC-RECORD-RAW
           CLOSE ACC-FILE.

       CALCULATE-IDR-VALUE.
           MULTIPLY TMP-BALANCE BY RAI-TO-IDR GIVING IDR-VALUE
           MOVE IDR-VALUE TO IDR-FORMATTED.

       CALCULATE-IDR-VALUE-FOR-NEW.
           MULTIPLY IN-AMOUNT BY RAI-TO-IDR GIVING IDR-VALUE
           MOVE IDR-VALUE TO IDR-FORMATTED.

       FINALIZE.
           IF UPDATED = "Y"
               CALL "SYSTEM" USING "mv temp.txt accounts.txt"
           END-IF
           
           OPEN OUTPUT OUT-FILE
           WRITE OUT-RECORD
           CLOSE OUT-FILE.