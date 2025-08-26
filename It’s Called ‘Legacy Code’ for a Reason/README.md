# It’s Called ‘Legacy Code’ for a Reason

> Seleksi Asisten Laboratorium Sistem Paralel dan Terdistribusi 2025
<p align="center">
    <img src="https://github.com/user-attachments/assets/acce5581-e534-4ae6-847f-974deb7b1967">
</p>
    <h3 align="center">C for Cobol</h3>

---

## About <a name="about"></a>

<p align="justify">This project implements a banking app run in cobol language</p>

---

## Analysis on the original code <a name="algorithms"></a>

- **Dep/WDR wrong logic (reversed)**  
     ```bash
    WHEN "DEP"
        SUBTRACT IN-AMOUNT FROM TMP-BALANCE
    WHEN "WDR"  
        ADD IN-AMOUNT TO TMP-BALANCE
   ```
<br/>

- **Field size mismatch**  
     ```bash
    FD ACC-FILE.
    01 ACC-RECORD-RAW        PIC X(15). // only 15 char

    MOVE FUNCTION NUMVAL(ACC-RECORD-RAW(10:9)) // but it needs 18
   ```
<br/>

- **Multi account vulnerabilities**  
     ```bash
    PERFORM UNTIL MATCH-FOUND = "Y"
        // No algorithm to copy rest of the new accounts created
    EXIT PERFORM
   ```
<br/>

- **No cobol compiler in Dockerfile**  
     ```bash
    RUN apt-get update && apt-get install -y --no-install-recommends \
        gcc \
        && rm -rf /var/lib/apt/lists/*
   ```
<br/>

All of that problem had been patched in the patched folder.

---

## Bonus <a name="algorithms"></a>

- **Rai stone to IDR conversion**
    calculation: 1 rai stone ≈ IDR 120 million

    Data structure for the conversion:

     ```bash
    77 RAI-TO-IDR            PIC 9(9) VALUE 120000000.
    77 IDR-VALUE             PIC 9(15)V99 COMP-3.
    77 IDR-FORMATTED         PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
   ```

     Calculations and overflow handling:
  ```bash
    CALCULATE-IDR-VALUE.
        MOVE "N" TO IDR-OVERFLOW
        
        IF TMP-BALANCE > MAX-SAFE-RAI
            MOVE "Y" TO IDR-OVERFLOW
        ELSE
            COMPUTE IDR-VALUE = TMP-BALANCE * RAI-TO-IDR
            
            IF IDR-VALUE > 9999999999999.99
                MOVE "Y" TO IDR-OVERFLOW
            ELSE
                MOVE IDR-VALUE TO IDR-FORMATTED
            END-IF
        END-IF.
  ```

  Banking Integrations:
    ```bash
    STRING "DEPOSITED | BAL: " DELIMITED SIZE
           RAI-FORMATTED DELIMITED SIZE
           " RAI (IDR " DELIMITED SIZE
           IDR-FORMATTED DELIMITED SIZE
           ")" DELIMITED SIZE
           INTO OUT-RECORD
  ```

<br/>

## Interest calculations (1% per 23 seconds) <a name="algorithms"></a>

- **Command line parsing (--apply interest)**
     In main.cob:
  
     ```bash
    MAIN.
        ACCEPT CMD-ARG FROM COMMAND-LINE
        
        IF CMD-ARG = "--apply-interest"
            PERFORM INTEREST-LOOP
        ELSE
            PERFORM READ-INPUT
            PERFORM NORMAL-BANKING
        END-IF
        STOP RUN.
   ```
     
     In Dockerfile:
     ```bash
    MAIN.
        ACCEPT CMD-ARG FROM COMMAND-LINE
        
        IF CMD-ARG = "--apply-interest"
            PERFORM INTEREST-LOOP
        ELSE
            PERFORM READ-INPUT
            PERFORM NORMAL-BANKING
        END-IF
        STOP RUN.
   ```

<br/>

- **Calculation loops**
     Main interest:
  
     ```bash
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
   ```
     
     Interest calculation logic:
     ```bash
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
   ```

<br/>

- **Reverse proxy and HTTPS implementations**: Will be explained below

---

## How to Run

### Requirements
- Python & Package installer for python (pip)
- Docker
- gnucobol (cobol compiler)
- VPS (Virtual Private Server, if you want to deploy it)
- Nginx Proxy Manager (NPM)

<br/>

### Running the Application
In your VPS, do:

1. Clone this repo
   ```bash
   https://github.com/Nayekah/seleksi-sister-v2.git
   ```

2. Navigate to the cloned repository.
   ```bash
   cd seleksi-sister-v2
   cd "It’s Called ‘Legacy Code’ for a Reason"/
   ```
   
3. Do
   ```dotenv
   docker-compose up --build -d
   ```
   
4. Add A record of your public ip to the registrar

5. Access http://[YOUR_PUBLIC_IP]:81, and confugure the proxy
    login:
    - username: admin@example.com
    - password: changeme
  
   then, change your credentials.   

5. In the NPM -> Details, do:
    - Add proxy host, fill the domain names, http scheme, forward hostname (172.17.0.1), forward port 8000. Then, turn on the Cache assets, Block Common Exploits, and Websocket Supports
    - Save config
  
6. Setup SSL, NPM -> SSL:
    - Click "Request a new SSL Certificate"
    - Save config
  
7. Add nginx config, NPM -> Advanced:
    - Insert this code:
    ```bash
    # Replace localhost URLs in HTML response
    sub_filter 'http://127.0.0.1:8000' 'https://seleksiasistenlabpro.xyz';
    sub_filter_once off;
    sub_filter_types text/html text/css application/javascript;
    
    # Ensure proper headers
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
    proxy_set_header Accept-Encoding "";
   ```
    
8. Make the image and run with:
    ```bash
    # Without interest
    docker stop cobol-banking
    docker build -t cobol-app .
    docker run --rm -p 8000:8000 --name cobol-banking cobol-app

    # With interest
    docker stop cobol-banking
    docker build -t cobol-app .
    docker run --rm -p 8000:8000 --name cobol-banking cobol-app --apply-interest
   ```
    
<br/>


> [!IMPORTANT]
> You can access the owner's web server on https://seleksiasistenlabpro.xyz
