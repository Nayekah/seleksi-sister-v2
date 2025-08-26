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

- **Dep/WDR wrong logic (reversed)**  
     ```bash
    WHEN "DEP"
        SUBTRACT IN-AMOUNT FROM TMP-BALANCE
    WHEN "WDR"  
        ADD IN-AMOUNT TO TMP-BALANCE
   ```
<br/>
