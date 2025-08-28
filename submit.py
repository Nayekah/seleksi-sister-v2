#!/usr/bin/env python3
# Seleksi Sister 2025
# Pretty messed-up code, but... it works? :3

# libs
import os
import time
import pyotp
import base64
import hashlib
import requests

# module
from pqcrypto.sign.sphincs_shake_256s_simple import generate_keypair as gk, sign


# server
r = "http://104.214.186.131:8000/"


# DON'T TOUCH THIS
PRIVATE_KEY = "Keys/priv.key"
PUBLIC_KEY = "Keys/pub.key"


# MACROS
usn = "13523090"
pwd = "ghana"
totp_secret = "kimminjeong" # my istri
github = "https://github.com/Nayekah/seleksi-sister-v2"


# Remote Execution Functions and some other functions
def pow() -> int:
    pref = f"{usn}:if:{pwd}"
    diff = 5
    target = '0' * diff

    print("\nSearching for Proof of Work nonce...")
    nonce = 0

    while True:
        test_str = f"{pref}:{nonce}"
        hash_result = hashlib.sha256(test_str.encode()).hexdigest()

        if hash_result.startswith(target):
            print(f"\nNonce found!")
            print(f"Nonce: {nonce}")
            return nonce
        
        nonce += 1

def maths() -> tuple:
    print("\nSolving math challenge :3 ...")
    conn = f"{r}challenge-math"

    try:
        start = time.time()

        response = requests.get(conn, timeout=0.5)
        response.raise_for_status()
        data = response.json()

        question = data.get("question")
        prob = question.split('||')[0]
        ans = eval(prob)
        end = time.time()

        print(f"Question: {prob}")
        print(f"Answer  : {ans}")
        print(f"Time    : {end - start:.2f} seconds")
        
        return question, ans
    
    except requests.exceptions.RequestException as e:
        print(f"Error contacting math server: {e}")
    except Exception as e:
        print(f"Unexpected error while solving math: {e}")

    return None, None

def totp() -> str:
    print("\nGenerating TOTP code...")
    secret = totp_secret

    secret_b32 = base64.b32encode(secret.encode('utf-8')).decode('utf-8')
    gen = pyotp.TOTP(secret_b32)
    totp_code = gen.now()
    print(f"TOTP: {totp_code}")

    return totp_code

def genkeys(priv_path: str, pub_path: str) -> str:
    print("\nGenerating SPHINCS+ keys...")
    pubkey, privkey = gk()
    
    os.makedirs(os.path.dirname(priv_path), exist_ok=True)
    privkey_b64 = base64.b64encode(privkey).decode('utf-8')

    with open(priv_path, "w") as f:
        f.write(privkey_b64)
    print(f"Private key successfully saved to {priv_path}")

    pubkey_b64 = base64.b64encode(pubkey).decode('utf-8')

    with open(pub_path, "w") as f:
        f.write(pubkey_b64)
    print(f"Public key successfully saved to {pub_path}")

    return pubkey_b64

def signup(nonce: int, pubkey: str) -> None:
    print("\nActivating account...")
    
    conn = f"{r}activate-account"
    headers = {'Accept': 'application/json', 'Content-Type': 'application/json'}
    payload = {
        "username": usn,
        "password": pwd,
        "nonce": int(nonce),
        "public_key": pubkey.strip()
    }

    print(f"Sending payload: {payload}")
    try:
        response = requests.post(conn, headers=headers, json=payload, timeout=1)
        response.raise_for_status()
        print("\nActivation successful!")
        print("Response:", response.json())

    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error {response.status_code}: {response.text}")
    except requests.exceptions.RequestException as e:
        print(f"Connection error during activation: {e}")

def update_pubkey() -> None:
    print("\nUpdating public key...")

    question, ans = maths()

    if question is None:
        print("Failed to get math challenge. Update process cancelled.")
        return

    totp_code = totp()
    if not totp_code:
        print("Failed to generate TOTP. Update process cancelled.")
        return

    new_pubkey = genkeys(PRIVATE_KEY, PUBLIC_KEY)

    if not new_pubkey:
        print("Failed to generate new keys. Update process cancelled.")
        return

    conn = f"{r}update-public-key"
    payload = {
        "username": usn,
        "password": pwd,
        "totp_code": totp_code,
        "math_question": question,
        "math_answer": int(ans),
        "new_public_key": new_pubkey.strip()
    }

    print(f"\nSending key update payload to {conn}: {payload}")
    try:
        response = requests.post(conn, json=payload, timeout=1)
        response.raise_for_status()
        print("\nPublic key update successful!")
        print("Response:", response.json())

    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error {response.status_code}: {response.text}")
    except requests.exceptions.RequestException as e:
        print(f"Connection error during key update: {e}")

def submit_a(question: str, ans: int, docpath: str)  -> None:
    print("\nStarting Stage A submission process...")
    conn = f"{r}stage-a/submit"
    
    totp_code = totp()
    if not totp_code:
        print("Failed to generate TOTP. Submission cancelled.")
        return

    try:
        print(f"\nReading PDF '{docpath}' and private key...")

        if not os.path.exists(docpath):
            print(f"Error: PDF document not found at '{docpath}'.")
            return
        
        with open(docpath, "rb") as f:
            content = f.read()

        if not os.path.exists(PRIVATE_KEY):
            print(f"Error: Private key not found at '{PRIVATE_KEY}'.")
            return

        with open(PRIVATE_KEY, "r") as f:
            private_key_b64 = f.read()
        private_key = base64.b64decode(private_key_b64)

        signature = sign(private_key, content)
        
        signature_b64 = base64.b64encode(signature).decode()
        print("Document signed successfully.")

    except Exception as e:
        print(f"Unexpected error during file reading or signing: {e}")
        return

    payload_data = {
        "username": usn,
        "totp_code": totp_code,
        "math_question": question,
        "math_answer": int(ans),
        "signature": signature_b64,
        "tahap": 2
    }

    payload_files = {
        "file": (os.path.basename(docpath), content, "application/pdf")
    }

    print(f"\nPayload data to be sent: {payload_data}")
    print(f"File to be sent: {os.path.basename(docpath)}")
    
    try:
        response = requests.post(conn, data=payload_data, files=payload_files, timeout=30)
        response.raise_for_status()
        print("\nStage A submission successful!")
        print("Response:", response.json())

    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error {response.status_code}: {response.text}")
    except requests.exceptions.RequestException as e:
        print(f"Connection error during Stage A submission: {e}")

def submit_b(question: str, ans: int)  -> None:
    print("\nStarting Stage B submission process...")
    conn = f"{r}stage-b/submit?username={usn}"
    
    totp_code = totp()
    if not totp_code:
        print("Failed to generate TOTP. Submission cancelled.")
        return

    try:
        print(f"\nReading private key...")

        if not os.path.exists(PRIVATE_KEY):
            print(f"Error: Private key not found at '{PRIVATE_KEY}'.")
            return

        with open(PRIVATE_KEY, "r") as f:
            private_key_b64 = f.read()
        private_key = base64.b64decode(private_key_b64)

        signature = sign(private_key, github.encode('utf-8'))

        signature_b64 = base64.b64encode(signature).decode()
        print("Signed successfully.")

    except Exception as e:
        print(f"Unexpected error during signing: {e}")
        return
    
    headers = {'Accept': 'application/json', 'Content-Type': 'application/json'}
    payload = {
        "github_url": github,
        "totp_code": totp_code,
        "math_question": question,
        "math_answer": int(ans),
        "signature": signature_b64,
        "tahap": 2
    }

    print(f"\nPayload data to be sent: {payload}")
    
    try:
        response = requests.post(conn, headers=headers, json=payload, timeout=30)
        response.raise_for_status()
        print("\nStage B submission successful!")
        print("Response:", response.json())

    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error {response.status_code}: {response.text}")
    except requests.exceptions.RequestException as e:
        print(f"Connection error during Stage A submission: {e}")

def check_submissions() -> None:
    print("\nChecking submission status...")
    conn = f"{r}user/{usn}/submissions?totp_code={totp()}"

    try:
        response = requests.get(conn, timeout=0.5)
        response.raise_for_status()
        data = response.json()

        print("\nSubmission status:")

        print("\nStage A Submissions (Top 3):")
        for sub in data.get("stage_a_submissions", []):
            print(f"  - Filename: {sub.get('filename')}")
            print(f"    Tahap: {sub.get('tahap')}")
            print(f"    Submitted at: {sub.get('submitted_at')}")
            print(f"    Verified: {'Yes' if sub.get('signature_verified') else 'No'}")

        print("\nStage B Submissions (Top 3):")
        for sub in data.get("stage_b_submissions", [])[:3]:
            print(f"  - GitHub URL: {sub.get('github_url')}")
            print(f"    Tahap: {sub.get('tahap')}")
            print(f"    Submitted at: {sub.get('submitted_at')}")
            print(f"    Verified: {'Yes' if sub.get('signature_verified') else 'No'}")

        print("\n")

    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error {response.status_code}: {response.text}")
    except requests.exceptions.RequestException as e:
        print(f"Connection error during activation: {e}")


# Miscellaneous functions
def acc() -> None:
    print("\nChecking list of accounts...")

    conn = f"{r}accounts"

    try:
        response = requests.get(conn, timeout=0.5)
        response.raise_for_status()
        data = response.json()

        print("\nActive account list:")
        active = [
            acc for acc in data.get("accounts", [])
            if acc.get("status") == "active"
        ]

        if not active:
            print("  No active accounts found.")
            return

        for acc in active:
            print(f"  - Username   : {acc.get('username')}")
            print(f"    Status     : {acc.get('status')}")
            print(f"    Created at : {acc.get('created_at')}")
            print(f"    Updated at : {acc.get('updated_at')}")

    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error {response.status_code}: {response.text}")
    except requests.exceptions.RequestException as e:
        print(f"Connection error during activation: {e}")

def healthcheck() -> None:
    print("\nPerforming health check...")
    conn = f"{r}health"

    try:
        response = requests.get(conn, timeout=0.5)
        response.raise_for_status()
        data = response.json()

        print("\nHealth check successful!")
        print("Response:", data)
        print("\n")

    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error {response.status_code}: {response.text}")
    except requests.exceptions.RequestException as e:
        print(f"Connection error during health check: {e}")

def stats() -> None:
    print("\nFetching server statistics...")
    conn = f"{r}stats"

    try:
        response = requests.get(conn, timeout=0.5)
        response.raise_for_status()
        data = response.json()

        print("\nServer Statistics:")
        print("Response:", data)
        print("\n")

    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error {response.status_code}: {response.text}")
    except requests.exceptions.RequestException as e:
        print(f"Connection error during stats fetch: {e}")




# Main
if __name__ == "__main__":
    while True:
        print("\n1. Activate Account (Signup)")
        print("2. Submit Stage A")
        print("3. Submit Stage B")
        print("4. Update Public Key (restricted)")
        print("5. Check Submissions")
        print("6. View Accounts")
        print("7. Health Check")
        print("8. Server Statistics")
        print("\npress 'q' to exit\n")
        choice = input(">> ").strip().lower()

        if choice == '1':
            nonce = pow()
            pubkey = genkeys(PRIVATE_KEY, PUBLIC_KEY)

            if nonce is not None and pubkey is not None:
                signup(nonce, pubkey)
        
        elif choice == '2':
            paths = input("Enter the full path to the PDF document: ").strip()

            if not paths:
                print("PDF path cannot be empty.")
                continue
                
            question, ans = maths()

            if question is not None:
                submit_a(question, ans, paths)
            else:
                print("Cannot proceed because failed to get math problem.")

        elif choice == '3':
            question, ans = maths()

            if question is not None:
                submit_b(question, ans)
            else:
                print("Cannot proceed because failed to get math problem.")
        
        elif choice == '4':
            print("Sorry, disabled for now.")

        elif choice == '5':
            check_submissions()

        elif choice == '6':
            acc()

        elif choice == '7':
            healthcheck()

        elif choice == '8':
            stats()

        elif choice == 'q':
            print("Exiting...")
            break
        
        else:
            print("Invalid choice.")