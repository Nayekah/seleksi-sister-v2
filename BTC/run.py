import subprocess
import time
import requests
import json
import threading
import signal
import sys

class NetworkDemo:
    def __init__(self):
        self.processes = []
        self.ports = [5000, 5001, 5002]
        self.base_url = "http://127.0.0.1"
    
    def start_nodes(self):
        print("Starting Bitcoin network nodes...")
        
        for port in self.ports:
            cmd = [sys.executable, "bitcoin.py", str(port)]
            process = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            self.processes.append(process)
            print(f"Node started on port {port}")
            time.sleep(2)
        
        print("All nodes started. Waiting for initialization...")
        time.sleep(3)
        
        # Wait for nodes to be ready
        return self.wait_for_nodes()
    
    def wait_for_nodes(self):
        print("Waiting for nodes to be ready...")
        max_attempts = 30
        
        for port in self.ports:
            attempts = 0
            while attempts < max_attempts:
                try:
                    response = requests.get(f"{self.base_url}:{port}/chain", timeout=2)
                    if response.status_code == 200:
                        print(f"✓ Node {port} is ready")
                        break
                except requests.exceptions.RequestException:
                    pass
                
                attempts += 1
                time.sleep(1)
                
                if attempts == max_attempts:
                    print(f"✗ Node {port} failed to start")
                    return False
        
        print("All nodes are ready!")
        return True
    
    def make_request(self, method, url, json_data=None, timeout=5, retries=3):
        """Make HTTP request with retry logic"""
        for attempt in range(retries):
            try:
                if method.upper() == 'GET':
                    response = requests.get(url, timeout=timeout)
                elif method.upper() == 'POST':
                    response = requests.post(url, json=json_data, timeout=timeout)
                else:
                    raise ValueError(f"Unsupported method: {method}")
                
                return response
                
            except requests.exceptions.RequestException as e:
                if attempt == retries - 1:  # Last attempt
                    print(f"Request failed after {retries} attempts: {url} - {e}")
                    return None
                else:
                    print(f"Request attempt {attempt + 1} failed, retrying... ({e})")
                    time.sleep(1)
        
        return None
    
    def register_peers(self):
        print("\nRegistering peers...")
        
        # Register peers for each node
        for i, port in enumerate(self.ports):
            peers = [f"127.0.0.1:{p}" for p in self.ports if p != port]
            
            response = self.make_request(
                'POST', 
                f"{self.base_url}:{port}/nodes/register",
                json_data={"nodes": peers}
            )
            
            if response and response.status_code == 200:
                print(f"✓ Node {port}: Registered {len(peers)} peers")
            else:
                print(f"✗ Node {port}: Failed to register peers")
    
    def add_transactions(self):
        print("\nAdding transactions...")
        
        transactions = [
            {"sender": "Alice", "recipient": "Bob", "amount": 10.5},
            {"sender": "Bob", "recipient": "Charlie", "amount": 3.2},
            {"sender": "Charlie", "recipient": "Diana", "amount": 1.7},
            {"sender": "Alice", "recipient": "Diana", "amount": 5.0}
        ]
        
        # Add all transactions to the first node to ensure consistency
        port = self.ports[0]
        for tx in transactions:
            response = self.make_request(
                'POST',
                f"{self.base_url}:{port}/transaction",
                json_data=tx
            )
            
            if response and response.status_code == 201:
                print(f"✓ Transaction added: {tx['sender']} -> {tx['recipient']} ({tx['amount']})")
            else:
                print(f"✗ Failed to add transaction: {tx}")
            
            time.sleep(0.5)  # Small delay between transactions
        
        # Wait for transaction propagation
        time.sleep(2)
        print("Waiting for transaction propagation...")
        self.check_transaction_pools()
    
    def check_transaction_pools(self):
        print("Checking transaction pools...")
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/pool")
            
            if response and response.status_code == 200:
                data = response.json()
                print(f"Node {port}: {data['count']} transactions in pool")
            else:
                print(f"Node {port}: Failed to get transaction pool")
        print()
    
    def mine_block(self, port):
        print(f"\nMining block on node {port}...")
        
        response = self.make_request('GET', f"{self.base_url}:{port}/mine", timeout=30)
        
        if response and response.status_code == 200:
            data = response.json()
            print(f"✓ Block mined successfully on node {port}")
            print(f"  Block hash: {data['block']['hash']}")
            print(f"  Block index: {data['block']['index']}")
            print(f"  Transactions mined: {data.get('transactions_mined', 'N/A')}")
            return data['block']
        else:
            error_msg = response.json().get('message', 'Unknown error') if response else 'Connection failed'
            print(f"✗ Mining failed on node {port}: {error_msg}")
            return None
    
    def check_chains(self):
        print("\nChecking blockchain status on all nodes...")
        
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/chain")
            
            if response and response.status_code == 200:
                data = response.json()
                print(f"Node {port}: Chain length = {data['length']}")
                
                for block in data['chain']:
                    if block['index'] > 0:  # Skip genesis block
                        tx_count = len(block['data'])
                        print(f"  Block {block['index']}: {tx_count} transactions")
                        print(f"    Hash: {block['hash'][:16]}...")
                        print(f"    Merkle Root: {block.get('merkle_root', 'N/A')[:16]}...")
                        # Show transaction details for better debugging
                        if tx_count > 0:
                            user_txs = [tx for tx in block['data'] if tx.get('sender') is not None]
                            reward_txs = [tx for tx in block['data'] if tx.get('sender') is None]
                            print(f"    User transactions: {len(user_txs)}, Mining rewards: {len(reward_txs)}")
            else:
                print(f"Node {port}: Failed to get chain")
        print()
    
    def check_balances(self):
        print("Checking balances...")
        
        # Get all miner addresses from the blockchain
        all_addresses = set(["Alice", "Bob", "Charlie", "Diana"])
        
        # Get miner addresses from the blockchain
        port = self.ports[0]
        response = self.make_request('GET', f"{self.base_url}:{port}/chain")
        
        if response and response.status_code == 200:
            data = response.json()
            for block in data['chain']:
                for tx in block['data']:
                    if tx.get('recipient') and tx.get('recipient').startswith('node_'):
                        all_addresses.add(tx['recipient'])
        
        # Check balance on first node
        for address in sorted(all_addresses):
            response = self.make_request('GET', f"{self.base_url}:{port}/balance/{address}")
            
            if response and response.status_code == 200:
                data = response.json()
                print(f"{address}: {data['balance']} BTC")
            else:
                print(f"Failed to get balance for {address}")
        print()
    
    def sync_nodes(self):
        print("Synchronizing nodes...")
        
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/nodes/resolve", timeout=10)
            
            if response and response.status_code == 200:
                data = response.json()
                print(f"Node {port}: {data['message']}")
            else:
                print(f"Node {port}: Sync failed")
        print()
    
    def run_demo(self):
        try:
            self.start_nodes()
            self.register_peers()
            
            print("\n" + "="*50)
            print("BITCOIN NETWORK DEMO")
            print("="*50)
            
            # Initial state
            print("\n1. Initial blockchain state:")
            self.check_chains()
            
            # Add transactions
            print("2. Adding transactions to the network:")
            self.add_transactions()
            
            # Check transaction pools before mining
            print("3. Checking transaction pools before mining:")
            self.check_transaction_pools()
            
            # Mine first block
            print("4. Mining first block:")
            mined_block = self.mine_block(self.ports[0])
            
            if mined_block:
                time.sleep(3)
                print("5. Checking synchronization after mining:")
                self.check_chains()
                
                print("6. Checking balances:")
                self.check_balances()
                
                # Add more transactions
                print("7. Adding more transactions:")
                self.add_transactions()
                
                # Mine on different node
                print("8. Mining on different node:")
                self.mine_block(self.ports[1])
                
                time.sleep(3)
                print("9. Final synchronization check:")
                self.sync_nodes()
                self.check_chains()
                
                print("10. Final balances:")
                self.check_balances()
            
            print("\n" + "="*50)
            print("DEMO COMPLETED")
            print("Nodes are still running. Press Ctrl+C to stop.")
            print("="*50)
            
            # Keep the demo running
            try:
                while True:
                    time.sleep(10)
                    print("Network is running... (Ctrl+C to stop)")
            except KeyboardInterrupt:
                pass
                
        except KeyboardInterrupt:
            pass
        finally:
            self.cleanup()
    
    def run_interactive(self):
        try:
            self.start_nodes()
            time.sleep(2)
            self.register_peers()
            
            print("\n" + "="*50)
            print("BITCOIN NETWORK - INTERACTIVE MODE")
            print("="*50)
            print("Nodes are running on ports:", self.ports)
            print("Available commands:")
            print("  1 - Add transaction")
            print("  2 - Mine block")
            print("  3 - Check chains")
            print("  4 - Check balances")
            print("  5 - Check transaction pools")
            print("  6 - Sync nodes")
            print("  7 - Show manual commands")
            print("  q - Quit")
            print("="*50)
            
            while True:
                try:
                    command = input("\nEnter command (1-7, q): ").strip().lower()
                    
                    if command == 'q':
                        break
                    elif command == '1':
                        self.interactive_add_transaction()
                    elif command == '2':
                        self.interactive_mine()
                    elif command == '3':
                        self.check_chains()
                    elif command == '4':
                        self.check_balances()
                    elif command == '5':
                        self.check_transaction_pools()
                    elif command == '6':
                        self.sync_nodes()
                    elif command == '7':
                        self.show_manual_commands()
                    else:
                        print("Invalid command. Use 1-7 or 'q' to quit.")
                        
                except KeyboardInterrupt:
                    break
                    
        except KeyboardInterrupt:
            pass
        finally:
            self.cleanup()
    
    def interactive_add_transaction(self):
        print("\nAdd Transaction:")
        try:
            sender = input("Enter sender: ").strip()
            recipient = input("Enter recipient: ").strip()
            amount = float(input("Enter amount: ").strip())
            
            port = int(input(f"Choose node port {self.ports} (default {self.ports[0]}): ").strip() or self.ports[0])
            if port not in self.ports:
                port = self.ports[0]
                
            tx = {"sender": sender, "recipient": recipient, "amount": amount}
            
            response = self.make_request(
                'POST',
                f"{self.base_url}:{port}/transaction",
                json_data=tx
            )
            
            if response and response.status_code == 201:
                print(f"✓ Transaction added: {sender} -> {recipient} ({amount} BTC)")
            else:
                error_msg = response.json().get('message', 'Unknown error') if response else 'Connection failed'
                print(f"✗ Failed to add transaction: {error_msg}")
                
        except ValueError as e:
            print(f"Error: Invalid input - {e}")
        except Exception as e:
            print(f"Error: {e}")
    
    def interactive_mine(self):
        print(f"\nChoose mining node from {self.ports}:")
        try:
            port = int(input(f"Enter port (default {self.ports[0]}): ").strip() or self.ports[0])
            if port not in self.ports:
                port = self.ports[0]
                
            print(f"Mining on node {port}...")
            self.mine_block(port)
            
        except ValueError:
            print("Invalid port number")
    
    def show_manual_commands(self):
        print("\nManual cURL Commands:")
        print("="*40)
        for port in self.ports:
            print(f"\nNode {port} commands:")
            print(f"Add transaction: curl -X POST http://127.0.0.1:{port}/transaction -H 'Content-Type: application/json' -d '{{\"sender\": \"Alice\", \"recipient\": \"Bob\", \"amount\": 10}}'")
            print(f"Mine block:     curl http://127.0.0.1:{port}/mine")
            print(f"Check chain:    curl http://127.0.0.1:{port}/chain")
            print(f"Check balance:  curl http://127.0.0.1:{port}/balance/Alice")
            print(f"Check pool:     curl http://127.0.0.1:{port}/pool")
            print(f"Sync nodes:     curl http://127.0.0.1:{port}/nodes/resolve")
    
    def cleanup(self):
        print("\nShutting down nodes...")
        for process in self.processes:
            try:
                process.terminate()
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
        print("All nodes stopped.")

def show_usage():
    print("Bitcoin Network Usage:")
    print("=" * 30)
    print("python run.py demo       - Run automatic demo")
    print("python run.py           - Run interactive mode")
    print("python run.py --help    - Show this help")
    print("\nInteractive mode allows you to:")
    print("- Add transactions manually")
    print("- Mine blocks on specific nodes")
    print("- Check blockchain status")
    print("- Monitor balances and transaction pools")

if __name__ == "__main__":
    demo = NetworkDemo()
    
    # Handle Ctrl+C gracefully
    def signal_handler(sig, frame):
        print("\nReceived interrupt signal...")
        demo.cleanup()
        sys.exit(0)
    
    signal.signal(signal.SIGINT, signal_handler)
    
    # Parse command line arguments
    if len(sys.argv) > 1:
        if sys.argv[1] in ["--help", "-h", "help"]:
            show_usage()
            sys.exit(0)
        elif sys.argv[1] == "demo":
            print("Running automatic demo mode...")
            demo.run_demo()
        else:
            print(f"Unknown parameter: {sys.argv[1]}")
            show_usage()
            sys.exit(1)
    else:
        print("Running interactive mode...")
        demo.run_interactive()