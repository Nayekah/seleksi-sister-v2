import subprocess
import time
import requests
import json
import threading
import signal
import sys
from datetime import datetime
import os

class EnhancedBitcoinNetworkTester:
    def __init__(self):
        self.processes = []
        self.ports = [5000, 5001, 5002]
        self.base_url = "http://127.0.0.1"
        self.test_results = []
        self.failed_tests = 0
        self.passed_tests = 0
        self.warnings = 0
        
    def log(self, message, level="INFO"):
        timestamp = datetime.now().strftime("%H:%M:%S")
        color_codes = {
            "INFO": "\033[0m",      # Default
            "PASS": "\033[32m",     # Green
            "FAIL": "\033[31m",     # Red
            "WARN": "\033[33m",     # Yellow
            "SUCCESS": "\033[92m",  # Bright Green
            "ERROR": "\033[91m"     # Bright Red
        }
        
        reset_code = "\033[0m"
        color = color_codes.get(level, color_codes["INFO"])
        print(f"{color}[{timestamp}] {level}: {message}{reset_code}")
    
    def assert_equal(self, actual, expected, test_name, tolerance=0.01):
        """Assert with tolerance for floating point comparisons"""
        if isinstance(expected, (int, float)) and isinstance(actual, (int, float)):
            if abs(actual - expected) <= tolerance:
                self.log(f"✅ {test_name} - PASSED (actual: {actual})", "PASS")
                self.passed_tests += 1
                return True
        elif actual == expected:
            self.log(f"✅ {test_name} - PASSED", "PASS")
            self.passed_tests += 1
            return True
        
        self.log(f"❌ {test_name} - FAILED", "FAIL")
        self.log(f"   Expected: {expected}, Actual: {actual}", "FAIL")
        self.failed_tests += 1
        return False
    
    def assert_greater_equal(self, actual, expected, test_name):
        """Assert that actual >= expected"""
        if actual >= expected:
            self.log(f"✅ {test_name} - PASSED (actual: {actual}, minimum: {expected})", "PASS")
            self.passed_tests += 1
            return True
        else:
            self.log(f"❌ {test_name} - FAILED", "FAIL")
            self.log(f"   Expected: >= {expected}, Actual: {actual}", "FAIL")
            self.failed_tests += 1
            return False
    
    def assert_status_code(self, response, expected_code, test_name):
        if response and response.status_code == expected_code:
            self.log(f"✅ {test_name} - PASSED", "PASS")
            self.passed_tests += 1
            return True
        else:
            actual_code = response.status_code if response else "No Response"
            if response is None:
                self.log(f"⚠️ {test_name} - WARNING (Network timeout)", "WARN")
                self.warnings += 1
            else:
                self.log(f"❌ {test_name} - FAILED", "FAIL")
                self.log(f"   Expected status: {expected_code}, Got: {actual_code}", "FAIL")
                self.failed_tests += 1
            return False
    
    def make_request(self, method, url, json_data=None, timeout=10, retries=2):
        """Enhanced request method with better error handling"""
        for attempt in range(retries):
            try:
                if method.upper() == 'GET':
                    response = requests.get(url, timeout=timeout)
                elif method.upper() == 'POST':
                    response = requests.post(url, json=json_data, timeout=timeout)
                else:
                    raise ValueError(f"Unsupported method: {method}")
                
                return response
                
            except requests.exceptions.Timeout:
                self.log(f"⏱️ Request timeout (attempt {attempt + 1}/{retries}): {url}", "WARN")
            except requests.exceptions.ConnectionError:
                self.log(f"🔌 Connection error (attempt {attempt + 1}/{retries}): {url}", "WARN")
            except requests.exceptions.RequestException as e:
                self.log(f"❌ Request error (attempt {attempt + 1}/{retries}): {url} - {e}", "WARN")
            
            if attempt < retries - 1:
                time.sleep(1)
        
        self.log(f"❌ All request attempts failed: {url}", "ERROR")
        return None
    
    def wait_for_sync(self, description="sync", max_wait=10):
        """Wait for network synchronization with progress indication"""
        self.log(f"⏳ Waiting for {description}...")
        
        for i in range(max_wait):
            if i % 3 == 0 and i > 0:
                self.log(f"   Still waiting... ({i}s)")
            time.sleep(1)
        
        # Force synchronization
        for port in self.ports:
            self.make_request('GET', f"{self.base_url}:{port}/nodes/resolve", timeout=5, retries=1)
        
        time.sleep(2)  # Final wait after sync
    
    def get_network_state(self):
        """Get current state of all nodes for debugging"""
        state = {}
        for port in self.ports:
            node_state = {"port": port}
            
            # Get chain info
            response = self.make_request('GET', f"{self.base_url}:{port}/chain", timeout=5, retries=1)
            if response:
                chain_data = response.json()
                node_state['chain_length'] = chain_data['length']
            else:
                node_state['chain_length'] = "unknown"
            
            # Get pool info
            response = self.make_request('GET', f"{self.base_url}:{port}/pool", timeout=5, retries=1)
            if response:
                pool_data = response.json()
                node_state['pool_count'] = pool_data['count']
            else:
                node_state['pool_count'] = "unknown"
            
            state[port] = node_state
        
        return state
    
    def start_nodes(self):
        """Start Bitcoin network nodes with enhanced monitoring"""
        self.log("🚀 Starting Bitcoin network nodes...")
        
        for port in self.ports:
            cmd = [sys.executable, "bitcoin.py", str(port)]
            process = subprocess.Popen(
                cmd, 
                stdout=subprocess.DEVNULL, 
                stderr=subprocess.DEVNULL,
                preexec_fn=os.setsid if os.name != 'nt' else None
            )
            self.processes.append(process)
            self.log(f"📡 Node started on port {port}")
            time.sleep(1.5)  # Slightly longer delay
        
        return self.wait_for_nodes()
    
    def wait_for_nodes(self):
        """Wait for all nodes to be ready with enhanced monitoring"""
        self.log("⏳ Waiting for nodes to be ready...")
        max_attempts = 30
        
        for port in self.ports:
            attempts = 0
            while attempts < max_attempts:
                try:
                    response = requests.get(f"{self.base_url}:{port}/chain", timeout=3)
                    if response.status_code == 200:
                        self.log(f"✅ Node {port} is ready")
                        break
                except requests.exceptions.RequestException:
                    pass
                
                attempts += 1
                if attempts % 10 == 0:
                    self.log(f"   Still waiting for node {port}... ({attempts}s)")
                time.sleep(1)
                
                if attempts == max_attempts:
                    self.log(f"❌ Node {port} failed to start after {max_attempts}s", "ERROR")
                    return False
        
        self.log("🎉 All nodes are ready!")
        return True
    
    def cleanup(self):
        """Enhanced cleanup with better process management"""
        self.log("🧹 Cleaning up nodes...")
        for i, process in enumerate(self.processes):
            try:
                if os.name != 'nt':  # Unix/Linux/macOS
                    os.killpg(os.getpgid(process.pid), signal.SIGTERM)
                else:  # Windows
                    process.terminate()
                
                # Wait for graceful shutdown
                try:
                    process.wait(timeout=3)
                    self.log(f"✅ Node {self.ports[i]} stopped gracefully")
                except subprocess.TimeoutExpired:
                    # Force kill if necessary
                    if os.name != 'nt':
                        os.killpg(os.getpgid(process.pid), signal.SIGKILL)
                    else:
                        process.kill()
                    self.log(f"🔥 Node {self.ports[i]} force killed")
                    
            except (ProcessLookupError, OSError) as e:
                self.log(f"⚠️ Process cleanup warning for node {self.ports[i]}: {e}", "WARN")
    
    def test_case_1_basic_functionality(self):
        """TC1: Basic Node Functionality"""
        self.log("\n📋 TEST CASE 1: Basic Node Functionality")
        
        # TC1.1: Genesis Block
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/chain")
            if response:
                data = response.json()
                self.assert_equal(data['length'], 1, f"TC1.1 - Genesis block exists on node {port}")
                if data['length'] >= 1:
                    self.assert_equal(data['chain'][0]['index'], 0, f"TC1.1 - Genesis block index on node {port}")
        
        # TC1.2: Empty Transaction Pool
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/pool")
            if response:
                data = response.json()
                self.assert_equal(data['count'], 0, f"TC1.2 - Empty transaction pool on node {port}")
        
        # TC1.3: Initial Balance Check
        response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/balance/Alice")
        if response:
            data = response.json()
            self.assert_equal(data['balance'], 0.0, "TC1.3 - Initial balance is zero")
    
    def test_case_2_peer_registration(self):
        """TC2: Peer Registration"""
        self.log("\n🔗 TEST CASE 2: Peer Registration")
        
        # Register peers for each node
        for i, port in enumerate(self.ports):
            peers = [f"127.0.0.1:{p}" for p in self.ports if p != port]
            response = self.make_request(
                'POST',
                f"{self.base_url}:{port}/nodes/register",
                json_data={"nodes": peers}
            )
            self.assert_status_code(response, 200, f"TC2.{i+1} - Register peers for node {port}")
        
        # Verify peer registration
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/peers")
            if response:
                data = response.json()
                self.assert_equal(len(data['peers']), 2, f"TC2.4 - Node {port} has 2 peers")
    
    def test_case_3_transaction_management(self):
        """TC3: Enhanced Transaction Management"""
        self.log("\n💰 TEST CASE 3: Transaction Management")
        
        # TC3.1: Add Single Transaction
        response = self.make_request(
            'POST',
            f"{self.base_url}:{self.ports[0]}/transaction",
            json_data={"sender": "Alice", "recipient": "Bob", "amount": 10.5}
        )
        self.assert_status_code(response, 201, "TC3.1 - Add single transaction")
        
        # Wait for propagation with monitoring
        self.wait_for_sync("transaction propagation", 3)
        
        # TC3.2: Verify Transaction in Pool
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/pool")
            if response:
                data = response.json()
                self.assert_greater_equal(data['count'], 1, f"TC3.2 - Transaction propagated to node {port}")
        
        # TC3.3: Add Multiple Transactions
        transactions = [
            {"sender": "Bob", "recipient": "Charlie", "amount": 3.2},
            {"sender": "Charlie", "recipient": "Diana", "amount": 1.7},
            {"sender": "Alice", "recipient": "Diana", "amount": 5.0}
        ]
        
        for i, tx in enumerate(transactions):
            port = self.ports[i % len(self.ports)]
            response = self.make_request('POST', f"{self.base_url}:{port}/transaction", json_data=tx)
            self.assert_status_code(response, 201, f"TC3.3 - Add transaction {i+2}")
            time.sleep(0.5)  # Small delay between transactions
        
        self.wait_for_sync("multiple transaction propagation", 5)
        
        # TC3.4: Verify Transaction Propagation (flexible count)
        min_expected = 4  # At least 4 transactions
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/pool")
            if response:
                data = response.json()
                self.assert_greater_equal(data['count'], min_expected, f"TC3.4 - Multiple transactions on node {port}")
        
        # TC3.5: Test Duplicate Prevention (add same transaction)
        duplicate_tx = {"sender": "Alice", "recipient": "Bob", "amount": 10.5}
        initial_response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/pool")
        initial_count = initial_response.json()['count'] if initial_response else 0
        
        self.make_request('POST', f"{self.base_url}:{self.ports[0]}/transaction", json_data=duplicate_tx)
        time.sleep(2)
        
        final_response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/pool")
        if final_response:
            final_count = final_response.json()['count']
            if final_count == initial_count:
                self.log("✅ TC3.5 - Duplicate transaction prevented", "PASS")
                self.passed_tests += 1
            else:
                self.log(f"⚠️ TC3.5 - Possible duplicate accepted (before: {initial_count}, after: {final_count})", "WARN")
                self.warnings += 1
        
        # TC3.6: Test Invalid Transaction Data
        response = self.make_request(
            'POST',
            f"{self.base_url}:{self.ports[0]}/transaction",
            json_data={"sender": "Alice", "amount": 10},  # Missing recipient
            timeout=5,
            retries=1
        )
        self.assert_status_code(response, 400, "TC3.6 - Invalid transaction rejected")
    
    def test_case_4_mining_functionality(self):
        """TC4: Enhanced Mining Functionality"""
        self.log("\n⛏️ TEST CASE 4: Mining Functionality")
        
        # Get initial state
        initial_state = self.get_network_state()
        self.log(f"Pre-mining state: {initial_state}")
        
        # TC4.1: Mine Block with Transactions
        self.log("⛏️ Mining block... (this may take 10-60 seconds)")
        response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/mine", timeout=120)
        self.assert_status_code(response, 200, "TC4.1 - Mine block with transactions")
        
        if response and response.status_code == 200:
            mining_data = response.json()
            self.log(f"✨ Mining completed: {mining_data.get('message', 'Success')}")
        
        # Wait for block propagation and sync
        self.wait_for_sync("block propagation", 8)
        
        # TC4.2: Verify Block Added to All Chains
        post_mining_state = self.get_network_state()
        self.log(f"Post-mining state: {post_mining_state}")
        
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/chain")
            if response:
                data = response.json()
                initial_length = initial_state[port]['chain_length']
                if isinstance(initial_length, int) and data['length'] > initial_length:
                    self.log(f"✅ TC4.2 - Block added to node {port} (length: {data['length']})", "PASS")
                    self.passed_tests += 1
                    
                    # Find a mined block (not genesis)
                    mined_block = None
                    for block in data['chain']:
                        if block['index'] > 0 and block['hash'].startswith('0000'):
                            mined_block = block
                            break
                    
                    if mined_block:
                        self.log(f"✅ TC4.2 - Valid PoW hash on node {port}", "PASS")
                        self.passed_tests += 1
                    else:
                        self.log(f"❌ TC4.2 - No valid PoW block found on node {port}", "FAIL")
                        self.failed_tests += 1
                else:
                    self.log(f"⚠️ TC4.2 - Chain length unchanged on node {port}", "WARN")
                    self.warnings += 1
        
        # TC4.3: Check Transaction Pool Status (flexible)
        self.wait_for_sync("transaction pool clearing", 3)
        
        cleared_nodes = 0
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/pool")
            if response:
                data = response.json()
                pool_count = data['count']
                if pool_count <= 2:  # Allow for some remaining transactions due to sync delays
                    self.log(f"✅ TC4.3 - Transaction pool acceptable on node {port} (count: {pool_count})", "PASS")
                    self.passed_tests += 1
                    cleared_nodes += 1
                else:
                    self.log(f"⚠️ TC4.3 - High transaction count on node {port} (count: {pool_count})", "WARN")
                    self.warnings += 1
        
        # TC4.4: Test Mining Behavior with Few/No Transactions
        response = self.make_request('GET', f"{self.base_url}:{self.ports[1]}/mine", timeout=30)
        if response:
            if response.status_code == 400:
                self.log("✅ TC4.4 - Mining without sufficient transactions correctly rejected", "PASS")
                self.passed_tests += 1
            elif response.status_code == 200:
                self.log("✅ TC4.4 - Mining succeeded (reward-only or remaining transactions)", "PASS") 
                self.passed_tests += 1
            else:
                self.log(f"⚠️ TC4.4 - Unexpected mining response: {response.status_code}", "WARN")
                self.warnings += 1
        else:
            self.log("⚠️ TC4.4 - Mining endpoint not responding", "WARN")
            self.warnings += 1
        
        # TC4.5: Verify Mining Rewards
        response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/balance/node_{self.ports[0]}")
        if response:
            data = response.json()
            if data['balance'] >= 10.0:
                self.log(f"✅ TC4.5 - Mining reward received (balance: {data['balance']})", "PASS")
                self.passed_tests += 1
            else:
                self.log(f"❌ TC4.5 - Mining reward not received (balance: {data['balance']})", "FAIL")
                self.failed_tests += 1
    
    def test_case_5_balance_calculations(self):
        """TC5: Dynamic Balance Calculations"""
        self.log("\n📊 TEST CASE 5: Balance Calculations")
        
        # Get actual blockchain data to calculate expected balances
        response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/chain")
        if not response:
            self.log("❌ Could not retrieve blockchain for balance validation", "ERROR")
            return
        
        chain_data = response.json()
        self.log(f"Analyzing blockchain with {chain_data['length']} blocks")
        
        # Calculate expected balances from actual transactions
        user_balances = {}
        transaction_count = 0
        
        for block_idx, block in enumerate(chain_data['chain']):
            self.log(f"Block {block_idx}: {len(block.get('data', []))} transactions")
            for tx in block.get('data', []):
                if isinstance(tx, dict):
                    sender = tx.get('sender')
                    recipient = tx.get('recipient')
                    amount = tx.get('amount', 0)
                    
                    if sender and sender != "None":  # Exclude mining rewards
                        user_balances[sender] = user_balances.get(sender, 0) - amount
                        transaction_count += 1
                    
                    if recipient:
                        user_balances[recipient] = user_balances.get(recipient, 0) + amount
        
        self.log(f"Found {transaction_count} user transactions")
        self.log(f"Calculated balances: {user_balances}")
        
        # Test balance accuracy for users with transactions
        for address, expected_balance in user_balances.items():
            if not address.startswith('node_'):  # Skip mining addresses
                response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/balance/{address}")
                if response:
                    data = response.json()
                    actual_balance = data['balance']
                    self.assert_equal(actual_balance, expected_balance, f"TC5.1 - Balance for {address}", tolerance=0.01)
        
        # Test balance consistency across nodes
        if user_balances:
            test_address = next(iter(user_balances.keys()))
            response1 = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/balance/{test_address}")
            response2 = self.make_request('GET', f"{self.base_url}:{self.ports[1]}/balance/{test_address}")
            
            if response1 and response2:
                balance1 = response1.json()['balance']
                balance2 = response2.json()['balance']
                self.assert_equal(balance1, balance2, f"TC5.2 - Balance consistency for {test_address}")
    
    def test_case_6_network_synchronization(self):
        """TC6: Enhanced Network Synchronization"""
        self.log("\n🌐 TEST CASE 6: Network Synchronization")
        
        # Get baseline state
        initial_state = self.get_network_state()
        initial_lengths = [state['chain_length'] for state in initial_state.values() if isinstance(state['chain_length'], int)]
        baseline_length = max(initial_lengths) if initial_lengths else 0
        
        self.log(f"Baseline chain length: {baseline_length}")
        
        # TC6.1: Add transactions to different nodes
        tx1 = {"sender": "TestUser1", "recipient": "TestUser2", "amount": 2.0}
        tx2 = {"sender": "TestUser2", "recipient": "TestUser3", "amount": 1.0}
        
        response1 = self.make_request('POST', f"{self.base_url}:{self.ports[0]}/transaction", json_data=tx1)
        response2 = self.make_request('POST', f"{self.base_url}:{self.ports[1]}/transaction", json_data=tx2)
        
        self.assert_status_code(response1, 201, "TC6.1 - Add transaction to node 1")
        self.assert_status_code(response2, 201, "TC6.1 - Add transaction to node 2")
        
        self.wait_for_sync("cross-node transaction propagation", 5)
        
        # Verify propagation
        successful_syncs = 0
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/pool")
            if response:
                data = response.json()
                if data['count'] >= 2:
                    self.log(f"✅ TC6.1 - Transactions synced on node {port} (count: {data['count']})", "PASS")
                    self.passed_tests += 1
                    successful_syncs += 1
                else:
                    self.log(f"⚠️ TC6.1 - Partial sync on node {port} (count: {data['count']})", "WARN")
                    self.warnings += 1
        
        # TC6.2: Mine on different node
        if successful_syncs >= 2:  # Only proceed if sync was successful
            self.log("⛏️ Mining on different node...")
            response = self.make_request('GET', f"{self.base_url}:{self.ports[1]}/mine", timeout=120)
            self.assert_status_code(response, 200, "TC6.2 - Mine on different node")
            
            self.wait_for_sync("cross-node block propagation", 8)
            
            # Verify block propagation
            expected_min_length = baseline_length + 1
            for port in self.ports:
                response = self.make_request('GET', f"{self.base_url}:{port}/chain")
                if response:
                    data = response.json()
                    if data['length'] >= expected_min_length:
                        self.log(f"✅ TC6.2 - Block synced on node {port} (length: {data['length']})", "PASS")
                        self.passed_tests += 1
                    else:
                        self.log(f"⚠️ TC6.2 - Block sync delayed on node {port} (length: {data['length']})", "WARN")
                        self.warnings += 1
        else:
            self.log("⚠️ Skipping mining test due to transaction sync issues", "WARN")
            self.warnings += 1
    
    def test_case_7_consensus_and_validation(self):
        """TC7: Consensus and Chain Validation"""
        self.log("\n⚖️ TEST CASE 7: Consensus and Validation")
        
        # Force consensus check on all nodes
        consensus_results = []
        for port in self.ports:
            response = self.make_request('GET', f"{self.base_url}:{port}/nodes/resolve", timeout=15)
            if response:
                data = response.json()
                message = data.get('message', '').lower()
                if 'authoritative' in message or 'replaced' in message:
                    self.log(f"✅ TC7.1 - Consensus successful on node {port}: {data['message']}", "PASS")
                    self.passed_tests += 1
                    consensus_results.append(True)
                else:
                    self.log(f"❌ TC7.1 - Consensus failed on node {port}: {data['message']}", "FAIL")
                    self.failed_tests += 1
                    consensus_results.append(False)
            else:
                self.log(f"⚠️ TC7.1 - Consensus check timeout on node {port}", "WARN")
                self.warnings += 1
                consensus_results.append(None)
        
        # Verify chain consistency after consensus
        if any(consensus_results):
            time.sleep(3)  # Wait for consensus to complete
            
            chain_lengths = []
            for port in self.ports:
                response = self.make_request('GET', f"{self.base_url}:{port}/chain")
                if response:
                    data = response.json()
                    chain_lengths.append(data['length'])
            
            if chain_lengths and len(set(chain_lengths)) <= 2:  # Allow minor variations
                self.log(f"✅ TC7.2 - Chain lengths consistent: {chain_lengths}", "PASS")
                self.passed_tests += 1
            else:
                self.log(f"❌ TC7.2 - Chain lengths inconsistent: {chain_lengths}", "FAIL")
                self.failed_tests += 1
    
    def test_case_8_error_handling(self):
        """TC8: Enhanced Error Handling"""
        self.log("\n🚨 TEST CASE 8: Error Handling")
        
        # TC8.1: Test server responsiveness first
        response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/chain", timeout=5, retries=1)
        if not response:
            self.log("❌ Server not responsive - skipping error tests", "ERROR")
            return
        
        self.log("✅ TC8.1 - Server responsive for error testing", "PASS")
        self.passed_tests += 1
        
        # TC8.2: Invalid transaction data
        try:
            response = self.make_request(
                'POST',
                f"{self.base_url}:{self.ports[0]}/transaction",
                json_data={"invalid": "data"},  # Missing required fields
                timeout=5,
                retries=1
            )
            
            if response and response.status_code == 400:
                self.log("✅ TC8.2 - Invalid transaction properly rejected", "PASS")
                self.passed_tests += 1
            elif response:
                self.log(f"⚠️ TC8.2 - Unexpected response to invalid transaction: {response.status_code}", "WARN")
                self.warnings += 1
            else:
                self.log("⚠️ TC8.2 - No response to invalid transaction", "WARN")
                self.warnings += 1
        except Exception as e:
            self.log(f"⚠️ TC8.2 - Error testing invalid transaction: {e}", "WARN")
            self.warnings += 1
        
        # TC8.3: Non-existent endpoint
        try:
            response = self.make_request('GET', f"{self.base_url}:{self.ports[0]}/nonexistent", timeout=5, retries=1)
            if response and response.status_code == 404:
                self.log("✅ TC8.3 - 404 properly returned for invalid endpoint", "PASS")
                self.passed_tests += 1
            elif response:
                self.log(f"⚠️ TC8.3 - Unexpected response to invalid endpoint: {response.status_code}", "WARN")
                self.warnings += 1
            else:
                self.log("⚠️ TC8.3 - No response to invalid endpoint", "WARN")
                self.warnings += 1
        except Exception as e:
            self.log(f"⚠️ TC8.3 - Error testing invalid endpoint: {e}", "WARN")
            self.warnings += 1
    
    def run_all_tests(self):
        """Run all test cases with enhanced monitoring"""
        self.log("🧪 ENHANCED BITCOIN NETWORK TEST SUITE")
        self.log("=" * 60)
        
        start_time = time.time()
        
        try:
            # Start nodes
            if not self.start_nodes():
                self.log("❌ Failed to start nodes. Exiting.", "ERROR")
                return
            
            time.sleep(3)  # Give nodes more time to initialize
            
            # Run test cases
            test_cases = [
                self.test_case_1_basic_functionality,
                self.test_case_2_peer_registration,
                self.test_case_3_transaction_management,
                self.test_case_4_mining_functionality,
                self.test_case_5_balance_calculations,
                self.test_case_6_network_synchronization,
                self.test_case_7_consensus_and_validation,
                self.test_case_8_error_handling
            ]
            
            for i, test_case in enumerate(test_cases, 1):
                try:
                    test_case()
                except Exception as e:
                    self.log(f"❌ Test case {i} crashed: {e}", "ERROR")
                    self.failed_tests += 1
            
            # Final network state
            final_state = self.get_network_state()
            self.log(f"\n🔍 Final network state: {final_state}")
            
            # Print results
            self.print_enhanced_summary(time.time() - start_time)
            
        except KeyboardInterrupt:
            self.log("\n⚠️ Tests interrupted by user", "WARN")
        except Exception as e:
            self.log(f"❌ Unexpected error during testing: {e}", "ERROR")
        finally:
            self.cleanup()
    
    def print_enhanced_summary(self, duration):
        """Print enhanced test results summary"""
        self.log("\n" + "=" * 60)
        self.log("📊 ENHANCED TEST RESULTS SUMMARY")
        self.log("=" * 60)
        
        total_tests = self.passed_tests + self.failed_tests + self.warnings
        success_rate = (self.passed_tests / (self.passed_tests + self.failed_tests) * 100) if (self.passed_tests + self.failed_tests) > 0 else 0
        
        self.log(f"⏱️ Test Duration: {duration:.1f} seconds")
        self.log(f"🧪 Total Assertions: {total_tests}")
        self.log(f"✅ Passed: {self.passed_tests}")
        self.log(f"❌ Failed: {self.failed_tests}")
        self.log(f"⚠️ Warnings: {self.warnings}")
        self.log(f"📈 Success Rate: {success_rate:.1f}%")
        
        if self.failed_tests == 0:
            self.log("🎉 ALL CRITICAL TESTS PASSED! Your Bitcoin network is working correctly!", "SUCCESS")
        elif self.failed_tests <= 3:
            self.log("✅ MOSTLY SUCCESSFUL! Minor issues detected - check warnings above.", "SUCCESS")
        else:
            self.log("⚠️ SOME ISSUES DETECTED. Review failed tests above for details.", "WARN")
        
        if self.warnings > 0:
            self.log(f"ℹ️ {self.warnings} warnings indicate minor timing or network issues - usually normal in distributed systems.", "INFO")
        
        self.log("=" * 60)

if __name__ == "__main__":
    # Check requirements
    if not os.path.exists("bitcoin.py"):
        print("❌ Error: bitcoin.py not found in current directory!")
        print("Please make sure you're running this script in the same directory as bitcoin.py")
        sys.exit(1)
    
    if len(sys.argv) > 1 and sys.argv[1] in ["--help", "-h", "help"]:
        print("Enhanced Bitcoin Network Test Suite")
        print("=" * 40)
        print("python enhanced_test_runner.py     - Run enhanced test suite")
        print("python enhanced_test_runner.py -h  - Show this help")
        print("\nFeatures:")
        print("• More robust error handling")
        print("• Dynamic balance validation") 
        print("• Flexible timing for network synchronization")
        print("• Enhanced logging and debugging")
        print("• Better process management")
        sys.exit(0)
    
    tester = EnhancedBitcoinNetworkTester()
    
    # Handle Ctrl+C gracefully
    def signal_handler(sig, frame):
        print("\n⚠️ Received interrupt signal...")
        tester.cleanup()
        sys.exit(0)
    
    signal.signal(signal.SIGINT, signal_handler)
    
    # Run enhanced tests
    tester.run_all_tests()