import hashlib
import json
import time
import threading
from flask import Flask, request, jsonify
import requests
from urllib.parse import urlparse

def calculate_merkle_root(transactions):
    if not transactions:
        return hashlib.sha256("".encode()).hexdigest()
    
    # Convert transactions to hash strings
    tx_hashes = []
    for tx in transactions:
        if isinstance(tx, Transaction):
            tx_str = json.dumps(tx.to_dict(), sort_keys=True)
        else:
            tx_str = json.dumps(tx, sort_keys=True)
        tx_hashes.append(hashlib.sha256(tx_str.encode()).hexdigest())
    
    # Build Merkle tree
    while len(tx_hashes) > 1:
        if len(tx_hashes) % 2 == 1:
            tx_hashes.append(tx_hashes[-1])  # Duplicate last hash if odd number
        
        next_level = []
        for i in range(0, len(tx_hashes), 2):
            combined = tx_hashes[i] + tx_hashes[i + 1]
            next_level.append(hashlib.sha256(combined.encode()).hexdigest())
        
        tx_hashes = next_level
    
    return tx_hashes[0] if tx_hashes else hashlib.sha256("".encode()).hexdigest()

class Transaction:
    def __init__(self, sender, recipient, amount):
        self.sender = sender
        self.recipient = recipient
        self.amount = amount
    
    def to_dict(self):
        return {
            'sender': self.sender,
            'recipient': self.recipient,
            'amount': self.amount
        }
    
    def __eq__(self, other):
        if not isinstance(other, Transaction):
            return False
        return (self.sender == other.sender and 
                self.recipient == other.recipient and 
                self.amount == other.amount)
    
    def __hash__(self):
        return hash((self.sender, self.recipient, self.amount))

class Block:
    def __init__(self, index, timestamp, data, previous_hash, nonce=0):
        self.index = index
        self.timestamp = timestamp
        self.data = data
        self.previous_hash = previous_hash
        self.nonce = nonce
        self.merkle_root = calculate_merkle_root(self.data)
        self.hash = self.calculate_hash()
    
    def calculate_hash(self):
        block_string = json.dumps({
            'index': self.index,
            'timestamp': self.timestamp,
            'merkle_root': self.merkle_root,
            'previous_hash': self.previous_hash,
            'nonce': self.nonce
        }, sort_keys=True)
        return hashlib.sha256(block_string.encode()).hexdigest()
    
    def mine_block(self, difficulty):
        target = "0" * difficulty
        while self.hash[:difficulty] != target:
            self.nonce += 1
            self.hash = self.calculate_hash()
        print(f"Block mined: {self.hash}")
    
    def to_dict(self):
        return {
            'index': self.index,
            'timestamp': self.timestamp,
            'data': [tx.to_dict() if isinstance(tx, Transaction) else tx for tx in self.data],
            'previous_hash': self.previous_hash,
            'nonce': self.nonce,
            'merkle_root': self.merkle_root,
            'hash': self.hash
        }

class Blockchain:
    def __init__(self):
        self.chain = [self.create_genesis_block()]
        self.difficulty = 4
        self.transaction_pool = []
        self.mining_reward = 10
    
    def create_genesis_block(self):
        return Block(0, time.time(), [], "0")
    
    def get_latest_block(self):
        return self.chain[-1]
    
    def add_transaction(self, transaction):
        # Check for duplicate transactions
        for existing_tx in self.transaction_pool:
            if (existing_tx.sender == transaction.sender and 
                existing_tx.recipient == transaction.recipient and 
                existing_tx.amount == transaction.amount):
                return  # Don't add duplicate
        self.transaction_pool.append(transaction)
    
    def mine_pending_transactions(self, mining_reward_address):
        reward_transaction = Transaction(None, mining_reward_address, self.mining_reward)
        self.transaction_pool.append(reward_transaction)
        
        block = Block(
            len(self.chain),
            time.time(),
            self.transaction_pool,
            self.get_latest_block().hash
        )
        
        block.mine_block(self.difficulty)
        
        print(f"Block successfully mined!")
        self.chain.append(block)
        self.transaction_pool = []
        
        return block
    
    def is_chain_valid(self, chain=None):
        if chain is None:
            chain = self.chain
            
        for i in range(1, len(chain)):
            current_block = chain[i]
            previous_block = chain[i-1]
            
            # Validate hash
            if current_block.hash != current_block.calculate_hash():
                return False
            
            # Validate previous hash link
            if current_block.previous_hash != previous_block.hash:
                return False
            
            # Validate proof of work
            if not current_block.hash.startswith("0" * self.difficulty):
                return False
            
            # Validate merkle root
            if current_block.merkle_root != calculate_merkle_root(current_block.data):
                return False
        
        return True
    
    def replace_chain(self, new_chain):
        if len(new_chain) > len(self.chain) and self.is_chain_valid(new_chain):
            self.chain = new_chain
            return True
        return False
    
    def get_balance(self, address):
        balance = 0.0
        
        for block in self.chain:
            for transaction in block.data:
                if isinstance(transaction, Transaction):
                    if transaction.sender == address:
                        balance -= transaction.amount
                    if transaction.recipient == address:
                        balance += transaction.amount
                elif isinstance(transaction, dict):
                    if transaction.get('sender') == address:
                        balance -= transaction.get('amount', 0)
                    if transaction.get('recipient') == address:
                        balance += transaction.get('amount', 0)
        
        return round(balance, 2)

class Node:
    def __init__(self, port):
        self.port = port
        self.blockchain = Blockchain()
        self.peers = set()
        self.app = Flask(__name__)
        self.setup_routes()
    
    def setup_routes(self):
        @self.app.route('/mine', methods=['GET'])
        def mine():
            # Sync transaction pools with peers before mining
            self.sync_transaction_pools()
            
            if not self.blockchain.transaction_pool:
                return jsonify({'message': 'No transactions to mine'}), 400
            
            last_block = self.blockchain.get_latest_block()
            block = self.blockchain.mine_pending_transactions(f"node_{self.port}")
            
            self.broadcast_block(block)
            
            return jsonify({
                'message': 'New block mined',
                'block': block.to_dict(),
                'transactions_mined': len(block.data)
            })
        
        @self.app.route('/transaction', methods=['POST'])
        def new_transaction():
            data = request.get_json()
            
            required = ['sender', 'recipient', 'amount']
            if not all(k in data for k in required):
                return jsonify({'message': 'Missing values'}), 400
            
            transaction = Transaction(data['sender'], data['recipient'], data['amount'])
            self.blockchain.add_transaction(transaction)
            
            # Broadcast transaction to peers
            self.broadcast_transaction(transaction)
            
            return jsonify({'message': 'Transaction added to pool'}), 201
        
        @self.app.route('/chain', methods=['GET'])
        def get_chain():
            chain_data = []
            for block in self.blockchain.chain:
                chain_data.append(block.to_dict())
            
            return jsonify({
                'chain': chain_data,
                'length': len(chain_data)
            })
        
        @self.app.route('/nodes/register', methods=['POST'])
        def register_nodes():
            data = request.get_json()
            nodes = data.get('nodes')
            
            if nodes is None:
                return jsonify({'message': 'Error: Please supply a valid list of nodes'}), 400
            
            for node in nodes:
                self.peers.add(node)
            
            return jsonify({
                'message': 'New nodes have been added',
                'total_nodes': list(self.peers)
            })
        
        @self.app.route('/nodes/resolve', methods=['GET'])
        def consensus():
            replaced = self.resolve_conflicts()
            
            if replaced:
                response = {
                    'message': 'Our chain was replaced',
                    'new_chain': [block.to_dict() for block in self.blockchain.chain]
                }
            else:
                response = {
                    'message': 'Our chain is authoritative',
                    'chain': [block.to_dict() for block in self.blockchain.chain]
                }
            
            return jsonify(response)
        
        @self.app.route('/balance/<address>', methods=['GET'])
        def get_balance(address):
            balance = self.blockchain.get_balance(address)
            return jsonify({'address': address, 'balance': balance})
        
        @self.app.route('/transaction/receive', methods=['POST'])
        def receive_transaction():
            data = request.get_json()
            
            required = ['sender', 'recipient', 'amount']
            if not all(k in data for k in required):
                return jsonify({'message': 'Invalid transaction data'}), 400
            
            transaction = Transaction(data['sender'], data['recipient'], data['amount'])
            self.blockchain.add_transaction(transaction)
            
            return jsonify({'message': 'Transaction received'}), 200
        
        @self.app.route('/pool', methods=['GET'])
        def get_transaction_pool():
            pool_data = []
            for tx in self.blockchain.transaction_pool:
                pool_data.append(tx.to_dict() if isinstance(tx, Transaction) else tx)
            
            return jsonify({
                'transactions': pool_data,
                'count': len(pool_data)
            })
        
        @self.app.route('/peers', methods=['GET'])
        def get_peers():
            return jsonify({'peers': list(self.peers)})
        
        @self.app.route('/block', methods=['POST'])
        def receive_block():
            data = request.get_json()
            
            if not self.is_valid_block_data(data):
                return jsonify({'message': 'Invalid block data'}), 400
            
            # Reconstruct transactions properly
            transactions = []
            for tx_data in data['data']:
                if isinstance(tx_data, dict):
                    transactions.append(Transaction(tx_data['sender'], tx_data['recipient'], tx_data['amount']))
                else:
                    transactions.append(tx_data)
            
            new_block = Block(
                data['index'],
                data['timestamp'],
                transactions,
                data['previous_hash'],
                data['nonce']
            )
            new_block.hash = data['hash']
            new_block.merkle_root = data.get('merkle_root', calculate_merkle_root(transactions))
            
            if self.validate_and_add_block(new_block):
                return jsonify({'message': 'Block added successfully'})
            else:
                self.resolve_conflicts()
                return jsonify({'message': 'Block rejected, chain synchronized'})
    
    def is_valid_block_data(self, data):
        required_fields = ['index', 'timestamp', 'data', 'previous_hash', 'nonce', 'hash']
        return all(field in data for field in required_fields)
    
    def validate_and_add_block(self, new_block):
        last_block = self.blockchain.get_latest_block()
        
        if (new_block.index == last_block.index + 1 and
            new_block.previous_hash == last_block.hash and
            new_block.hash == new_block.calculate_hash() and
            new_block.hash.startswith("0" * self.blockchain.difficulty) and
            new_block.merkle_root == calculate_merkle_root(new_block.data)):
            
            self.blockchain.chain.append(new_block)
            self.blockchain.transaction_pool = []
            return True
        
        return False
    
    def sync_transaction_pools(self):
        for peer in self.peers:
            try:
                response = requests.get(f'http://{peer}/pool', timeout=5)
                if response.status_code == 200:
                    data = response.json()
                    for tx_data in data['transactions']:
                        transaction = Transaction(tx_data['sender'], tx_data['recipient'], tx_data['amount'])
                        self.blockchain.add_transaction(transaction)
            except requests.exceptions.RequestException as e:
                print(f"Failed to sync transaction pool from {peer}: {e}")
    
    def broadcast_transaction(self, transaction):
        for peer in self.peers:
            try:
                response = requests.post(
                    f'http://{peer}/transaction/receive',
                    json=transaction.to_dict(),
                    timeout=5
                )
            except requests.exceptions.RequestException as e:
                print(f"Failed to broadcast transaction to {peer}: {e}")
    
    def broadcast_block(self, block):
        for peer in self.peers:
            try:
                response = requests.post(
                    f'http://{peer}/block',
                    json=block.to_dict(),
                    timeout=5
                )
            except requests.exceptions.RequestException as e:
                print(f"Failed to broadcast to {peer}: {e}")
    
    def resolve_conflicts(self):
        neighbours = self.peers
        new_chain = None
        max_length = len(self.blockchain.chain)
        
        for node in neighbours:
            try:
                response = requests.get(f'http://{node}/chain', timeout=5)
                
                if response.status_code == 200:
                    data = response.json()
                    length = data['length']
                    chain_data = data['chain']
                    
                    chain = []
                    for block_data in chain_data:
                        # Reconstruct transactions properly
                        transactions = []
                        for tx_data in block_data['data']:
                            if isinstance(tx_data, dict):
                                transactions.append(Transaction(tx_data['sender'], tx_data['recipient'], tx_data['amount']))
                            else:
                                transactions.append(tx_data)
                        
                        block = Block(
                            block_data['index'],
                            block_data['timestamp'],
                            transactions,
                            block_data['previous_hash'],
                            block_data['nonce']
                        )
                        block.hash = block_data['hash']
                        block.merkle_root = block_data.get('merkle_root', calculate_merkle_root(transactions))
                        chain.append(block)
                    
                    if length > max_length and self.blockchain.is_chain_valid(chain):
                        max_length = length
                        new_chain = chain
                        
            except requests.exceptions.RequestException as e:
                print(f"Failed to get chain from {node}: {e}")
        
        if new_chain:
            self.blockchain.chain = new_chain
            return True
        
        return False
    
    def run(self, debug=False):
        self.app.run(host='127.0.0.1', port=self.port, debug=debug, threaded=True)

def start_node(port):
    node = Node(port)
    node.run()

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) != 2:
        print("Usage: python bitcoin.py <port>")
        sys.exit(1)
    
    port = int(sys.argv[1])
    start_node(port)