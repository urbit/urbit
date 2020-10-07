# py-scanner

Scans the BTC blockchain's raw txs. Faster than RPC'ing.
Uses [Python Blockchain Parser](https://github.com/alecalve/python-bitcoin-blockchain-parser)

## Usage
1. Build the Docker image
```
docker image build --rm -t py-scanner .`docker image build --rm -t py-scanner .
```
2. Run it (opens Python CLI)
```
BTC="/Volumes/sandisk/BTC"
docker container run -v $BTC:/btc -it py-scanner
# OR add `bash` to get a CLI
docker container run -v $BTC:/btc -it py-scanner bash
```
3. Run commands. The below takes a couple seconds to index, but then it blows through blocks.
```
import os
from blockchain_parser.blockchain import Blockchain
path='/btc/blocks'
blockchain = Blockchain(os.path.expanduser(path))

for block in blockchain.get_ordered_blocks(os.path.expanduser('/btc/blocks/index'), start=651000, end=651300):
    print("height=%d block=%s" % (block.height, block.hash))
    
for block in blockchain.get_ordered_blocks(os.path.expanduser('/btc/blocks/index'), start=651000, end=651001):
    print(block.transactions)
```
