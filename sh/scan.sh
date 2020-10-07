#!/bin/bash
usage() { echo "Usage: $0 BLOCK_START BLOCK_END" 1>&2; exit 1; }
if [ $# -eq 0 ]; then
  usage
  exit 2
fi

BLOCK_START=$1
BLOCK_END=$2
BITCOIN_CLI="bitcoin-cli -rpcport=18443"

TXS=$($BITCOIN_CLI getblock $(bitcoin-cli -rpcport=18443 getblockhash $BLOCK_START) | jq -r '.tx[]')

BLOCK=$($BITCOIN_CLI getblock $(bitcoin-cli -rpcport=18443 getblockhash $BLOCK_START) false)
echo $BLOCK

#for TX in $TXS; do
  #  TX_INFO=$($BITCOIN_CLI getrawtransaction $TX true)
#  echo $TX_INFO | jq -r '.vout | .[]'
  # see page 50 of Mastering BitCoin  # see page 50 of Mastering BitCoin
  #  echo all outputs > 0
  #  echo $TX_INFO | jq -r '.vout | .[] | select(.value>0) | .value,(.scriptPubKey.addresses | .[])'
#  echo $TX
#done
