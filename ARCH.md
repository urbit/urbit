# Urbit Bitcoin Architecture

## Intro
This architecture is, by Urbit standards, awkward. The awkwardness arises mainly from the asymmetry of full nodes: only a few nodes are providers/full nodes, and they have to keep remote clients updated as to the state of the blockchain. The system also requires providers to run a node side-by-side with their Urbit, although this can mostly be abstracted away as HTTP calls out.

My goal in designing this was to isolate the architecture's awkwardness as much as possible to specific chokepoints, and to keep the non-provider portions as clean, simple state machine primitives.

System summary:
- `btc-wallet-store`: holds wallets and watches their addresses
  * tracks whether a wallet has been scanned
  * generates receiving addresses and change addresses
  * can take address input from any agent on its own ship
- `btc-wallet-hook`: requests BTC state from provider and forwards it
  * subscribes to wallet-store for any address requests.
  * pokes wallet-store with new address info
- `btc-provider`: 
- helper BTC libraries for address and transaction generation.

## btc-wallet-store
Intentionally very limited in function. It is a primitive for tracking wallet state, including available addresses an existing/watched addresses.

It can take address inputs from any local source. This allows the possibility of creating import programs for pre-existing wallet data, or large amounts of wallet data.

Outgoing data:

Incoming data:

## btc-wallet-hook
I don't like the name "hook" here, but can't think of anything better atm. It's closer to a data source/manager on top of the wallet, potentially just one of many.

Outgoing data:

Incoming data:

Error conditions:

## btc-provider

Outgoing data:

Incoming data:

Error conditions:


## Needed Extensions
- Invoice generator that asks for addresses on behalf of ships and tracks whether they've made payments. `wallet-hook` could probably be renamed to `wallet-manager` and extended for this purpose.


## Possible Improvements
- Multiple Providers
- Gossip network for both pulling and pushing address updates

