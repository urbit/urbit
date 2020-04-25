/+  eth-abi, eth-contracts-erc20=erc20, *test
:: =/  erc20=json
::   %-  need  %-  de-json:html
::   '''
::   {
::     "contractName": "ERC20",
::     "abi": [
::       {
::         "anonymous": false,
::         "inputs": [
::           {
::             "indexed": true,
::             "internalType": "address",
::             "name": "owner",
::             "type": "address"
::           },
::           {
::             "indexed": true,
::             "internalType": "address",
::             "name": "spender",
::             "type": "address"
::           },
::           {
::             "indexed": false,
::             "internalType": "uint256",
::             "name": "value",
::             "type": "uint256"
::           }
::         ],
::         "name": "Approval",
::         "type": "event"
::       },
::       {
::         "anonymous": false,
::         "inputs": [
::           {
::             "indexed": true,
::             "internalType": "address",
::             "name": "from",
::             "type": "address"
::           },
::           {
::             "indexed": true,
::             "internalType": "address",
::             "name": "to",
::             "type": "address"
::           },
::           {
::             "indexed": false,
::             "internalType": "uint256",
::             "name": "value",
::             "type": "uint256"
::           }
::         ],
::         "name": "Transfer",
::         "type": "event"
::       },
::       {
::         "constant": true,
::         "inputs": [],
::         "name": "totalSupply",
::         "outputs": [
::           {
::             "internalType": "uint256",
::             "name": "",
::             "type": "uint256"
::           }
::         ],
::         "payable": false,
::         "stateMutability": "view",
::         "type": "function"
::       },
::       {
::         "constant": true,
::         "inputs": [
::           {
::             "internalType": "address",
::             "name": "account",
::             "type": "address"
::           }
::         ],
::         "name": "balanceOf",
::         "outputs": [
::           {
::             "internalType": "uint256",
::             "name": "",
::             "type": "uint256"
::           }
::         ],
::         "payable": false,
::         "stateMutability": "view",
::         "type": "function"
::       },
::       {
::         "constant": false,
::         "inputs": [
::           {
::             "internalType": "address",
::             "name": "recipient",
::             "type": "address"
::           },
::           {
::             "internalType": "uint256",
::             "name": "amount",
::             "type": "uint256"
::           }
::         ],
::         "name": "transfer",
::         "outputs": [
::           {
::             "internalType": "bool",
::             "name": "",
::             "type": "bool"
::           }
::         ],
::         "payable": false,
::         "stateMutability": "nonpayable",
::         "type": "function"
::       },
::       {
::         "constant": true,
::         "inputs": [
::           {
::             "internalType": "address",
::             "name": "owner",
::             "type": "address"
::           },
::           {
::             "internalType": "address",
::             "name": "spender",
::             "type": "address"
::           }
::         ],
::         "name": "allowance",
::         "outputs": [
::           {
::             "internalType": "uint256",
::             "name": "",
::             "type": "uint256"
::           }
::         ],
::         "payable": false,
::         "stateMutability": "view",
::         "type": "function"
::       },
::       {
::         "constant": false,
::         "inputs": [
::           {
::             "internalType": "address",
::             "name": "spender",
::             "type": "address"
::           },
::           {
::             "internalType": "uint256",
::             "name": "amount",
::             "type": "uint256"
::           }
::         ],
::         "name": "approve",
::         "outputs": [
::           {
::             "internalType": "bool",
::             "name": "",
::             "type": "bool"
::           }
::         ],
::         "payable": false,
::         "stateMutability": "nonpayable",
::         "type": "function"
::       },
::       {
::         "constant": false,
::         "inputs": [
::           {
::             "internalType": "address",
::             "name": "sender",
::             "type": "address"
::           },
::           {
::             "internalType": "address",
::             "name": "recipient",
::             "type": "address"
::           },
::           {
::             "internalType": "uint256",
::             "name": "amount",
::             "type": "uint256"
::           }
::         ],
::         "name": "transferFrom",
::         "outputs": [
::           {
::             "internalType": "bool",
::             "name": "",
::             "type": "bool"
::           }
::         ],
::         "payable": false,
::         "stateMutability": "nonpayable",
::         "type": "function"
::       },
::       {
::         "constant": false,
::         "inputs": [
::           {
::             "internalType": "address",
::             "name": "spender",
::             "type": "address"
::           },
::           {
::             "internalType": "uint256",
::             "name": "addedValue",
::             "type": "uint256"
::           }
::         ],
::         "name": "increaseAllowance",
::         "outputs": [
::           {
::             "internalType": "bool",
::             "name": "",
::             "type": "bool"
::           }
::         ],
::         "payable": false,
::         "stateMutability": "nonpayable",
::         "type": "function"
::       },
::       {
::         "constant": false,
::         "inputs": [
::           {
::             "internalType": "address",
::             "name": "spender",
::             "type": "address"
::           },
::           {
::             "internalType": "uint256",
::             "name": "subtractedValue",
::             "type": "uint256"
::           }
::         ],
::         "name": "decreaseAllowance",
::         "outputs": [
::           {
::             "internalType": "bool",
::             "name": "",
::             "type": "bool"
::           }
::         ],
::         "payable": false,
::         "stateMutability": "nonpayable",
::         "type": "function"
::       }
::     ]
::   }
::   '''
:: =/  expected-erc20=contract-raw:eth-abi
::   :-  name='ERC20'
::   ^=  entries
::   :~  [%event 'Approval' ~[['address' &] ['address' &] ['uint256' |]]]
::       [%event 'Transfer' ~[['address' &] ['address' &] ['uint256' |]]]
::       [%function ['totalSupply' ~ ~[%uint256] | |]]
::       [%function ['balanceOf' ~[%address] ~[%uint256] | |]]
::       [%function ['transfer' ~[%address %uint256] ~[%bool] & |]]
::       [%function ['allowance' ~[%address %address] ~[%uint256] | |]]
::       [%function ['approve' ~[%address %uint256] ~[%bool] & |]]
::       [%function ['transferFrom' ~[%address %address %uint256] ~[%bool] & |]]
::       [%function ['increaseAllowance' ~[%address %uint256] ~[%bool] & |]]
::       [%function ['decreaseAllowance' ~[%address %uint256] ~[%bool] & |]]
::   ==
|%
:: ++  test-erc20
::   ^-  tang
::   %+  expect-eq  !>((parse-contract:eth-abi erc20))  !>(expected-erc20)
++  test-get-hash
  ^-  tang
  %+  expect-eq
  !>((get-hash:eth-abi 'OwnerChanged' ~['uint32' 'address']))
  !>(owner-changed:azimuth-events:azimuth)
--
