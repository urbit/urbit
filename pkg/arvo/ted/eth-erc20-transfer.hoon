/+  ethio, strandio, eth-abi
::
/=  erc20-abi
  /;  parse-contract:eth-abi
  /:  /===/app/eth-wallet/erc20  /json/
::
=*  eth  ethereum-types
=*  eth-rpc  rpc:ethereum
=*  eth-key  key:ethereum
|=  args=vase
=+  !<  $:  url=@t
            contract-id=@t
            contract=address:eth
            to=address:eth
            amount=@ud
            private-key=@
        ==
    args
=/  m  (strand:strandio ,vase)
^-  form:m
=/  from=address:eth  (address-from-prv:eth-key private-key)
=/  public-key=@  (pub-from-prv:eth-key private-key)
::
;<  nonce=@ud  bind:m  (get-next-nonce:ethio url from)
=/  txn=transaction:eth-rpc
  :*  nonce
      gas-price=8
      gas=45.000
      to
      value=0
      data=*@ux  ::  TODO actual call args
      chain-id=0x1
  ==
=/  signed=@ux        (sign-transaction:eth-key txn private-key)
=/  =request:eth-rpc  [%eth-send-raw-transaction signed]
;<  res=json  bind:m  (request-rpc:ethio url `contract-id request)
(pure:m !>(res))
