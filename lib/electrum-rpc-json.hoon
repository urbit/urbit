/-  *btc-bridge
/+  lib=btc-node-json
::  dummy core until needed
|%
++  blank  42
::  TODO: generate GET URLS here, or even the whole http request!!
::  ++  electrum-rpc
::    |%
::    ++  request-to-rpc
::      =,  enjs:format
::      |=  req=request:electrum:rpc
::      ^-  request:rpc:jstd
::      ?-  -.req
::        %get-address-balance
::        :*  id='0'
::            method='blockchain.scripthash.get_balance'
::            params=[%list ~[[%s address.req]]]
::        ==
::      ==
::    --
--
