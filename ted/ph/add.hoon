/-  spider
/+  *ph-io, bip32
=,  strand=strand:spider
=,  secp256k1:secp:crypto
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
=/  zpub=tape
  "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
=/  pubkey
  derive-public:(from-extended:bip32 zpub)
;<  ~  bind:m  start-simple
;<  ~  bind:m  (raw-ship ~bud ~)
;<  ~  bind:m  (dojo ~bud "|start %btc-node-store")
;<  ~  bind:m  (dojo ~bud "|start %btc-node-hook")
;<  ~  bind:m  (dojo ~bud "(add 9 299)")
;<  ~  bind:m  (wait-for-output ~bud "308")
;<  ~  bind:m  end-simple
(pure:m *vase)
