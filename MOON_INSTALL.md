# Installing on a Moon
Requires base hash at least: `rd3oe`

Moon keys:
```
~sarsev-dapwel-timluc-miptev
0w2.LVGJb.ufXUR.0bn--.rQ6qF.L5Foj.lkIzX.pR~pr.BHZ7x.G~HGK.JUGoG.riHNr.g7v8o.UZ~Hl.Stsdh.uqwRc.4bJcM.Zi~-1.Q84g0.efa28.mu072.tg0g1

~pidlun-hadwyx-timluc-miptev
0w5fCcu.UT9HM.853Z5.2vBGL.YglSz.H9wVI.BIjZT.xOpnG.A31-6.qR29j.Fq7Gu.ZSeI4.No9n0.0HCXS.5eloq.3Be9X.GEz7Q.hgwf0.0URA4.yL01M.Dk7E1
```

## Create Moon
In your Urbit:
```
|moon
```
Copy the key and note the moon name.

## Install New `zuse.hoon`
```
./urbit -w $MOON_NAME -G $COPIED_KEY -c $PIER_DIR
```
The moon will compile and apply OTAs. After that is done, run:
```
|mount %
```

Install files:
```
./install.sh $MOON_PIER
```

## End to End

### On Moon1:
```
=moon1 ~sarsev-dapwel-timluc-miptev
=moon2 ~pidlun-hadwyx-timluc-miptev
|commit %home

|start %btc-provider
|start %btc-wallet-store
|start %btc-wallet-hook
:btc-provider|command [%set-credentials api-url='http://localhost:50002' %main]
:btc-provider|command [%whitelist-clients `(set ship)`(sy ~[moon2])]
=xpub1 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
:btc-wallet-hook|action [%set-provider moon1 %main]

=fprint [%4 0xdead.beef]
:btc-wallet-store|action [%add-wallet xpub1 fprint ~ [~ 8] [~ 6]]
```

### On Moon2:
```
::  xpub from PRIVATE.md
=moon1 ~sarsev-dapwel-timluc-miptev
=moon2 ~pidlun-hadwyx-timluc-miptev
|commit %home

|start %btc-wallet-store
|start %btc-wallet-hook
:btc-wallet-hook|action [%set-provider moon1 %main]
=xpub2 'zpub6r8dKyWJ31XF6n69KKeEwLjVC5ruqAbiJ4QCqLsrV36Mvx9WEjUaiPNPGFLHNCCqgCdy6iZC8ZgHsm6a1AUTVBMVbKGemNcWFcwBGSjJKbD'
=fprint [%4 0xbeef.dead]
:btc-wallet-store|action [%add-wallet xpub2 fprint ~ [~ 8] [~ 1]]
```

### Request Address
Moon2:
```
:btc-wallet-hook|action [%req-pay-address payee=~sarsev-dapwel-timluc-miptev value=10.000 [~ 37]]
:btc-wallet-hook +dbug [%state 'poym']
```

## scrys
```
.^((list @t) %gx /=btc-wallet-store=/scanned/noun)

.^(@ud %gx /=btc-wallet-store=/balance/[xpub2]/noun)
```
