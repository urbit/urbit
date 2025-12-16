/-  dice, *aquarium
/+  ethereum, azimuth
::
|%
::
++  extract-request
  |=  [uf=unix-effect dest=@t]
  ^-  (unit [num=@ud =request:http])
  ?.  ?=(%request -.q.uf)  ~
  ?.  =(dest url.request.q.uf)  ~
  `[id.q.uf request.q.uf]
::
++  router
  |=  [our=ship her=ship uf=unix-effect azi=az-state]
  ^-  (unit card:agent:gall)
  =,  enjs:format
  =/  ask-load
    %+  extract-request  uf
    'https://bootstrap.urbit.org/mainnet.azimuth-snapshot'
  ?^  ask-load
    =/  events=(list aqua-event)
      :_  ~
      :*  %event
          her
          /i/http-client/0v1n.2m9vh
          %receive
          num.u.ask-load
          [%start [200 ~] `(as-octs:mimes:html (jam *versioned-snap:dice)) &]
      ==
    %-  some
    :*  %pass  /aqua-events
        %agent  [our %aqua]
        %poke  %aqua-events
        !>(events)
    ==
  =/  ask  (extract-request uf 'http://fake.aqua.domain/')
  ?~  ask
    ~
  ?~  body.request.u.ask
    ~
  =/  req  q.u.body.request.u.ask
  |^  ^-  (unit card:agent:gall)
  =/  method  (get-method req)
  ?:  =(method 'eth_blockNumber')
    :-  ~
    %+  answer-request  req
    s+(crip (num-to-hex:ethereum latest-block))
  ?:  =(method 'eth_getBlockByNumber')
    :-  ~
    %+  answer-request  req
    :-  %o
    =/  number  (hex-to-num:ethereum (get-first-param req))
    =/  hash  (number-to-hash number)
    =/  parent-hash  (number-to-hash ?~(number number (dec number)))
    %-  malt
    ^-  (list (pair term json))
    :~  hash+s+(crip (prefix-hex:ethereum (render-hex-bytes:ethereum 32 hash)))
        number+s+(crip (num-to-hex:ethereum number))
        'parentHash'^s+(crip (num-to-hex:ethereum parent-hash))
    ==
  ?:  =(method 'eth_getLogs')
    :-  ~
    %+  answer-request  req
    ?^  (get-param-obj-maybe req 'blockHash')
      %-  logs-by-hash
      (get-param-obj req 'blockHash')
    %+  logs-by-range
      (get-param-obj req 'fromBlock')
    (get-param-obj req 'toBlock')
  ~&  [%ph-azimuth-miss req]
  ~
  ::
  ++  latest-block
    (add launch:contracts:azimuth (dec (lent logs.azi)))
  ::
  ++  get-single-req
    |=  req=@t
    =/  batch
      ((ar:dejs:format same) (need (de:json:html req)))
    ?>  ?=([* ~] batch)
    i.batch
  ::
  ++  get-id
    |=  req=@t
    =,  dejs:format
    %.  (get-single-req req)
    (ot id+so ~)
  ::
  ++  get-method
    |=  req=@t
    =,  dejs:format
    ~|  req=req
    %.  (get-single-req req)
    (ot method+so ~)
  ::
  ++  get-param-obj
    |=  [req=@t param=@t]
    =,  dejs:format
    %-  hex-to-num:ethereum
    =/  array
      %.  (get-single-req req)
      (ot params+(ar (ot param^so ~)) ~)
    ?>  ?=([* ~] array)
    i.array
  ::
  ++  get-param-obj-maybe
    |=  [req=@t param=@t]
    ^-  (unit @ud)
    =,  dejs-soft:format
    =/  array
      %.  (get-single-req req)
      (ot params+(ar (ot param^so ~)) ~)
    ?~  array
      ~
    :-  ~
    ?>  ?=([* ~] u.array)
    %-  hex-to-num:ethereum
    i.u.array
  ::
  ++  get-first-param
    |=  req=@t
    =,  dejs:format
    =/  id
      %.  (get-single-req req)
      (ot params+(at so bo ~) ~)
    -.id
  ::
  ++  answer-request
    |=  [req=@t result=json]
    ^-  card:agent:gall
    =/  resp
      %-  en:json:html
      :-  %a  :_  ~
      %-  pairs
      :~  id+s+(get-id req)
          jsonrpc+s+'2.0'
          result+result
      ==
    =/  events=(list aqua-event)
      :_  ~
      :*  %event
          her
          /i/http-client/0v1n.2m9vh
          %receive
          num.u.ask
          [%start [200 ~] `(as-octs:mimes:html resp) &]
      ==
    :*  %pass  /aqua-events
        %agent  [our %aqua]
        %poke  %aqua-events
        !>(events)
    ==
  ::
  ++  number-to-hash
    |=  =number:block:jael
    ^-  @
    ?:  (lth number launch:contracts:azimuth)
      (cat 3 0x5364 (sub launch:contracts:azimuth number))
    (cat 3 0x5363 (sub number launch:contracts:azimuth))
  ::
  ++  hash-to-number
    |=  =hash:block:jael
    (add launch:contracts:azimuth (div hash 0x1.0000))
  ::
  ++  logs-by-range
    |=  [from-block=@ud to-block=@ud]
    %+  logs-to-json  (max launch:contracts:azimuth from-block)
    ?:  (lth to-block launch:contracts:azimuth)
      ~
    %+  swag
      ?:  (lth from-block launch:contracts:azimuth)
         [0 +((sub to-block launch:contracts:azimuth))]
      :-  (sub from-block launch:contracts:azimuth)
      +((sub to-block from-block))
    logs.azi
  ::
  ++  logs-by-hash
    |=  =hash:block:jael
    =/  =number:block:jael  (hash-to-number hash)
    (logs-by-range number number)
  ::
  ++  logs-to-json
    |=  [count=@ud selected-logs=(list az-log)]
    ^-  json
    :-  %a
    |-  ^-  (list json)
    ?~  selected-logs
      ~
    :_  $(selected-logs t.selected-logs, count +(count))
    %-  pairs
    :~  'logIndex'^s+'0x0'
        'transactionIndex'^s+'0x0'
        :+  'transactionHash'  %s
        (crip (prefix-hex:ethereum (render-hex-bytes:ethereum 32 `@`0x5362)))
      ::
        :+  'blockHash'  %s
        =/  hash  (number-to-hash count)
        (crip (prefix-hex:ethereum (render-hex-bytes:ethereum 32 hash)))
      ::
        :+  'blockNumber'  %s
        (crip (num-to-hex:ethereum count))
      ::
        :+  'address'  %s
        (crip (address-to-hex:ethereum azimuth:contracts:azimuth))
      ::
        'type'^s+'mined'
      ::
        'data'^s+data.i.selected-logs
        :+  'topics'  %a
        %+  turn  topics.i.selected-logs
        |=  topic=@ux
        ^-  json
        :-  %s
        %-  crip
        %-  prefix-hex:ethereum
        (render-hex-bytes:ethereum 32 `@`topic)
    ==
  --
::
++  get-keys
  |=  [who=@p lyfe=life]
  ?~  cum=(~(get by comets) who)
    %^  pit:nu:cric:crypto  32
      (can 5 [1 (scot %p who)] [1 (scot %ud lyfe)] ~)
    [%b ~]
  ?.  =(lyfe 1)
    %^  pit:nu:cric:crypto  32
      (can 5 [1 (scot %p who)] [1 (scot %ud lyfe)] ~)
    [%c 0xdead.beef.cafe]
  ?:  ?=(%b suite.u.cum)
    (pit:nu:cric:crypto 512 seed.u.cum %b ~)
  (pit:nu:cric:crypto 512 seed.u.cum %c 0xdead.beef.cafe)
::
++  get-public
  |=  [who=@p lyfe=life]
  ^-  public-keys:ames
  ded:ex:(get-keys who lyfe)
::  +comets: allowed comets, their +cric suite and seeds
::    the tweak for %c comets is 0xdead.beef.cafe
::
++  comets
  ^~  ^-  (map ship [suite=?(%b %c) seed=@uw])
  %-  ~(gas by *(map ship [suite=?(%b %c) seed=@uw]))
  ^-  (list [=ship suite=?(%b %c) seed=@uw])
  %+  zip
    ::  comet names
    ^-  (list @p)
    :~  :: marbud, %c suite
        ~fasteg-dinhet-malrum-ransub--hocduc-digtev-radsut-marbud
        ~daldyl-nildem-dispec-tilryx--dondus-dirmet-tintyl-marbud
        ~dansyr-ponbec-tocfel-laddux--socnut-nisnyx-dinsut-marbud
        :: marbud, %b suite
        ~harrep-podpec-torsut-docnyx--mopsyx-fosdus-ladpen-marbud
        ~liblyn-togrut-tabwel-hodbet--dovbex-parryt-mirbyt-marbud
        ~hidreb-naptev-banben-bicrup--massup-dantus-fodwet-marbud
        :: mardev, %c suite
        ~molpyx-novtyc-wortyc-noswyd--taltyv-loplev-dabwen-mardev
        ~fosnys-noctyd-talfyl-borryl--davhus-disbyn-fotnec-mardev
        ~tonmep-tabrux-rinbep-firmur--silmex-saldef-pasfer-mardev
        :: mardev, %b suite
        ~holwyx-ramped-tognet-barsyn--navler-ronmeg-topbex-mardev
        ~hacmet-doslyr-narhut-tiptec--micbyl-motnev-worsyn-mardev
        ~ribmut-nopdul-minmet-pardeg--wisfex-rosfus-fogsyn-mardev
    ==
  %+  zip
    ::  comet suites
    ^-  (list ?(%b %c))
    ~[%c %c %c %b %b %b %c %c %c %b %b %b]
  ::  comet seeds
  ^-  (list @uw)
  :~  0w2.5sfF0.~inVv.dQ7zb.ykQSG.aX5nF.uGQsm.keVzY.6Pu1S.
      quvGI.b0Ht2.Ctbbr.-ADfG.7yIL4.NXJ5a.lGmJZ.5wkdb.9Z775
      0w5Disb.xWJtw.cszH3.YBTFu.9k6Nc.JjeyV.origh.VkYmT.
      9-Obr.T3TOs.IPdWd.MmsUQ.ZZGZa.OLHMe.5azFd.l7hXr.~vuI~
      0w1.7wOws.lF20Y.WRmex.htiLX.WrZ43.yxBCD.Ow3oE.kumTc.
      dRou7.xGeQm.Lbbx-.6hTii.hzYgP.Z2iQ9.7YYLB.2qb1b.PDItX
      0w3.7aZCR.XIcSt.sKqRG.AS4KD.A-FAT.bbZwc.2N4z5.pez5t.
      aZGIz.d0Hy9.C~RPd.87GcR.LM0Jt.6oVFF.LL4v7.rzlwk.~Fm5Z
      0w1.fuip3.x~XMr.eE02V.K4RC5.OvDaK.jug28.75z30.UY476.
      ZlB3Q.bD78k.M8E~g.I4LRY.OytPc.XD2Bm.XDM9t.iQEhl.LNCMM
      0w1.BHOHC.VyVuo.4kS0o.VKJNU.-zMyL.T2zJo.j1EF5.symnK.
      yQB8T.TvCPN.Z8~P~.KS6j4.~055y.E-jBn.UhIxJ.mItiE.PmML1
      0w3.mAqpe.eRL-v.65LTo.aHWFA.5kTRF.qQ1o-.xK2W-.tae8A.
      FLBV~.wL3iP.A~53S.izniF.SiLrJ.DDxNO.A9Yps.QLFta.LmorX
      0w3.L29ce.OZsch.LKI2F.f86PX.JuhkV.8gnMT.FSqcd.~MqL3.
      v4wEj.yFnGN.DHr-Z.TiCRY.tG-7r.E1oza.pW2FM.i097b.yA~Ql
      0w2.iUm0y.wCmrI.GrVKW.r5yu9.Stccm.3diy3.vS4r7.tV~jd.
      -mxoM.S1nFG.soxnp.dDr6X.DUI99.4uhQO.ntSQJ.UYiQi.pMRi2
      0w2.i8vIr.hWTd1.aC9jk.F6Y3e.r5OEr.nzm8U.KHzQN.RsEzF.
      trAnj.MqRRu.397ik.L8o9k.RSIip.0vZ4Q.qhnSI.eXfhu.brJPS
      0ws1~UQ.v~fJv.C5MPg.LFX3N.ZmJmu.0LeVG.lyyT7.shhvL.
      2~det.i-jOI.OVI8v.9ldMk.16MGj.AZxso.qsTpQ.inrUz.aE1sa
      0w~w9s8.YLtr3.bSQ8H.SIK5g.Dnh9M.aIcT2.mqIqG.geVWH.
      lJUzq.OTuUl.oM9ww.7MwQh.pQ7Q9.NB38f.FzzKE.S7is8.~0Gg-
  ==
::  +zip: combine two lists into a list of cells of their elements
::
++  zip
    |*  [a=(list) b=(list)]
    ^-  (list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
    =|  out=(list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
    ?>  =((lent a) (lent b))
    |-
    ?~  a  (flop out)
    ?~  b  (flop out)
    $(out [[i.a i.b] out], a t.a, b t.b)
::
::  Generate logs
::
++  lo
  =,  azimuth-events:azimuth
  |%
  ++  broke-continuity
    |=  [who=ship rut=rift]
    ^-  az-log
    :-  ~[^broke-continuity who]
    %-  crip
    %-  prefix-hex:ethereum
    (render-hex-bytes:ethereum 32 `@`rut)
  ::
  ++  changed-keys
    |=  [who=ship enc=@ux aut=@ux crypto=@ud lyfe=life]
    ^-  az-log
    :-  ~[^changed-keys who]
    %-  crip
    %-  prefix-hex:ethereum
    ;:  welp
        (render-hex-bytes:ethereum 32 `@`enc)
        (render-hex-bytes:ethereum 32 `@`aut)
        (render-hex-bytes:ethereum 32 `@`crypto)
        (render-hex-bytes:ethereum 32 `@`lyfe)
    ==
  --
--
