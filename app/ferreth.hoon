::  "veriferreth": verify ethereum-side azimuth state
::  has naming gone too far?
::
/-  json-rpc
::
=,  ethereum
=,  ethe
=,  constitution
::
|%
++  state
  $:  deeds=(map ship deek)
      queue=(list ship)
  ==
::
+$  deek  [keys deed:eth-noun]
+$  keys  [crypt=byts auth=byts]
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
      [%info wire ship desk nori:clay]
  ==
--
::
::
|%
++  azimuth         0x223c.067f.8cf2.8ae1.73ee.5caf.ea60.ca44.c335.fecb
++  conditional-sr  0x8c24.1098.c3d3.498f.e126.1421.633f.d579.86d7.4aea
++  linear-sr       0x86cd.9cd0.992f.0423.1751.e376.1de4.5cec.ea5d.1801
::
++  parity   'http://104.198.35.227:8545'
--
::
::
|_  [bol=bowl:gall state]
::
++  prep
  |=  old=(unit state)
  ^-  [(list move) _+>]
  ?~  old  [~ +>]
  [~ +>.$(+<+ u.old)]
::
++  poke-noun
  |=  a=$@(@t [wat=@t dat=*])
  ^-  [(list move) _+>]
  ?@  a
    ?:  =('call' a)  [[initial-call ~] +>]
    ?:  =('cont' a)  next-in-queue
    ?:  =('file' a)  [[write-file ~] +>]
    ?:  =('show' a)
      ~&  ^-  (map ship [[[@ud @ux] [@ud @ux]] deed:eth-noun])
          deeds
      [~ +>]
    ?:  =('verify linear' a)  [[(verify 'linear') ~] +>]
    ?:  =('verify conditional' a)  [[(verify 'conditional') ~] +>]
    !!
  ?:  =('list' wat.a)
    =/  lis=(list @p)  ((list @p) dat.a)
    [[(call lis %hull) ~] +>.$]
    ::TODO  this might end up giving you a ton of calls still, if a galaxy
    ::      is involved. fix that (special "no-recurse" flag) if it becomes
    ::      a problem.
  !!
::
++  write-file
  :*  ost.bol
      %info
      /write
      our.bol
      %home
    ::
      =-  &+[/chain/txt -]~
      =-  %+  feel:space:userlib
            /(scot %p our.bol)/home/(scot %da now.bol)/chain/txt
          txt+!>(-)
      ^-  (list @t)
      %+  weld
        :-  'locked up:'
        %+  murn  (gulf 0x0 0xff)
        |=  gal=ship
        ^-  (unit @t)
        ?.  (~(has by deeds) gal)  ~
        =+  (~(got by deeds) gal)
        =-  ::  [con=@ud lin=@ud]
          =|  count=tape
          =?  count  !=(0 con)  "{(scow %ud con)} in conditional lockup; "
          =?  count  !=(0 lin)  "{(scow %ud lin)} in linear lockup"
          ?~  count  ~
          =-  `(crip (weld - count))
          "{(scow %ud gal)} aka {(scow %p gal)}'s stars: "
        %+  roll  (gulf 0x1 0xff)
        |=  [suf=@ con=@ud lin=@ud]
        =+  sar=(cat 3 gal suf)
        ?.  (~(has by deeds) sar)  [con lin]
        =+  (~(got by deeds) sar)
        ?:  =(conditional-sr owner)  [+(con) lin]
        ?:  =(linear-sr owner)  [con +(lin)]
        [con lin]
      :-  'deeded:'
      =/  hout
        |=  num=@
        ?:  =(0x0 num)  "\"\""
        ?:  =(0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964 num)
          "tmp held by ceremony"
        (address-to-hex num)
      =/  kout
        |=  key=octs
        ?:  =('' q.key)  "\"\""
        ((x-co:co 64) q.key)
      %+  murn  (sort ~(tap in ~(key by deeds)) lth)
      |=  who=ship
      ^-  (unit @t)
      =+  (~(got by deeds) who)
      ?:  ?|  =(conditional-sr owner)
              =(linear-sr owner)
          ==
        ~
      :-  ~
      %-  crip
      ;:  weld
        ((d-co:co 1) who)  ","        ::  ship,
      ::
        ?+  (clan:title who)  !!      ::  shipGlass (STAR, PLANET, GALAXY),
          %czar  "galaxy"
          %king  "star"
          %duke  "planet"
        ==
      ::
        ","  "id"                     ::  idCode
        ","  (hout owner)             ::  ownership,
        ","  (hout transfer-proxy)    ::  transfer,
        ","  (hout spawn-proxy)       ::  spawn,
        ","  (hout management-proxy)  ::  mgmt,
        ","  (hout voting-proxy)      ::  voting,
        ","  (kout auth)              ::  auth,
        ","  (kout crypt)             ::  crypt,
        ","  "code"                   ::  confirmationCode,
        ","  "timestamp"              ::  confirm
      ==
  ==
::
++  initial-call
  (call (gulf 0x0 0xff) %hull)
::
++  call
  |=  [who=(list ship) wat=?(%hull %kids %deed)]
  ^-  move
  %+  ask-node  [wat ~]
  %+  turn  who
  |=  who=ship
  ^-  proto-read-request
  :+  `(scot %p who)
    azimuth
  :_  [%uint `@`who]~
  ?-  wat
    %hull  'points(uint32)'
    %kids  'getSpawned(uint32)'
    %deed  'rights(uint32)'
  ==
::
++  verify
  |=  wat=@t
  ^-  move
  %+  ask-node  /verify/[wat]
  %+  murn
    .^((list @t) %cx /(scot %p our.bol)/home/(scot %da now.bol)/[`@ta`wat]/txt)
  |=  l=@t
  ^-  (unit proto-read-request)
  ?:  =('' l)  ~
  :-  ~
  :+  `l
    ?:  =('linear' wat)  linear-sr
    ?:  =('conditional' wat)  conditional-sr
    !!
  :-  'verifyBalance(address)'
  ~|  l
  :~  address+(rash l ;~(pfix (jest '0x') hex))
  ==
::
++  ask-node
  |=  [wir=wire req=(list proto-read-request)]
  ^-  move
  :-  ost.bol
  :^  %hiss  wir  ~
  :+  %json-rpc-response  %hiss
  ^-  hiss:eyre
  %+  json-request
    (need (de-purl:html parity))
  (batch-read-request req)
::
++  sigh-json-rpc-response-verify
  |=  [wir=wire res=response:json-rpc]
  ^-  [(list move) _+>]
  ~|  ?:(?=(?(%error %fail) -.res) res -.res)
  ?>  ?=(%batch -.res)
  =+  ^-  (list @t)
      %+  murn  bas.res
      |=  r=response:json-rpc
      ^-  (unit @t)
      ?>  ?=(%result -.r)
      ?>  ?=(%s -.res.r)
      ?:  .=  p.res.r
          '0x0000000000000000000000000000000000000000000000000000000000000001'
        `id.r
      ~&  [%incomplete id.r]
      ~
  =-  [[- ~] +>.$]
  %+  ask-node  (weld /verify2 wir)
  %+  turn  -
  |=  id=@t
  ^-  proto-read-request
  :+  `id
    ?+  wir  ~|(wir !!)
      [%linear *]  linear-sr
      [%conditional *]  conditional-sr
    ==
  :-  ?+  wir  ~|(wir !!)
        [%linear *]  'batches(address)'
        [%conditional *]  'commitments(address)'
      ==
  :~  address+(rash id ;~(pfix (jest '0x') hex))
  ==
::
++  sigh-json-rpc-response-verify2
  |=  [wir=wire res=response:json-rpc]
  ^-  [(list move) _+>]
  ~|  ?:(?=(?(%error %fail) -.res) res -.res)
  ?>  ?=(%batch -.res)
  =-  ~&  [%done2 (lent bas.res)]
      [~ +>.$]
  %+  turn  bas.res
  |=  r=response:json-rpc
  ~|  ?:(?=(?(%error %fail) -.r) r -.r)
  ?>  ?=(%result -.r)
  ?>  ?=(%s -.res.r)
  ?+  wir  ~|(wir !!)
      [%linear *]
    =+  ^-  [@ud @ud @ud @ud amount=@ud @ux]
        %+  decode-results  `@t`p.res.r
        [%uint %uint %uint %uint %uint %address ~]
    ?.  =(0 amount)  ~
    ~&  [%unregistered-linear id.r]
    ~
  ::
      [%conditional *]
    =+  ^-  [@ud @ux total=@ud @ud]
        %+  decode-results  `@t`p.res.r
        [%uint %address %uint %uint ~]
    ?.  =(0 total)  ~
    ~&  [%unregistered-conditional id.r]
    ~
  ==
::
++  sigh-json-rpc-response
  |=  [wir=wire res=response:json-rpc]
  ^-  [(list move) _+>]
  ~|  ?:(?=(?(%error %fail) -.res) res -.res)
  ?>  ?=(%batch -.res)
  %.  %+  turn  bas.res
      |=  r=response:json-rpc
      ^-  [ship json]
      ?>  ?=(%result -.r)
      [(slav %p id.r) res.r]
  ?+  wir  !!
    [%hull *]  hear-hulls
    [%deed *]  hear-deeds
    [%kids *]  hear-kids
  ==
::
++  hear-hulls
  |=  hus=(list [ship json])
  ^-  [(list move) _+>]
  =;  liv=(list [who=ship crypt=octs auth=octs])
    :_  =-  +>.$(deeds (~(gas in deeds) -))
        %+  turn  liv
        |=  [who=ship keys=[octs octs]]
        ^-  (pair ship [[octs octs] deed:eth-noun])
        [who keys *deed:eth-noun]
    ~&  ['active ships:' (lent liv)]
    ?:  =(~ liv)  ~  ::  ?~ is tmi reeeee
    :~  (call (turn liv head) %deed)
      ::
        =-  (call - %kids)
        %+  skip  (turn liv head)
        |=(w=ship ?=(%duke (clan:title w)))
    ==
  %+  murn  hus
  |=  [who=ship hul=json]
  ^-  (unit [ship octs octs])
  :: ~|  hul
  ?>  ?=(%s -.hul)
  =-  ?:(active `[who crypt auth] ~)
  ^-  $:  crypt=octs
          auth=octs
          has-sponsor=?
          active=?
          escape-requested=?
          sponsor=@
          escape-requested-to=@
          crypto-suite-version=@
          key-revision-number=@
          continuity-number=@
      ==
  %+  decode-results  p.hul
  ~[[%bytes-n 32] [%bytes-n 32] %bool %bool %bool %uint %uint %uint %uint %uint]
::
++  hear-deeds
  |=  des=(list [ship json])
  ^-  [(list move) _+>]
  =,  constitution
  =-  [~ +>.$(deeds -)]
  %-  ~(gas by deeds)
  %+  turn  des
  |=  [who=ship ded=json]
  ^-  (pair ship [[octs octs] deed:eth-noun])
  :: ~|  ded
  ?>  ?=(%s -.ded)
  =+  `deed:eth-noun`(decode-results p.ded deed:eth-type)
  ~|  [%wtf-no-owner who]
  ?<  =(0x0 owner)
  [who [crypt auth]:(~(got by deeds) who) -]
::
++  hear-kids
  |=  kis=(list [ship json])
  ^-  [(list move) _+>]
  =;  lis=(list ship)
    ~&  ['child ships:' (lent lis)]
    ::  doing them all in one request is likely to lead to a meme eventually
    ::  but doing individual request for each has too much overhead
    ::  so we send them out in groups of 255 instead
    next-in-queue(queue (weld queue lis))
  %-  zing
  %+  turn  kis
  |=  [ship kid=json]
  ^-  (list ship)
  ?>  ?=(%s -.kid)
  %-  (list @)  ::NOTE  yes, i know this is bad, but output is (list)...
  %+  decode-results  `@t`p.kid
  [[%array %uint] ~]
::
++  next-in-queue
  ^-  [(list move) _.]
  ~&  ['popping queue, remaining: ' (lent queue)]
  ?:  =(~ queue)  [~ .]
  :-  [(call (scag 0xff queue) %hull) ~]
  ..prep(queue (slag 0xff queue))
::
::TODO  flow
::    read galaxy table
::    for each active galaxy, read deed, read spawned
::    for each spawned star, read hull, deed, spawned
::    for each spawned planet, read hull, deed
--
