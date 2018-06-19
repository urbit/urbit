!:
::
:: moves and state
::
|%
+=  move  (pair bone card)
+=  poke  $%  [%dns-bind for=ship him=ship target]
              [%dns-bond for=ship him=ship turf]
              [%dns-authority authority]
          ==
+=  card  $%  [%tend wire ~]
              [%poke wire dock poke]
              [%hiss wire [~ ~] %httr %hiss hiss:eyre]
          ==
:: +turf: a domain, TLD first
::
+=  turf  (list @t)
:: +provider: DNS service provider (gcloud only for now)
::
+=  provider
  $%  [%gcloud project=@ta zone=@ta]
  ==
:: +authority: responsibility for a DNS zone
::
+=  authority
  $:  :: dom: authority over a fully-qualified domain
      ::
      dom=turf
      :: pro: DNS service provider
      ::
      pro=provider
  ==
:: +target: a ship is bound to a ...
::
+=  target
  $%  :: %direct: an A record
      ::
      [%direct %if p=@if]
      :: %indirect: a CNAME record
      ::
      [%indirect p=ship]
  ==
:: +bound: an established binding, plus history
::
+=  bound
  $:  :: wen: established
      ::
      wen=@da
      :: cur: current target
      ::
      cur=target
      :: hit: historical targets
      ::
      hit=(list (pair @da target))
  ==
:: +nameserver: a b s o l u t e  p o w e r
::
+=  nameserver
  $:  aut=authority
      pen=(map ship target)
      bon=(map ship bound)
  ==
:: +relay: a good parent keeps track
::
+=  relay
  $:  wen=@da
      wer=(unit @if)
      bon=?
      tar=target
  ==
:: +state: complete app state
::
+=  state
  $:  :: dom: the set of our bindings
      ::
      dom=(set turf)
      :: per: per-dependent ips &c
      ::
      per=(map ship relay)
      :: nem: authoritative state
      ::
      nem=(unit nameserver)
  ==
:: +join: dedup with :acme
::
++  join
  |=  [sep=@t hot=(list @t)]
  ^-  @t
  ?>  ?=(^ hot)
  %+  rap  3
  |-  ^-  (list @t)
  ?~  t.hot  hot
  [i.hot sep $(hot t.hot)]
:: |gcloud: provider-specific functions
::
++  gcloud
  |%
  ::  +base: provider service endpoint
  ::
  ++  base
    (need (de-purl:html 'https://www.googleapis.com/dns/v1/projects'))
  :: +name: fully-qualified domain name
  ::
  ++  name
    |=  [dom=turf him=ship]
    (cat 3 (join '.' [(crip +:(scow %p him)) (flop dom)]) '.')
  :: +record: JSON-formatted provider-specific dns record
  ::
  ++  record
    |=  [dom=turf him=ship tar=target]
    ^-  json
    =+  ^-  [typ=cord dat=cord]
      ?:  ?=(%direct -.tar)
        ['A' (crip +:(scow %if p.tar))]
      ['CNAME' (name dom p.tar)]
    :-  %o  %-  my  :~
      name+s+(name dom him)
      type+s+typ
      :: XX make configureable?
      ttl+n+~.300
      rrdatas+a+[s+dat ~]
    ==
  :: +request: provider-specific record-creation request
  ::
  ++  request
    =,  eyre
    |=  [dom=turf him=ship tar=target pro=provider]
    ^-  hiss
    ?>  ?=([%gcloud *] pro)
    =/  url=purl
      =+  base
      -(q.q (weld q.q.- /[project.pro]/['managedZones']/[zone.pro]/changes))
    =/  hed=math
      (my content-type+['application/json' ~] ~)
    =/  bod=octs
      %-  as-octt:mimes:html
      %-  en-json:html
      o+(my additions+a+[(record dom him tar) ~] ~)
    [url %post hed `bod]
  --
--
::
::  the app itself
::
|_  [bow=bowl:gall state]
++  this  .
:: +poke-noun: debugging
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?+  a  ~&  +<+:this
         [~ this]
  ::
      %aut
    :_  this  :_  ~
    :*  ost.bow
        %poke
        /foo
        [our.bow dap.bow]
        %dns-authority
        [/org/urbit/dyndns %gcloud %tonal-griffin-853 %dyndns]
    ==
  ::
      %bin
    :_  this  :_  ~
    :*  ost.bow
        %poke
        /bar
        [our.bow dap.bow]
        %dns-bind
        :: [for=~binzod him=~ridbyl-dovwyd tar=[%indirect p=~binzod]]
        [for=~binzod him=~ridbyl-dovwyd tar=[%direct %if .8.8.8.8]]
    ==
  ==
:: +sigh-httr: accept http response
::
++  sigh-httr
  |=  [wir=wire rep=httr:eyre]
  ^-  (quip move _this)
  ?-  wir
      [%authority %confirm ~]
    ?~  nem
      ~&  [%strange-authority wire=wir response=rep]
      [~ this]
    ?.  =(200 p.rep)
      ~&  [%authority-confirm-fail rep]
      [~ this(nem ~)]
    :: XX anything to do here? parse body?
    ~&  %authority-confirmed
    [~ this]
  ::
      [%authority %create @ %for @ ~]
    ?~  nem
      ~&  [%strange-authority wire=wir response=rep]
      [~ this]
    ?.  =(200 p.rep)
      ~&  [%authority-create-fail wire=wir response=rep]
      [~ this]
    =/  him=ship  (slav %p i.t.t.wir)
    =/  for=ship  (slav %p i.t.t.t.t.wir)
    abet:(~(confirm bind u.nem) for him)
  ::
      *
    ~&  +<
    [~ this]
  ==
:: +sigh-tang: failed to make http request
::
++  sigh-tang
  |=  [wir=wire saw=tang]
  ^-  (quip move _this)
  ~&  [%sigh-tang wir]
  ?+  wir
        [((slog saw) ~) this]
  ::
      [%authority %confirm ~]
    ~&  %authority-confirm-fail
    [((slog saw) ~) this(nem ~)]
  ==
::
:: +poke-dns-authority: configure self as an authority
::
++  poke-dns-authority
  |=  aut=authority
  ^-  (quip move _this)
  ~|  %authority-reset-wat-do
  ?<  ?=(^ nem)
  abet:(init:bind aut)
:: +poke-dns-bind: create binding (if authority), forward request
::
++  poke-dns-bind
  |=  [for=ship him=ship tar=target]
  ^-  (quip move _this)
  ~&  [%bind src=src.bow +<.$]
  =/  lan  (clan:title him)
  ?:  ?=(%czar lan)
    ~|(%bind-galazy !!)
  ?:  =(for him)
    ~|(%bind-yoself !!)
  ?:  ?&  ?=(%king lan)
          ?=(%indirect -.tar)
      ==
    ~|(%bind-indirect-star !!)
  :: always forward, there may be multiple authorities
  ::
  =^  zom=(list move)  ..this
    abet:(~(forward tell him ~) for tar)
  =^  zam=(list move)  ..this
    ?~  nem  [~ this]
    abet:(~(create bind u.nem) for him tar)
  [(weld zom zam) this]
:: +poke-dns-bond: process established dns binding
::
++  poke-dns-bond
  |=  [for=ship him=ship dom=turf]
  ^-  (quip move _this)
  ~&  [%bond +<]
  ~|  %bond-yoself
  ?<  =(for him)
  ?:  =(our.bow him)
    :: XX notify eyre/hood/acme etc
    ~&  [%bound-us dom]
    :-  ~
    this(dom (~(put in ^dom) dom))
  ?:  =(our.bow for)
    ~&  [%bound-him him dom]
    =<  abet
    (~(bake tell [him (~(get by per) him)]) dom)
  ~&  [%strange-bond +<]
  [~ this]
:: +coup: general poke acknowledgement or error
::
++  coup
  |=  [wir=wire saw=(unit tang)]
  ?~  saw  [~ this]
  ~&  [%coup-fallthru wir]
  [((slog u.saw) ~) this]
:: +rove: hear %ames +lane change for child ships
::
++  rove
  |=  [wir=wire p=ship q=lane:ames]
  ^-  (quip move _this)
  ?.  =(our.bow (sein:title p))  :: XX check will
    ~&  [%rove-false p]
    [~ this]
  ~&  [%rove wir p q]
  ::  XX assert that we intend to be listening?
  =<  abet
  (~(hear tell [p (~(get by per) p)]) q)
:: +prep: adapt state
::
:: ++  prep  _[~ this]
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?^  old
    [~ this(+<+ u.old)]
  ?:  ?=(?(%czar %king) (clan:title our.bow))
    abet:listen:tell
  [~ this]
:: |bind: acting as zone authority
::
++  bind
  =|  moz=(list move)
  |_  nam=nameserver
  ++  this  .
  :: +abet: finalize state changes, produce moves
  ::
  ++  abet
    ^-  (quip move _^this)
    [(flop moz) ^this(nem `nam)]
  :: +emit: emit a move
  ::
  ++  emit
    |=  car=card
    ~&  [%emit-bind car]
    ^+  this
    this(moz [[ost.bow car] moz])
  :: +emil: emit a list of moves
  ::
  ++  emil
    |=  rac=(list card)
    q:(spin rac this |=([a=card b=_this] [~ (emit:b a)]))
  :: +init: establish zone authority (request confirmation)
  ::
  ++  init
    |=  aut=authority
    :: ?>  ?=(%gcloud pro.aut)
    =/  wir=wire  /authority/confirm
    =/  url=purl:eyre  base:gcloud
    =.  q.q.url
      %+  weld  q.q.url
      /[project.pro.aut]/['managedZones']/[zone.pro.aut]
    ~&  url
    %-  emit(nam [aut ~ ~])
    [%hiss wir [~ ~] %httr %hiss url %get ~ ~]
  :: +create: bind :him, on behalf of :for
  ::
  ++  create
    |=  [for=ship him=ship tar=target]
    :: XX defer %indirect where target isn't yet bound
    ?>  ?|  ?=(%direct -.tar)
            (~(has by bon.nam) p.tar)
        ==
    =/  wir=wire
      /authority/create/(scot %p him)/for/(scot %p for)
    =/  req=hiss:eyre
      (request:gcloud dom.aut.nam him tar pro.aut.nam)
    %-  emit(pen.nam (~(put by pen.nam) him tar)) :: XX save for
    [%hiss wir [~ ~] %httr %hiss req]
  :: +confirm: successfully bound
  ::
  ++  confirm
    |=  [for=ship him=ship]
    =/  tar=target  (~(got by pen.nam) him)
    =/  bon=(unit bound)
      (~(get by bon.nam) him)
    =/  nob=bound
      [now.bow tar ?~(bon ~ [[wen.u.bon cur.u.bon] hit.u.bon])]
    =.  pen.nam  (~(del by pen.nam) him)
    =.  bon.nam  (~(put by bon.nam) him nob)
    =/  wir=wire
      /forward/bound/(scot %p him)/for/(scot %p for)
    =/  pok=poke
      [%dns-bond him for *turf]
    %-  emil  :~
      [%poke wir [him dap.bow] pok]
      [%poke wir [for dap.bow] pok]
    ==
  --
:: |tell: acting as planet parent or relay
::
++  tell
  =|  moz=(list move)
  |_  [him=ship rel=(unit relay)]
  ++  this  .
  :: +abet: finalize state changes, produce moves
  ::
  ++  abet
    ^-  (quip move _^this)
    :-  (flop moz)
    ?~  rel
      ^this
    ^this(per (~(put by per) him u.rel))
  :: +emit: emit a move
  ::
  ++  emit
    |=  car=card
    ~&  [%emit-tell car]
    ^+  this
    this(moz [[ost.bow car] moz])
  :: +listen: subscribe to %ames +lane changes for child ships
  ::
  ++  listen
    ^+  this
    (emit [%tend /tend ~])
  :: +hear: hear +lane change, maybe emit binding request
  ::
  ++  hear
    |=  lan=lane:ames
    ^+  this
    =/  adr=(unit @if)
      ?.(?=([%if *] lan) ~ `r.lan)
    =/  tar=target
      ?:  ?|  ?=(~ adr)
              ?=(%duke (clan:title him))
          ==
        [%indirect our.bow]
      [%direct %if u.adr]
    =/  ler=relay
      [now.bow adr | tar]
    ?.  ?|  ?=(~ rel)
            !=(tar tar.u.rel)
        ==
      this
    :: we may be an authority, so we poke ourselves
    =/  wir=wire
      /bind/(scot %p him)/for/(scot %p our.bow)
    %-  emit(rel `ler)
    [%poke wir [our.bow dap.bow] %dns-bind our.bow him tar]
  :: +bake: successfully bound
  ::
  ++  bake
    |=  dom=turf
    ~&  [%bake dom]
    ^+  this
    ?>  ?=(^ rel)
    =.  bon.u.rel  &
    :: XX save domain?
    :: XX notify ship?
    this
  :: +forward: sending binding request up the network
  ::
  ++  forward
    |=  [for=ship tar=target]
    ~&  [%forward tar]
    ^+  this
    ?:  ?=(%~zod our.bow)  ::  ~zod don't forward
      ~&  [%zod-no-forward him tar]
      this
    =/  to=ship
      ?-  (clan:title our.bow)
        %czar  ~zod
        *      (sein:title our.bow)
      ==
    =/  wir=wire
      /forward/bind/(scot %p him)/for/(scot %p for)
    %-  emit  :: XX for
    [%poke wir [to dap.bow] %dns-bind for him tar]
  --
--
