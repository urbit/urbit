section 3bI, Arvo models
========================

<h3 id="++acru"><code>++acru</code></h3>

    ++  acru                                                ::  asym cryptosuite
              $_  ^?  |%                                    ::  opaque object

Cryptosuite interface, see %ames documentation

    ~zod/main=> `acru`crua
    <6?guc 243.nxm 41.spz 374.iqw 100.rip 1.ypj %164>
    ~zod/main=> `acru`crub
    <6?guc 243.nxm 41.spz 374.iqw 100.rip 1.ypj %164>
    ~zod/main=> *acru
    <6?guc 243.nxm 41.spz 374.iqw 100.rip 1.ypj %164>

<h3 id="++as"><code>++as</code></h3>

              ++  as  ^?                                    ::  asym ops
                |%  ++  seal  |=([a=pass b=@ c=@] _@)       ::  encrypt to a

XX document

<h3 id="++seal"><code>++seal</code></h3>

    ++  seal                                                ::  auth conversation
              $:  whu=(unit ship)                           ::  client identity
                  pul=purl                                  ::  destination url
                  wit=?                                     ::  wait for partner
                  foy=(unit ,[p=ship q=hole])               ::  partner to notify
                  pus=(unit ,@ta)                           ::  password
              ==                                            ::

XX document

<h3 id="++sign"><code>++sign</code></h3>

                    ++  sign  |=([a=@ b=@] _@)              ::  certify as us

XX document

<h3 id="++sure"><code>++sure</code></h3>

                    ++  sure  |=([a=@ b=@] *(unit ,@))      ::  authenticate from us

XX document

<h3 id="++tear"><code>++tear</code></h3>

                    ++  tear  |=  [a=pass b=@]              ::  accept from a 
                              *(unit ,[p=@ q=@])            ::

XX document

<h3 id="++de"><code>++de</code></h3>

              ++  de  |+([a=@ b=@] *(unit ,@))              ::  symmetric de, soft

XX document

<h3 id="++dy"><code>++dy</code></h3>

              ++  dy  |+([a=@ b=@] _@)                      ::  symmetric de, hard

XX document

<h3 id="++en"><code>++en</code></h3>

              ++  en  |+([a=@ b=@] _@)                      ::  symmetric en

XX document

<h3 id="++ex"><code>++ex</code></h3>

              ++  ex  ^?                                    ::  export
                |%  ++  fig  _@uvH                          ::  fingerprint

XX document

<h3 id="++fig"><code>++fig</code></h3>


    XX document

    ###++pac

    ```
                    ++  pac  _@uvG                          ::  default passcode
    ```

    XX document

    ###++pub

    ```
                    ++  pub  *pass                          ::  public key
    ```

    XX document

    ###++sec

    ```
                    ++  sec  *ring                          ::  private key
    ```

    XX document

    ###++nu

    ```
              ++  nu  ^?                                    ::  reconstructors
                 |%  ++  pit  |=([a=@ b=@] ^?(..nu))        ::  from [width seed]
    ```

    XX document

    ###++pit

XX document

<h3 id="++nol"><code>++nol</code></h3>

                     ++  nol  |=(a=@ ^?(..nu))              ::  from naked ring

XX document

<h3 id="++com"><code>++com</code></h3>

                     ++  com  |=(a=@ ^?(..nu))              ::  from naked pass

XX document

<h3 id="++aeon"><code>++aeon</code></h3>

    ++  aeon  ,@ud                                          ::

Clay revision number

<h3 id="++agon"><code>++agon</code></h3>

    ++  agon  (map ,[p=ship q=desk] ,[p=@ud q=@ud r=waks])  ::  mergepts

See %clay doc

    ~zod/main=> *agon
    {}

<h3 id="++ankh"><code>++ankh</code></h3>

    ++  ankh                                                ::  fs node (new)
              $:  p=cash                                    ::  recursive hash
                  q=(unit ,[p=cash q=*])                    ::  file
                  r=(map ,@ta ankh)                         ::  folders
              ==                                            ::

State at path

See also ++ze, %clay documentation

<h3 id="++ankz"><code>++ankz</code></h3>

    ++  ankz  ,[p=@ (map ,@ta ankz)]                        ::  trimmed ankh

XX document

<h3 id="++apex"><code>++apex</code></h3>

    ++  apex  ,[p=@uvI q=(map ,@ta ,@uvI) r=(map ,@ta ,~)]  ::  node report (old)

XX document

<h3 id="++ares"><code>++ares</code></h3>

    ++  ares  (unit ,[p=term q=(list tank)])                ::  possible error

Failure cause: unknown, or machine-readable term and stack trace.

    ~zod/main=> `ares`~
    ~
    ~zod/main=> `ares`[~ %syntax-error leaf/"[1 27]" ~]
    [~ [p=%syntax-error q=~[[%leaf p="[1 27]"]]]]
    ~zod/main=> 

<h3 id="++ball"><code>++ball</code></h3>

    ++  ball  ,@uw                                          ::  statement payload

XX document

<h3 id="++bait"><code>++bait</code></h3>

    ++  bait  ,[p=skin q=@ud r=dove]                        ::  fmt nrecvd spec

XX document

<h3 id="++bath"><code>++bath</code></h3>

    ++  bath                                                ::  convo per client
              $:  sop=shed                                  ::  not stalled
                  raz=(map path race)                       ::  statements inbound
                  ryl=(map path rill)                       ::  statements outbound
              ==                                            ::

XX document

<h3 id="++beam"><code>++beam</code></h3>

    ++  beam  ,[[p=ship q=desk r=case] s=path]              ::  global name

See section 2dF, %clay documentation

    ~zod/try=> (need (tome %/bin))
    [[p=~zod q=%try r=[%da p=~2014.11.3..17.30.07..ca8f]] s=/bin]

<h3 id="++beak"><code>++beak</code></h3>

    ++  beak  ,[p=ship q=desk r=case]                       ::  garnish with beak

Global root

<h3 id="++bird"><code>++bird</code></h3>

    ++  bird                                                ::  packet in travel
              $:  gom=soap                                  ::  message identity
                  mup=@ud                                   ::  pktno in msg
                  nux=@ud                                   ::  xmission count
                  lys=@da                                   ::  last sent
                  pac=rock                                  ::  packet data
              ==                                            ::

XX document

<h3 id="++blob"><code>++blob</code></h3>

    ++  blob  $%  [%delta p=lobe q=lobe r=udon]             ::  delta on q
                  [%direct p=lobe q=* r=umph]               ::
                  [%indirect p=lobe q=* r=udon s=lobe]      ::
              ==                                            ::

Stored data, see ++ze

<h3 id="++boat"><code>++boat</code></h3>

    ++  boat  ,[(list slip) tart]                           ::  user stage

XX deprecated

<h3 id="++boon"><code>++boon</code></h3>

    ++  boon                                                ::  fort output
              $%  [%beer p=ship q=@uvG]                     ::  gained ownership
                  [%cake p=sock q=soap r=coop s=duct]       ::  e2e message result
                  [%coke p=sock q=soap r=cape s=duct]       ::  message result
                  [%mead p=lane q=rock]                     ::  accept packet
                  [%milk p=sock q=soap r=*]                 ::  accept message
                  [%mulk p=sock q=soap r=*]                 ::  e2e pass message
                  [%ouzo p=lane q=rock]                     ::  transmit packet
                  [%wine p=sock q=tape]                     ::  notify user
              ==                                            ::

See %ford documentation

<h3 id="++bowl"><code>++bowl</code></h3>

    ++  bowl  ,[p=(list gift) q=(unit boat)]                ::  app product

XX deprecated

<h3 id="++bray"><code>++bray</code></h3>

    ++  bray  ,[p=life q=(unit life) r=ship s=@da]          ::  our parent us now

Ship identity. See %ames documentation

<h3 id="++brow"><code>++brow</code></h3>

    ++  brow  ,[p=@da q=@tas]                               ::  browser version

XX unused?

<h3 id="++buck"><code>++buck</code></h3>

    ++  buck  ,[p=mace q=will]                              ::  all security data

XX document

<h3 id="++cake"><code>++cake</code></h3>

    ++  cake  ,[p=sock q=skin r=@]                          ::  top level packet

XX document

<h3 id="++cape"><code>++cape</code></h3>

    ++  cape                                                ::  end-to-end result
              $?  %good                                     ::  delivered
                  %dead                                     ::  rejected
              ==                                            ::

XX document

<h3 id="++cart"><code>++cart</code></h3>

    ++  cart  ,[p=cash q=cash]                              ::  hash change

XX document

<h3 id="++care"><code>++care</code></h3>

    ++  care  ?(%u %v %w %x %y %z)                          ::  clay submode

XX document

<h3 id="++case"><code>++case</code></h3>

    ++  case                                                ::  ship desk case spur
              $%  [%da p=@da]                               ::  date
                  [%tas p=@tas]                             ::  label
                  [%ud p=@ud]                               ::  number
              ==                                            ::

Access by absolute date, term label, or revision number. See %clay
documentation

<h3 id="++cash"><code>++cash</code></h3>

    ++  cash  ,@uvH                                         ::  ankh hash

XX document

<h3 id="++chum"><code>++chum</code></h3>

    ++  chum  ,@uvI                                         ::  hashed passcode

XX document

<h3 id="++clot"><code>++clot</code></h3>

    ++  clot                                                ::  symmetric record
              $:  yed=(unit ,[p=hand q=code])               ::  outbound
                  heg=(map hand code)                       ::  proposed
                  qim=(map hand code)                       ::  inbound
              ==                                            ::

XX document

<h3 id="++coal"><code>++coal</code></h3>

    ++  coal  ,*                                            ::  untyped vase

XX document

<h3 id="++code"><code>++code</code></h3>

    ++  code  ,@uvI                                         ::  symmetric key

XX document

<h3 id="++cone"><code>++cone</code></h3>

    ++  cone                                                ::  reconfiguration
              $%  [& p=twig]                                ::  transform
                  [| p=(list ,@tas)]                        ::  alter
              ==                                            ::

XX document

<h3 id="++chum"><code>++chum</code></h3>

    ++  chum  ,@uvI                                         ::  hashed passcode

XX document

<h3 id="++claw"><code>++claw</code></h3>

    ++  claw                                                ::  startup chain
              $:  joy=(unit coal)                           ::  local context
                  ran=(unit coal)                           ::  arguments
                  pux=(unit path)                           ::  execution path
                  jiv=(unit coal)                           ::  app configuration
                  kyq=(unit coal)                           ::  app customization
                  gam=(unit coal)                           ::  app image
              ==                                            ::

XX document

<h3 id="++clip"><code>++clip</code></h3>

    ++  clip  (each ,@if ,@is)                              ::  client IP

See %eyre documentation.

    ~zod/try=> `clip`[%& .127.0.0.1]
    [%.y p=.127.0.0.1]
    ~zod/try=> `clip`[%| .12.0.0.0.342d.d24d.0.0]
    [%.n p=.12.0.0.0.342d.d24d.0.0]
    ~zod/try=> `clip`[%| .0.0.0.1]
    ! type-fail
    ! exit

<h3 id="++coal"><code>++coal</code></h3>

    ++  coal  ,*                                            ::  untyped vase

XX document

<h3 id="++code"><code>++code</code></h3>

    ++  code  ,@uvI                                         ::  symmetric key

XX document

<h3 id="++cone"><code>++cone</code></h3>

    ++  cone                                                ::  reconfiguration
              $%  [& p=twig]                                ::  transform
                  [| p=(list ,@tas)]                        ::  alter
              ==                                            ::

XX document

<h3 id="++coop"><code>++coop</code></h3>

    ++  coop  (unit ares)                                   ::  e2e ack

XX document

<h3 id="++corn"><code>++corn</code></h3>

    ++  corn                                                ::  flow by server
              $:  hen=duct                                  ::  admin channel
                  nys=(map flap bait)                       ::  packets incoming
                  olz=(map flap cape)                       ::  packets completed
                  wab=(map ship bath)                       ::  relationship
              ==                                            ::

XX document

<h3 id="++cred"><code>++cred</code></h3>

    ++  cred                                                ::  credential
              $:  hut=hoot                                  ::  client host
                  aut=(jug ,@tas ,@t)                       ::  client identities
                  orx=oryx                                  ::  CSRF secret
                  acl=(unit ,@t)                            ::  accept-language
                  cip=(each ,@if ,@is)                      ::  client IP
                  cum=(map ,@tas ,*)                        ::  custom dirt
              ==                                            ::

XX document

<h3 id="++cuff"><code>++cuff</code></h3>

    ++  cuff                                                ::  permissions
              $:  p=(unit (set monk))                       ::  readers
                  q=(set monk)                              ::  authors
              ==                                            ::

XX document

<h3 id="++deed"><code>++deed</code></h3>

    ++  deed  ,[p=@ q=step r=?]                             ::  sig, stage, fake?

XX document

<h3 id="++dome"><code>++dome</code></h3>

    ++  dome                                                ::  project state
              $:  ang=agon                                  ::  pedigree
                  ank=ankh                                  ::  state
                  let=@ud                                   ::  top id
                  hit=(map ,@ud tako)                       ::  changes by id
                  lab=(map ,@tas ,@ud)                      ::  labels
              ==                                            ::

XX document

<h3 id="++dore"><code>++dore</code></h3>

    ++  dore                                                ::  foreign contact
              $:  wod=road                                  ::  connection to
                  wyl=will                                  ::  inferred mirror
                  caq=clot                                  ::  symmetric key state
              ==                                            ::

XX document

<h3 id="++dove"><code>++dove</code></h3>

    ++  dove  ,[p=@ud q=(map ,@ud ,@)]                      ::  count hash 13-blocks

XX document

<h3 id="++epic"><code>++epic</code></h3>

    ++  epic                                                ::  FCGI parameters
              $:  qix=(map ,@t ,@t)                         ::  query
                  ced=cred                                  ::  client credentials
                  bem=beam                                  ::  original path
                  but=path                                  ::  ending
                  nyp=path                                  ::  request model
              ==                                            ::

XX document

<h3 id="++flap"><code>++flap</code></h3>

    ++  flap  ,@uvH                                         ::  network packet id

XX document

<h3 id="++flow"><code>++flow</code></h3>

    ++  flow                                                ::  packet connection
              $:  rtt=@dr                                   ::  decaying avg rtt
                  wid=@ud                                   ::  logical wdow msgs
              ==                                            ::

XX document

<h3 id="++fort"><code>++fort</code></h3>

    ++  fort                                                ::  formal state
              $:  %0                                        ::  version
                  gad=duct                                  ::  client interface
                  hop=@da                                   ::  network boot date
                  ton=town                                  ::  security
                  zac=(map ship corn)                       ::  flows by server
              ==                                            ::

XX document

<h3 id="++frog"><code>++frog</code></h3>

    ++  frog  ,[p=@da q=nori]                               ::  time and change

XX document

<h3 id="++gank"><code>++gank</code></h3>

    ++  gank  (each vase (list tank))                       ::  abstract result

XX document

<h3 id="++gift"><code>++gift</code></h3>

    ++  gift                                                ::  one-way effect
              $%  [%$ p=vase]                               ::  trivial output
                  [%cc p=(unit case)]                       ::  change case
                  [%ck p=@tas]                              ::  change desk
                  [%cs p=path]                              ::  change spur
                  [%de p=@ud q=tank]                        ::  debug/level
                  [%ex p=(unit vase) q=lath]                ::  exec/patch
                ::[%fd p=vase]                              ::  fundamental down
                ::[%fo p=vase]                              ::  fundamental forward
                ::[%fu p=vase]                              ::  fundamental up
                  [%ha p=tank]                              ::  single error
                  [%ho p=(list tank)]                       ::  multiple error
                  [%la p=tank]                              ::  single statement
                  [%lo p=(list tank)]                       ::  multiple statement
                  [%mu p=type q=(list)]                     ::  batch emit
                  [%mx p=(list gift)]                       ::  batch gift
                  [%ok p=@ta q=nori]                        ::  save changes
                  [%og p=@ta q=mizu]                        ::  save direct
                  [%sc p=(unit skit)]                       ::  stack library
                  [%sp p=(list lark)]                       ::  spawn task(s)
                  [%sq p=ship q=@tas r=path s=*]            ::  send request
                  [%sr p=ship q=path r=*]                   ::  send response
                  [%te p=(list ,@t)]                        ::  dump lines
                  [%th p=@ud q=love]                        ::  http response
                  [%tq p=path q=hiss]                       ::  http request
                  [%va p=@tas q=(unit vase)]                ::  set/clear variable
                  [%xx p=curd]                              ::  return card
                  [%xy p=path q=curd]                       ::  push card
                  [%xz p=[p=ship q=term] q=ship r=mark s=zang]
                  [%zz p=path q=path r=curd]                ::
              ==                                            ::

XX document

<h3 id="++zang"><code>++zang</code></h3>

    ++  zang                                                ::  XX evil hack
              $%  [%backlog p=path q=?(%da %dr %ud) r=@]    ::
                  [%hola p=path]                            ::
                  $:  %mess  p=path                         ::
                    $=  q                                   ::
                  $%  [%do p=@t]                            ::  act
                      [%exp p=@t q=tank]                    ::  code
                      [%say p=@t]                           ::  speak
                  ==  ==                                    ::
                  [%tint p=ship]                            ::
              ==                                            ::

XX document

<h3 id="++gilt"><code>++gilt</code></h3>

    ++  gilt  ,[@tas *]                                     ::  presumed gift

XX document

<h3 id="++gens"><code>++gens</code></h3>

    ++  gens  ,[p=lang q=gcos]                              ::  general identity

XX document

<h3 id="++germ"><code>++germ</code></h3>

    ++  germ  ?(%init %fine %that %this %mate %meld)        ::  merge style

XX document

<h3 id="++gcos"><code>++gcos</code></h3>

    ++  gcos                                                ::  id description
              $%  [%czar ~]                                 ::  8-bit ship
                  [%duke p=what]                            ::  32-bit ship
                  [%earl p=@t]                              ::  64-bit ship
                  [%king p=@t]                              ::  16-bit ship
                  [%pawn p=(unit ,@t)]                      ::  128-bit ship
              ==                                            ::

XX document

<h3 id="++goad"><code>++goad</code></h3>

    ++  goad                                                ::  common note
              $%  [%eg p=riot]                              ::  simple result
                  [%gr p=mark q=*]                          ::  gall rush/rust
                  [%hp p=httr]                              ::  http response
                  ::  [%ht p=@ud q=scab r=cred s=moth]          ::  http request
                  [%it p=~]                                 ::  interrupt event
                  [%lq p=ship q=path r=*]                   ::  client request
                  [%ly p=newt q=tape]                       ::  lifecycle event
                  [%ow p=cape]                              ::  one-way reaction
                  [%rt p=(unit)]                            ::  roundtrip response
                  [%up p=@t]                                ::  prompt response
                  [%wa ~]                                   ::  alarm
              ==                                            ::

XX document

<h3 id="++goal"><code>++goal</code></h3>

    ++  goal                                                ::  app request
              $%  [%$ p=type]                               ::  open for input
                  [%do p=vase q=vase]                       ::  call gate sample
                  [%eg p=kite]                              ::  single request
                  [%es p=ship q=desk r=rave]                ::  subscription
                  [%gr ~]                                   ::  gall response
                  [%ht p=(list rout)]                       ::  http server
                  [%hp ~]                                   ::  http response
                  [%lq p=@tas]                              ::  listen for service
                  [%ow ~]                                   ::  one-way reaction
                  [%rt ~]                                   ::  roundtrip response
                  [%up p=prod]                              ::  user prompt
                  [%wa p=@da]                               ::  alarm
              ==                                            ::

XX document

<h3 id="++govt"><code>++govt</code></h3>

    ++  govt  path                                          ::  country/postcode

XX document

<h3 id="++hand"><code>++hand</code></h3>

    ++  hand  ,@uvH                                         ::  hash of code

XX document

<h3 id="++hart"><code>++hart</code></h3>

    ++  hart  ,[p=? q=(unit ,@ud) r=host]                   ::  http sec/port/host

XX document

<h3 id="++hate"><code>++hate</code></h3>

    ++  hate  ,[p=purl q=@p r=moth]                         ::  semi-cooked request

XX document

<h3 id="++heir"><code>++heir</code></h3>

    ++  heir  ,[p=@ud q=mess r=(unit love)]                 ::  status/headers/data

XX document

<h3 id="++hiss"><code>++hiss</code></h3>

    ++  hiss  ,[p=purl q=moth]                              ::  outbound request

XX document

<h3 id="++hist"><code>++hist</code></h3>

    ++  hist  ,[p=@ud q=(list ,@t)]                         ::  depth texts

XX document

<h3 id="++hole"><code>++hole</code></h3>

    ++  hole  ,@t                                           ::  session identity

XX document

<h3 id="++hoot"><code>++hoot</code></h3>

    ++  hoot  ,[p=? q=(unit ,@ud) r=host]                   ::  secure/port/host

XX document

<h3 id="++hort"><code>++hort</code></h3>

    ++  hort  ,[p=(unit ,@ud) q=host]                       ::  http port/host

XX document

<h3 id="++host"><code>++host</code></h3>

    ++  host  $%([& p=(list ,@t)] [| p=@if])                ::  http host

XX document

<h3 id="++httq"><code>++httq</code></h3>

    ++  httq                                                ::  raw http request
              $:  p=meth                                    ::  method
                  q=@t                                      ::  unparsed url
                  r=(list ,[p=@t q=@t])                     ::  headers
                  s=(unit octs)                             ::  body
              ==                                            ::

XX document

<h3 id="++httr"><code>++httr</code></h3>

    ++  httr  ,[p=@ud q=mess r=(unit octs)]                 ::  raw http response

XX document

<h3 id="++httx"><code>++httx</code></h3>

    ++  httx                                                ::  encapsulated http
              $:  p=?                                       ::  https?
                  q=clip                                    ::  source IP
                  r=httq                                    ::
              ==                                            ::

XX document

<h3 id="++kite"><code>++kite</code></h3>

    ++  kite  ,[p=care q=case r=ship s=desk t=spur]         ::  parsed global name

XX document

<h3 id="++json"><code>++json</code></h3>

    ++  json                                                ::  normal json value
              $|  ~                                         ::  null
              $%  [%a p=(list json)]                        ::  array
                  [%b p=?]                                  ::  boolean
                  [%o p=(map ,@t json)]                     ::  object
                  [%n p=@ta]                                ::  number
                  [%s p=@ta]                                ::  string
              ==                                            ::

XX document

<h3 id="++jsot"><code>++jsot</code></h3>

    ++  jsot                                                ::  strict json top
              $%  [%a p=(list json)]                        ::  array
                  [%o p=(map ,@t json)]                     ::  object
              ==                                            ::

XX document

<h3 id="++lamb"><code>++lamb</code></h3>

    ++  lamb                                                ::  short path
              $%  [& p=@tas]                                ::  auto
                  [| p=twig]                                ::  manual
              ==                                            ::

XX document

<h3 id="++lane"><code>++lane</code></h3>

    ++  lane                                                ::  packet route
              $%  [%if p=@da q=@ud r=@if]                   ::  IP4/public UDP/addr
                  [%is p=@ud q=(unit lane) r=@is]           ::  IPv6 w/alternates
                  [%ix p=@da q=@ud r=@if]                   ::  IPv4 provisional
              ==                                            ::

XX document

<h3 id="++lang"><code>++lang</code></h3>

    ++  lang  ,@ta                                          ::  IETF lang as code

XX document

<h3 id="++lark"><code>++lark</code></h3>

    ++  lark  ,[p=(unit ,@tas) q=lawn]                      ::  parsed command

XX document

<h3 id="++lass"><code>++lass</code></h3>

    ++  lass  ?(%0 %1 %2)                                   ::  power increment

XX document

<h3 id="++lath"><code>++lath</code></h3>

    ++  lath  $%                                            ::  pipeline stage
                  [%0 p=lass q=lamb r=(list cone) s=twig]   ::  command
                  [%1 p=twig]                               ::  generator
                  [%2 p=twig]                               ::  filter
              ==                                            ::

XX document

<h3 id="++lawn"><code>++lawn</code></h3>

    ++  lawn  (list lath)                                   ::

XX document

<h3 id="++lice"><code>++lice</code></h3>

    ++  lice  ,[p=ship q=buck]                              ::  full license

XX document

<h3 id="++life"><code>++life</code></h3>

    ++  life  ,@ud                                          ::  regime number

XX document

<h3 id="++lint"><code>++lint</code></h3>

    ++  lint  (list rock)                                   ::  fragment array

XX document

<h3 id="++lobe"><code>++lobe</code></h3>

    ++  lobe  ,@                                            ::  blob ref

XX document

<h3 id="++love"><code>++love</code></h3>

    ++  love  $%                                            ::  http response
                  [%ham p=manx]                             ::  html node
                  [%mid p=mite q=octs]                      ::  mime-typed data
                  [%raw p=httr]                             ::  raw http response
                  [%wan p=wain]                             ::  text lines
                  [%zap p=@ud q=(list tank)]                ::  status/error
              ==                                            ::

XX document

<h3 id="++luge"><code>++luge</code></h3>

    ++  luge  ,[p=mark q=*]                                 ::  fully typed content

XX document

<h3 id="++maki"><code>++maki</code></h3>

    ++  maki  ,[p=@ta q=@ta r=@ta s=path]                   ::

XX document

<h3 id="++mace"><code>++mace</code></h3>

    ++  mace  (list ,[p=life q=ring])                       ::  private secrets

XX document

<h3 id="++marv"><code>++marv</code></h3>

    ++  marv  ?(%da %tas %ud)                               ::  release form

XX document

<h3 id="++math"><code>++math</code></h3>

    ++  math  (map ,@t (list ,@t))                          ::  semiparsed headers

XX document

<h3 id="++meal"><code>++meal</code></h3>

    ++  meal                                                ::  payload
              $%  [%back p=cape q=flap r=@dr]               ::  acknowledgment
                  [%buck p=coop q=flap r=@dr]               ::  e2e ack
                  [%bond p=life q=path r=@ud s=*]           ::  message
                  [%bund p=life q=path r=@ud s=*]           ::  e2e message
                  [%carp p=@ q=@ud r=@ud s=flap t=@]        ::  skin/inx/cnt/hash
                  [%fore p=ship q=(unit lane) r=@]          ::  forwarded packet
              ==                                            ::

XX document

<h3 id="++mess"><code>++mess</code></h3>

    ++  mess  (list ,[p=@t q=@t])                           ::  raw http headers

XX document

<h3 id="++meta"><code>++meta</code></h3>

    ++  meta                                                ::  path metadata
              $%  [& q=@uvI]                                ::  hash
                  [| q=(list ,@ta)]                         ::  dir
              ==                                            ::

XX document

<h3 id="++meth"><code>++meth</code></h3>

    ++  meth                                                ::  http methods
              $?  %conn                                     ::  CONNECT
                  %delt                                     ::  DELETE
                  %get                                      ::  GET
                  %head                                     ::  HEAD
                  %opts                                     ::  OPTIONS
                  %post                                     ::  POST
                  %put                                      ::  PUT
                  %trac                                     ::  TRACE
              ==                                            ::

XX document

<h3 id="++mite"><code>++mite</code></h3>

    ++  mite  (list ,@ta)                                   ::  mime type

XX document

<h3 id="++miso"><code>++miso</code></h3>

    ++  miso                                                ::  ankh delta
              $%  [%del p=*]                                ::  delete
                  [%ins p=*]                                ::  insert
                  [%mut p=udon]                             ::  mutate
              ==                                            ::

XX document

<h3 id="++mizu"><code>++mizu</code></h3>

    ++  mizu  ,[p=@u q=(map ,@ud tako) r=rang]              ::  new state

XX document

<h3 id="++moar"><code>++moar</code></h3>

    ++  moar  ,[p=@ud q=@ud]                                ::  normal change range

XX document

<h3 id="++moat"><code>++moat</code></h3>

    ++  moat  ,[p=case q=case r=path]                       ::  change range

XX document

<h3 id="++mood"><code>++mood</code></h3>

    ++  mood  ,[p=care q=case r=path]                       ::  request in desk

XX document

<h3 id="++moth"><code>++moth</code></h3>

    ++  moth  ,[p=meth q=math r=(unit octs)]                ::  http operation

XX document

<h3 id="++name"><code>++name</code></h3>

    ++  name  ,[p=@t q=(unit ,@t) r=(unit ,@t) s=@t]        ::  first mid/nick last

XX document

<h3 id="++newt"><code>++newt</code></h3>

    ++  newt  ?(%boot %kick %mess %slay %wake)              ::  lifecycle events

XX document

<h3 id="++nose"><code>++nose</code></h3>

    ++  nose                                                ::  response, kernel
              $?  [%$ p=(unit ,[p=tutu q=(list)])]          ::  standard input
                  goad                                      ::
              ==                                            ::

XX document

<h3 id="++note"><code>++note</code></h3>

    ++  note                                                ::  response, user
              $?  [%$ p=(unit ,[p=type q=(list)])]          ::  standard input
                  [%do p=vase]                              ::  execution result
                  goad                                      ::
              ==                                            ::

XX document

<h3 id="++nori"><code>++nori</code></h3>

    ++  nori                                                ::  repository action
              $%  [& q=soba]                                ::  delta
                  [| p=@tas]                                ::  label
              ==                                            ::

XX document

<h3 id="++octs"><code>++octs</code></h3>

    ++  octs  ,[p=@ud q=@]                                  ::  octet-stream

XX document

<h3 id="++oryx"><code>++oryx</code></h3>

    ++  oryx  ,@t                                           ::  CSRF secret

XX document

<h3 id="++pact"><code>++pact</code></h3>

    ++  pact  path                                          ::  routed path

XX document

<h3 id="++pail"><code>++pail</code></h3>

    ++  pail  ?(%none %warm %cold)                          ::  connection status

XX document

<h3 id="++plan"><code>++plan</code></h3>

    ++  plan  (trel view (pair ,@da (unit ,@dr)) path)      ::  subscription

XX document

<h3 id="++plea"><code>++plea</code></h3>

    ++  plea  ,[p=@ud q=[p=? q=@t]]                         ::  live prompt

XX document

<h3 id="++pork"><code>++pork</code></h3>

    ++  pork  ,[p=(unit ,@ta) q=(list ,@t)]                 ::  fully parsed url

XX document

<h3 id="++pred"><code>++pred</code></h3>

    ++  pred  ,[p=@ta q=@tas r=@ta ~]                       ::  proto-path

XX document

<h3 id="++prod"><code>++prod</code></h3>

    ++  prod  ,[p=prom q=tape r=tape]                       ::  prompt

XX document

<h3 id="++prom"><code>++prom</code></h3>

    ++  prom  ?(%text %pass %none)                          ::  format type

XX document

<h3 id="++purl"><code>++purl</code></h3>

    ++  purl  ,[p=hart q=pork r=quay]                       ::  parsed url

XX document

<h3 id="++putt"><code>++putt</code></h3>

    ++  putt                                                ::  outgoing message
              $:  ski=snow                                  ::  sequence acked/sent
                  wyv=(list rock)                           ::  packet list XX gear
              ==                                            ::

XX document

<h3 id="++pyre"><code>++pyre</code></h3>

    ++  pyre                                                ::  cascade stash
              $:  p=(map ,[p=path q=path r=coal] coal)      ::  by path
                  q=(map ,[p=path q=@uvI r=coal] coal)      ::  by source hash
                  r=(map ,[p=* q=coal] coal)                ::  by (soft) twig
              ==                                            ::

XX document

<h3 id="++quay"><code>++quay</code></h3>

    ++  quay  (list ,[p=@t q=@t])                           ::  parsed url query

XX document

<h3 id="++quri"><code>++quri</code></h3>

    ++  quri                                                ::  request-uri
              $%  [& p=purl]                                ::  absolute
                  [| p=pork q=quay]                         ::  relative
              ==                                            ::

XX document

<h3 id="++race"><code>++race</code></h3>

    ++  race                                                ::  inbound stream
              $:  did=@ud                                   ::  filled sequence
                  dod=?                                     ::  not processing
                  bum=(map ,@ud ares)                       ::  nacks
                  mis=(map ,@ud ,[p=cape q=lane r=flap s=(unit)]) ::  misordered
              ==                                            ::

XX document

<h3 id="++rank"><code>++rank</code></h3>

    ++  rank  ?(%czar %king %duke %earl %pawn)              ::  ship width class

XX document

<h3 id="++rang"><code>++rang</code></h3>

    ++  rang  $:  hut=(map tako yaki)                       ::
                  lat=(map lobe blob)                       ::
              ==                                            ::

XX document

<h3 id="++rant"><code>++rant</code></h3>

    ++  rant                                                ::  namespace binding
              $:  p=[p=care q=case r=@tas]                  ::  clade release book
                  q=path                                    ::  spur
                  r=*                                       ::  data
              ==                                            ::

XX document

<h3 id="++rave"><code>++rave</code></h3>

    ++  rave                                                ::  general request
              $%  [& p=mood]                                ::  single request
                  [| p=moat]                                ::  change range
              ==                                            ::

XX document

<h3 id="++rill"><code>++rill</code></h3>

    ++  rill                                                ::  outbound stream
              $:  sed=@ud                                   ::  sent
                  san=(map ,@ud duct)                       ::  outstanding
              ==                                            ::

XX document

<h3 id="++riot"><code>++riot</code></h3>

    ++  riot  (unit rant)                                   ::  response/complete

XX document

<h3 id="++road"><code>++road</code></h3>

    ++  road                                                ::  secured oneway route
              $:  exp=@da                                   ::  expiration date
                  lun=(unit lane)                           ::  route to friend
                  lew=will                                  ::  will of friend
              ==                                            ::

XX document

<h3 id="++rock"><code>++rock</code></h3>

    ++  rock  ,@uvO                                         ::  packet

XX document

<h3 id="++rout"><code>++rout</code></h3>

    ++  rout  ,[p=(list host) q=path r=oryx s=path]         ::  http route (new)

XX document

<h3 id="++rump"><code>++rump</code></h3>

    ++  rump  ,[p=care q=case r=@tas s=path]                ::  relative path

XX document

<h3 id="++saba"><code>++saba</code></h3>

    ++  saba  ,[p=ship q=@tas r=moar s=dome]                ::  patch/merge

XX document

<h3 id="++sack"><code>++sack</code></h3>

    ++  sack  ,[p=ship q=ship]                              ::  incoming [our his]

XX document

<h3 id="++sufi"><code>++sufi</code></h3>

    ++  sufi                                                ::  domestic host
              $:  hoy=(list ship)                           ::  hierarchy
                  val=wund                                  ::  private keys
                  law=will                                  ::  server will
                  seh=(map hand ,[p=ship q=@da])            ::  key cache
                  hoc=(map ship dore)                       ::  neighborhood
              ==                                            ::

XX document

<h3 id="++salt"><code>++salt</code></h3>

    ++  salt  ,@uv                                          ::  entropy

XX document

<h3 id="++seal"><code>++seal</code></h3>

    ++  seal                                                ::  auth conversation
              $:  whu=(unit ship)                           ::  client identity
                  pul=purl                                  ::  destination url
                  wit=?                                     ::  wait for partner
                  foy=(unit ,[p=ship q=hole])               ::  partner to notify
                  pus=(unit ,@ta)                           ::  password
              ==                                            ::

XX document

<h3 id="++sect"><code>++sect</code></h3>

    ++  sect  ?(%black %blue %red %orange %white)           ::  banner

XX document

<h3 id="++shed"><code>++shed</code></h3>

    ++  shed                                                ::  packet flow
              $:  $:  rtt=@dr                               ::  smoothed rtt
                      rto=@dr                               ::  retransmit timeout
                      rtn=(unit ,@da)                       ::  next timeout
                      rue=(unit ,@da)                       ::  last heard from
                  ==                                        ::
                  $:  nus=@ud                               ::  number sent
                      nif=@ud                               ::  number live
                      nep=@ud                               ::  next expected
                      caw=@ud                               ::  logical window
                      cag=@ud                               ::  congest thresh
                  ==                                        ::
                  $:  diq=(map flap ,@ud)                   ::  packets sent
                      pyz=(map soup ,@ud)                   ::  message/unacked
                      puq=(qeu ,[p=@ud q=soul])             ::  packet queue
                  ==                                        ::
              ==                                            ::

XX document

<h3 id="++skit"><code>++skit</code></h3>

    ++  skit  ,[p=(unit ,@ta) q=(list ,@ta) r=(list ,@ta)]  ::  tracking path

XX document

<h3 id="++skin"><code>++skin</code></h3>

    ++  skin  ?(%none %open %fast %full)                    ::  encoding stem

XX document

<h3 id="++slip"><code>++slip</code></h3>

    ++  slip  ,[p=path q=goal]                              ::  traceable request

XX document

<h3 id="++snow"><code>++snow</code></h3>

    ++  snow  ,[p=@ud q=@ud r=(set ,@ud)]                   ::  window exceptions

XX document

<h3 id="++soap"><code>++soap</code></h3>

    ++  soap  ,[p=[p=life q=life] q=path r=@ud]             ::  statement id

XX document

<h3 id="++soup"><code>++soup</code></h3>

    ++  soup  ,[p=path q=@ud]                               ::  new statement id

XX document

<h3 id="++soul"><code>++soul</code></h3>

    ++  soul                                                ::  packet in travel
              $:  gom=soup                                  ::  message identity
                  nux=@ud                                   ::  xmission count
                  liv=?                                     ::  deemed live
                  lys=@da                                   ::  last sent
                  pac=rock                                  ::  packet data
              ==                                            ::

XX document

<h3 id="++soba"><code>++soba</code></h3>

    ++  soba  ,[p=cart q=(list ,[p=path q=miso])]           ::  delta

XX document

<h3 id="++sock"><code>++sock</code></h3>

    ++  sock  ,[p=ship q=ship]                              ::  outgoing [from to]

XX document

<h3 id="++spur"><code>++spur</code></h3>

    ++  spur  path                                          ::  ship desk case spur

XX document

<h3 id="++step"><code>++step</code></h3>

    ++  step  ,[p=bray q=gens r=pass]                       ::  identity stage

XX document

<h3 id="++tako"><code>++tako</code></h3>

    ++  tako  ,@                                            ::  yaki ref

XX document

<h3 id="++tart"><code>++tart</code></h3>

    ++  tart  $+([@da path note] bowl)                      ::  process core

XX document

<h3 id="++taxi"><code>++taxi</code></h3>

    ++  taxi  ,[p=lane q=rock]                              ::  routed packet

XX document

<h3 id="++tick"><code>++tick</code></h3>

    ++  tick  ,@ud                                          ::  process id

XX document

<h3 id="++toro"><code>++toro</code></h3>

    ++  toro  ,[p=@ta q=nori]                               ::  general change

XX document

<h3 id="++town"><code>++town</code></h3>

    ++  town                                                ::  all security state
              $:  lit=@ud                                   ::  imperial modulus
                  any=@                                     ::  entropy
                  urb=(map ship sufi)                       ::  all keys and routes
                  fak=?                                     ::
              ==                                            ::

XX document

<h3 id="++tube"><code>++tube</code></h3>

    ++  tube  ,[p=@ta q=@ta r=@ta s=path]                   ::  canonical path

XX document

<h3 id="++tutu"><code>++tutu</code></h3>

    ++  tutu  ,*                                            ::  presumed type

XX document

<h3 id="++yaki"><code>++yaki</code></h3>

    ++  yaki  ,[p=(list tako) q=(map path lobe) r=tako t=@da] ::  commit

XX document

<h3 id="++view"><code>++view</code></h3>

    ++  view  ?(%u %v %w %x %y %z)                          ::  view mode

XX document

<h3 id="++waks"><code>++waks</code></h3>

    ++  waks  (map path woof)                               ::  list file states

XX document

<h3 id="++what"><code>++what</code></h3>

    ++  what                                                ::  logical identity
              $%  [%anon ~]                                 ::  anonymous
                  [%lady p=whom]                            ::  female person ()
                  [%lord p=whom]                            ::  male person []
                  [%punk p=sect q=@t]                       ::  opaque handle ""
              ==                                            ::

XX document

<h3 id="++whom"><code>++whom</code></h3>

    ++  whom  ,[p=@ud q=govt r=sect s=name]                 ::  year/govt/id

XX document

<h3 id="++woof"><code>++woof</code></h3>

    ++  woof  $|  %know                                     ::  udon transform
                  [%chan (list $|(@ud [p=@ud q=@ud]))]      ::

XX document

<h3 id="++wund"><code>++wund</code></h3>

    ++  wund  (list ,[p=life q=ring r=acru])                ::  mace in action

XX document

<h3 id="++will"><code>++will</code></h3>

    ++  will  (list deed)                                   ::  certificate

XX document

<h3 id="++worm"><code>++worm</code></h3>

    ++  worm  ,*                                            ::  vase of tart

XX document

<h3 id="++zuse"><code>++zuse</code></h3>

    ++  zuse  %314                                          ::  hoon/zuse kelvin
    --

XX document
