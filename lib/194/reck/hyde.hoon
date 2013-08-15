!:
::          %yen, zuse archetypes.   This file is in the public domain.
::
|%
++  arch                                                ::  fs node
          $%  [& clod]                                  ::  ie, file
              [| dirt]                                  ::  ie, directory
          ==                                            ::
++  ball  ,@uw                                          ::  statement payload
++  band  ,@uvH                                         ::  network message id
++  bait  ,[p=skin q=@ud r=dove]                        ::  fmt nrecvd spec
++  bath                                                ::  outgoing per client
          $:  ski=snow                                  ::  sequence state
              foy=flow                                  ::  flow statistics
              par=(map soap putt)                       ::  message by id
              maz=(qeu soap)                            ::  round-robin next
              air=(map flap ,@ud)                       ::  unacked by content
              sea=shed                                  ::  packet pump
          ==                                            ::
++  bird                                                ::  packet in flight
          $:  gom=soap                                  ::  message identity
              mup=@ud                                   ::  pktno in msg
              org=?                                     ::  original tx?
              tim=@dr                                   ::  logical timer
              ded=@da                                   ::  next expire
              pac=rock                                  ::  packet data
          ==                                            ::
++  bond  (list post)                                   ::  atomic statement
++  boon                                                ::  fort output
          $%  [%beer p=flag q=@uvG]                     ::  gained ownership
              [%coke p=cape q=soap]                     ::  message conclusion
              [%mead p=rock]                            ::  accept packet
              [%milk p=sock q=@da r=bond]               ::  learn bindings
              [%ouzo p=lane q=rock]                     ::  transmit packet
              [%sack p=sock q=cape r=band]              ::  send ack
              [%wine p=flag]                            ::  lost ownership
          ==                                            ::
++  bran  ,[p=mark q=(unit mark) r=flag]                ::  version parent id
++  buck  ,[p=mace q=will]                              ::  all security data
++  cake  ,[p=flag q=? r=skin s=@]                      ::  top level packet
++  cape                                                ::  end-to-end result
          $?  %dead                                     ::  maybe undelivered
              %good                                     ::  delivered
              %weak                                     ::  rejected
          ==                                            ::
++  card                                                ::  action
          $%  [%bbye ~]                                 ::  log out
              [%boot p=@]                               ::  reset soft state
              [%cash p=@p q=buck]                       ::  civil license
              [%crap p=(list)]                          ::  error with trace
              [%dire p=@tas q=dram]                     ::  apply directory
              [%edit p=@p q=@tas r=(list ukaz)]         ::  commit edits
              [%file p=@tas q=@]                        ::  apply atomic file
              [%hear p=@]                               ::  receive packet
              [%helo ~]                                 ::  ready to prompt
              [%init p=@p]                              ::  initialize revs
              [%junk p=@]                               ::  entropy
              [%line p=@t]                              ::  source line
              [%load p=@tas q=path]                     ::  request atomic file
              [%logn p=@p q=chum]                       ::  name hashed-pass
              [%logp p=@p]                              ::  privileged login
              [%loot p=@tas q=path]                     ::  request directory
              [%make p=@t q=@ud r=@]                    ::  wild license
              [%pace p=@ud]                             ::  compute background
              [%pour p=path q=dram]                     ::  write directory
              [%prop p=? q=@t]                          ::  visible, prompt
              [%pump ~]                                 ::  produce packets
              [%save p=path q=@]                        ::  write atomic file
              [%send p=@]                               ::  transmit packet
              [%ship p=@tas q=@tas]                     ::  label release
              [%sync ~]                                 ::  reset soft state
              [%task p=@tas q=*]                        ::  application task
              [%tell p=?(0 1 2 3) q=tank]               ::  report to human
              [%text p=(list ,@t)]                      ::  raw text lines
              [%tory p=(list ,@t)]                      ::  history dump
              [%word p=chum]                            ::  set password
          ==                                            ::
++  cask                                                ::  symmetric record
          $:  yed=(unit ,[p=hand q=code])               ::  outbound
              heg=(map hand code)                       ::  proposed 
              qim=(map hand code)                       ::  inbound
          ==                                            ::
++  clod  ,[p=@da q=@uvI r=*]                           ::  mtime hash content
++  club  ?(tone [3 p=@ta])
++  code  ,@uvI                                         ::  symmetric key
++  corp  ,[p=@t q=@t r=@tas]                           ::  name auth country
++  caul  (list path)                                   ::  causal history
++  chum  ,@uvI                                         ::  hashed passcode
++  deed  ,[p=@ q=step]                                 ::  signature, stage
++  desk                                                ::  project state
          $:  lab=(map ,@tas ,@ud)                      ::  labels
              let=@                                     ::  (lent hit)
              hit=(list ,[p=@da q=ukaz r=arch])         ::  history
          ==                                            ::
++  dirt  ,[p=@da q=(map ,@ta arch)]                    ::  mtime tree
++  door                                                ::  foreign contact
          $:  wod=road                                  ::  connection to
              fer=road                                  ::  inferred conn from
              caq=cask                                  ::  symmetric key state
          ==                                            ::
++  dove  ,[p=@ud q=(map ,@ud (unit ,@))]               ::  count 13-blocks
++  flag  ,@p                                           ::  host identity
++  flap  ,@uvH                                         ::  network packet id
++  flow                                                ::  packet connection
          $:  rtt=@dr                                   ::  official rtt
              wid=@ud                                   ::  logical wdow msgs
              yed=@ud                                   ::  actual wdow msgs
          ==                                            ::
++  fort                                                ::  formal state
          $:  wen=@da                                   ::  next wakeup
              ton=town                                  ::  security
              zac=(map flag oven)                       ::  flows by server
          ==                                            ::
++  gcos                                                ::  id description
          $%  [%czar p=@t]                              ::  8-bit flag
              [%duke p=what]                            ::  32-bit flag
              [%jack p=what]                            ::  64-bit flag
              [%king p=@t]                              ::  16-bit flag
              [%pawn p=@t]                              ::  128-bit flag
          ==                                            ::
++  gram  ,@uw                                          ::  physical datagram
++  hand  ,@uvH                                         ::  hash of code
++  hook  path                                          ::  request origin
++  lens  ?(%z %y %x %w)                                ::  repository view
++  lice  ,[p=flag q=buck]                              ::  full license
++  lint  (list rock)                                   ::  fragment array
++  lane                                                ::  packet route
          $%  [%if p=@ud q=@if]                         ::  IP4/public UDP/addr
              [%is p=@ud q=@is]                         ::  IP6/public UDP/addr
          ==                                            ::
++  link  ,[p=code q=sock]                              ::  connection
++  mace  (list ,[p=mark q=ring])                       ::  private secrets
++  mark  ,@ud                                          ::  regime number
++  meal                                                ::  payload
          $%  [%back p=cape q=flap r=@dr]               ::  acknowledgment
              [%bond p=flag q=(list post)]              ::  statement
              [%buck p=mace q=will]                     ::  license
              [%carp p=@ud q=@band r=@]                 ::  leaf fragment
              [%ping ~]                                 ::  no-op
          ==                                            ::
++  meta                                                ::  path metadata
          $%  [& p=@da q=@uvI]                          ::  mtime hash
              [| p=@da q=(list ,@ta)]                   ::  mtime dir
          ==                                            ::
++  move  ,[p=(unit flag) q=caul r=card]                ::  internal event
++  name  ,[p=@t q=(unit ,[p=? q=@t]) r=@t]             ::  first mid/nick last
++  nope  ^~(^-(arch [%| @ ~]))                         ::  empty node
++  note  ,[p=soap q=sock r=meal]                       ::  output source
++  oven                                                ::  flow by server
          $:  wen=@da                                   ::  next activation
              nys=(map band ,[p=@da q=bait])            ::  incoming
              wab=(map flag bath)                       ::  outgoing by client
          ==                                            ::
++  ovum  ,[p=path q=card]                              ::  external event
++  post  ,[p=path q=*]                                 ::  statement
++  putt                                                ::  outgoing message
          $:  ski=snow                                  ::  sequence acked/sent
              saq=?                                     ::  secure ack required
              ryn=(unit lane)                           ::  implied mirror lane
              wyv=(list rock)                           ::  packet list XX gear
          ==                                            ::
++  rank  ?(%czar %king %duke %jack %pawn)              ::  flag width class
++  road                                                ::  secured oneway route
          $:  exp=@da                                   ::  expiration date
              lun=(unit lane)                           ::  route to friend
              lew=will                                  ::  will of friend
          ==                                            ::
++  roof  (map ,@p room)                                ::  revision control
++  room                                                ::  author by flag
          $:  own=?                                     ::  local/foreign
              dos=(map ,@ta desk)                       ::  projects 
          ==                                            ::
++  rock  ,@uvO                                         ::  packet
++  safe                                                ::  domestic host
          $:  loc=(unit lane)                           ::  packet route
              val=wand                                  ::  private keys
              law=will                                  ::  server will
              seh=(map hand ,[p=flag q=@da])            ::  key cache
              hoc=(map flag door)                       ::  friends & relations
          ==                                            ::
++  salt  ,@uv                                          ::  entropy
++  shed  ,[p=@da q=(qeu ,[p=@ud q=bird])]              ::  packet pump
++  sink                                                ::  incoming per server
          $:  nes=(map band ,[p=@da q=bait])            ::  fragment actions
          ==                                            ::
++  skin  ?(%none %open %fast %full)                    ::  encoding stem
++  snow  ,[p=@ud q=@ud r=(set ,@ud)]                   ::  window exceptions
++  soap  ,*                                            ::  opaque msg identity
++  sock  ,[p=flag q=flag]                              ::  from to
++  step  ,[p=bran q=gcos r=pass]                       ::  identity stage
++  taxi  ,[p=lane q=rock]                              ::  routed packet
++  tray  ,[p=(unit lane) q=meal]                       ::  routed payload
++  town                                                ::  all security state
          $:  lit=@ud                                   ::  imperial modulus
              any=@                                     ::  entropy
              urb=(map flag safe)                       ::  all keys and routes
          ==                                            ::
++  ukaz                                                ::  change (pl ukazy)
          $:  p=path                                    ::  change site
              ^=  q                                     ::  change content
              $%  [%del p=@uvI]                         ::  delete old-hash
                  [%set p=@uvI q=@uvI r=*]              ::  new old data
              ==                                        ::
          ==                                            ::
++  umaz  ,[p=(list ukaz) q=(list ukaz)]                ::  dual change
++  wand  (list ,[p=mark q=ac])                         ::  mace in action
++  what                                                ::  logical identity
          $%  [%crew p=corp]                            ::  business
              [%dept p=corp]                            ::  govt/education
              [%dude p=whom]                            ::  male individual
              [%fair p=corp]                            ::  nonprofit
              [%home p=corp]                            ::  family
              [%girl p=whom]                            ::  female individual
              [%holy p=corp]                            ::  church
              [%punk p=@t]                              ::  fictitious id
          ==                                            ::
++  whom  ,[p=@ud q=@t r=@tas s=name]                   ::  yob/state/nation/me
++  will  (list deed)                                   ::  certificate
--
