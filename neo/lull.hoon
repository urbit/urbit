!:                                                      ::  /van/york
::                                                      ::  %reference/1
::  %york: arvo models and metamodels.
::
::  %york, like the library %zuse, is split into cores for
::  arvo's eight major vanes (kernel modules).  these are:
::
::      - %ames: networking         (rhymes with "games")
::      - %behn: scheduling         ("bane")
::      - %clay: revision control   ("play")
::      - %dill: console            ("pill")
::      - %eyre: web                ("fair")
::      - %ford: build              ("lord")
::      - %gall: application        ("ball")
::      - %jael: security           ("jail")
::
::  any vane can use any of these models, of course.
::
|%
::                                                      ::::
::::                      ++ames                          ::  (1a) network
  ::                                                    ::::
++  ames  ^?
  |%
  ::                                                    ::
  ::::                  ++able:ames                     ::  (1a1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  card                                            ::  out cards
      $%  {$went p/sack q/path r/@ud s/coop}            ::  response confirm
          {$west p/sack q/path r/@ud s/*}               ::  network request
      ==                                                ::
    ++  gift                                            ::  out result <-$
      $%  {$drop $~}                                    ::  drop packet
          {$hear p/lane q/@}                            ::  receive packet
          {$east p/sock q/*}                            ::  network response
          {$init p/@p}                                  ::  report install
          {$mass p/mass}                                ::  memory usage
          {$send p/lane q/@}                            ::  transmit packet
          {$waft p/ship q/path r/*}                     ::  response message
          {$wart p/sock q/@tas r/path s/*}              ::  network request
          {$went p/ship q/cape}                         ::  reaction message
          {$woot p/ship q/path r/coop}                  ::  e2e reaction message
      ==                                                ::
    ++  note                                            ::  out request $->
      $%  {$c card}                                     ::  to %clay
          {$e card}                                     ::  to %eyre
          {$g card}                                     ::  to %gall
      ==                                                ::
    ++  task                                            ::  in request ->$
      $%  ::  {$born p/@p q/@pG r/?}                    ::  ticket birth
          {$barn $~}                                    ::  new unix process
          {$crud p/@tas q/(list tank)}                  ::  error with trace
          {$cash p/@p q/buck}                           ::  civil license
          ::  {$funk p/@p q/@p r/@}                     ::  symtix from/to/key
          {$hear p/lane q/@}                            ::  receive packet
          {$hole p/lane q/@}                            ::  packet failed
          {$junk p/@}                                   ::  entropy
          {$kick p/@da}                                 ::  wake up
          {$make p/(unit @t) q/@ud r/@ s/?}             ::  wild license
          {$sith p/@p q/@uw r/?}                        ::  imperial generator
          {$wake $~}                                    ::  timer activate
          {$want p/sock q/path r/*}                     ::  send message
          {$wegh $~}                                    ::  report memory
          {$wont p/sock q/path r/*}                     ::  e2e send message
      ==                                                ::
    --  ::able
  ::
  ::::                                                  ::  (1a2)
    ::
  ++  acru  $_  ^?                                      ::  asym cryptosuite
    |%                                                  ::  opaque object
    ++  as  ^?                                          ::  asym ops
      |%  ++  seal  |~({a/pass b/@ c/@} *@)             ::  encrypt to a
          ++  sign  |~({a/@ b/@} *@)                    ::  certify as us
          ++  sure  |~({a/@ b/@} *(unit @))             ::  authenticate from us
          ++  tear  |~  {a/pass b/@}                    ::  accept from a
                    *(unit {p/@ q/@})                   ::
      --  ::as                                          ::
    ++  de  |~({a/@ b/@} *(unit @))                     ::  symmetric de, soft
    ++  dy  |~({a/@ b/@} *@)                            ::  symmetric de, hard
    ++  en  |~({a/@ b/@} *@)                            ::  symmetric en
    ++  ex  ^?                                          ::  export
      |%  ++  fig  *@uvH                                ::  fingerprint
          ++  pac  *@uvG                                ::  default passcode
          ++  pub  *pass                                ::  public key
          ++  sec  *ring                                ::  private key
      --  ::ex                                          ::
    ++  nu  ^?                                          ::  reconstructors
      |%  ++  pit  |~({a/@ b/@} ^?(..nu))               ::  from [width seed]
          ++  nol  |~(a/@ ^?(..nu))                     ::  from naked ring
          ++  com  |~(a/@ ^?(..nu))                     ::  from naked pass
      --  ::nu                                          ::
    --  ::acru                                          ::
  ++  bait  {p/skin q/@ud r/dove}                       ::  fmt nrecvd spec
  ++  boon                                              ::  fort output
    $%  {$beer p/ship}                                  ::  gained ownership
        {$bier p/ship q/deyd}                           ::  unsigned deyd
        {$cake p/sock q/soap r/coop s/duct}             ::  e2e message result
        {$coke p/sock q/soap r/cape s/duct}             ::  message result
        {$mead p/lane q/rock}                           ::  accept packet
        {$milk p/sock q/soap r/*}                       ::  accept message
        {$mulk p/sock q/soap r/*}                       ::  e2e pass message
        {$ouzo p/lane q/rock}                           ::  transmit packet
        {$wine p/sock q/tape}                           ::  notify user
    ==                                                  ::
  ++  bray  {p/life q/(unit life) r/ship s/@da}         ::  our parent us now
  ++  buck  {p/mace q/wyll}                             ::  all security data
  ++  cake  {p/sock q/skin r/@}                         ::  top level packet
  ++  cape                                              ::  end-to-end result
    $?  $good                                           ::  delivered
        $dead                                           ::  rejected
    ==                                                  ::
  ++  clot                                              ::  symmetric record
    $:  yed/(unit {p/hand q/code})                      ::  outbound
        heg/(map hand code)                             ::  proposed
        qim/(map hand code)                             ::  inbound
    ==                                                  ::
  ++  code  @uvI                                        ::  symmetric key
  ++  deyd  {p/@ q/step r/?}                            ::  sig stage fake?
  ++  dore                                              ::  foreign contact
    $:  wod/road                                        ::  connection to
        wyl/wyll                                        ::  inferred mirror
        caq/clot                                        ::  symmetric key state
    ==                                                  ::
  ++  dove  {p/@ud q/(map @ud @)}                       ::  count hash 13-blocks
  ++  flap  @uvH                                        ::  network packet id
  ++  flow                                              ::  packet connection
    $:  rtt/@dr                                         ::  decaying avg rtt
        wid/@ud                                         ::  logical wdow msgs
    ==                                                  ::
  ++  gcos                                              ::  id description
    $%  {$czar $~}                                      ::  8-bit ship
        {$duke p/what}                                  ::  32-bit ship
        {$earl p/@t}                                    ::  64-bit ship
        {$king p/@t}                                    ::  16-bit ship
        {$pawn p/(unit @t)}                             ::  128-bit ship
    ==                                                  ::
  ++  gens  {p/lang q/gcos}                             ::  general identity
  ++  govt  path                                        ::  country+postcode
  ++  hand  @uvH                                        ::  128-bit hash
  ++  lane                                              ::  packet route
    $%  {$if p/@da q/@ud r/@if}                         ::  IP4/public UDP/addr
        {$is p/@ud q/(unit lane) r/@is}                 ::  IPv6 w+alternates
        {$ix p/@da q/@ud r/@if}                         ::  IPv4 provisional
    ==                                                  ::
  ++  lang  @ta                                         ::  IETF lang as code
  ++  lice  {p/ship q/buck}                             ::  full license
  ++  life  @ud                                         ::  regime number
  ++  mace  (list {p/life q/ring})                      ::  private secrets
  ++  meal                                              ::  payload
    $%  {$back p/cape q/flap r/@dr}                     ::  acknowledgment
        {$buck p/coop q/flap r/@dr}                     ::  e2e ack
        {$bond p/life q/path r/@ud s/*}                 ::  message
        {$bund p/life q/path r/@ud s/*}                 ::  e2e message
        {$carp p/@ q/@ud r/@ud s/flap t/@}              ::  skin+inx+cnt+hash
        {$fore p/ship q/(unit lane) r/@}                ::  forwarded packet
    ==                                                  ::
  ++  name  {p/@t q/(unit @t) r/(unit @t) s/@t}         ::  first mid+nick last
  ++  putt                                              ::  outgoing message
    $:  ski/snow                                        ::  sequence acked+sent
        wyv/(list rock)                                 ::  packet list XX gear
    ==                                                  ::
  ++  race                                              ::  inbound stream
    $:  did/@ud                                         ::  filled sequence
        dod/?                                           ::  not processing
        bum/(map @ud ares)                              ::  nacks
        mis/(map @ud {p/cape q/lane r/flap s/(unit)})   ::  misordered
    ==                                                  ::
  ++  rank  ?($czar $king $duke $earl $pawn)            ::  ship width class
  ++  rill                                              ::  outbound stream
    $:  sed/@ud                                         ::  sent
        san/(map @ud duct)                              ::  outstanding
    ==                                                  ::
  ++  road                                              ::  secured oneway route
    $:  exp/@da                                         ::  expiration date
        lun/(unit lane)                                 ::  route to friend
        lew/wyll                                        ::  wyll of friend
    ==                                                  ::
  ++  rock  @uvO                                        ::  packet
  ++  sect  ?($black $blue $red $orange $white)         ::  banner
  ++  shed                                              ::  packet flow
    $:  $:  rtt/@dr                                     ::  smoothed rtt
            rto/@dr                                     ::  retransmit timeout
            rtn/(unit @da)                              ::  next timeout
            rue/(unit @da)                              ::  last heard from
        ==                                              ::
        $:  nus/@ud                                     ::  number sent
            nif/@ud                                     ::  number live
            nep/@ud                                     ::  next expected
            caw/@ud                                     ::  logical window
            cag/@ud                                     ::  congest thresh
        ==                                              ::
        $:  diq/(map flap @ud)                          ::  packets sent
            pyz/(map soup @ud)                          ::  message+unacked
            puq/(qeu {p/@ud q/soul})                    ::  packet queue
        ==                                              ::
    ==                                                  ::
  ++  skin  ?($none $open $fast $full)                  ::  encoding stem
  ++  snow  {p/@ud q/@ud r/(set @ud)}                   ::  window exceptions
  ++  soap  {p/{p/life q/life} q/path r/@ud}            ::  statement id
  ++  soup  {p/path q/@ud}                              ::  new statement id
  ++  soul                                              ::  packet in travel
    $:  gom/soup                                        ::  message identity
        nux/@ud                                         ::  xmission count
        liv/?                                           ::  deemed live
        lys/@da                                         ::  last sent
        pac/rock                                        ::  packet data
    ==                                                  ::
  ++  step  {p/bray q/gens r/pass}                      ::  identity stage
  ++  sufi                                              ::  domestic host
    $:  hoy/(list ship)                                 ::  hierarchy
        val/wund                                        ::  private keys
        law/wyll                                        ::  server wyll
        seh/(map hand {p/ship q/@da})                   ::  key cache
        hoc/(map ship dore)                             ::  neighborhood
    ==                                                  ::
  ++  tick  @ud                                         ::  process id
  ++  town                                              ::  all security state
    $:  lit/@ud                                         ::  imperial modulus
        any/@                                           ::  entropy
        urb/(map ship sufi)                             ::  all keys and routes
        fak/?                                           ::
    ==                                                  ::
  ++  what                                              ::  logical identity
    $%  {$anon $~}                                      ::  anonymous
        {$lady p/whom}                                  ::  female person ()
        {$lord p/whom}                                  ::  male person []
        {$punk p/sect q/@t}                             ::  opaque handle ""
    ==                                                  ::
  ++  whom  {p/@ud q/govt r/sect s/name}                ::  year+govt+id
  ++  wund  (list {p/life q/ring r/acru})               ::  mace in action
  ++  wyll  (list deyd)                                 ::  certificate
  --  ::ames
::                                                      ::::
::::                    ++behn                            ::  (1b) timekeeping
  ::                                                    ::::
++  behn  ^?
  |%
  ::                                                    ::
  ::::                  ++able:behn                     ::  (1b1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  out result <-$
      $%  {$mass p/mass}                                ::  memory usage
          {$wake $~}                                    ::  wakeup
      ==                                                ::
    ++  task                                            ::  in request ->$
      $%  {$rest p/@da}                                 ::  cancel alarm
          {$wait p/@da}                                 ::  set alarm
          {$wake $~}                                    ::  timer activate
          {$wegh $~}                                    ::  report memory
      ==                                                ::
    --  ::able
  --  ::behn
::                                                      ::::
::::                    ++clay                            ::  (1c) versioning
  ::                                                    ::::
++  clay  ^?
  |%
  ::                                                    ::
  ::::                  ++able:clay                     ::  (1c1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  out result <-$
      $%  {$dirk p/@tas}                                ::  mark mount dirty
          {$ergo p/@tas q/mode}                         ::  version update
          {$hill p/(list @tas)}                         ::  mount points
          {$mack p/(unit tang)}                         ::  ack
          {$mass p/mass}                                ::  memory usage
          {$mere p/(each (set path) (pair term tang))}  ::  merge result
          {$note p/@tD q/tank}                          ::  debug message
          {$ogre p/@tas}                                ::  delete mount point
          {$writ p/riot}                                ::  response
      ==                                                ::
    ++  task                                            ::  in request ->$
      $%  {$boat $~}                                    ::  pier rebooted
          {$drop p/@p q/desk}                           ::  cancel pending merge
          {$info p/@p q/desk r/nori}                    ::  internal edit
          {$init p/@p}                                  ::  report install
          {$into p/desk q/? r/mode}                     ::  external edit
          {$merg p/@p q/desk r/@p s/desk t/case u/germ} ::  merge desks
          {$mont p/desk q/beam}                         ::  mount to unix
          {$dirk p/desk}                                ::  mark mount dirty
          {$ogre p/$@(desk beam)}                       ::  delete mount point
          {$warp p/sock q/riff}                         ::  file request
          {$wegh $~}                                    ::  report memory
          {$went p/sack q/path r/@ud s/coop}            ::  response confirm
          {$west p/sack q/path r/@ud s/*}               ::  network request
      ==                                                ::
    --  ::able
  ::
  ::::                                                  ::  (1c2)
    ::
  ++  aeon  @ud                                         ::  version number
  ++  ankh                                              ::  fs node (new)
    $:  fil/(unit {p/lobe q/cage})                      ::  file
        dir/(map @ta ankh)                              ::  folders
    ==                                                  ::
  ++  beam  {{p/ship q/desk r/case} s/path}             ::  global name
  ++  beak  {p/ship q/desk r/case}                      ::  path prefix
  ++  blob                                              ::  fs blob
    $%  {$delta p/lobe q/{p/mark q/lobe} r/page}        ::  delta on q
        {$direct p/lobe q/page}                         ::  immediate
    ==                                                  ::
  ++  care  ?($d $u $v $w $x $y $z)                     ::  clay submode
  ++  case                                              ::  ship desk case spur
    $%  {$da p/@da}                                     ::  date
        {$tas p/@tas}                                   ::  label
        {$ud p/@ud}                                     ::  number
    ==                                                  ::
  ++  coop  (unit ares)                                 ::  e2e ack
  ++  dome                                              ::  project state
    $:  ank/ankh                                        ::  state
        let/@ud                                         ::  top id
        hit/(map @ud tako)                              ::  changes by id
        lab/(map @tas @ud)                              ::  labels
    ==                                                  ::
  ++  germ                                              ::  merge style
    $?  $init                                           ::  new desk
        $this                                           ::  ours with parents
        $that                                           ::  hers with parents
        $fine                                           ::  fast forward
        $meet                                           ::  orthogonal files
        $mate                                           ::  orthogonal changes
        $meld                                           ::  force merge
    ==                                                  ::
  ++  khan                                              ::
    $:  fil/(unit (unit cage))                          ::  see ++khan-to-soba
        dir/(unit (map @ta (unit khan)))                ::
    ==                                                  ::
  ++  lobe  @uvI                                        ::  blob ref
  ++  maki  {p/@ta q/@ta r/@ta s/path}                  ::
  ++  miso                                              ::  ankh delta
    $%  {$del $~}                                       ::  delete
        {$ins p/cage}                                   ::  insert
        {$dif p/cage}                                   ::  mutate from diff
        {$mut p/cage}                                   ::  mutate from raw
    ==                                                  ::
  ++  misu                                              ::  computed delta
    $%  {$del $~}                                       ::  delete
        {$ins p/cage}                                   ::  insert
        {$dif p/lobe q/cage}                            ::  mutate from diff
    ==                                                  ::
  ++  mizu  {p/@u q/(map @ud tako) r/rang}              ::  new state
  ++  moar  {p/@ud q/@ud}                               ::  normal change range
  ++  moat  {p/case q/case r/path}                      ::  change range
  ++  mode  (list {path (unit mime)})                   ::  external files
  ++  mood  {p/care q/case r/path}                      ::  request in desk
  ++  nori                                              ::  repository action
    $%  {$& p/soba}                                     ::  delta
        {$| p/@tas}                                     ::  label
    ==                                                  ::
  ++  nuri                                              ::  repository action
    $%  {$& p/suba}                                     ::  delta
        {$| p/@tas}                                     ::  label
    ==                                                  ::
  ++  page  (cask *)                                    ::  untyped cage
  ++  plop  blob                                        ::  unvalidated blob
  ++  rang                                              ::  repository
    $:  hut/(map tako yaki)                             ::  changes
        lat/(map lobe blob)                             ::  data
    ==                                                  ::
  ++  rant                                              ::  response to request
    $:  p/{p/care q/case r/@tas}                        ::  clade release book
        q/path                                          ::  spur
        r/cage                                          ::  data
    ==                                                  ::
  ++  rave                                              ::  general request
    $%  {$sing p/mood}                                  ::  single request
        {$next p/mood}                                  ::  await next version
        {$many p/? q/moat}                              ::  track range
    ==                                                  ::
  ++  riff  {p/desk q/(unit rave)}                      ::  request+desist
  ++  riot  (unit rant)                                 ::  response+complete
  ++  rump  {p/care q/case r/@tas s/path}               ::  relative path
  ++  saba  {p/ship q/@tas r/moar s/dome}               ::  patch+merge
  ++  soba  (list {p/path q/miso})                      ::  delta
  ++  suba  (list {p/path q/misu})                      ::  delta
  ++  tako  @                                           ::  yaki ref
  ++  toro  {p/@ta q/nori}                              ::  general change
  ++  unce                                              ::  change part
    |*  a/mold                                          ::
    $%  {$& p/@ud}                                      ::  skip[copy]
        {$| p/(list a) q/(list a)}                      ::  p -> q[chunk]
    ==                                                  ::
  ++  urge  |*(a/mold (list (unce a)))                  ::  list change
  ++  yaki                                              ::  commit
    $:  p/(list tako)                                   ::  parents
        q/(map path lobe)                               ::  namespace
        r/tako                                          ::  self-reference
        t/@da                                           ::  date
    ==                                                  ::
  --  ::clay
::                                                      ::::
::::                    ++dill                            ::  (1d) console
  ::                                                    ::::
++  dill  ^?
  |%
  ::                                                    ::
  ::::                  ++able:dill                     ::  (1d1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  out result <-$
      $%  {$bbye $~}                                    ::  reset prompt
          {$blit p/(list blit)}                         ::  terminal output
          {$burl p/@t}                                  ::  activate url
          {$init p/@p}                                  ::  set owner
          {$logo $~}                                    ::  logout
          {$mass p/mass}                                ::  memory usage
          {$veer p/@ta q/path r/@t}                     ::  install vane
          {$vega p/path}                                ::  old reboot
          {$velo p/@t q/@t}                             ::  reboot
          {$verb $~}                                    ::  verbose mode
      ==                                                ::
    ++  task                                            ::  in request ->$
      $%  {$belt p/belt}                                ::  terminal input
          {$blew p/blew}                                ::  terminal config
          {$boot p/*}                                   ::  weird %dill boot
          {$crud p/@tas q/(list tank)}                  ::  error with trace
          {$flog p/flog}                                ::  wrapped error
          {$flow p/@tas q/(list gill:gall)}             ::  terminal config
          {$hail $~}                                    ::  terminal refresh
          {$heft $~}                                    ::  memory report
          {$hook $~}                                    ::  this term hung up
          {$harm $~}                                    ::  all terms hung up
          {$init p/ship}                                ::  after gall ready
          {$tick p/@p q/@p}                             ::  initial ticket
          {$noop $~}                                    ::  no operation
          {$talk p/tank}                                ::
          {$text p/tape}                                ::
          {$veer p/@ta q/path r/@t}                     ::  install vane
          {$vega p/path}                                ::  old reboot
          {$velo p/@t q/@t}                             ::  reboot
          {$verb $~}                                    ::  verbose mode
      ==                                                ::
    --  ::able
  ::
  ::::                                                  ::  (1d2)
    ::
  ++  blew  {p/@ud q/@ud}                               ::  columns rows
  ++  belt                                              ::  old belt
    $%  {$aro p/?($d $l $r $u)}                         ::  arrow key
        {$bac $~}                                       ::  true backspace
        {$ctl p/@c}                                     ::  control-key
        {$del $~}                                       ::  true delete
        {$met p/@c}                                     ::  meta-key
        {$ret $~}                                       ::  return
        {$txt p/(list @c)}                              ::  utf32 text
    ==                                                  ::
  ++  blit                                              ::  old blit
    $%  {$bel $~}                                       ::  make a noise
        {$clr $~}                                       ::  clear the screen
        {$hop p/@ud}                                    ::  set cursor position
        {$lin p/(list @c)}                              ::  set current line
        {$mor $~}                                       ::  newline
        {$sag p/path q/*}                               ::  save to jamfile
        {$sav p/path q/@}                               ::  save to file
        {$url p/@t}                                     ::  activate url
    ==                                                  ::
  ++  deco  ?($~ $bl $br $un)                           ::  text decoration
  ++  dill-belt                                         ::  new belt
    $%  {$aro p/?($d $l $r $u)}                         ::  arrow key
        {$bac $~}                                       ::  true backspace
        {$cru p/@tas q/(list tank)}                     ::  echo error
        {$ctl p/@}                                      ::  control-key
        {$del $~}                                       ::  true delete
        {$hey $~}                                       ::  refresh
        {$met p/@}                                      ::  meta-key
        {$ret $~}                                       ::  return
        {$rez p/@ud q/@ud}                              ::  resize, cols, rows
        {$txt p/(list @c)}                              ::  utf32 text
        {$yow p/gill:gall}                              ::  connect to app
    ==                                                  ::
  ++  dill-blit                                         ::  new blit
    $%  {$bel $~}                                       ::  make a noise
        {$clr $~}                                       ::  clear the screen
        {$hop p/@ud}                                    ::  set cursor position
        {$klr p/stub}                                   ::  styled text
        {$mor p/(list dill-blit)}                       ::  multiple blits
        {$pom p/stub}                                   ::  styled prompt
        {$pro p/(list @c)}                              ::  show as cursor+line
        {$qit $~}                                       ::  close console
        {$out p/(list @c)}                              ::  send output line
        {$sag p/path q/*}                               ::  save to jamfile
        {$sav p/path q/@}                               ::  save to file
        {$url p/@t}                                     ::  activate url
    ==                                                  ::
  ++  flog                                              ::  sent to %dill
    $%  {$crud p/@tas q/(list tank)}                    ::
        {$heft $~}                                      ::
        {$text p/tape}                                  ::
        {$veer p/@ta q/path r/@t}                       ::  install vane
        {$vega p/path}                                  ::  old reboot
        {$velo p/@t q/@t}                               ::  reboot
        {$verb $~}                                      ::  verbose mode
    ==                                                  ::
  ++  stub  (list (pair stye (list @c)))                ::  styled tuba
  ++  stye  (pair (set deco) (pair tint tint))          ::  decos/bg/fg
  ++  styl                                              ::  cascading stye
    %+  pair  (unit deco)                               ::
    (pair (unit tint) (unit tint))                      ::
  ::                                                    ::
  ++  styx  (list $@(@t (pair styl styx)))              ::  styled text
  ++  tint  ?($~ $r $g $b $c $m $y $k $w)               ::  text color
  --  ::dill
::                                                      ::::
::::                    ++eyre                            ::  (1e) oldweb
  ::                                                    ::::
++  eyre  ^?
  |%
  ::                                                    ::
  ::::                  ++able:eyre                     ::  (1e1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  out result <-$
      $%  {$mass p/mass}                                ::  memory usage
          {$mack p/(unit tang)}                         ::  message ack
          {$sigh p/cage}                                ::  marked http response
          {$thou p/httr}                                ::  raw http response
          {$thus p/@ud q/(unit hiss)}                   ::  http request+cancel
          {$veer p/@ta q/path r/@t}                     ::  drop-through
          {$vega p/path}                                ::  drop-through
          {$velo p/@t q/@t}                             ::  drop-through
          {$mini-jael-gift *}
      ==                                                ::
    ++  task                                            ::  in request ->$
      $%  {$born $~}                                    ::  new unix process
          {$crud p/@tas q/(list tank)}                  ::  XX rethink
          {$hiss p/(unit user) q/mark r/cage}           ::  outbound user req
          {$init p/@p}                                  ::  report install
          {$serv p/$@(desk beam)}                       ::  set serving root
          {$them p/(unit hiss)}                         ::  outbound request
          {$they p/@ud q/httr}                          ::  inbound response
          {$chis p/? q/clip r/httq}                     ::  IPC inbound request
          {$this p/? q/clip r/httq}                     ::  inbound request
          {$thud $~}                                    ::  inbound cancel
          {$wegh $~}                                    ::  report memory
          {$went p/sack q/path r/@ud s/coop}            ::  response confirm
          {$west p/sack q/{path @ud *}}                 ::  network request
          {$mini-jael-task *}
      ==                                                ::
    --  ::able
  ::
  ::::                                                  ::  (1e2)
    ::
  ++  bale                                              ::  driver state
    |*  a/_*                                            ::  %jael keys type
    $:  {our/ship now/@da eny/@uvJ byk/beak}            ::  base info
        {usr/user dom/(list @t)}                        ::  req user, domain
        key/a                                           ::  secrets from %jael
    ==                                                  ::
  ::
  ++  clip  (each @if @is)                              ::  client IP
  ++  cred                                              ::  credential
    $:  hut/hart                                        ::  client host
        aut/(jug @tas @t)                               ::  client identities
        orx/oryx                                        ::  CSRF secret
        acl/(unit @t)                                   ::  accept-language
        cip/(each @if @is)                              ::  client IP
        cum/(map @tas *)                                ::  custom dirt
    ==                                                  ::
  ++  epic                                              ::  FCGI parameters
    $:  qix/(map @t @t)                                 ::  query
        ced/cred                                        ::  client credentials
        bem/beam                                        ::  original path
        but/path                                        ::  ending
    ==                                                  ::
  ++  gram                                              ::  inter-ship message
    $?  {{$get $~} p/@uvH q/{? clip httq}}              ::  remote request
        {{$got $~} p/@uvH q/httr}                       ::  remote response
        {{$gib $~} p/@uvH}                              ::  remote cancel
    ==                                                  ::
  ++  hart  {p/? q/(unit @ud) r/host}                   ::  http sec+port+host
  ++  hate  {p/purl q/@p r/moth}                        ::  semi-cooked request
  ++  heir  {p/@ud q/mess r/(unit love)}                ::  status+headers+data
  ++  hiss  {p/purl q/moth}                             ::  outbound request
  ++  hole  @t                                          ::  session identity
  ++  hort  {p/(unit @ud) q/host}                       ::  http port+host
  ++  host  (each (list @t) @if)                        ::  http host
  ++  hoke  %+  each   {$localhost $~}                  ::  local host
            ?($.0.0.0.0 $.127.0.0.1)                    ::
  ++  httq                                              ::  raw http request
    $:  p/meth                                          ::  method
        q/@t                                            ::  unparsed url
        r/(list {p/@t q/@t})                            ::  headers
        s/(unit octs)                                   ::  body
    ==                                                  ::
  ++  httr  {p/@ud q/mess r/(unit octs)}                ::  raw http response
  ++  httx                                              ::  encapsulated http
    $:  p/?                                             ::  https?
        q/clip                                          ::  source IP
        r/httq                                          ::
    ==                                                  ::
  ++  user  knot                                        ::  username
  ++  love                                              ::  http response
    $%  {$ham p/manx}                                   ::  html node
        {$mid p/mite q/octs}                            ::  mime-typed data
        {$raw p/httr}                                   ::  raw http response
        {$wan p/wain}                                   ::  text lines
        {$zap p/@ud q/(list tank)}                      ::  status+error
    ==                                                  ::
  ++  math  (map @t (list @t))                          ::  semiparsed headers
  ++  mess  (list {p/@t q/@t})                          ::  raw http headers
  ++  meth                                              ::  http methods
    $?  $conn                                           ::  CONNECT
        $delt                                           ::  DELETE
        $get                                            ::  GET
        $head                                           ::  HEAD
        $opts                                           ::  OPTIONS
        $post                                           ::  POST
        $put                                            ::  PUT
        $trac                                           ::  TRACE
    ==                                                  ::
  ++  mite  (list @ta)                                  ::  mime type
  ++  moth  {p/meth q/math r/(unit octs)}               ::  http operation
  ++  octs  {p/@ud q/@t}                                ::  octet-stream
  ++  oryx  @t                                          ::  CSRF secret
  ++  pork  {p/(unit @ta) q/(list @t)}                  ::  fully parsed url
  ++  purf  (pair purl (unit @t))                       ::  url with fragment
  ++  purl  {p/hart q/pork r/quay}                      ::  parsed url
  ++  quay  (list {p/@t q/@t})                          ::  parsed url query
  ++  quer  |-($@($~ {p/@t q/@t t/$}))                  ::  query tree
  ++  quri                                              ::  request-uri
    $%  {$& p/purl}                                     ::  absolute
        {$| p/pork q/quay}                              ::  relative
    ==                                                  ::
  ++  rout  {p/(list host) q/path r/oryx s/path}        ::  http route (new)
  ++  sec-move                                          ::  driver effect
    $%  {$send p/hiss}                                  ::  http out
        {$show p/purl}                                  ::  direct user to url
        {$give p/httr}                                  ::  respond immediately
        {$redo $~}                                      ::  restart request qeu
    ==                                                  ::
  --  ::eyre
::                                                      ::::
::::                    ++ford                            ::  (1f) build
  ::                                                    ::::
++  ford  ^?
  |%
  ::                                                    ::
  ::::                  ++able:ford                     ::  (1f1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  out result <-$
      $%  {$made p/@uvH q/gage}                         ::  computed result
          {$mass p/mass}                                ::  memory usage
          {$news p/@uvH}                                ::  fresh depends
      ==                                                ::
    ++  task                                            ::  in request ->$
      $%  {$exec p/@p q/(unit bilk)}                    ::  make / kill
          {$wasp p/@p q/{@uvH ?}}                       ::  depends ask / kill
          {$wegh $~}                                    ::  report memory
          {$wipe p/@p $~}                               ::  clear cache
      ==                                                ::
    --  ::able
  ++  bilk  (pair beak silk)                            ::  sourced request
  ++  gage                                              ::  recursive cage+tang
    $%  {$& p/cage}                                     ::  success
        {$| p/tang}                                     ::  error
        {$tabl p/(list (pair gage gage))}               ::  table of results
    ==                                                  ::
  ++  hood                                              ::  assembly plan
    $:  zus/@ud                                         ::  zuse kelvin
        sur/(list hoof)                                 ::  structures
        lib/(list hoof)                                 ::  libraries
        fan/(list horn)                                 ::  resources
        src/(list hoop)                                 ::  program
    ==                                                  ::
  ++  hoof  (trel ? term (unit (pair case ship)))       ::  resource reference
  ++  hoop                                              ::  source in hood
    $%  {$& p/twig}                                     ::  direct twig
        {$| p/beam}                                     ::  resource location
    ==                                                  ::
  ++  hops                                              ::  XX late-bound path
    $:  pre/(unit tyke)                                 ::
        pof/(unit {p/@ud q/tyke})                       ::
    ==                                                  ::
  ++  horn                                              ::  resource tree
    $%  {$ape p/twig}                                   ::  /~  twig by hand
        {$arg p/twig}                                   ::  /$  argument
        {$alt p/(list horn)}                            ::  /|  options
        {$dep p/horn}                                   ::  /#  insert dephash
        {$dub p/term q/horn}                            ::  /=  apply face
        {$fan p/(list horn)}                            ::  /.  list
        {$for p/(list (pair spur horn))}                ::  /,  switch by path
        {$hel p/horn}                                   ::  /%  propagate args
        {$lin p/(list mark) q/horn}                     ::  /&  translates
        {$man p/(map knot horn)}                        ::  /*  hetero map
        {$nap p/horn}                                   ::  /_  homo map
        {$now p/horn}                                   ::  deprecated
        {$nod p/term q/horn}                            ::  /_  @  list by odor
        {$saw p/twig q/horn}                            ::  /;  operate on
        {$see p/hops q/horn}                            ::  /:  relative to
        {$sic p/twig q/horn}                            ::  /^  cast
        {$toy p/? q/mark}                               ::  /mark/  static/hook
    ==                                                  ::
  ++  milk  (trel ship desk silk)                       ::  sourced silk
  ++  silk                                              ::  construction layer
    $^  {p/silk q/silk}                                 ::  cons
    $%  {$$ p/cage}                                     ::  literal
        {$alts p/(list silk)}                           ::  options
        {$cntr p/mark q/coin r/beam}                    ::  local synthesis
        {$bunt p/mark}                                  ::  example of mark
        {$call p/silk q/silk}                           ::  slam
        {$cast p/mark q/silk}                           ::  translate
        {$core p/beam}                                  ::  build program
        {$diff p/silk q/silk}                           ::  diff
        {$dude p/(trap tank) q/silk}                    ::  error wrap
        {$file p/beam}                                  ::  from clay
        {$flag p/(set $@(@uvH beam)) q/silk}            ::  add dependencies
        {$join p/mark q/silk r/silk}                    ::  merge
        {$mash p/mark q/milk r/milk}                    ::  annotate
        {$mute p/silk q/(list (pair wing silk))}        ::  mutant
        {$pact p/silk q/silk}                           ::  patch
        {$plan p/beam q/coin r/hood}                    ::  structured assembly
        {$reef $~}                                      ::  kernel reef
        {$ride p/twig q/silk}                           ::  silk thru twig
        {$tabl p/(list (pair silk silk))}               ::  list
        {$vale p/mark q/*}                              ::  validate
        {$volt p/(cask *)}                              ::  unsafe add type
    ==                                                  ::
  --  ::ford
::                                                      ::::
::::                    ++gall                            ::  (1g) extensions
  ::                                                    ::::
++  gall  ^?
  |%
  ::                                                    ::
  ::::                  ++able:gall                     ::  (1g1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  outgoing result
      $%  {$mass p/mass}                                ::  memory usage
          {$onto p/(each suss tang)}                    ::  about agent
          {$rend p/path q/*}                            ::  network request
          {$unto p/cuft}                                ::
          {$mack p/(unit tang)}                         ::  message ack
      ==                                                ::
    ++  task                                            ::  incoming request
      $%  {$conf p/dock q/culm}                         ::  configure app
          {$init p/ship}                                ::  set owner
          {$deal p/sock q/cush}                         ::  full transmission
          {$went p/sack q/path r/@ud s/coop}            ::  response confirm
          {$west p/sack q/path r/@ud s/*}               ::  network request
          {$wegh $~}                                    ::  report memory
      ==                                                ::
    --  ::able
  ++  bitt  (map bone (pair ship path))                 ::  incoming subs
  ++  boat                                              ::  outgoing subs
    %+  map  (pair bone wire)                           ::
    (trel bean ship path)                               ::
  ++  bowl                                              ::  standard app state
          $:  $:  our/ship                              ::  host
                  src/ship                              ::  guest
                  dap/term                              ::  agent
              ==                                        ::
              $:  wex/boat                              ::  outgoing subs
                  sup/bitt                              ::  incoming subs
              ==                                        ::
              $:  ost/bone                              ::  opaque cause
                  act/@ud                               ::  change number
                  eny/@uvJ                              ::  entropy
                  now/@da                               ::  current time
                  byk/beak                              ::  load source
          ==  ==                                        ::
  ++  club                                              ::  agent action
    $%  {$peel p/mark q/path}                           ::  translated peer
        {$peer p/path}                                  ::  subscribe
        {$poke p/cage}                                  ::  apply
        {$puff p/mark q/noun}                           ::  unchecked poke
        {$pull $~}                                      ::  unsubscribe
        {$punk p/mark q/cage}                           ::  translated poke
        {$pump $~}                                      ::  pump yes+no
    ==                                                  ::
  ++  cuft                                              ::  internal gift
    $%  {$coup p/(unit tang)}                           ::  poke result
        {$diff p/cage}                                  ::  subscription output
        {$doff p/mark q/noun}                           ::  untyped diff
        {$quit $~}                                      ::  close subscription
        {$reap p/(unit tang)}                           ::  peer result
    ==                                                  ::
  ++  culm                                              ::  config action
    $%  {$load p/scup}                                  ::  load+reload
    ::  {$kick $~}                                      ::  restart everything
    ::  {$stop $~}                                      ::  toggle suspend
    ::  {$wipe $~}                                      ::  destroy all state
    ==                                                  ::
  ++  cush  (pair term club)                            ::  internal task
  ++  dude  term                                        ::  server identity
  ++  gill  (pair ship term)                            ::  general contact
  ++  scup  (pair ship desk)                            ::  autoupdate
  ++  suss  (trel dude @tas @da)                        ::  config report
  ++  well  (pair desk term)                            ::
  --  ::gall
::                                                      ::::
::::                    ++jael                          ::  (1h) security
  ::                                                    ::::
++  jael  ^?
  |%
  ::                                                    ::
  ::::                  ++able:jael                     ::  (1h1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    =,  pki
    =,  rights
    |%
    ::  %jael has two general kinds of task: changes
    ::  and change subscriptions.
    ::
    ::  change tasks are designed to match high-level
    ::  operations - for instance, we have %ktsg, %mint,
    ::  and %move, not just a single delta operation.
    ::  more of these operations will probably be added,
    ::  and invariants enforced at transaction end.
    ::
    ::  subscriptions are also user-focused - for instance,
    ::  %vein sends all the information needed to maintain
    ::  the secure channel, both rights and certificates.
    ::  the security-critical tasks (%veil, %vein, %vine)
    ::  should probably be bound to a whitelisted duct set.
    ::  (all secrets are redacted from %vest gifts.)
    ::
    ::  %jael only talks to %ames and %behn.  we send messages
    ::  through %ames and use %behn timers.
    ::
    ++  action                                          ::  balance change
      %+  pair  ship                                    ::  partner
      %+  each  bump                                    ::  &/liability change
      bump                                              ::  |/asset change
    ::                                                  ::
    ++  balance                                         ::  balance sheet
      %+  pair                                          ::
        (map ship safe)                                 ::  liabilities
      (map ship safe)                                   ::  assets
    ::                                                  ::
    ++  change                                          ::  urbit change
      $%  $:  $fact                                     ::  certificate change
              rex/ship                                  ::  owner
              vie/(unit (unit ship))                    ::  made/heard from
              lyf/life                                  ::  deed added/modified
              gan/growth                                ::  info gained
          ==                                            ::
          $:  $rite                                     ::  rights change
              rex/ship                                  ::  issuer
              pal/ship                                  ::  issued to
              del/bump                                  ::  change
      ==  ==                                            ::
    ::                                                  ::
    ++  channel                                         ::  secure channel
      $:  out/(unit (pair hand bill))                   ::  outbound key
          inn/(map hand bill)                           ::  inbound keys
          cur/(unit life)                               ::  their version
          sax/(list ship)                               ::  their ancestry
          pub/will                                      ::  their public keys
       ==                                               ::
    ++  gift                                            ::  out result <-$
      $?  {$veil p/channel}                             ::  secure channel
          {$vest p/tally}                               ::  balance update
          {$vein p/life q/(map life ring)}              ::  private keys
          {$vine p/(list change)}                       ::  all raw changes
      ==                                                ::
    ++  growth                                          ::  unit of learning
      $%  {$sign p/mind q/@}                            ::  add/update signature
          {$step p/cert}                                ::  add whole deed
      ==                                                ::
    ++  note                                            ::  out request $->
      $%  {$b $wait p/@da}                              ::  wait until
          {$x $mess p/ship q/path r/*}                  ::  send message
      ==                                                ::
    ++  remote                                          ::  remote notification
      %+  each  safe                                    ::  &/addition
      safe                                              ::  |/replacement
    ::                                                  ::
    ++  sign                                            ::  in result $<-
      $%  {$b $wake $~}                                 ::  wakeup
          {$x $rest p/coop}                             ::  message result
      ==                                                ::
    ++  tally                                           ::  balance update
      %+  each  balance                                 ::  complete
      action                                            ::  change
    ::
    ++  task                                            ::  in request ->$
      $%  {$ktsg p/ship q/safe}                         ::  destroy rights
          {$hail p/ship q/remote}                       ::  remote update
          {$init p/@pG q/arms}                          ::  initialize urbit
          {$meet p/(unit (unit ship)) q/farm}           ::  integrate pki from
          {$mint p/ship q/safe}                         ::  create rights
          {$move p/ship q/ship r/safe}                  ::  transfer from/to
          {$next p/bull}                                ::  update private key
          {$nuke $~}                                    ::  cancel tracker from
          {$veil p/ship}                                ::  view secret channel
          {$vein $~}                                    ::  view signing keys
          {$vest $~}                                    ::  view public balance
          {$vine $~}                                    ::  view secret history
          {$jaelwomb p/task:womb}                       ::  XX not factored in
          {$west p/ship q/path r/*}                     ::  remote request
      ==                                                ::
    ++  gilt  gilt:womb
    --
  ::
  ++  womb  ^?
    ::  types used to serve the lib/womb invite controller
    |%
    ++  ticket  @G                                      ::  old 64-bit ticket
    ++  passcode  @uvH                                  ::  128-bit passcode
    ++  passhash  @uwH                                  ::  passocde hash
    ++  mail  @t                                        ::  email address
    ++  invite                                          ::
      $:  who/mail                                      ::  owner email
          pla/@ud                                       ::  planets to send
          sta/@ud                                       ::  stars to send
      ==                                                ::
    ::                                                  ::
    ++  reinvite  {tid/passcode inv/invite}             ::  new from old
    ++  task                                            ::  manage ship %fungi
      $%  {$claim aut/passcode her/@p tik/ticket}       ::  convert to %final
          {$bonus tid/passcode pla/@ud sta/@ud}         ::  supplement passcode
          {$invite tid/passcode inv/invite}             ::  alloc to passcode
          {$reinvite aut/passcode reinvite}             ::  move to another
      ==                                                ::
    ++  scry                                            ::
      $%  {$shop typ/?($star $planet) nth/@u}           ::  available ships
          {$stats who/ship}                             ::  ship details
          {$balance aut/passcode}                       ::  invite details
      ==                                                ::
    ++  balance  {who/mail pla/@ud sta/@ud}             ::  XX same as invite?
    ++  gilt                                            ::
      $%  {$ships (list ship)}                          ::
          {$womb-owner (unit mail)}                     ::
          {$womb-balance (unit balance)}                ::
      ==
    --
  ::                                                    ::
  ::::                  ++pki:jael                      ::  (1h2) certificates
    ::                                                  ::::
  ++  pki  ^?
    |%
    ::  the urbit meta-certificate (++will) is a sequence
    ::  of certificates (++cert).  each cert in a will
    ::  revokes and replaces the previous cert.  the
    ::  version number of a ship is a ++life.
    ::
    ::  the deed contains an ++arms, a definition
    ::  of cosmetic identity; a semi-trusted parent,
    ::  which signs the initial certificate and provides
    ::  routing services; and a dirty bit.  if the dirty
    ::  bit is set, the new life of this ship may have
    ::  lost information that the old life had.
    ::
    ++  arms  (map chip (pair @ta @t))                  ::  stated identity
    ++  bull                                            ::  cert metadata
      $:  dad/ship                                      ::  parent
          dob/?                                         ::  & clean, | dirty
          nym/arms                                      ::  identity strings
      ==                                                ::
    ++  cert  (tale deed)                               ::  signed deed
    ++  chip                                            ::  standard identity
      $?  $giv                                          ::  given name
          $sur                                          ::  surname
          $had                                          ::  fictitious name
          $mid                                          ::  middle name
      ==                                                ::
    ++  deed                                            ::  certificate deed
      $:  doc/bull                                      ::  metadata
          pub/pass                                      ::  public key
      ==                                                ::
    ++  farm  (map ship will)                           ::  pki dump set
    ++  hand  @uvH                                      ::  128-bit hash
    ++  life  @ud                                       ::  ship version
    ++  mind  {who/ship lyf/life}                       ::  key identifier
    ++  name  (pair @ta @t)                             ::  ascii / unicode
    ++  oath  @                                         ::  signature
    ++  tale                                            ::  urbit-signed *
      |*  typ/mold                                      ::  payload mold
      $:  dat/typ                                       ::  data
          syg/(map ship (pair life oath))               ::  signatures
      ==                                                ::
    ++  will  (map life cert)                           ::  meta-certificate
    --  ::  pki
  ::                                                    ::
  ::::                  ++rights:jael                   ::  (1h3) claims
    ::                                                  ::::
  ++  rights  ^?
    =,  pki
    |%
    ::  %jael tracks promises (++rite) from ship to ship.
    ::  a rite may be any right, badge, asset, secret, etc.
    ::  un-shared secret or private asset is stored as a
    ::  rite from self to self.
    ::
    ::  each rite is really a class of rights, and often
    ::  has its own internal set or map structure.
    ::
    ::  present kinds of rite:
    ::
    ::    %apple: application secret for a web api.
    ::    %block: the promisee is banned.
    ::    %email: email tied to promissee's ship.
    ::    %final: ship/ticket pair, ready to launch.
    ::    %fungi: fungible, countable asset.
    ::    %guest: permission to adopt foreign child.
    ::    %hotel: block of unissued children.
    ::    %jewel: urbit private keys.
    ::    %login: user's login passcode.
    ::    %pword: password for a website/api.
    ::    %token: user access token for a web api.
    ::    %urban: symmetric key for urbit networking.
    ::
    ::  %fungi keys can be anything, but don't reuse
    ::  currency codes.  codes for urbit invitations:
    ::  %ugl == galaxy, %usr == star, %upl == planet
    ::
    ::  you can think of [our her rite] as an rdf triple.
    ::
    ++  bill  (pair @da @)                              ::  expiring value
    ++  bump                                            ::  rights change
      $:  mor/safe                                      ::  add rights
          les/safe                                      ::  lose rights
      ==                                                ::
    ++  dorm  (pair ship bloq)                          ::  issuing group
    ++  pile  (tree (pair @ @))                         ::  efficient ship set
    ++  rite                                            ::  urbit commitment
      $%  {$apple p/(map site @)}                       ::  web api key
          {$block $~}                                   ::  banned
          {$email p/(set @t)}                           ::  email addresses
          {$final p/@pG}                                ::  recognize by ticket
          {$fungi p/(map term @ud)}                     ::  fungibles
          {$guest $~}                                   ::  refugee visa
          {$hotel p/(map dorm pile)}                    ::  reserved block
          {$jewel p/(map life ring)}                    ::  private keyring
          {$login p/(set @pG)}                          ::  login secret
          {$pword p/(map site (map @t @t))}             ::  web passwd by user
          {$token p/(map site (map @t @t))}             ::  app tokens by user
          {$urban p/(map hand bill)}                    ::  urbit symmetric keys
      ==                                                ::
    ++  site  (list @ta)                                ::  [%com %yahoo %www ~]
    ++  safe  (tree rite)                               ::  rights set
    --  ::  rights
  --  ::  jael
::                                                      ::::
::::                    ++xmas                            ::  (1i) new network
  ::                                                    ::::
++  xmas  ^?
  ::                                                    ::
  ::::                  ++able:xmas                     ::  (1i1) arvo moves
    ::                                                  ::::
  |%
  ++  able  ^?
    |%
    ++  gift                                            ::
      $%  {$east p/*}                                   ::  response message
          {$home p/lane q/@}                            ::  process forward
          {$send p/lane q/@}                            ::  send packet
          {$rest p/coop}                                ::  acknowledgment
      ==                                                ::
    ++  task                                            ::  in request ->$
      $%  {$hear p/lane q/@}                            ::
          {$mess p/ship q/path r/*}                     ::  send message
          {$wake $~}                                    ::
      ==                                                ::
    ++  card                                            ::  out cards
      $%  {$west p/ship q/path r/*}                     ::  network request
      ==                                                ::
    ++  sign                                            ::  in response $-<
      $%  {$g $rend p/path q/*}                         ::  network request
          {$g $mack p/(unit tang)}                      ::  message ack
      ==                                                ::
    ++  note                                            ::  out request $->
      $%  {$c $west p/ship q/path r/*}                  ::  to %clay
          {$e $west p/ship q/path r/*}                  ::  to %eyre
          {$g $west p/ship q/path r/*}                  ::  to %gall
          $:  $j                                        ::  to %jael
              $%  {$line p/ship q/@da r/code}           ::
                  {$link p/ship q/@da r/code}           ::
                  {$meet p/farm:pki:jael}               ::
                  {$veil p/ship}                        ::
                  {$west p/ship q/path r/*}             ::  to %gall
      ==  ==  ==                                        ::
    --  ::  able
  ::
  ::::                                                  ::  (1i2)
    ::
  ++  code  @uvI                                        ::  symmetric key
  ++  lane                                              ::  packet route
    $%  {$if p/@da q/@ud r/@if}                         ::  IP4/public UDP/addr
        {$is p/@ud q/(unit lane) r/@is}                 ::  IPv6 w+alternates
        {$ix p/@da q/@ud r/@if}                         ::  IPv4 provisional
    ==                                                  ::
  ++  life  @ud                                         ::  regime number
  --  ::xmas
--  ::
