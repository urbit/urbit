!:                                                      ::  /van/zuse
::                                                      ::  !%reference
::  %zuse: arvo library.
::
::  %zuse contains models (data structures) and engines
::  (functions/classes) shared across arvo's eight major
::  vanes (kernel modules).  these are:
::
::      - %ames: networking
::      - %behn: scheduling
::      - %clay: revision control
::      - %dill: console
::      - %eyre: web
::      - %ford: build
::      - %gall: application
::      - %jael: security
::  
::  every model or engine in %zuse is attached to some 
::  vane, but any vane can use it (in its namespace),
::  as can any user-level code that uses `%zuse`.
::
~%  %zuse  +>  ~
=>  
::                                                      ::::
::::                    # models                        ::  data structures
  ::                                                    ::::
|%
::                                                      ::
::::                    ## ames/models                  ::  networking
  ::                                                    ::::
++  ames
  |%
  ++  $  %$
  --
::                                                      ::
::::                    ## behn/models                  ::  scheduling
  ::                                                    ::::
++  behn
  |%
  ++  $  %$
  --
::                                                      ::
::::                    ## clay/models                  ::  storage
  ::                                                    ::::
++  clay
  |%
::                                                      ::
::::                    ### differ/clay/models          ::  hunt-mcilroy diff
  ::                                                    ::::
  ++  differ  ^?
    |%
    ++  udal                                            ::  atomic change (%b)
      $:  p/@ud                                         ::  blockwidth
          q/(list {p/@ud q/(unit {p/@ q/@})})           ::  indels
      ==                                                ::
    ++  udon                                            ::  abstract delta
      $:  p/umph                                        ::  preprocessor
          $=  q                                         ::  patch
          $%  {$a p/* q/*}                              ::  trivial replace
              {$b p/udal}                               ::  atomic indel
              {$c p/(urge)}                             ::  list indel
              {$d p/upas q/upas}                        ::  tree edit
      ==  ==                                            ::
    ++  umph                                            ::  change filter
      $@  $?  $a                                        ::  no filter
              $b                                        ::  jamfile
              $c                                        ::  LF text
          ==                                            ::
      $%  {$d p/@ud}                                    ::  blocklist
      ==                                                ::
    ++  unce                                            ::  change part
      |*  a/mold                                        ::
      $%  {$& p/@ud}                                    ::  skip[copy]
          {$| p/(list a) q/(list a)}                    ::  p -> q[chunk]
      ==                                                ::
    ++  upas                                            ::  tree change (%d)
      $^  {p/upas q/upas}                               ::  cell
      $%  {$0 p/axis}                                   ::  copy old
          {$1 p/*}                                      ::  insert new
          {$2 p/axis q/udon}                            ::  mutate!
      ==                                                ::
    ++  urge  |*(a/mold (list (unce a)))                ::  list change
    --  ::differ
  --
::                                                      ::
::::                    ## dill/models                  ::  console
  ::                                                    ::::
++  dill
  |%
  ++  $  %$                                             ::  placeholder
  --
::                                                      ::
::::                    ## eyre/models                  ::  web
  ::                                                    ::::
++  eyre
  |%
  ++  $  %$
  --
::                                                      ::
::::                    ## ford/models                  ::  build
  ::                                                    ::::
++  ford
  |%
  ++  $  %$
  --
::                                                      ::
::::                    ## gall/models                  ::  application
  ::                                                    ::::
++  gall
  |%
  ++  $  %$
  --
::                                                      ::
::::                    ## jael/models                  ::  security
  ::                                                    ::::
++  jael  ^?
  |%
::                                                      ::
::::                    ### pki/jael/models             ::  certificates
  ::                                                    ::::
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
    ++  ares  (unit {p/term q/(list tank)})             ::  possible error
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
    ++  coop  (unit ares)                               ::  e2e ack
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
    --  ::pki
::                                                      ::
::::                    ### rights/jael/models          ::  rights or promises
  ::                                                    ::
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
          {$email p/(set @ta)}                          ::  email addresses
          {$final p/(map ship @pG)}                     ::  ticketed ships
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
    --  ::rights
::                                                      ::
::::                    ### moves/jael/models           ::  i/o
  ::                                                    ::::
  ++  moves  ^?
    =,  pki
    =,  rights
    |%
    ::  %jael has two general kinds of task: changes
    ::  and change subscriptions.
    ::
    ::  change tasks are designed to match high-level
    ::  operations - for instance, we have %burn, %mint,
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
      $%  {$burn p/ship q/safe}                         ::  destroy rights
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
          {$west p/ship q/path r/*}                     ::  remote request
      ==                                                ::
    --  ::moves
  --  ::jael
--
::                                                      ::::
::::                    # engines                       ::  engines
  ::                                                    ::::
|%
::                                                      ::
::::                    ## ames/engines                 ::  networking
  ::                                                    ::::
++  ames
  |%
  ++  $  %$
  --
::                                                      ::
::::                    ## behn/engines                 ::  scheduling
  ::                                                    ::::
++  behn
  |%
::                                                      ::
::::                    ### chrono/engines/behn         ::  time
  ::                                                    ::::
  ++  chrono  ^?
    |%
    ::                                                  ::  ++dawn:chrono:behn
    ++  dawn                                            ::  Jan 1 weekday
      |=  yer/@ud
      =+  yet=(sub yer 1)
      %-  mod  :_  7
      ;:  add
        1 
        (mul 5 (mod yet 4)) 
        (mul 4 (mod yet 100)) 
        (mul 6 (mod yet 400))
      ==
    ::                                                  ::  ++daws:chrono:behn
    ++  daws                                            ::  date weekday
      |=  yed/date
      %-  mod  :_  7
      %+  add
        (dawn y.yed) 
      (sub (yawn [y.yed m.yed d.t.yed]) (yawn y.yed 1 1))
    ::                                                  ::  ++deal:chrono:behn
    ++  deal                                            ::  to leap sec time
      |=  yer/@da
      =+  n=0
      =+  yud=(yore yer)
      |-  ^-  date
      ?:  (gte yer (add (snag n lef:yu) ~s1))
        (yore (year yud(s.t (add n s.t.yud))))
      ?:  &((gte yer (snag n lef:yu)) (lth yer (add (snag n lef:yu) ~s1)))
        yud(s.t (add +(n) s.t.yud))
      ?:  =(+(n) (lent lef:yu))
        (yore (year yud(s.t (add +(n) s.t.yud))))
      $(n +(n))
    ::                                                  ::  ++lead:chrono:behn
    ++  lead                                            ::  from leap sec time
      |=  ley/date
      =+  ler=(year ley)
      =+  n=0
      |-  ^-  @da
      =+  led=(sub ler (mul n ~s1))
      ?:  (gte ler (add (snag n les:yu) ~s1))
        led
      ?:  &((gte ler (snag n les:yu)) (lth ler (add (snag n les:yu) ~s1)))
        ?:  =(s.t.ley 60)
          (sub led ~s1)
        led
      ?:  =(+(n) (lent les:yu))
        (sub led ~s1)
      $(n +(n))
    ::                                                  ::  ++dust:chrono:behn
    ++  dust                                            ::  print UTC format
      |=  yed/date
      ^-  tape
      =+  wey=(daws yed)
      ;:  weld
          `tape`(snag wey (turn wik:yu |=(a/tape (scag 3 a))))
          ", "  ~(rud at d.t.yed)  " "
          `tape`(snag (dec m.yed) (turn mon:yu |=(a/tape (scag 3 a))))
          " "  (scag 1 ~(rud at y.yed))  (slag 2 ~(rud at y.yed))  " "
          ~(rud at h.t.yed)  ":"  ~(rud at m.t.yed)  ":"  ~(rud at s.t.yed)
          " "  "+0000"
      ==
    ::                                                  ::  ++stud:chrono:behn
    ++  stud                                            ::  parse UTC format
      =<  |=  a/cord
          %+  biff  (rush a (more sepa elem))
          |=  b/(list _(wonk *elem))  ^-  (unit date)
          =-  ?.((za:jo:markup -) ~ (some (zp:jo:markup -)))
          ^+  =+  [*date u=unit]
              *{(u _[a y]) (u _m) (u _d.t) (u _+.t) $~}
          :~
              |-(?~(b ~ ?.(?=($y -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=($m -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=($d -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=($t -.i.b) $(b t.b) `+.i.b)))
          ==
      |%
      ++  snug                                          ::  position in list
        |=  a/(list tape)
        |=  b/tape
        =+  [pos=1 len=(lent b)]
        |-  ^-  (unit @u)
        ?~  a  ~
        ?:  =(b (scag len i.a))
          `pos
        $(pos +(pos), a t.a)
      ::
      ++  sepa                                          ::  separator
        ;~(pose ;~(plug com (star ace)) (plus ace))
      ::
      ++  elem                                          ::  date element
        ;~  pose 
          (stag %t t)  (stag %y y)  (stag %m m)  (stag %d d)
          (stag %w w)  (stag %z z)
        == 
      ::
      ++  y                                             ::  year
        (stag %& (bass 10 (stun 3^4 dit)))
      ::
      ++  m                                             ::  month
        (sear (snug mon:yu) (plus alf))
      ::
      ++  d                                             ::  day
        (bass 10 (stun 1^2 dit))
      ::
      ++  t                                             ::
        [;~(plug - - + (easy ~))]:[;~(sfix d col) d]
      ::
      ++  w                                             ::  week
        (sear (snug wik:yu) (plus alf))
      ::
      ++  z                                             ::
        [;~(plug (mask "-+") . .)]:(bass 10 (stun 2^2 dit))
      --
    ::                                                  ::  ++unt:chrono:behn
    ++  unt                                             ::  Urbit to Unix time
      |=  a/@
      (div (sub a ~1970.1.1) ~s1)
    ::                                                  ::  ++yu:chrono:behn
    ++  yu                                              ::  UTC format constants
      |%
      ::                                                ::  ++mon:yu:chrono:
      ++  mon                                           ::  months
        ^-  (list tape)
        :~  "January"  "February"  "March"  "April"  "May"  "June"  "July"
            "August"  "September"  "October"  "November"  "December"
        ==
      ::                                                ::  ++wik:yu:chrono:
      ++  wik                                           ::  weeks
        ^-  (list tape)
        :~  "Sunday"  "Monday"  "Tuesday"  "Wednesday"  "Thursday"
            "Friday"  "Saturday"
        ==
      ::                                                ::  ++lef:yu:chrono:
      ++  lef                                           ::  leapsecond dates
        ^-  (list @da)
        :~  ~2015.6.30..23.59.59   ~2012.6.30..23.59.59
            ~2008.12.31..23.59.58  ~2005.12.31..23.59.57
            ~1998.12.31..23.59.56  ~1997.6.30..23.59.55
            ~1995.12.31..23.59.54  ~1994.6.30..23.59.53
            ~1993.6.30..23.59.52   ~1992.6.30..23.59.51
            ~1990.12.31..23.59.50  ~1989.12.31..23.59.49
            ~1987.12.31..23.59.48  ~1985.6.30..23.59.47
            ~1983.6.30..23.59.46   ~1982.6.30..23.59.45
            ~1981.6.30..23.59.44   ~1979.12.31..23.59.43
            ~1978.12.31..23.59.42  ~1977.12.31..23.59.41
            ~1976.12.31..23.59.40  ~1975.12.31..23.59.39
            ~1974.12.31..23.59.38  ~1973.12.31..23.59.37
            ~1972.12.31..23.59.36  ~1972.6.30..23.59.35
        ==
      ::                                                ::  ++les:yu:chrono:
      ++  les                                           ::  leapsecond days
        ^-  (list @da)
        :~  ~2015.7.1  ~2012.7.1  ~2009.1.1  ~2006.1.1  ~1999.1.1  ~1997.7.1
            ~1996.1.1  ~1994.7.1  ~1993.7.1  ~1992.7.1  ~1991.1.1  ~1990.1.1
            ~1988.1.1  ~1985.7.1  ~1983.7.1  ~1982.7.1  ~1981.7.1  ~1980.1.1
            ~1979.1.1  ~1978.1.1  ~1977.1.1  ~1976.1.1  ~1975.1.1  ~1974.1.1
            ~1973.1.1  ~1972.7.1
        ==
      --  ::yu
    --  ::chrono
  --  ::behn
::                                                      ::
::::                    ## clay/engines                 ::  storage
  ::                                                    ::::
++  clay
  |%
::                                                      ::
::::                    ### differ/clay/engines         ::  hunt-mcilroy diff
  ::                                                    ::::
  ++  differ  ^?
    =,  differ:^clay
    =,  lines
    |%
    ::                                                  ::  ++berk:differ:clay
    ++  berk                                            ::  invert diff patch
      |*  bur/(urge)
      |-  ^+  bur
      ?~  bur  ~
      :_  $(bur t.bur)
      ?-  -.i.bur
        $&  i.bur
        $|  [%| q.i.bur p.i.bur]
      ==
    ::                                                  ::  ++diff:differ:clay
    ++  diff                                            ::  generate patch
      |=  pum/umph
      |=  {old/* new/*}  ^-  udon
      :-  pum
      ?+  pum  ~|(%unsupported !!)
        $a  [%d (nude old new)]
        $b  =+  [hel=(cue ((hard @) old)) hev=(cue ((hard @) new))]
            [%d (nude hel hev)]
        $c  =+  [hel=(lore ((hard @) old)) hev=(lore ((hard @) new))]
            [%c (lusk hel hev (loss hel hev))]
      ==
    ::                                                  ::  ++loss:differ:clay
    ++  loss                                            ::  longest subsequence
      ~%  %loss  ..is  ~
      |*  {hel/(list) hev/(list)}
      |-  ^+  hev
      =+  ^=  sev
          =+  [inx=0 sev=*(map _i.-.hev (list @ud))]
          |-  ^+  sev
          ?~  hev  sev
          =+  guy=(~(get by sev) i.hev)
          %=  $
            hev  t.hev
            inx  +(inx)
            sev  (~(put by sev) i.hev [inx ?~(guy ~ u.guy)])
          ==
      =|  gox/{p/@ud q/(map @ud {p/@ud q/_hev})}
      =<  abet
      =<  main
      |%
      ::                                                ::  ++abet:loss:differ:
      ++  abet                                          ::  subsequence
        ^+  hev
        ?:  =(0 p.gox)  ~
        (flop q:(need (~(get by q.gox) (dec p.gox))))
      ::                                                ::  ++hink:loss:differ:
      ++  hink                                          ::  extend fits top
        |=  {inx/@ud goy/@ud}  ^-  ?
        ?|  =(p.gox inx) 
            (lth goy p:(need (~(get by q.gox) inx)))
        ==
      ::                                                ::  ++lonk:loss:differ:
      ++  lonk                                          ::  extend fits bottom
        |=  {inx/@ud goy/@ud}  ^-  ?
        ?|  =(0 inx) 
            (gth goy p:(need (~(get by q.gox) (dec inx))))
        ==
      ::                                                ::  ++luna:loss:differ:
      ++  luna                                          ::  extend
        |=  {inx/@ud goy/@ud}
        ^+  +>
        %_    +>.$
            gox
          :-  ?:(=(inx p.gox) +(p.gox) p.gox)
          %+  ~(put by q.gox)  inx
          :+  goy
            (snag goy hev) 
          ?:(=(0 inx) ~ q:(need (~(get by q.gox) (dec inx))))
        ==
      ::                                                ::  ++merg:loss:differ:
      ++  merg                                          ::  merge all matches
        |=  gay/(list @ud)
        ^+  +>
        =+  ^=  zes
            =+  [inx=0 zes=*(list {p/@ud q/@ud})]
            |-  ^+  zes
            ?:  |(?=($~ gay) (gth inx p.gox))  zes
            ?.  (lonk inx i.gay)  $(gay t.gay)
            ?.  (hink inx i.gay)  $(inx +(inx))
            $(inx +(inx), gay t.gay, zes [[inx i.gay] zes])
        |-  ^+  +>.^$
        ?~(zes +>.^$ $(zes t.zes, +>.^$ (luna i.zes)))
      ::                                                ::  ++main:loss:differ:
      ++  main                                          ::
        =+  hol=hel
        |-  ^+  +>
        ?~  hol  +>
        =+  guy=(~(get by sev) i.hol)
        $(hol t.hol, +> (merg (flop `(list @ud)`?~(guy ~ u.guy))))
      --
    ::                                                  ::  ++lump:differ:clay
    ++  lump                                            ::  apply patch
      |=  {don/udon src/*}
      ^-  *
      ?+    p.don  ~|(%unsupported !!)
          $a
        ?+  -.q.don  ~|(%unsupported !!)
          $a  q.q.don
          $c  (lurk ((hard (list)) src) p.q.don)
          $d  (lure src p.q.don)
        ==
      ::
          $c
        =+  dst=(lore ((hard @) src))
        %-  role
        ?+  -.q.don  ~|(%unsupported !!)
          ::
          ::  XX  these hards should not be needed; udon 
          ::      should be parameterized
          ::
          $a  ((hard (list @t)) q.q.don)
          $c  ((hard (list @t)) (lurk `(list *)`dst p.q.don))
        ==
      ==
    ::                                                  ::  ++lure:differ:clay
    ++  lure                                            ::  apply tree diff
      |=  {a/* b/upas}
      ^-  *
      ?^  -.b
        [$(b -.b) $(b +.b)]
      ?+  -.b  ~|(%unsupported !!)
        $0  .*(a [0 p.b])
        $1  .*(a [1 p.b])
      ==
    ::                                                  ::  ++limp:differ:clay
    ++  limp                                            ::  invert patch
      |=  don/udon  ^-  udon
      :-  p.don
      ?+  -.q.don  ~|(%unsupported !!)
        $a  [%a q.q.don p.q.don]
        $c  [%c (berk p.q.don)]
        $d  [%d q.q.don p.q.don]
      ==
    ::                                                  ::  ++hump:differ:clay
    ++  hump                                            ::  general prepatch
      |=  {pum/umph src/*}  ^-  *
      ?+  pum  ~|(%unsupported !!)
        $a  src
        $b  (cue ((hard @) src))
        $c  (lore ((hard @) src))
      ==
    ::                                                  ::  ++husk:differ:clay
    ++  husk                                            ::  unprepatch
      |=  {pum/umph dst/*}  ^-  *
      ?+  pum  ~|(%unsupported !!)
        $a  dst
        $b  (jam dst)
        $c  (role ((hard (list @)) dst))
      ==
    ::                                                  ::  ++lurk:differ:clay
    ++  lurk                                            ::  apply list patch
      |*  {hel/(list) rug/(urge)}
      ^+  hel
      =+  war=`_hel`~
      |-  ^+  hel
      ?~  rug  (flop war)
      ?-    -.i.rug
          $&
        %=   $
          rug  t.rug
          hel  (slag p.i.rug hel)
          war  (weld (flop (scag p.i.rug hel)) war)
        ==
      ::
          $|
        %=  $
          rug  t.rug
          hel  =+  gur=(flop p.i.rug)
               |-  ^+  hel
               ?~  gur  hel
               ?>(&(?=(^ hel) =(i.gur i.hel)) $(hel t.hel, gur t.gur))
          war  (weld q.i.rug war)
        ==
      ==
    ::                                                  ::  ++lusk:differ:clay
    ++  lusk                                            ::  lcs to list patch
      |*  {hel/(list) hev/(list) lcs/(list)}
      =+  ^=  rag
          ^-  {$%({$& p/@ud} {$| p/_lcs q/_lcs})}
          [%& 0]
      =>  .(rag [p=rag q=*(list _rag)])
      =<  abet  =<  main
      |%
      ::                                                ::  ++abet:lusk:differ:
      ++  abet                                          ::
        =.  q.rag  ?:(=([& 0] p.rag) q.rag [p.rag q.rag])
        (flop q.rag)
      ::                                                ::  ++done:lusk:differ:
      ++  done                                          ::
        |=  new/_p.rag
        ^+  rag
        ?-  -.p.rag
          $|   ?-  -.new
                $|  [[%| (weld p.new p.p.rag) (weld q.new q.p.rag)] q.rag]
                $&  [new [p.rag q.rag]]
              ==
          $&   ?-  -.new
                $|  [new ?:(=(0 p.p.rag) q.rag [p.rag q.rag])]
                $&  [[%& (add p.p.rag p.new)] q.rag]
              ==
        ==
      ::                                                ::  ++main:lusk:differ:
      ++  main                                          ::
        |-  ^+  +
        ?~  hel
          ?~  hev
            ?>(?=($~ lcs) +)
          $(hev t.hev, rag (done %| ~ [i.hev ~]))
        ?~  hev
          $(hel t.hel, rag (done %| [i.hel ~] ~))
        ?~  lcs
          +(rag (done %| (flop hel) (flop hev)))
        ?:  =(i.hel i.lcs)
          ?:  =(i.hev i.lcs)
            $(lcs t.lcs, hel t.hel, hev t.hev, rag (done %& 1))
          $(hev t.hev, rag (done %| ~ [i.hev ~]))
        ?:  =(i.hev i.lcs)
          $(hel t.hel, rag (done %| [i.hel ~] ~))
        $(hel t.hel, hev t.hev, rag (done %| [i.hel ~] [i.hev ~]))
      --
    ::                                                  ::  ++nude:differ:clay
    ++  nude                                            ::  tree change
      =<  |=  {a/* b/*}  ^-  {p/upas q/upas}
          [p=(tred a b) q=(tred b a)]
      |%
      ::                                                ::  ++axes:nude:differ:
      ++  axes                                          ::  locs of nouns
        |=  {a/@ b/*}  ^-  (map * axis)
        =+  c=*(map * axis)
        |-  ^-  (map * axis)
        =>  .(c (~(put by c) b a))
        ?@  b
          c
        %-  ~(uni by c)
        %-  ~(uni by $(a (mul 2 a), b -.b))
        $(a +((mul 2 a)), b +.b)
      ::                                                ::  ++tred:nude:differ:
      ++  tred                                          ::  diff a->b
        |=  {a/* b/*}  ^-  upas
        =|  c/(unit *)
        =+  d=(axes 1 a)
        |-  ^-  upas
        =>  .(c (~(get by d) b))
        ?~  c
          ?@  b
            [%1 b]
          =+  e=^-(upas [$(b -.b) $(b +.b)])
          ?-  e
            {{$1 *} {$1 *}}  [%1 [p.p.e p.q.e]]
            *  e
          ==
        [%0 u.c]
      --  ::nude
    --  ::differ
::                                                      ::
::::                    ### lines^clay/engines          ::  unix line files
  ::                                                    ::::
  ++  lines  ^?
    |%
    ::                                                  ::  ++lore:lines:clay
    ++  lore                                            ::  atom to line list
      ~%  %lore  ..is  ~
      |=  lub/@
      =|  tez/(list @t)
      |-  ^+  tez
      =+  ^=  wor
        =+  [meg=0 i=0]
        |-  ^-  {meg/@ i/@ end/@f}
        =+  gam=(cut 3 [i 1] lub)
        ?:  =(0 gam)
          [meg i %.y]
        ?:  =(10 gam)
          [meg i %.n]
        $(meg (cat 3 meg gam), i +(i))
      ?:  end.wor
        (flop ^+(tez [meg.wor tez]))
      ?:  =(0 lub)  (flop tez)
      $(lub (rsh 3 +(i.wor) lub), tez [meg.wor tez])
    ::                                                  ::  ++lune:lines:clay
    ++  lune                                            ::  cord by unix line
      ~%  %lune  ..is  ~
      |=  txt/@t
      ?~  txt
        ^-  (list @t)  ~
      =+  [byt=(rip 3 txt) len=(met 3 txt)]
      =|  {lin/(list @t) off/@}
      ^-  (list @t)
      %-  flop
      |-  ^+  lin
      ?:  =(off len)
        ~|  %noeol  !!
      ?:  =((snag off byt) 10)
        ?:  =(+(off) len)
          [(rep 3 (scag off byt)) lin]
        %=  $
          lin  [(rep 3 (scag off byt)) lin]
          byt  (slag +(off) byt)
          len  (sub len +(off))
          off  0
        ==
      $(off +(off))
    ::                                                ::  ++nule:lines:clay
    ++  nule                                          ::  lines to unix cord
      ~%  %nule  ..is  ~
      |=  lin/(list @t)
      ^-  @t
      %+  can  3
      %+  turn  lin
      |=  t/@t
      [+((met 3 t)) (cat 3 t 10)]
    ::                                                ::  ++role:differ:clay
    ++  role                                          ::  line list to atom
      |=  tez/(list @t)
      =|  {our/@ i/@ud}
      |-  ^-  @
      ?~  tez
        our
      ?:  =(%$ i.tez)
        $(i +(i), tez t.tez, our (cat 3 our 10))
      ?:  =(0 i)
        $(i +(i), tez t.tez, our i.tez)
      $(i +(i), tez t.tez, our (cat 3 (cat 3 our 10) i.tez))
    --  ::lines
  --  ::clay
::                                                      ::
::::                    # dill/engines                  ::  console
  ::                                                    ::::
++  dill
  |%
  ++  $  %$
  --
::                                                      ::
::::                    # eyre/engines                  ::  web
  ::                                                    ::::
++  eyre
  |%
  ++  $  %$
  --
::                                                      ::
::::                    # ford/engines                  ::  build
  ::                                                    ::::
++  ford
  |%
  ++  $  %$
  --
::                                                      ::
::::                    # gall/engines                  ::  application
  ::                                                    ::::
++  gall
  |%
  ++  $  %$
  --
::                                                      ::
::::                    # jael/engines                  ::  security
  ::                                                    ::::
++  jael
  |%
  ++  $  %$
  --
::                                                      ::
::::                    ## number/engines               ::  misc number theory
  ::                                                    ::::
++  number
  |%
  ::                                                    ::  ++fu:number
  ++  fu                                                ::  modulo (mul p q)
    |=  a/{p/@ q/@}
    =+  b=?:(=([0 0] a) 0 (~(inv fo p.a) (~(sit fo p.a) q.a)))
    |%
    ::                                                  ::  ++dif:fu:number
    ++  dif                                             ::  subtract
      |=  {c/{@ @} d/{@ @}}
      [(~(dif fo p.a) -.c -.d) (~(dif fo q.a) +.c +.d)]
    ::                                                  ::  ++exp:fu:number
    ++  exp                                             ::  exponent
      |=  {c/@ d/{@ @}}
      :-  (~(exp fo p.a) (mod c (dec p.a)) -.d)
      (~(exp fo q.a) (mod c (dec q.a)) +.d)
    ::                                                  ::  ++out:fu:number
    ++  out                                             ::  garner's formula
      |=  c/{@ @}
      %+  add  +.c
      %+  mul  q.a
      %+  ~(pro fo p.a)  b
      (~(dif fo p.a) -.c (~(sit fo p.a) +.c))
    ::                                                  ::  ++pro:fu:number   
    ++  pro                                             ::  multiply
      |=  {c/{@ @} d/{@ @}}
      [(~(pro fo p.a) -.c -.d) (~(pro fo q.a) +.c +.d)]
    ::                                                  ::  ++sum:fu:number
    ++  sum                                             ::  add
      |=  {c/{@ @} d/{@ @}}
      [(~(sum fo p.a) -.c -.d) (~(sum fo q.a) +.c +.d)]
    ::                                                  ::  ++sit:fu:number
    ++  sit                                             ::  represent
      |=  c/@
      [(mod c p.a) (mod c q.a)]
    --
  ::                                                    ::  ++pram:number
  ++  pram                                              ::  rabin-miller
    |=  a/@  ^-  ?
    ?:  ?|  =(0 (end 0 1 a))
            =(1 a)
            =+  b=1
            |-  ^-  ?
            ?:  =(512 b)
              |
            ?|(=+(c=+((mul 2 b)) &(!=(a c) =(a (mul c (div a c))))) $(b +(b)))
        ==
      |
    =+  ^=  b
        =+  [s=(dec a) t=0]
        |-  ^-  {s/@ t/@}
        ?:  =(0 (end 0 1 s))
          $(s (rsh 0 1 s), t +(t))
        [s t]
    ?>  =((mul s.b (bex t.b)) (dec a))
    =+  c=0
    |-  ^-  ?
    ?:  =(c 64)
      &
    =+  d=(~(raw og (add c a)) (met 0 a))
    =+  e=(~(exp fo a) s.b d)
    ?&  ?|  =(1 e)
            =+  f=0
            |-  ^-  ?
            ?:  =(e (dec a))
              &
            ?:  =(f (dec t.b))
              |
            $(e (~(pro fo a) e e), f +(f))
        ==
        $(c +(c))
    ==
  ::                                                    ::  ++ramp:number
  ++  ramp                                              ::  make r-m prime
    |=  {a/@ b/(list @) c/@}  ^-  @ux                   ::  {bits snags seed}
    =>  .(c (shas %ramp c))
    =+  d=*@
    |-
    ?:  =((mul 100 a) d)
      ~|(%ar-ramp !!)
    =+  e=(~(raw og c) a)
    ?:  &((levy b |=(f/@ !=(1 (mod e f)))) (pram e))
      e
    $(c +(c), d (shax d))
  ::                                                    ::  ++curt:number
  ++  curt                                              ::  curve25519
    |=  {a/@ b/@}
    =>  %=    .
            +
          =>  +
          =+  =+  [p=486.662 q=(sub (bex 255) 19)]
              =+  fq=~(. fo q)
              [p=p q=q fq=fq]
          |%
          ++  cla
            |=  raw/@
            =+  low=(dis 248 (cut 3 [0 1] raw))
            =+  hih=(con 64 (dis 127 (cut 3 [31 1] raw)))
            =+  mid=(cut 3 [1 30] raw)
            (can 3 [[1 low] [30 mid] [1 hih] ~])
          ++  sqr  |=(a/@ (mul a a))
          ++  inv  |=(a/@ (~(exp fo q) (sub q 2) a))
          ++  cad
            |=  {n/{x/@ z/@} m/{x/@ z/@} d/{x/@ z/@}}
            =+  ^=  xx
                ;:  mul  4  z.d
                  %-  sqr  %-  abs:si
                  %+  dif:si
                    (sun:si (mul x.m x.n))
                  (sun:si (mul z.m z.n))
                ==
            =+  ^=  zz
                ;:  mul  4  x.d
                  %-  sqr  %-  abs:si
                  %+  dif:si
                    (sun:si (mul x.m z.n))
                  (sun:si (mul z.m x.n))
                ==
            [(sit.fq xx) (sit.fq zz)]
          ++  cub
            |=  {x/@ z/@}
            =+  ^=  xx
                %+  mul
                  %-  sqr  %-  abs:si
                  (dif:si (sun:si x) (sun:si z))
                (sqr (add x z))
            =+  ^=  zz
                ;:  mul  4  x  z
                  :(add (sqr x) :(mul p x z) (sqr z))
                ==
            [(sit.fq xx) (sit.fq zz)]
          --
        ==
    =+  one=[b 1]
    =+  i=253
    =+  r=one
    =+  s=(cub one)
    |-
    ?:  =(i 0)
      =+  x=(cub r)
      (sit.fq (mul -.x (inv +.x)))
    =+  m=(rsh 0 i a)
    ?:  =(0 (mod m 2))
       $(i (dec i), s (cad r s one), r (cub r))
    $(i (dec i), r (cad r s one), s (cub s))
  ::                                                    ::  ++ga:number
  ++  ga                                                ::  GF (bex p.a)
    |=  a/{p/@ q/@ r/@}                                 ::  dim poly gen
    =+  si=(bex p.a)
    =+  ma=(dec si)
    =>  |%
        ::                                              ::  ++dif:ga:number
        ++  dif                                         ::  add and sub
          |=  {b/@ c/@}
          ~|  [%dif-ga a]
          ?>  &((lth b si) (lth c si))
          (mix b c)
        ::                                              ::  ++dub:ga:number
        ++  dub                                         ::  mul by x
          |=  b/@
          ~|  [%dub-ga a]
          ?>  (lth b si)
          ?:  =(1 (cut 0 [(dec p.a) 1] b))
            (dif (sit q.a) (sit (lsh 0 1 b)))
          (lsh 0 1 b)
        ::                                              ::  ++pro:ga:number
        ++  pro                                         ::  slow multiply
          |=  {b/@ c/@}
          ?:  =(0 b)
            0
          ?:  =(1 (dis 1 b))
            (dif c $(b (rsh 0 1 b), c (dub c)))
          $(b (rsh 0 1 b), c (dub c))
        ::                                              ::  ++toe:ga:number
        ++  toe                                         ::  exp+log tables
          =+  ^=  nu
              |=  {b/@ c/@}
              ^-  (map @ @)
              =+  d=*(map @ @)
              |-
              ?:  =(0 c)
                d
              %=  $
                c  (dec c)
                d  (~(put by d) c b)
              ==
          =+  [p=(nu 0 (bex p.a)) q=(nu ma ma)]
          =+  [b=1 c=0]
          |-  ^-  {p/(map @ @) q/(map @ @)}
          ?:  =(ma c)
            [(~(put by p) c b) q]
          %=  $
            b  (pro r.a b)
            c  +(c)
            p  (~(put by p) c b)
            q  (~(put by q) b c)
          ==
        ::                                              ::  ++sit:ga:number
        ++  sit                                         ::  reduce
          |=  b/@
          (mod b (bex p.a))
        --
    =+  toe
    |%
    ::                                                  ::  ++fra:ga:number
    ++  fra                                             ::  divide
      |=  {b/@ c/@}
      (pro b (inv c))
    ::                                                  ::  ++inv:ga:number
    ++  inv                                             ::  invert
      |=  b/@
      ~|  [%inv-ga a]
      =+  c=(~(get by q) b)
      ?~  c  !!
      =+  d=(~(get by p) (sub ma u.c))
      (need d)
    ::                                                  ::  ++pow:ga:number
    ++  pow                                             ::  exponent
      |=  {b/@ c/@}
      =+  [d=1 e=c f=0]
      |-
      ?:  =(p.a f)
        d
      ?:  =(1 (cut 0 [f 1] b))
        $(d (pro d e), e (pro e e), f +(f))
      $(e (pro e e), f +(f))
    ::                                                  ::  ++pro:ga:number
    ++  pro                                             ::  multiply
      |=  {b/@ c/@}
      ~|  [%pro-ga a]
      =+  d=(~(get by q) b)
      ?~  d  0
      =+  e=(~(get by q) c)
      ?~  e  0
      =+  f=(~(get by p) (mod (add u.d u.e) ma))
      (need f)
    --
  --
::                                                      ::
::::                    ## crypto/engines               ::  crypto standards
  ::                                                    ::::
++  crypto
  =,  number
  ^?  |%
  ++  aes  !.
    ~%  %aes  ..is  ~
    |%
    ::                                                  ::  ++ahem:aes:crypto
    ++  ahem                                            ::  AES engine
      |=  {nnk/@ nnb/@ nnr/@}
      =>
        =+  =>  [gr=(ga 8 0x11b 3) few==>(fe .(a 5))]
            [pro=pro.gr dif=dif.gr pow=pow.gr ror=ror.few]
        =>  |%
            ++  cipa                                    ::  AES params
              $_  ^?  |%
              ++  co  *{p/@ q/@ r/@ s/@}                ::  col coefs
              ++  ix  |~(a/@ *@)                        ::  key index
              ++  ro  *{p/@ q/@ r/@ s/@}                ::  row shifts
              ++  su  *@                                ::  s-box
              --
            --
        |%
        ::                                              ::  ++pen:aes:crypto
        ++  pen                                         ::  encrypt
          ^-  cipa
          |%
          ++  co  [0x2 0x3 1 1]
          ++  ix  |~(a/@ a)
          ++  ro  [0 1 2 3]
          ++  su  0x16bb.54b0.0f2d.9941.6842.e6bf.0d89.a18c.
                    df28.55ce.e987.1e9b.948e.d969.1198.f8e1.
                    9e1d.c186.b957.3561.0ef6.0348.66b5.3e70.
                    8a8b.bd4b.1f74.dde8.c6b4.a61c.2e25.78ba.
                    08ae.7a65.eaf4.566c.a94e.d58d.6d37.c8e7.
                    79e4.9591.62ac.d3c2.5c24.0649.0a3a.32e0.
                    db0b.5ede.14b8.ee46.8890.2a22.dc4f.8160.
                    7319.5d64.3d7e.a7c4.1744.975f.ec13.0ccd.
                    d2f3.ff10.21da.b6bc.f538.9d92.8f40.a351.
                    a89f.3c50.7f02.f945.8533.4d43.fbaa.efd0.
                    cf58.4c4a.39be.cb6a.5bb1.fc20.ed00.d153.
                    842f.e329.b3d6.3b52.a05a.6e1b.1a2c.8309.
                    75b2.27eb.e280.1207.9a05.9618.c323.c704.
                    1531.d871.f1e5.a534.ccf7.3f36.2693.fdb7.
                    c072.a49c.afa2.d4ad.f047.59fa.7dc9.82ca.
                    76ab.d7fe.2b67.0130.c56f.6bf2.7b77.7c63
          --
        ::                                              ::  ++pin:aes:crypto
        ++  pin                                         :: decrypt
          ^-  cipa
          |%
          ++  co  [0xe 0xb 0xd 0x9]
          ++  ix  |~(a/@ (sub nnr a))
          ++  ro  [0 3 2 1]
          ++  su  0x7d0c.2155.6314.69e1.26d6.77ba.7e04.2b17.
                    6199.5383.3cbb.ebc8.b0f5.2aae.4d3b.e0a0.
                    ef9c.c993.9f7a.e52d.0d4a.b519.a97f.5160.
                    5fec.8027.5910.12b1.31c7.0788.33a8.dd1f.
                    f45a.cd78.fec0.db9a.2079.d2c6.4b3e.56fc.
                    1bbe.18aa.0e62.b76f.89c5.291d.711a.f147.
                    6edf.751c.e837.f9e2.8535.ade7.2274.ac96.
                    73e6.b4f0.cecf.f297.eadc.674f.4111.913a.
                    6b8a.1301.03bd.afc1.020f.3fca.8f1e.2cd0.
                    0645.b3b8.0558.e4f7.0ad3.bc8c.00ab.d890.
                    849d.8da7.5746.155e.dab9.edfd.5048.706c.
                    92b6.655d.cc5c.a4d4.1698.6886.64f6.f872.
                    25d1.8b6d.49a2.5b76.b224.d928.66a1.2e08.
                    4ec3.fa42.0b95.4cee.3d23.c2a6.3294.7b54.
                    cbe9.dec4.4443.8e34.87ff.2f9b.8239.e37c.
                    fbd7.f381.9ea3.40bf.38a5.3630.d56a.0952
          --
        ::                                              ::  ++mcol:aes:crypto
        ++  mcol
          |=  {a/(list @) b/{p/@ q/@ r/@ s/@}}  ^-  (list @)
          =+  c=[p=*@ q=*@ r=*@ s=*@]
          |-  ^-  (list @)
          ?~  a  ~
          =>  .(p.c (cut 3 [0 1] i.a))
          =>  .(q.c (cut 3 [1 1] i.a))
          =>  .(r.c (cut 3 [2 1] i.a))
          =>  .(s.c (cut 3 [3 1] i.a))
          :_  $(a t.a)
          %+  rep  3
          %+  turn
            %-  limo
            :~  [[p.c p.b] [q.c q.b] [r.c r.b] [s.c s.b]]
                [[p.c s.b] [q.c p.b] [r.c q.b] [s.c r.b]]
                [[p.c r.b] [q.c s.b] [r.c p.b] [s.c q.b]]
                [[p.c q.b] [q.c r.b] [r.c s.b] [s.c p.b]]
            ==
          |=  {a/{@ @} b/{@ @} c/{@ @} d/{@ @}}
          :(dif (pro a) (pro b) (pro c) (pro d))
        ::                                              ::  ++pode:aes:crypto
        ++  pode                                        ::  explode to block
          |=  {a/bloq b/@ c/@}  ^-  (list @)
          =+  d=(rip a c)
          =+  m=(met a c)
          |-
          ?:  =(m b)
            d
          $(m +(m), d (weld d (limo [0 ~])))
        ::                                              ::  ++sube:aes:crypto
        ++  sube                                        ::  s-box word
          |=  {a/@ b/@}  ^-  @
          (rep 3 (turn (pode 3 4 a) |=(c/@ (cut 3 [c 1] b))))
        --
      |%
      ::                                                ::  ++be:aes:crypto
      ++  be                                            ::  block cipher
        |=  {a/? b/@ c/@H}  ^-  @uxH
        ~|  %be-aesc
        =>  %=    .
                +
              =>  +
              |%
              ++  ankh
                |=  {a/cipa b/@ c/@}
                (pode 5 nnb (cut 5 [(mul (ix.a b) nnb) nnb] c))
              ++  sark
                |=  {c/(list @) d/(list @)}  ^-  (list @)
                ?~  c  ~
                ?~  d  !!
                [(mix i.c i.d) $(c t.c, d t.d)]
              ++  srow
                |=  {a/cipa b/(list @)}  ^-  (list @)
                =+  [c=0 d=~ e=ro.a]
                |-
                ?:  =(c nnb)
                  d
                :_  $(c +(c))
                %+  rep  3
                %+  turn
                  (limo [0 p.e] [1 q.e] [2 r.e] [3 s.e] ~)
                |=  {f/@ g/@}
                (cut 3 [f 1] (snag (mod (add g c) nnb) b))
              ++  subs
                |=  {a/cipa b/(list @)}  ^-  (list @)
                ?~  b  ~
                [(sube i.b su.a) $(b t.b)]
              --
            ==
        =+  [d=?:(a pen pin) e=(pode 5 nnb c) f=1]
        =>  .(e (sark e (ankh d 0 b)))
        |-
        ?.  =(nnr f)
          =>  .(e (subs d e))
          =>  .(e (srow d e))
          =>  .(e (mcol e co.d))
          =>  .(e (sark e (ankh d f b)))
          $(f +(f))
        =>  .(e (subs d e))
        =>  .(e (srow d e))
        =>  .(e (sark e (ankh d nnr b)))
        (rep 5 e)
      ::                                                ::  ++ex:aes:crypto
      ++  ex                                            ::  key expand
        |=  a/@I  ^-  @
        =+  [b=a c=0 d=su:pen i=nnk]
        |-
        ?:  =(i (mul nnb +(nnr)))
          b
        =>  .(c (cut 5 [(dec i) 1] b))
        =>  ?:  =(0 (mod i nnk))
              =>  .(c (ror 3 1 c))
              =>  .(c (sube c d))
              .(c (mix c (pow (dec (div i nnk)) 2)))
            ?:  &((gth nnk 6) =(4 (mod i nnk)))
              .(c (sube c d))
            .
        =>  .(c (mix c (cut 5 [(sub i nnk) 1] b)))
        =>  .(b (can 5 [i b] [1 c] ~))
        $(i +(i))
      ::                                                ::  ++ix:aes:crypto
      ++  ix                                            ::  key expand, inv
        |=  a/@  ^-  @
        =+  [i=1 j=*@ b=*@ c=co:pin]
        |-
        ?:  =(nnr i)
          a
        =>  .(b (cut 7 [i 1] a))
        =>  .(b (rep 5 (mcol (pode 5 4 b) c)))
        =>  .(j (sub nnr i))
        %=    $
            i  +(i)
            a
          %+  can  7
          :~  [i (cut 7 [0 i] a)]
              [1 b]
              [j (cut 7 [+(i) j] a)]
          ==
        ==
      --
    ::                                                  ::  ++ecba:aes:crypto
    ++  ecba                                            ::  AES-128 ECB
      ~%  %ecba  +>  ~
      |_  key/@H
      ::                                                ::  ++en:ecba:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk/@H  ^-  @uxH
        =+  (ahem 4 4 10)
        =:
          key  (~(net fe 7) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecba:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk/@H  ^-  @uxH
        =+  (ahem 4 4 10)
        =:
          key  (~(net fe 7) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --
    ::                                                  ::  ++ecbb:aes:crypto
    ++  ecbb                                            ::  AES-192 ECB
      ~%  %ecbb  +>  ~
      |_  key/@I
      ::                                                ::  ++en:ecbb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk/@H  ^-  @uxH
        =+  (ahem 6 4 12)
        =:
          key  (rsh 6 1 (~(net fe 8) key))
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecbb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk/@H  ^-  @uxH
        =+  (ahem 6 4 12)
        =:
          key  (rsh 6 1 (~(net fe 8) key))
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --
    ::                                                  ::  ++ecbc:aes:crypto
    ++  ecbc                                            ::  AES-256 ECB
      ~%  %ecbc  +>  ~
      |_  key/@I
      ::                                                ::  ++en:ecbc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk/@H  ^-  @uxH
        =+  (ahem 8 4 14)
        =:
          key  (~(net fe 8) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecbc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk/@H  ^-  @uxH
        =+  (ahem 8 4 14)
        =:
          key  (~(net fe 8) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --
    ::                                                  ::  ++cbca:aes:crypto
    ++  cbca                                            ::  AES-128 CBC
      ~%  %cbca  +>  ~
      |_  {key/@H prv/@H}
      ::                                                ::  ++en:cbca:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecba key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbca:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt/@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecba key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --
    ::                                                  ::  ++cbcb:aes:crypto
    ++  cbcb                                            ::  AES-192 CBC
      ~%  %cbcb  +>  ~
      |_  {key/@I prv/@H}
      ::                                                ::  ++en:cbcb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecbb key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbcb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt/@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecbb key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --
    ::                                                  ::  ++cbcc:aes:crypto
    ++  cbcc                                            ::  AES-256 CBC
      ~%  %cbcc  +>  ~
      |_  {key/@I prv/@H}
      ::                                                ::  ++en:cbcc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecbc key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbcc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt/@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecbc key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --
    ::                                                  ::  ++inc:aes:crypto
    ++  inc                                             ::  inc. low bloq
      |=  {mod/bloq ctr/@H}
      ^-  @uxH
      =+  bqs=(rip mod ctr)
      ?~  bqs  0x1
      %+  rep  mod
      [(~(sum fe mod) i.bqs 1) t.bqs]
    ::                                                  ::  ++ctra:aes:crypto
    ++  ctra                                            ::  AES-128 CTR
      ~%  %ctra  +>  ~
      |_  {key/@H mod/bloq len/@ ctr/@H}
      ::                                                ::  ++en:ctra:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@
        ^-  @ux
        =/  encrypt  ~(en ecba key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %^  rsh  3  (sub (mul 16 blocks) len)
        %+  rep  7
        %-  flop  ::  stupid backwards AES
        |-  ^-  (list @ux)
        ?:  =(blocks 0)  ~
        :-  (encrypt ctr)
            $(ctr (inc mod ctr), blocks (dec blocks))
      ::                                                ::  ++de:ctra:aes:crypto
      ++  de                                            ::  decrypt
        en
      --
    ::                                                  ::  ++ctrb:aes:crypto
    ++  ctrb                                            ::  AES-192 CTR
      ~%  %ctrb  +>  ~
      |_  {key/@I mod/bloq len/@ ctr/@H}
      ++  en
        ~/  %en
        |=  txt/@
        ^-  @ux
        =/  encrypt  ~(en ecbb key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %^  rsh  3  (sub (mul 16 blocks) len)
        %+  rep  7
        %-  flop  ::  stupid backwards AES
        |-  ^-  (list @ux)
        ?:  =(blocks 0)  ~
        :-  (encrypt ctr)
            $(ctr (inc mod ctr), blocks (dec blocks))
      ::                                                ::  ++de:ctrb:aes:crypto
      ++  de                                            ::  decrypt
        en
      --
    ::                                                  ::  ++ctrc:aes:crypto
    ++  ctrc                                            ::  AES-256 CTR
      ~%  %ctrc  +>  ~
      |_  {key/@I mod/bloq len/@ ctr/@H}
      ::                                                ::  ++en:ctrc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@
        ^-  @ux
        =/  encrypt  ~(en ecbc key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %^  rsh  3  (sub (mul 16 blocks) len)
        %+  rep  7
        %-  flop  ::  stupid backwards AES
        |-  ^-  (list @ux)
        ?:  =(blocks 0)  ~
        :-  (encrypt ctr)
            $(ctr (inc mod ctr), blocks (dec blocks))
      ::                                                ::  ++de:ctrc:aes:crypto
      ++  de                                            ::  decrypt
        en
      --
    ::                                                  ::  ++doub:aes:crypto
    ++  doub                                            ::  double 128-bit
      |=  str/@H                                        ::  string mod finite
      ^-  @uxH                                          ::  field (see spec)
      %-  ~(sit fe 7)
      ?.  =((xeb str) 128)
        (lsh 0 1 str)
      (mix 0x87 (lsh 0 1 str))
    ::                                                  ::  ++mpad:aes:crypto
    ++  mpad                                            ::
      |=  {oct/@ txt/@}     
      ::
      ::  pad message to multiple of 128 bits
      ::  by appending 1, then 0s
      ::  the spec is unclear, but it must be octet based
      ::  to match the test vectors
      ^-  @ux               
      =+  pad=(mod oct 16)
      ?:  =(pad 0)  0x8000.0000.0000.0000.0000.0000.0000.0000
      (lsh 3 (sub 15 pad) (mix 0x80 (lsh 3 1 txt)))
    ::                                                  ::  ++suba:aes:crypto
    ++  suba                                            ::  AES-128 subkeys
      |=  key/@H
      =+  l=(~(en ecba key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  {@ux @ux}
      [k1 k2]
    ::                                                  ::  ++subb:aes:crypto
    ++  subb                                            ::  AES-192 subkeys
      |=  key/@I
      =+  l=(~(en ecbb key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  {@ux @ux}
      [k1 k2]
    ::                                                  ::  ++subc:aes:crypto
    ++  subc                                            ::  AES-256 subkeys
      |=  key/@I
      =+  l=(~(en ecbc key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  {@ux @ux}
      [k1 k2]
    ::                                                  ::  ++maca:aes:crypto
    ++  maca                                            ::  AES-128 CMAC
      ~/  %maca
      |=  {key/@H oct/(unit @) txt/@}
      ^-  @ux
      =+  [sub=(suba key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbca key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          $&  -.sub
          $|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++macb:aes:crypto
    ++  macb                                            ::  AES-192 CMAC
      ~/  %macb
      |=  {key/@I oct/(unit @) txt/@}
      ^-  @ux
      =+  [sub=(subb key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbcb key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          $&  -.sub
          $|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++macc:aes:crypto
    ++  macc                                            :: AES-256 CMAC
      ~/  %macc
      |=  {key/@I oct/(unit @) txt/@}
      ^-  @ux
      =+  [sub=(subc key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbcc key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          $&  -.sub
          $|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++s2va:aes:crypto
    ++  s2va                                            ::  AES-128 S2V
      ~/  %s2va
      |=  {key/@H ads/(list @)}
      =+  res=(maca key `16 0x0)
      %^  maca  key  ~
      |-  ^-  @uxH
      ?~  ads  (maca key `16 0x1)
      ?~  t.ads
        ?:  (gte (xeb i.ads) 128)
          (mix i.ads res)
        %+  mix
          (doub res)
          (mpad (met 3 i.ads) i.ads)
      %=  $
        res  %+  mix
               (doub res)
               (maca key ~ i.ads)
        ads  t.ads
      ==
    ::                                                  ::  ++s2vb:aes:crypto
    ++  s2vb                                            ::  AES-192 S2V
      ~/  %s2vb
      |=  {key/@I ads/(list @)}
      =+  res=(macb key `16 0x0)
      %^  macb  key  ~
      |-  ^-  @uxH
      ?~  ads  (macb key `16 0x1)
      ?~  t.ads
        ?:  (gte (xeb i.ads) 128)
          (mix i.ads res)
        %+  mix
          (doub res)
          (mpad (met 3 i.ads) i.ads)
      %=  $
        res  %+  mix
               (doub res)
               (macb key ~ i.ads)
        ads  t.ads
      ==
    ::                                                  ::  ++s2vc:aes:crypto
    ++  s2vc                                            ::  AES-256 S2V
      ~/  %s2vc
      |=  {key/@I ads/(list @)}
      =+  res=(macc key `16 0x0)
      %^  macc  key  ~
      |-  ^-  @uxH
      ?~  ads  (macc key `16 0x1)
      ?~  t.ads
        ?:  (gte (xeb i.ads) 128)
          (mix i.ads res)
        %+  mix
          (doub res)
          (mpad (met 3 i.ads) i.ads)
      %=  $
        res  %+  mix
               (doub res)
               (macc key ~ i.ads)
        ads  t.ads
      ==
    ::                                                  ::  ++siva:aes:crypto
    ++  siva                                            ::  AES-128 SIV
      ~%  %siva  +>  ~
      |_  {key/@I vec/(list @)}
      ::                                                ::  ++en:siva:aes:crypto
      ++  en                                            ::  encrypt 
        ~/  %en
        |=  txt/@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh 7 1 key) k2=(end 7 1 key)]
        =+  iv=(s2va k1 (weld vec (limo ~[txt])))
        =+  len=(met 3 txt)
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        :+
          iv
          len
        (~(en ctra k2 7 len hib) txt)
      ::                                                ::  ++de:siva:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  {iv/@H len/@ txt/@}
        ^-  (unit @ux)
        =+  [k1=(rsh 7 1 key) k2=(end 7 1 key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctra k2 7 len hib) txt)
        ?.  =((s2va k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --
    ::                                                  ::  ++sivb:aes:crypto
    ++  sivb                                            ::  AES-192 SIV
      ~%  %sivb  +>  ~
      |_  {key/@J vec/(list @)}
      ::                                                ::  ++en:sivb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh 5 3 key) k2=(end 5 3 key)]
        =+  iv=(s2vb k1 (weld vec (limo ~[txt])))
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  len=(met 3 txt)
        :+  iv
          len
        (~(en ctrb k2 7 len hib) txt) 
      ::                                                ::  ++de:sivb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  {iv/@H len/@ txt/@}
        ^-  (unit @ux)
        =+  [k1=(rsh 5 3 key) k2=(end 5 3 key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctrb k2 7 len hib) txt)
        ?.  =((s2vb k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --
    ::                                                  ::  ++sivc:aes:crypto
    ++  sivc                                            ::  AES-256 SIV
      ~%  %sivc  +>  ~
      |_  {key/@J vec/(list @)}
      ::                                                ::  ++en:sivc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en                                     
        |=  txt/@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh 8 1 key) k2=(end 8 1 key)]
        =+  iv=(s2vc k1 (weld vec (limo ~[txt])))
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  len=(met 3 txt)
        :+
          iv
          len
        (~(en ctrc k2 7 len hib) txt)
      ::                                                ::  ++de:sivc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  {iv/@H len/@ txt/@}
        ^-  (unit @ux)
        =+  [k1=(rsh 8 1 key) k2=(end 8 1 key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctrc k2 7 len hib) txt)
        ?.  =((s2vc k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --
    --
  ::                                                    ::  ++crua:crypto
  ++  crua  !:                                          ::  cryptosuite A (RSA)
    ^-  acru
    =|  {mos/@ pon/(unit {p/@ q/@ r/{p/@ q/@} s/_*fu})}
    =>  |%
        ::                                              ::  ++mx:crua:crypto
        ++  mx                                          ::  bit length
          (dec (met 0 mos))                         
        ::                                              ::  ++dap:crua:crypto
        ++  dap                                         ::  OEAP decode
          |=  {wid/@ xar/@ dog/@}  ^-  {p/@ q/@}
          =+  pav=(sub wid xar)
          =+  qoy=(cut 0 [xar pav] dog)
          =+  dez=(mix (end 0 xar dog) (shaw %pad-b xar qoy))
          [dez (mix qoy (shaw %pad-a pav dez))]
        ::                                              ::  ++pad:crua:crypto
        ++  pad                                         ::  OEAP encode
          |=  {wid/@ rax/{p/@ q/@} meg/@}  ^-  @
          =+  pav=(sub wid p.rax)
          ?>  (gte pav (met 0 meg))
          ^-  @
          =+  qoy=(mix meg (shaw %pad-a pav q.rax))
          =+  dez=(mix q.rax (shaw %pad-b p.rax qoy))
          (can 0 [p.rax dez] [pav qoy] ~)
        ::                                              ::  ++pull:crua:crypto
        ++  pull                                        ::
          |=(a/@ (~(exp fo mos) 3 a))
        ::                                              ::  ++push:crua:crypto
        ++  push                                        :: 
          |=(a/@ (~(exp fo mos) 5 a))
        ::                                              ::  ++pump:crua:crypto
        ++  pump                                        ::
          |=  a/@  ^-  @
          ?~  pon  !!
          (out.s.u.pon (exp.s.u.pon p.r.u.pon (sit.s.u.pon a)))
        ::                                              ::  ++punt:crua:crypto
        ++  punt                                        ::
          |=  a/@  ^-  @
          ?~  pon  !!
          (out.s.u.pon (exp.s.u.pon q.r.u.pon (sit.s.u.pon a)))
        --
    |%
    ::                                                  ::  ++as:crua:crypto
    ++  as                                              ::
      =>  |%
          ++  haul                                      ::  ++haul:as:crua
            |=  a/pass                                  ::
            =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
            ?>  =('a' mag)
            ..as(mos bod, pon ~)
          --
      ^?
      |%  ::                                            ::  ++seal:as:crua
          ++  seal                                      ::
            |=  {a/pass b/@ c/@}
            ^-  @
            =>  .(c (sign b c))
            =+  her=(haul a)
            =+  det=(lte (add 256 (met 0 c)) mx.her)
            =+  lip=?:(det c 0)
            =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
            ^=  mav  ^-  {p/? q/@}
            :-  det
            =+  dog=(pad mx.her [256 b] lip)
            =+  hog=(push.her dog)
            =+  ben=(en b c)
            ?:(det hog (jam hog ben))
          ::                                            ::  ++sign:as:crua
          ++  sign                                      ::
            |=  {a/@ b/@}  ^-  @
            =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
            ^=  mav  ^-  {p/? q/@}
            =+  det=(lte (add 128 (met 0 b)) mx)
            :-  det
            =+  hec=(shaf (mix %agis a) b)
            =+  dog=(pad mx [128 hec] ?:(det b 0))
            =+  hog=(pump dog)
            ?:(det hog (jam hog b))
          ::                                            ::  ++sure:as:crua
          ++  sure                                      ::
            |=  {a/@ b/@}
            ^-  (unit @)
            =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
            =+  gox=?:(det [p=bod q=0] ((hard {p/@ q/@}) (cue bod)))
            =+  dog=(pull p.gox)
            =+  pig=(dap mx 128 dog)
            =+  log=?:(det q.pig q.gox)
            ?.(=(p.pig (shaf (mix %agis a) log)) ~ [~ log])
          ::                                            ::  ++tear:as:crua
          ++  tear                                      ::
            |=  {a/pass b/@}                            
            ^-  (unit {p/@ q/@})
            =+  her=(haul a)
            =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
            =+  gox=?:(det [p=bod q=0] ((hard {p/@ q/@}) (cue bod)))
            =+  dog=(punt p.gox)
            =+  pig=(dap mx 256 dog)
            =+  ^=  cow
                ^-  (unit @)
                ?:  det
                  [~ q.pig]
                (de p.pig q.gox)
            ?~  cow  ~
            =>  .(cow (sure:as.her p.pig u.cow))
            ?~  cow  ~
            [~ p.pig u.cow]
      --
    ::                                                  ::  ++de:crua:crypto
    ++  de                                              ::  decrypt
      |~  {key/@ cep/@}  ^-  (unit @)
      =+  toh=(met 8 cep)
      ?:  (lth toh 2)
        ~
      =+  adj=(dec toh)
      =+  [hax=(end 8 1 cep) bod=(rsh 8 1 cep)]
      =+  msg=(mix (~(raw og (mix hax key)) (mul 256 adj)) bod)
      ?.  =(hax (shax (mix key (shax (mix adj msg)))))
        ~
      [~ msg]
    ::                                                  ::  ++dy:crua:crypto
    ++  dy                                              ::  need decrypt
      |~({a/@ b/@} (need (de a b)))
    ::
    ++  en                                              ::  ++en:crua:crypto
      |~  {key/@ msg/@}  ^-  @ux                        ::  encrypt
      =+  len=(met 8 msg)
      =+  adj=?:(=(0 len) 1 len)
      =+  hax=(shax (mix key (shax (mix adj msg))))
      (rap 8 hax (mix msg (~(raw og (mix hax key)) (mul 256 adj))) ~)
    ::                                                  ::  ++ex:crua
    ++  ex                                              ::  extract
      ^?
      |%  ::                                            ::  ++fig:ex:crua
      ++  fig                                           ::  fingerprint 
        `@uvH`(shaf %afig mos)
      ::                                                ::  ++pac:ex:crua
      ++  pac                                           ::  private fingerprint
        `@uvG`(end 6 1 (shaf %acod sec))
      ::                                                ::  ++pub:ex:crua
      ++  pub                                           ::  public fingerprint
        `pass`(cat 3 'a' mos)
      ::                                                ::  ++sec:ex:crua
      ++  sec                                           ::  private key
        `ring`?~(pon !! (cat 3 'A' (jam p.u.pon q.u.pon)))
      --
    ::                                                  ::  ++nu:crua
    ++  nu                                              ::
      =>  |%
          ::                                            ::  ++elcm:nu:crua
          ++  elcm                                      ::
            |=  {a/@ b/@}
            (div (mul a b) d:(egcd a b))
          ::                                            ::  ++eldm:nu:crua
          ++  eldm                                      ::
            |=  {a/@ b/@ c/@}
            (~(inv fo (elcm (dec b) (dec c))) a)
          ::                                            ::  ++ersa:nu:crua
          ++  ersa                                      ::
            |=  {a/@ b/@}
            [a b [(eldm 3 a b) (eldm 5 a b)] (fu a b)]
          --
      ^?
      |%                                                ::  ++com:nu:crua
          ++  com                                       ::
            |=  a/@
            ^+  ^?(..nu)
            ..nu(mos a, pon ~)
          ::                                            ::  ++pit:nu:crua
          ++  pit                                       ::
            |=  {a/@ b/@}
            =+  c=(rsh 0 1 a)
            =+  [d=(ramp c [3 5 ~] b) e=(ramp c [3 5 ~] +(b))]
            ^+  ^?(..nu)
            ..nu(mos (mul d e), pon [~ (ersa d e)])
          ::                                            ::  ++nol:nu:crua
          ++  nol                                       ::
            |=  a/@
            ^+  ^?(..nu)
            =+  b=((hard {p/@ q/@}) (cue a))
            ..nu(mos (mul p.b q.b), pon [~ (ersa p.b q.b)])
      --
    --                                                
  ::                                                    ::  ++crub:crypto
  ++  crub                                              ::  cryptosuite B (Ed)
    ^-  acru
    =,  number
    =,  crypto
    =|  {pub/{cry/@ sgn/@} sek/(unit {cry/@ sgn/@})}
    |%
    ::                                                  ::  ++as:crub:crypto
    ++  as                                              ::  
      |%
      ::                                                ::  ++sign:as:crub
      ++  sign                                          ::
        |=  {@ msg/@}
        ^-  @ux
        ?~  sek  ~|  %pubkey-only  !!
        (jam [(sign:ed msg sgn.u.sek) msg])
      ::                                                ::  ++sure:as:crub
      ++  sure                                          ::
        |=  {@ txt/@}
        ^-  (unit @ux)
        =+  ((hard {sig/@ msg/@}) (cue txt))
        ?.  (veri:ed sig msg sgn.pub)  ~
        (some msg)
      ::                                                ::  ++seal:as:crub
      ++  seal                                          ::
        |=  {bpk/pass m1/@ m2/@}
        ^-  @ux
        ?~  sek  ~|  %pubkey-only  !!
        ?>  =('b' (end 3 1 bpk))
        =+  pk=(rsh 8 1 (rsh 3 1 bpk))
        =+  shar=(shax (shar:ed pk cry.u.sek))
        =+  msg=(jam m1 m2)
        =+  smsg=(sign ~ msg)
        (jam (~(en siva:aes shar ~) smsg))
      ::                                                ::  ++tear:as:crub
      ++  tear                                          ::
        |=  {bpk/pass txt/@}
        ^-  (unit (pair @ux @ux))
        ?~  sek  ~|  %pubkey-only  !!
        ?>  =('b' (end 3 1 bpk))
        =+  pk=(rsh 8 1 (rsh 3 1 bpk))
        =+  shar=(shax (shar:ed pk cry.u.sek))
        =+  ((hard {iv/@ len/@ cph/@}) (cue txt))
        =+  try=(~(de siva:aes shar ~) iv len cph)
        ?~  try  ~
        =+  veri=(sure:as:(com:nu:crub bpk) ~ u.try)
        ?~  veri  ~
        (some ((hard (pair @ux @ux)) (cue u.veri)))
      --
    ::                                                  ::  ++de:crub:crypto
    ++  de                                              ::  decrypt
      |=  {key/@J txt/@}
      ^-  (unit @ux)
      =+  ((hard {iv/@ len/@ cph/@}) (cue txt))
      %^    ~(de sivc:aes (shaz key) ~)
          iv
        len
      cph
    ::                                                  ::  ++dy:crub:crypto
    ++  dy                                              ::  need decrypt
      |=  {key/@I cph/@} 
      (need (de key cph))
    ::                                                  ::  ++en:crub:crypto
    ++  en                                              ::  encrypt
      |=  {key/@J msg/@}
      ^-  @ux
      (jam (~(en sivc:aes (shaz key) ~) msg))
    ::                                                  ::  ++ex:crub:crypto
    ++  ex                                              ::  extract
      |%
      ::                                                ::  ++fig:ex:crub
      ++  fig                                           ::  fingerprint
        ^-  @uvH  
        (shaf %bfig sgn.^pub)
      ::                                                ::  ++pac:ex:crub
      ++  pac                                           ::  private fingerprint
        ^-  @uvG  
        ?~  sek  ~|  %pubkey-only  !!
        (end 6 1 (shaf %bcod sgn.u.sek))
      ::                                                ::  ++pub:ex:crub
      ++  pub                                           ::  public key
        ^-  pass  
        (cat 3 'b' (cat 8 sgn.^pub cry.^pub))
      ::                                                ::  ++sec:ex:crub
      ++  sec                                           ::  private key
        ^-  ring  
        ?~  sek  ~|  %pubkey-only  !!
        (cat 3 'B' (cat 8 sgn.u.sek cry.u.sek))
      --
    ++  nu
      |%
      ++  pit
        |=  {w/@ seed/@}
        =+  wid=(add (div w 8) ?:(=((mod w 8) 0) 0 1))
        =+  bits=(shal wid seed)
        =+  [c=(rsh 8 1 bits) s=(end 8 1 bits)]
        ..nu(pub [cry=(puck:ed c) sgn=(puck:ed s)], sek `[cry=c sgn=s])
      ++  nol
        |=  a/ring
        =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
        ~|  %not-crub-seckey  ?>  =('B' mag)
        =+  [c=(rsh 8 1 bod) s=(end 8 1 bod)]
        ..nu(pub [cry=(puck:ed c) sgn=(puck:ed s)], sek `[cry=c sgn=s])
      ++  com
        |=  a/pass
        =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
        ~|  %not-crub-pubkey  ?>  =('b' mag)
        ..nu(pub [cry=(rsh 8 1 bod) sgn=(end 8 1 bod)], sek ~)
      --
    --
  ::                                                    ::  ++ed:crypto
  ++  ed                                                ::  ed25519
    =>
      =+  =+  [b=256 q=(sub (bex 255) 19)]
          =+  fq=~(. fo q)
          =+  ^=  l
               %+  add
                 (bex 252)
               27.742.317.777.372.353.535.851.937.790.883.648.493
          =+  d=(dif.fq 0 (fra.fq 121.665 121.666))
          =+  ii=(exp.fq (div (dec q) 4) 2)
          [b=b q=q fq=fq l=l d=d ii=ii]
      ~%  %coed  +>  ~
      |%
      ::                                                ::  ++norm:ed:crypto
      ++  norm                                          ::
        |=(x/@ ?:(=(0 (mod x 2)) x (sub q x)))
      ::                                                ::  ++xrec:ed:crypto
      ++  xrec                                          ::  recover x-coord
        |=  y/@  ^-  @
        =+  ^=  xx
            %+  mul  (dif.fq (mul y y) 1)
                     (inv.fq +(:(mul d y y)))
        =+  x=(exp.fq (div (add 3 q) 8) xx)
        ?:  !=(0 (dif.fq (mul x x) (sit.fq xx)))
          (norm (pro.fq x ii))
        (norm x)
      ::                                                ::  ++ward:ed:crypto
      ++  ward                                          ::  edwards multiply
        |=  {pp/{@ @} qq/{@ @}}  ^-  {@ @}
        =+  dp=:(pro.fq d -.pp -.qq +.pp +.qq)
        =+  ^=  xt
            %+  pro.fq
              %+  sum.fq
                (pro.fq -.pp +.qq)
              (pro.fq -.qq +.pp)
            (inv.fq (sum.fq 1 dp))
        =+  ^=  yt
            %+  pro.fq
              %+  sum.fq
                (pro.fq +.pp +.qq)
              (pro.fq -.pp -.qq)
            (inv.fq (dif.fq 1 dp))
        [xt yt]
      ::                                                ::  ++scam:ed:crypto
      ++  scam                                          ::  scalar multiply
        |=  {pp/{@ @} e/@}  ^-  {@ @}
        ?:  =(0 e)
          [0 1]
        =+  qq=$(e (div e 2))
        =>  .(qq (ward qq qq))
        ?:  =(1 (dis 1 e))
          (ward qq pp)
        qq
      ::                                                ::  ++etch:ed:crypto
      ++  etch                                          ::  encode point
        |=  pp/{@ @}  ^-  @
        (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
      ::                                                ::  ++curv:ed:crypto
      ++  curv                                          ::  point on curve?
        |=  {x/@ y/@}  ^-  ?
        .=  0
            %+  dif.fq
              %+  sum.fq
                (pro.fq (sub q (sit.fq x)) x)
              (pro.fq y y)
            (sum.fq 1 :(pro.fq d x x y y))
      ::                                                ::  ++deco:ed:crypto
      ++  deco                                          ::  decode point
        |=  s/@  ^-  (unit {@ @})
        =+  y=(cut 0 [0 (dec b)] s)
        =+  si=(cut 0 [(dec b) 1] s)
        =+  x=(xrec y)
        =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
        =+  pp=[x y]
        ?.  (curv pp)
          ~
        [~ pp]
      ::                                                ::  ++bb:ed:crypto
      ++  bb                                            ::
        =+  bby=(pro.fq 4 (inv.fq 5))
        [(xrec bby) bby]
      ::
      --
    ~%  %ed  +>  ~
    |%
    ::                                                  ::  ++puck:ed:crypto
    ++  puck                                            ::  public key
      ~/  %puck
      |=  sk/@I  ^-  @
      ?:  (gth (met 3 sk) 32)  !!
      =+  h=(shal (rsh 0 3 b) sk)
      =+  ^=  a
          %+  add
            (bex (sub b 2))
          (lsh 0 3 (cut 0 [3 (sub b 5)] h))
      =+  aa=(scam bb a)
      (etch aa)
    ::                                                  ::  ++suck:ed:crypto
    ++  suck                                            ::  keypair from seed
      |=  se/@I  ^-  @uJ
      =+  pu=(puck se)
      (can 0 ~[[b se] [b pu]])
    ::                                                  ::  ++shar:ed:crypto
    ++  shar                                            ::  curve25519 secret
      ~/  %shar
      |=  {pub/@ sek/@}
      ^-  @ux
      =+  exp=(shal (rsh 0 3 b) (suck sek))
      =.  exp  (dis exp (can 0 ~[[3 0] [251 (fil 0 251 1)]]))
      =.  exp  (con exp (lsh 3 31 0b100.0000))
      =+  prv=(end 8 1 exp)
      =+  crv=(fra.fq (sum.fq 1 pub) (dif.fq 1 pub))
      (curt prv crv)
    ::                                                  ::  ++sign:ed:crypto
    ++  sign                                            ::  certify
      ~/  %sign
      |=  {m/@ se/@}  ^-  @
      =+  sk=(suck se)
      =+  pk=(cut 0 [b b] sk)
      =+  h=(shal (rsh 0 3 b) sk)
      =+  ^=  a
          %+  add
            (bex (sub b 2))
          (lsh 0 3 (cut 0 [3 (sub b 5)] h))
      =+  ^=  r
          =+  hm=(cut 0 [b b] h)
          =+  ^=  i
              %+  can  0
              :~  [b hm]
                  [(met 0 m) m]
              ==
          (shaz i)
      =+  rr=(scam bb r)
      =+  ^=  ss
          =+  er=(etch rr)
          =+  ^=  ha
              %+  can  0
              :~  [b er]
                  [b pk]
                  [(met 0 m) m]
              ==
          (~(sit fo l) (add r (mul (shaz ha) a)))
      (can 0 ~[[b (etch rr)] [b ss]])
    ::                                                  ::  ++veri:ed:crypto
    ++  veri                                            ::  validate
      ~/  %veri
      |=  {s/@ m/@ pk/@}  ^-  ?
      ?:  (gth (div b 4) (met 3 s))  |
      ?:  (gth (div b 8) (met 3 pk))  |
      =+  cb=(rsh 0 3 b)
      =+  rr=(deco (cut 0 [0 b] s))
      ?~  rr  |
      =+  aa=(deco pk)
      ?~  aa  |
      =+  ss=(cut 0 [b b] s)
      =+  ha=(can 3 ~[[cb (etch u.rr)] [cb pk] [(met 3 m) m]])
      =+  h=(shaz ha)
      =((scam bb ss) (ward u.rr (scam u.aa h)))
    ::
    --
  ::                                                    ::  ++hmac:crypto
  ++  hmac                                              ::  HMAC-SHA1
    |=  {key/@ mes/@}
    =+  ip=(fil 3 64 0x36)
    =+  op=(fil 3 64 0x5c)
    =+  ^=  kex
        ?:  (gth (met 3 key) 64)
          (lsh 3 44 (shan key))
        (lsh 3 (sub 64 (met 3 key)) (swp 3 key))
    =+  inn=(shan (swp 3 (cat 3 (swp 3 mes) (mix ip kex))))
    (shan (swp 3 (cat 3 inn (mix op kex))))
  ::                                                    ::  ++scr:crypto
  ++  scr                                               ::  scrypt
    ~%  %scr  +>  ~
    |%
    ::                                                  ::  ++sal:scr:crypto
    ++  sal                                             ::  salsa20 hash
      |=  {x/@ r/@}                                     ::  with r rounds
      ?>  =((mod r 2) 0)                                ::
      =+  few==>(fe .(a 5))
      =+  ^=  rot
        |=  {a/@ b/@}
        (mix (end 5 1 (lsh 0 a b)) (rsh 0 (sub 32 a) b))
      =+  ^=  lea
        |=  {a/@ b/@}
        (net:few (sum:few (net:few a) (net:few b)))
      =>  |%
          ::                                            ::  ++qr:sal:scr:crypto
          ++  qr                                        ::  quarterround
            |=  y/{@ @ @ @ $~}
            =+  zb=(mix &2.y (rot 7 (sum:few &1.y &4.y)))
            =+  zc=(mix &3.y (rot 9 (sum:few zb &1.y)))
            =+  zd=(mix &4.y (rot 13 (sum:few zc zb)))
            =+  za=(mix &1.y (rot 18 (sum:few zd zc)))
            ~[za zb zc zd]
          ::                                            ::  ++rr:sal:scr:crypto
          ++  rr                                        ::  rowround
            |=  {y/(list @)}
            =+  za=(qr ~[&1.y &2.y &3.y &4.y])
            =+  zb=(qr ~[&6.y &7.y &8.y &5.y])
            =+  zc=(qr ~[&11.y &12.y &9.y &10.y])
            =+  zd=(qr ~[&16.y &13.y &14.y &15.y])
            ^-  (list @)  :~
              &1.za  &2.za  &3.za  &4.za
              &4.zb  &1.zb  &2.zb  &3.zb 
              &3.zc  &4.zc  &1.zc  &2.zc
              &2.zd  &3.zd  &4.zd  &1.zd  ==
          ::                                            ::  ++cr:sal:scr:crypto
          ++  cr                                        ::  columnround
            |=  {x/(list @)}
            =+  ya=(qr ~[&1.x &5.x &9.x &13.x])
            =+  yb=(qr ~[&6.x &10.x &14.x &2.x])
            =+  yc=(qr ~[&11.x &15.x &3.x &7.x])
            =+  yd=(qr ~[&16.x &4.x &8.x &12.x])
            ^-  (list @)  :~
              &1.ya  &4.yb  &3.yc  &2.yd
              &2.ya  &1.yb  &4.yc  &3.yd
              &3.ya  &2.yb  &1.yc  &4.yd
              &4.ya  &3.yb  &2.yc  &1.yd  ==
          ::                                            ::  ++dr:sal:scr:crypto
          ++  dr                                        ::  doubleround
            |=  {x/(list @)}
            (rr (cr x))
          ::                                            ::  ++al:sal:scr:crypto
          ++  al                                        ::  add two lists
            |=  {a/(list @) b/(list @)}
            |-  ^-  (list @)
            ?~  a  ~  ?~  b  ~
            [i=(sum:few -.a -.b) t=$(a +.a, b +.b)]
          --
      =+  xw=(rpp 5 16 x)
      =+  ^=  ow  |-  ^-  (list @)
                  ?~  r  xw
                  $(xw (dr xw), r (sub r 2))
      (rep 5 (al xw ow))
    ::
    ++  rpp                                             ::  ++rpp:scr:crypto
      |=  {a/bloq b/@ c/@}                              ::  rip+filler blocks
      =+  q=(rip a c)
      =+  w=(lent q)
      ?.  =(w b)
        ?.  (lth w b)  (slag (sub w b) q)
        ^+  q  (weld q (reap (sub b (lent q)) 0))
      q
    ::                                                  ::  ++bls:scr:crypto
    ++  bls                                             ::  split to sublists
      |=  {a/@ b/(list @)}                                
      ?>  =((mod (lent b) a) 0)
      |-  ^-  (list (list @))
      ?~  b  ~
      [i=(scag a `(list @)`b) t=$(b (slag a `(list @)`b))]
    ::                                                  ::  ++slb:scr:crypto
    ++  slb                                             ::
      |=  {a/(list (list @))} 
      |-  ^-  (list @)
      ?~  a  ~
      (weld `(list @)`-.a $(a +.a))
    ::                                                  ::  ++sbm:scr:crypto
    ++  sbm                                             ::  scryptBlockMix
      |=  {r/@ b/(list @)}
      ?>  =((lent b) (mul 2 r))
      =+  [x=(snag (dec (mul 2 r)) b) c=0]
      =|  {ya/(list @) yb/(list @)}
      |-  ^-  (list @)
      ?~  b  (flop (weld yb ya))
      =.  x  (sal (mix x -.b) 8)
      ?~  (mod c 2)
        $(c +(c), b +.b, ya [i=x t=ya])
      $(c +(c), b +.b, yb [i=x t=yb])
    ::                                                  ::  ++srm:scr:crypto
    ++  srm                                             ::  scryptROMix
      |=  {r/@ b/(list @) n/@}                            
      ?>  ?&  =((lent b) (mul 2 r))
              =(n (bex (dec (xeb n))))
              (lth n (bex (mul r 16)))
          ==
      =+  [v=*(list (list @)) c=0]
      =.  v
        |-  ^-  (list (list @))
        =+  w=(sbm r b)
        ?:  =(c n)  (flop v)
        $(c +(c), v [i=[b] t=v], b w)
      =+  x=(sbm r (snag (dec n) v))
      |-  ^-  (list @)
      ?:  =(c n)  x
      =+  q=(snag (dec (mul r 2)) x)
      =+  z=`(list @)`(snag (mod q n) v)
      =+  ^=  w  |-  ^-  (list @)
                 ?~  x  ~  ?~  z  ~
                 [i=(mix -.x -.z) t=$(x +.x, z +.z)]
      $(x (sbm r w), c +(c))
    ::                                                  ::  ++hmc:scr:crypto
    ++  hmc                                             ::  HMAC-SHA-256
      |=  {k/@ t/@}
      (hml k (met 3 k) t (met 3 t))
    ::                                                  ::  ++hml:scr:crypto
    ++  hml                                             ::  w+length 
      |=  {k/@ kl/@ t/@ tl/@}
      =>  .(k (end 3 kl k), t (end 3 tl t))
      =+  b=64
      =.  k  ?.  (gth kl b)  k  (shay kl k)
      =+  ^=  q  %+  shay  (add b tl)
       (add (lsh 3 b t) (mix k (fil 3 b 0x36)))
      %+  shay  (add b 32)
      (add (lsh 3 b q) (mix k (fil 3 b 0x5c)))
    ::                                                  ::  ++pbk:scr:crypto
    ++  pbk                                             :: PBKDF2-HMAC-SHA256
      ~/  %pbk
      |=  {p/@ s/@ c/@ d/@}
      (pbl p (met 3 p) s (met 3 s) c d)
    ::                                                  ::  ++pbl:scr:crypto
    ++  pbl                                             ::  w+length
      ~/  %pbl
      |=  {p/@ pl/@ s/@ sl/@ c/@ d/@}
      =>  .(p (end 3 pl p), s (end 3 sl s))
      =+  h=32
      ::
      ::  max key length 1GB
      ::  max iterations 2^28
      ::
      ?>  ?&  (lte d (bex 30))
              (lte c (bex 28))
              !=(c 0)
          ==
      =+  ^=  l  ?~  (mod d h)
          (div d h)
        +((div d h))
      =+  r=(sub d (mul h (dec l)))
      =+  [t=0 j=1 k=1]
      =.  t  |-  ^-  @
        ?:  (gth j l)  t
        =+  u=(add s (lsh 3 sl (rep 3 (flop (rpp 3 4 j)))))
        =+  f=0  =.  f  |-  ^-  @
          ?:  (gth k c)  f
          =+  q=(hml p pl u ?:(=(k 1) (add sl 4) h))
          $(u q, f (mix f q), k +(k))
        $(t (add t (lsh 3 (mul (dec j) h) f)), j +(j))
      (end 3 d t)
    ::                                                  ::  ++hsh:scr:crypto
    ++  hsh                                             ::  scrypt
      ~/  %hsh
      |=  {p/@ s/@ n/@ r/@ z/@ d/@}
      (hsl p (met 3 p) s (met 3 s) n r z d)
    ::                                                  ::  ++hsl:scr:crypto
    ++  hsl                                             ::  w+length
      ~/  %hsl
      |=  {p/@ pl/@ s/@ sl/@ n/@ r/@ z/@ d/@}
      =|  v/(list (list @))
      =>  .(p (end 3 pl p), s (end 3 sl s))
      =+  u=(mul (mul 128 r) z)
      ::
      ::  n is power of 2; max 1GB memory
      ::
      ?>  ?&  =(n (bex (dec (xeb n))))
              !=(r 0)  !=(z 0)
              %+  lte
                  (mul (mul 128 r) (dec (add n z)))
                (bex 30)
              (lth pl (bex 31))
              (lth sl (bex 31))
          ==
      =+  ^=  b  =+  (rpp 3 u (pbl p pl s sl 1 u))
        %+  turn  (bls (mul 128 r) -)
        |=(a/(list @) (rpp 9 (mul 2 r) (rep 3 a)))
      ?>  =((lent b) z)
      =+  ^=  q
        =+  |-  ?~  b  (flop v)
            $(b +.b, v [i=(srm r -.b n) t=v])
        %+  turn  `(list (list @))`-
        |=(a/(list @) (rpp 3 (mul 128 r) (rep 9 a)))
      (pbl p pl (rep 3 (slb q)) u 1 d)
    ::                                                  ::  ypt:scr:crypto
    ++  ypt                                             ::  256bit {salt pass}
      |=  {s/@ p/@}
      ^-  @
      (hsh p s 16.384 8 1 256)
    --
  ::
  ++  test
      ::
      ::  XX: ++bruw, ++haul and ++weur are obsolete crua interfaces,
      ::  delete or restructure
    ::::
    |%
    ::                                                  ::  ++bruw:crypto
    ++  bruw                                            ::  create keypair
      |=  ::  {width seed}
          ::
          {a/@ b/@}
      ^-  acru
      (pit:nu:crua a b)
    ::                                                  ::  ++haul:crypto
    ++  haul                                            ::  activate public key
      |=  a/pass
      ^-  acru
      =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
      ?>  =('a' mag)
      (com:nu:crua bod)
    ::                                                  ::  ++weur:crypto
    ++  weur                                            ::  activate secret key
      |=  a/ring
      ^-  acru
      =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
      ?>  =('A' mag)
      (nol:nu:crua bod)
    ::                                                  ::  ++trsa:crypto
    ++  trua                                            ::  test rsa
      |=  msg/@tas
      ^-  @
      =+  ali=(bruw 1.024 (shax 'ali'))
      =+  bob=(bruw 1.024 (shax 'bob'))
      =+  tef=(sign:as.ali [0 msg])
      =+  lov=(sure:as.ali [0 tef])
      ?.  &(?=(^ lov) =(msg u.lov))
        ~|(%test-fail-sign !!)
      =+  key=(shax (shax (shax msg)))
      =+  sax=(seal:as.ali pub:ex.bob key msg)
      =+  tin=(tear:as.bob pub:ex.ali sax)
      ?.  &(?=(^ tin) =(key p.u.tin) =(msg q.u.tin))
        ~|(%test-fail-seal !!)
      msg
    ::                                                  ::  ++trub:crypto
    ++  trub                                            ::  test crub
      |=  msg/@t
      ::
      ::  make acru cores
      ::
      =/  ali      (pit:nu:crub 512 (shaz 'Alice'))
      =/  ali-pub  (com:nu:crub pub:ex.ali)
      =/  bob      (pit:nu:crub 512 (shaz 'Robert'))
      =/  bob-pub  (com:nu:crub pub:ex.bob)
      ::
      ::  alice signs and encrypts a symmetric key to bob
      ::
      =/  secret-key  %-  shaz
          'Let there be no duplicity when taking a stand against him.'
      =/  signed-key   (sign:as.ali ~ secret-key)
      =/  crypted-key  (seal:as.ali pub:ex.bob-pub ~ signed-key)
      ::  bob decrypts and verifies
      =/  decrypt-key-attempt  (tear:as.bob pub:ex.ali-pub crypted-key)
      =/  decrypted-key    ~|  %decrypt-fail  (need decrypt-key-attempt)
      =/  verify-key-attempt   (sure:as.ali-pub ~ q.decrypted-key)
      =/  verified-key     ~|  %verify-fail  (need verify-key-attempt)
      ::  bob encrypts with symmetric key
      =/  crypted-msg  (en.bob verified-key msg)
      ::  alice decrypts with same key
      `@t`(dy.ali secret-key crypted-msg)
    --
  --
::                                                      ::
::::                    ## markup/engines               ::  JSON and XML
  ::                                                    ::::
++  markup
  ^?  |%
  ::                                                    ::  ++moon:markup
  ++  moon                                              ::  mime type to text
    |=  myn/mite
    %-  crip
    |-  ^-  tape
    ?~  myn  ~
    ?:  =(~ t.myn)  (trip i.myn)
    (weld (trip i.myn) `tape`['/' $(myn t.myn)])
  ::                                                    ::  ++poja:markup
  ++  poja                                              ::  parse JSON
    =<  |=(a/cord `(unit json)`(rush a apex))
    |%
    ::                                                  ::  ++abox:poja:markup
    ++  abox                                            ::  array
      %+  stag  %a
      (ifix [sel (wish ser)] (more (wish com) apex))
    ::                                                  ::  ++apex:poja:markup
    ++  apex                                            ::  any value
      %+  knee  *json  |.  ~+
      %+  ifix  [spac spac]
      ;~  pose
        (cold ~ (jest 'null'))
        (stag %b bool)
        (stag %s stri)
        (cook |=(s/tape [%n p=(rap 3 s)]) numb)
        abox
        obox
      ==
    ::                                                  ::  ++bool:poja:markup
    ++  bool                                            ::  boolean
      ;~  pose 
        (cold & (jest 'true'))
        (cold | (jest 'false'))
      ==
    ::                                                  ::  ++digs:poja:markup
    ++  digs                                            ::  digits
      (star (shim '0' '9'))
    ::                                                  ::  ++esca:poja:markup
    ++  esca                                            ::  escaped character
      ;~  pfix  bas
        =*  loo
          =*  lip  
            ^-  (list (pair @t @))
            [b+8 t+9 n+10 f+12 r+13 ~]
          =*  wow  `(map @t @)`(malt lip)
          (sear ~(get by wow) low)
        =*  tuf  ;~(pfix (just 'u') (cook tuft qix:ab))
        ;~(pose doq fas soq bas loo tuf)
      ==
    ::                                                  ::  ++expo:poja:markup
    ++  expo                                            ::  exponent
      ;~  (comp twel)
        (piec (mask "eE"))
        (mayb (piec (mask "+-")))
        digs
      ==                                                  
    ::                                                  ::  ++frac:poja:markup
    ++  frac                                            ::  fraction
      ;~(plug dot digs)
    ::                                                  ::  ++jcha:poja:markup
    ++  jcha                                            ::  string character
      ;~(pose ;~(less doq bas prn) esca)
    ::                                                  ::  ++mayb:poja:markup
    ++  mayb                                            ::  optional
      |*(bus/rule ;~(pose bus (easy ~)))
    ::                                                  ::  ++numb:poja:markup
    ++  numb                                            ::  number
      ;~  (comp twel)
        (mayb (piec hep))
        ;~  pose
          (piec (just '0'))
          ;~(plug (shim '1' '9') digs)
        ==
        (mayb frac)
        (mayb expo)
      ==
    ::                                                  ::  ++obje:poja:markup
    ++  obje                                            ::  object list
      %+  ifix  [(wish kel) (wish ker)] 
      (more (wish com) pear)
    ::                                                  ::  ++obox:poja:markup
    ++  obox                                            ::  object
      (stag %o (cook malt obje))
    ::                                                  ::  ++pear:poja:markup
    ++  pear                                            ::  key-value
      ;~(plug ;~(sfix (wish stri) (wish col)) apex)
    ::
    ++  piec                                            ::  ++piec:poja:markup
      |*  bus/rule                                      ::  listify
      (cook |=(a/@ [a ~]) bus)
    ::                                                  ::  ++stri:poja:markup
    ++  stri                                            ::  string
      (cook crip (ifix [doq doq] (star jcha)))
    ::                                                  ::  ++tops:poja:markup
    ++  tops                                            ::  strict value
      ;~(pose abox obox)
    ::                                                  ::  ++spac:poja:markup
    ++  spac                                            ::  whitespace
      (star (mask [`@`9 `@`10 `@`13 ' ' ~]))
    ::                                                  ::  ++twel:poja:markup
    ++  twel                                            ::  tape weld
      |=({a/tape b/tape} (weld a b))
    ::                                                  ::  ++wish:poja:markup
    ++  wish                                            ::  with whitespace
      |*(sef/rule ;~(pfix spac sef))
    --
  ::                                                    ::  ++pojo:markup
  ++  pojo                                              ::  print json
    =|  rez/tape
    |=  val/json
    ^-  tape
    ?~  val  (weld "null" rez)
    ?-    -.val
        $a
      :-  '['
      =.  rez  [']' rez]
      !.
      ?~  p.val  rez
      |-
      ?~  t.p.val  ^$(val i.p.val)
      ^$(val i.p.val, rez [',' $(p.val t.p.val)])
   ::
        $b  (weld ?:(p.val "true" "false") rez)
        $n  (weld (trip p.val) rez)
        $s
      :-  '"'
      =.  rez  ['"' rez]
      =+  viz=(trip p.val)
      !.
      |-  ^-  tape
      ?~  viz  rez
      =+  hed=(jesc i.viz)
      ?:  ?=({@ $~} hed)
        [i.hed $(viz t.viz)]
      (weld hed $(viz t.viz))
   ::
        $o
      :-  '{'
      =.  rez  ['}' rez]
      =+  viz=(~(tap by p.val))
      ?~  viz  rez
      !.
      |-  ^+  rez
      ?~  t.viz  ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
      =.  rez  [',' $(viz t.viz)]
      ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
    ==
  ::                                                    ::  ++poxo:markup
  ++  poxo                                              ::  xml parser
    =<  |=(a/manx `tape`(apex a ~))
    |_  _[unq=`?`| cot=`?`|]
    ::                                                  ::  ++apex:poxo:markup
    ++  apex                                            ::  top level
      |=  {mex/manx rez/tape}
      ^-  tape
      ?:  ?=({$$ {{$$ *} $~}} g.mex)
        (escp v.i.a.g.mex rez)
      =+  man=`mane`n.g.mex
      =.  unq  |(unq =(%script man) =(%style man))
      =+  tam=(name man)
      =+  att=`mart`a.g.mex
      :-  '<'
      %+  welp  tam
      =-  ?~(att rez [' ' (attr att rez)])
      ^-  rez/tape
      ?:  &(?=($~ c.mex) |(cot (clot man)))
        [' ' '/' '>' rez]
      :-  '>'
      (many c.mex :(weld "</" tam ">" rez))
    ::                                                  ::  ++attr:poxo:markup
    ++  attr                                            ::  attributes to tape
      |=  {tat/mart rez/tape}
      ^-  tape
      ?~  tat  rez
      =.  rez  $(tat t.tat)
      ;:  weld 
        (name n.i.tat)
        "=\"" 
        (escp(unq |) v.i.tat '"' ?~(t.tat rez [' ' rez]))
      ==
    ::                                                  ::  ++escp:poxo:markup
    ++  escp                                            ::  escape for xml
      |=  {tex/tape rez/tape}
      ?:  unq
        (weld tex rez)
      =+  xet=`tape`(flop tex)
      !.
      |-  ^-  tape
      ?~  xet  rez
      %=    $
        xet  t.xet
        rez  ?-  i.xet
               $34  ['&' 'q' 'u' 'o' 't' ';' rez]
               $38  ['&' 'a' 'm' 'p' ';' rez]
               $39  ['&' '#' '3' '9' ';' rez]
               $60  ['&' 'l' 't' ';' rez]
               $62  ['&' 'g' 't' ';' rez]
               *    [i.xet rez]
             ==
      ==
    ::                                                  ::  ++many:poxo:markup
    ++  many                                            ::  nodelist to tape
      |=  {lix/(list manx) rez/tape}
      |-  ^-  tape
      ?~  lix  rez
      (apex i.lix $(lix t.lix))
    ::                                                  ::  ++name:poxo:markup
    ++  name                                            ::  name to tape
      |=  man/mane  ^-  tape
      ?@  man  (trip man)
      (weld (trip -.man) `tape`[':' (trip +.man)])
    ::                                                  ::  ++clot:poxo:markup
    ++  clot  ~+                                        ::  self-closing tags
      %~  has  in
          %-  silt  ^-  (list term)  :~
            %area  %base  %br  %col  %command  %embed  %hr  %img  %inputt
            %keygen  %link  %meta  %param     %source   %track  %wbr 
      ==  ==
    --
  ::                                                    ::  ++poxa:markup
  ++  poxa                                              ::  xml parser
    =<  |=(a/cord (rush a apex))
    |_  ent/_`(map term @t)`[[%apos '\''] ~ ~]
    ::                                                  ::  ++apex:poxa:markup
    ++  apex                                            ::  top level
      =+  spa=;~(pose comt whit)
      %+  knee  *manx  |.  ~+
      %+  ifix  [(star spa) (star spa)]
      ;~  pose
        %+  sear  |=({a/marx b/marl c/mane} ?.(=(c n.a) ~ (some [a b])))
          ;~(plug head many tail)
        empt
      == 
    ::                                                  ::  ++attr:poxa:markup
    ++  attr                                            ::  attributes
      %+  knee  *mart  |.  ~+ 
      %-  star
      ;~  plug  
        ;~(pfix (plus whit) name)
        ;~  pose
          (ifix [;~(plug tis doq) doq] (star ;~(less doq escp)))
          (ifix [;~(plug tis soq) soq] (star ;~(less soq escp)))
          (easy ~)
        ==
      ==
    ::                                                  ::  ++chrd:poxa:markup
    ++  chrd                                            ::  character data
      %+  cook  |=(a/tape ^-(mars ;/(a)))
      (plus ;~(less doq ;~(pose (just `@`10) escp)))
    ::                                                  ::  ++comt:poxa:markup
    ++  comt                                            ::  comments 
      =-  (ifix [(jest '<!--') (jest '-->')] (star -))
      ;~  pose
        ;~(less hep prn)
        whit
        ;~(less (jest '-->') hep)
      ==
    ::                                                  ::  ++escp:poxa:markup
    ++  escp                                            ::
      ;~(pose ;~(less gal gar pam prn) enty)
    ::                                                  ::  ++enty:poxa:markup
    ++  enty                                            ::  entity
      %+  ifix  pam^sem
      ;~  pose
        =+  def=^+(ent (my [%gt '>'] [%lt '<'] [%amp '&'] [%quot '"'] ~))
        %+  sear  ~(get by (~(uni by def) ent))
        (cook crip ;~(plug alf (stun 1^31 aln)))
        %+  cook  |=(a/@c ?:((gth a 0x10.ffff) '�' (tuft a)))
        =<  ;~(pfix hax ;~(pose - +))
        :-  (bass 10 (stun 1^8 dit))
        (bass 16 ;~(pfix (mask "xX") (stun 1^8 hit)))
      ==
    ::                                                  ::  ++empt:poxa:markup
    ++  empt                                            ::  self-closing tag
      %+  ifix  [gal (jest '/>')]
      ;~(plug ;~(plug name attr) (cold ~ (star whit)))
    ::                                                  ::  ++head:poxa:markup
    ++  head                                            ::  opening tag
      (ifix [gal gar] ;~(plug name attr))
    ::                                                  ::  ++many:poxa:markup
    ++  many                                            ::
      (more (star comt) ;~(pose apex chrd))
    ::                                                  ::  ++name:poxa:markup
    ++  name                                            ::  tag name 
      =+  ^=  chx
          %+  cook  crip 
          ;~  plug 
              ;~(pose cab alf) 
              (star ;~(pose cab dot alp))
          ==
      ;~(pose ;~(plug ;~(sfix chx col) chx) chx)
    ::                                                  ::  ++tail:poxa:markup
    ++  tail                                            ::  closing tag
      (ifix [(jest '</') gar] name)             
    ::                                                  ::  ++whit:poxa:markup
    ++  whit                                            ::  whitespace
      (mask ~[' ' `@`0x9 `@`0xa])
    --
  ::                                                    ::  ++jo:markup
  ++  jo                                                ::  json reparser
    =>  |%  ++  grub  (unit *) 
            ++  fist  $-(json grub)
        --
    |%
    ::                                                  ::  ++ar:jo:markup
    ++  ar                                              ::  array as list
      |*  wit/fist
      |=  jon/json
      ?.  ?=({$a *} jon)  ~
      %-  zl
      |-  
      ?~  p.jon  ~
      [i=(wit i.p.jon) t=$(p.jon t.p.jon)]
    ::                                                  ::  ++at:jo:markup
    ++  at                                              ::  array as tuple
      |*  wil/(pole fist)
      |=  jon/json
      ?.  ?=({$a *} jon)  ~
      =+  raw=((at-raw wil) p.jon)
      ?.((za raw) ~ (some (zp raw)))
    ::                                                  ::  ++at-raw:jo:markup
    ++  at-raw                                          ::  array as tuple
      |*  wil/(pole fist)
      |=  jol/(list json)
      ?~  wil  ~
      :-  ?~(jol ~ (-.wil i.jol))
      ((at-raw +.wil) ?~(jol ~ t.jol))
    ::                                                  ::  ++bo:jo:markup
    ++  bo                                              ::  boolean
      |=(jon/json ?.(?=({$b *} jon) ~ [~ u=p.jon]))
    ::                                                  ::  ++bu:jo:markup
    ++  bu                                              ::  boolean not
      |=(jon/json ?.(?=({$b *} jon) ~ [~ u=!p.jon]))
    ::                                                  ::  ++ci:jo:markup
    ++  ci                                              ::  maybe transform
      |*  {poq/gate wit/fist}
      |=  jon/json
      (biff (wit jon) poq)
    ::                                                  ::  ++cu:jo:markup
    ++  cu                                              ::  transform
      |*  {poq/gate wit/fist}
      |=  jon/json
      (bind (wit jon) poq)
    ::                                                  ::  ++da:jo:markup
    ++  da                                              ::  UTC date
      |=  jon/json
      ?.  ?=({$s *} jon)  ~
      (bind (stud:chrono p.jon) |=(a/date (year a)))
    ::                                                  ::  ++di:jo:markup
    ++  di                                              ::  millisecond date
      %+  cu
        |=  a/@u  ^-  @da
        (add ~1970.1.1 (div (mul ~s1 a) 1.000))
      ni
    ::                                                  ::  ++mu:jo:markup
    ++  mu                                              ::  true unit
      |*  wit/fist
      |=  jon/json
      ?~(jon (some ~) (bind (wit jon) some))
    ::                                                  ::  ++ne:jo:markup
    ++  ne                                              ::  number as real
      |=  jon/json
      ^-  (unit @rd)
      !!
    ::                                                  ::  ++ni:jo:markup
    ++  ni                                              ::  number as integer
      |=  jon/json 
      ?.  ?=({$n *} jon)  ~
      (rush p.jon dem)
    ::                                                  ::  ++no:jo:markup
    ++  no                                              ::  number as cord
      |=  jon/json
      ?.  ?=({$n *} jon)  ~
      (some p.jon)
    ::                                                  ::  ++of:jo:markup
    ++  of                                              ::  object as frond
      |*  wer/(pole {cord fist})
      |=  jon/json
      ?.  ?=({$o {@ *} $~ $~} jon)  ~
      |-
      ?~  wer  ~
      ?:  =(-.-.wer p.n.p.jon)  
        ((pe -.-.wer +.-.wer) q.n.p.jon)
      ((of +.wer) jon)
    ::                                                  ::  ++ot:jo:markup
    ++  ot                                              ::  object as tuple
      |*  wer/(pole {cord fist})
      |=  jon/json
      ?.  ?=({$o *} jon)  ~
      =+  raw=((ot-raw wer) p.jon)
      ?.((za raw) ~ (some (zp raw)))
    ::                                                  ::  ++ot-raw:jo:markup
    ++  ot-raw                                          ::  object as tuple
      |*  wer/(pole {cord fist})
      |=  jom/(map @t json)
      ?~  wer  ~
      =+  ten=(~(get by jom) -.-.wer)
      [?~(ten ~ (+.-.wer u.ten)) ((ot-raw +.wer) jom)]
    ::                                                  ::  ++om:jo:markup
    ++  om                                              ::  object as map
      |*  wit/fist
      |=  jon/json
      ?.  ?=({$o *} jon)  ~
      (zm (~(run by p.jon) wit))
    ::                                                  ::  ++op:jo:markup
    ++  op                                              ::  parse keys of map
      |*  {fel/rule wit/fist}
      %+  cu  my
      %-  ci  :_  (om wit)
      |=  a/(map cord _(need *wit))
      ^-  (unit (list _[(wonk *fel) (need *wit)]))
      =-  (zl (turn (~(tap by a)) -))
      |*  {a/cord b/*}
      =+  nit=(rush a fel) 
      ?~  nit  ~
      (some [u.nit b])
    ::                                                  ::  ++pe:jo:markup
    ++  pe                                              ::  prefix
      |*  {pre/* wit/fist}
      (cu |*(* [pre +<]) wit)
    ::                                                  ::  ++sa:jo:markup
    ++  sa                                              ::  string as tape
      |=  jon/json
      ?.(?=({$s *} jon) ~ (some (trip p.jon)))
    ::                                                  ::  ++so:jo:markup
    ++  so                                              ::  string as cord
      |=  jon/json
      ?.(?=({$s *} jon) ~ (some p.jon))
    ::                                                  ::  ++su:jo:markup
    ++  su                                              ::  parse string
      |*  sab/rule
      |=  jon/json
      ?.  ?=({$s *} jon)  ~
      (rush p.jon sab)
    ::                                                  ::  ++ul:jo:markup
    ++  ul                                              ::  null
      |=(jon/json ?~(jon (some ~) ~))
    ::                                                  ::  ++za:jo:markup
    ++  za                                              ::  full unit pole
      |*  pod/(pole (unit))
      ?~  pod  &
      ?~  -.pod  |
      (za +.pod)
    ::                                                  ::  ++zl:jo:markup
    ++  zl                                              ::  collapse unit list
      |*  lut/(list (unit))
      ?.  |-  ^-  ?
          ?~(lut & ?~(i.lut | $(lut t.lut)))
        ~
      %-  some
      |-
      ?~  lut  ~
      [i=u:+.i.lut t=$(lut t.lut)]
    ::                                                  ::  ++zm:jo:markup
    ++  zm                                              ::  collapse unit map
      |*  lum/(map term (unit))
      ?:  (~(rep by lum) |=({{@ a/(unit)} b/_|} |(b ?=($~ a))))
        ~
      (some (~(run by lum) need))
    ::                                                  ::  ++zp:jo:markup
    ++  zp                                              ::  unit tuple
      |*  but/(pole (unit))
      ?~  but  !!
      ?~  +.but  
        u:->.but
      [u:->.but (zp +.but)]
    --
  ::                                                    ::  ++joba:markup
  ++  joba                                              ::  object from k-v pair
    |=  {p/@t q/json}
    ^-  json
    [%o [[p q] ~ ~]]
  ::                                                    ::  ++jobe:markup
  ++  jobe                                              ::  object from k-v list
    |=  a/(list {p/@t q/json})
    ^-  json
    [%o (~(gas by *(map @t json)) a)]
  ::                                                    ::  ++jape:markup
  ++  jape                                              ::  string from tape
    |=  a/tape
    ^-  json
    [%s (crip a)]
  ::                                                    ::  ++jone:markup
  ++  jone                                              ::  number from unsigned
    |=  a/@u
    ^-  json
    :-  %n
    ?:  =(0 a)  '0'
    %-  crip
    %-  flop
    |-  ^-  tape 
    ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])
  ::                                                    ::  ++jode:markup
  ++  jode                                              ::  ms timestamp
    |=  a/time 
    =-  (jone (div (mul - 1.000) ~s1))
    (add (div ~s1 2.000) (sub a ~1970.1.1))
  ::                                                    ::  ++jesc:markup
  ++  jesc                                              ::  escaped
    =+  utf=|=(a/@ ['\\' 'u' ((x-co 4):co a)]) 
    |=  a/@  ^-  tape
    ?+  a  ?:((gth a 0x1f) [a ~] (utf a))
      $10  "\\n"
      $34  "\\\""
      $92  "\\\\"
    ==
  --
::
++  scanf                                              ::  formatted scan
  |*  {tape (pole _;/(*{$^(rule tape)}))}
  =>  .(+< [a b]=+<)
  (scan a (parsf b))
::
++  parsf                                              ::  make parser from:
  |^  |*  a/(pole _;/(*{$^(rule tape)}))            ::  ;"chars{rule}chars"
      =-  (cook - (boil (norm a)))
      |*  (list)
      ?~  +<  ~
      ?~  t  i
      [i $(+< t)]
  ::
  ::  .=  (boil ~[[& dim] [| ", "] [& dim]]:ag)
  ::  ;~(plug dim ;~(pfix com ace ;~(plug dim (easy)))):ag
  ::
  ++  boil
    |*  (list (each rule tape))
    ?~  +<  (easy ~)
    ?:  ?=($| -.i)  ;~(pfix (jest (crip p.i)) $(+< t))
    %+  cook  |*({* *} [i t]=+<)
    ;~(plug p.i $(+< t))
  ::
  ::  .=  (norm [;"{n}, {n}"]:n=dim:ag)  ~[[& dim] [| ", "] [& dim]]:ag
  ::
  ++  norm                                             
    |*  (pole _;/(*{$^(rule tape)}))
    ?~  +<  ~
    =>  .(+< [i=+<- t=+<+])
    :_  t=$(+< t)
    =+  rul=->->.i
    ^=  i
    ?~  rul     [%| p=rul]
    ?~  +.rul   [%| p=rul]
    ?@  &2.rul  [%| p=;;(tape rul)]
    [%& p=rul]
  --
::
++  taco                                                ::  atom to octstream
  |=  tam/@  ^-  octs
  [(met 3 tam) tam]
::
++  tact                                                ::  tape to octstream
  |=  tep/tape  ^-  octs
  (taco (rap 3 tep))
::
++  tell                                                ::  wall to octstream
  |=  wol/wall  ^-  octs
  =+  buf=(rap 3 (turn wol |=(a/tape (crip (weld a `tape`[`@`10 ~])))))
  [(met 3 buf) buf]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bF, filesystem interface     ::
::
++  feel                                                ::  simple file write
  |=  {pax/path val/cage}
  ^-  miso
  =+  dir=.^(arch %cy pax)
  ?~  fil.dir  [%ins val]
  [%mut val]
::
++  file                                                ::  simple file load
  |=  pax/path
  ^-  (unit)
  =+  dir=.^(arch %cy pax)
  ?~(fil.dir ~ [~ .^(* %cx pax)])
::
++  foal                                                ::  high-level write
  |=  {pax/path val/cage}
  ^-  toro
  ?>  ?=({* * * *} pax)
  [i.t.pax [%& [[[t.t.t.pax (feel pax val)] ~]]]]
::
++  fray                                                ::  high-level delete
  |=  pax/path
  ^-  toro
  ?>  ?=({* * * *} pax)
  [i.t.pax [%& [[[t.t.t.pax [%del ~]] ~]]]]
::
++  furl                                                ::  unify changes
  |=  {one/toro two/toro}
  ^-  toro
  ~|  %furl
  ?>  ?&  =(p.one p.two)                                ::  same path
          &(?=($& -.q.one) ?=($& -.q.two))              ::  both deltas
      ==
  [p.one [%& (weld p.q.one p.q.two)]]
::
++  tame                                                ::  parse kite path
  |=  hap/path
  ^-  (unit kite)
  ?.  ?=({@ @ @ @ *} hap)  ~
  =+  :*  hyr=(slay i.hap)
          fal=(slay i.t.hap)
          dyc=(slay i.t.t.hap)
          ved=(slay i.t.t.t.hap)
          ::  ved=(slay i.t.hap)
          ::  fal=(slay i.t.t.hap)
          ::  dyc=(slay i.t.t.t.hap)
          tyl=t.t.t.t.hap
      ==
  ?.  ?=({$~ $$ $tas @} hyr)  ~
  ?.  ?=({$~ $$ $p @} fal)  ~
  ?.  ?=({$~ $$ $tas @} dyc)  ~
  ?.  ?=({$~ $$ case} ved)  ~
  =+  his=`@p`q.p.u.fal
  =+  [dis=(end 3 1 q.p.u.hyr) rem=(rsh 3 1 q.p.u.hyr)]
  ?.  ?&(?=($c dis) ?=(?($v $w $x $y $z) rem))  ~
  [~ rem p.u.ved q.p.u.fal q.p.u.dyc tyl]
::
++  tome                                                ::  parse path to beam
  |=  pax/path
  ^-  (unit beam)
  ?.  ?=({* * * *} pax)  ~
  %+  biff  (slaw %p i.pax)
  |=  who/ship
  %+  biff  (slaw %tas i.t.pax)
  |=  dex/desk
  %+  biff  (slay i.t.t.pax)
  |=  cis/coin
  ?.  ?=({$$ case} cis)  ~
  `(unit beam)`[~ [who dex `case`p.cis] (flop t.t.t.pax)]
::
++  tope                                                ::  beam to path
  |=  bem/beam
  ^-  path
  [(scot %p p.bem) q.bem (scot r.bem) (flop s.bem)]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bG, URL handling             ::
::
++  deft                                                ::  parse url extension
  |=  rax/(list @t)
  |-  ^-  pork
  ?~  rax
    [~ ~]
  ?^  t.rax
    [p.pok [ire q.pok]]:[pok=$(rax t.rax) ire=i.rax]
  =+  ^-  raf/(like term)
      =>  |=(a/@ ((sand %tas) (crip (flop (trip a)))))
      (;~(sfix (sear . sym) dot) [1^1 (flop (trip i.rax))])
  ?~  q.raf
    [~ [i.rax ~]]
  =+  `{ext/term {@ @} fyl/tape}`u.q.raf
  :-  `ext
  ?:(=(~ fyl) ~ [(crip (flop fyl)) ~])
::
++  fuel                                                ::  parse fcgi
  |=  {bem/beam ced/noun:cred quy/|-($@($~ {p/@t q/@t t/$}))}
  ^-  epic
  =+  qix=|-(`quay`?~(quy quy [[p q]:quy $(quy t.quy)]))
  [(malt qix) ((hard cred) ced) bem /]
::
++  sifo                                                ::  64-bit encode
  |=  tig/@
  ^-  tape
  =+  poc=(~(dif fo 3) 0 (met 3 tig))
  =+  pad=(lsh 3 poc (swp 3 tig))
  =+  ^=  cha
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  =+  ^=  sif
      |-  ^-  tape
      ?~  pad
        ~
      =+  d=(end 0 6 pad)
      [(cut 3 [d 1] cha) $(pad (rsh 0 6 pad))]
  (weld (flop (slag poc sif)) (reap poc '='))
::
++  ofis  ::  XX broken
  =-  |=(a/cord (rash a fel))
  =<  fel=(cook |~(a/@ `@t`(swp 3 a)) (bass 64 .))
  =-  (cook welp ;~(plug (plus siw) (stun 0^2 (cold %0 tis))))
  ^=  siw
  ;~  pose
     (cook |=(a/@ (sub a 'A')) (shim 'A' 'Z'))
     (cook |=(a/@ (sub a 'G')) (shim 'a' 'z'))
     (cook |=(a/@ (add a 4)) (shim '0' '9'))
     (cold 62 (just '+'))
     (cold 63 (just '/'))
   ==
::
::  .=  ~[p=~.ack q=~.~sarnel r=~..y]
::  (dray ~[p=%tas q=%p r=%f] %ack ~sarnel &)
++  dray                                                ::  load tuple into path
  =-  |*  {a/{@tas (pole @tas)} b/*}  ^-  (paf a)
      =>  .(b `(tup -.a +.a)`b)
      ?~  +.a  [(scot -.a b) ~]
      [(scot -.a -.b) `(paf +.a)`(..$ +.a +.b)]
  :-  paf=|*(a/(pole) ?~(a $~ {(odo:raid -.a(. %ta)) (..$ +.a)}))
  tup=|*({a/@tas b/(pole @tas)} =+(c=(odo:raid a) ?~(b c {c (..$ -.b +.b)})))
::
::  .=  [p=%ack q=~sarnel r=&]
::  (raid /ack/~sarnel+.y p=%tas q=%p r=%f ~)
++  raid                                                ::  demand path odors
  =-  |*  {a/path b/{@tas (pole @tas)}}
      ?~  +.b  `(odo -.b)`(slav -.b -.a)
      [`(odo -.b)`(slav -.b -.a) (..$ +.a +.b)]
  ^=  odo
  |*  a/@tas
  |=  b/*
  =<  a(, (. b))                  ::  preserve face
  ?+  a   @
    $c  @c  $da  @da  $dr  @dr  $f   @f   $if  @if  $is  @is  $p   @p
    $u  @u  $uc  @uc  $ub  @ub  $ui  @ui  $ux  @ux  $uv  @uv  $uw  @uw
    $s  @s  $t   @t   $ta  @ta  $tas  @tas
  ==
::
++  read                                                ::  parse odored path
  =<  |*({a/path b/{@tas (pole @tas)}} ((+> b) a))
  |*  b/{@tas (pole @tas)}
  |=  a/path
  ?~  a  ~
  =+  hed=(slaw -.b i.a)
  ?~  +.b
    ^-  (unit (odo:raid -.b))
    ?^(+.a ~ hed)
  ^-  (unit {(odo:raid -.b) _(need *(..^$ +.b))})
  (both hed ((..^$ +.b) +.a))
::
++  urle                                                ::  URL encode
  |=  tep/tape
  ^-  tape
  %-  zing
  %+  turn  tep
  |=  tap/char
  =+  xen=|=(tig/@ ?:((gte tig 10) (add tig 55) (add tig '0')))
  ?:  ?|  &((gte tap 'a') (lte tap 'z'))
          &((gte tap 'A') (lte tap 'Z'))
          &((gte tap '0') (lte tap '9'))
          =('.' tap)
          =('-' tap)
          =('~' tap)
          =('_' tap)
      ==
    [tap ~]
  ['%' (xen (rsh 0 4 tap)) (xen (end 0 4 tap)) ~]
::
++  urld                                                ::  URL decode
  |=  tep/tape
  ^-  (unit tape)
  ?~  tep  [~ ~]
  ?:  =('%' i.tep)
    ?.  ?=({@ @ *} t.tep)  ~
    =+  nag=(mix i.t.tep (lsh 3 1 i.t.t.tep))
    =+  val=(rush nag hex:ag)
    ?~  val  ~
    =+  nex=$(tep t.t.t.tep)
    ?~(nex ~ [~ [`@`u.val u.nex]])
  =+  nex=$(tep t.tep)
  ?~(nex ~ [~ i.tep u.nex])
::
++  earf                                                ::  purf to tape
  |=  purf
  (weld (earn p) ?~(q "" `tape`['#' (trip u.q)]))
::
++  earl                                                ::  localize purl
  |=  {who/@p pul/purl}
  ^-  purl
  pul(q.q [(rsh 3 1 (scot %p who)) q.q.pul])
::
++  earn                                                ::  purl to tape
  =<  |=(pul/purl `tape`(apex %& pul))
  |%
  ++  apex
    |=  qur/quri  ^-  tape
    ?-  -.qur
      $&  (weld (head p.p.qur) `tape`$(qur [%| +.p.qur]))
      $|  ['/' (weld (body p.qur) (tail q.qur))]
    ==
  ::
  ++  body
    |=  pok/pork  ^-  tape
    ?~  q.pok  ~
    |-
    =+  seg=(urle (trip i.q.pok))
    ?~  t.q.pok
      ?~(p.pok seg (welp seg '.' (trip u.p.pok)))
    (welp seg '/' $(q.pok t.q.pok))
  ::
  ++  head
    |=  har/hart
    ^-  tape
    ;:  weld
      ?:(&(p.har !?=(hoke r.har)) "https://" "http://")
    ::
      ?-  -.r.har
        $|  (trip (rsh 3 1 (scot %if p.r.har)))
        $&  =+  rit=(flop p.r.har)
            |-  ^-  tape
            ?~(rit ~ (weld (trip i.rit) ?~(t.rit "" `tape`['.' $(rit t.rit)])))
      ==
    ::
      ?~(q.har ~ `tape`[':' ((d-co:co 1) u.q.har)])
    ==
  ::
  ++  tail
    |=  kay/quay
    ^-  tape
    ?:  =(~ kay)  ~
    :-  '?'
    |-  ^-  tape
    ?~  kay  ~
    ;:  welp
      (urle (trip p.i.kay))
      ?~(q.i.kay ~ ['=' (urle (trip q.i.kay))])
      ?~(t.kay ~ `tape`['&' $(kay t.kay)])
    ==
  --
::
++  urlp                                                ::  url+header parser
  |%
  ++  apat                                              ::  2396 abs_path
    %+  cook  deft
    (ifix [fas ;~(pose fas (easy ~))] (more fas smeg))
  ++  aurf                                              ::  2396 with fragment
    %+  cook  |~(a/purf a)
    ;~(plug auri (punt ;~(pfix hax (cook crip (star pque)))))
  ++  auru                                              ::  2396 with maybe user
    %+  cook
      |=  $:  a/{p/? q/(unit iden) r/{(unit @ud) host}}
              b/{pork quay}
          ==
      ^-  (pair (unit iden) purl)
      [q.a [[p.a r.a] b]]
    ::
    ;~  plug
      ;~(plug htts (punt ;~(sfix urt:ab pat)) thor)
      ;~(plug ;~(pose apat (easy *pork)) yque)
    ==
  ++  auri                                              ::  2396 URL
    %+  cook
      |=  a/purl
      ?.(?=(hoke r.p.a) a a(p.p &))
    ;~  plug
      ;~(plug htts thor)
      ;~(plug ;~(pose apat (easy *pork)) yque)
    ==
  ++  htts  
    %+  sear  ~(get by (malt `(list (pair term ?))`[http+| https+& ~]))
    ;~(sfix scem ;~(plug col fas fas))
  ::
  ++  cock                                              ::  cookie
    (most ;~(plug sem ace) ;~(plug toke ;~(pfix tis tosk)))
  ++  dlab                                              ::  2396 domainlabel
    %+  sear
      |=  a/@ta
      ?.(=('-' (rsh 3 (dec (met 3 a)) a)) [~ u=a] ~)
    %+  cook  cass
    ;~(plug aln (star alp))
  ::
  ++  fque  (cook crip (plus pquo))                     ::  normal query field
  ++  fquu  (cook crip (star pquo))                     ::  optional field
  ++  pcar  ;~(pose pure pesc psub col pat)             ::  2396 path char
  ++  pcok  ;~(less bas sem com doq prn)                ::  cookie char
  ++  pesc  ;~(pfix cen mes)                            ::  2396 escaped
  ++  pold  (cold ' ' (just '+'))                       ::  old space code
  ++  pque  ;~(pose pcar fas wut)                       ::  3986 query char
  ++  pquo  ;~(pose pure pesc pold fas wut)             ::  normal query char
  ++  pure  ;~(pose aln hep dot cab sig)                ::  2396 unreserved
  ++  psub  ;~  pose                                    ::  3986 sub-delims
              zap  buc  pam  soq  pel  per
              tar  lus  com  sem  tis
            ==
  ++  ptok  ;~  pose                                    ::  2616 token
              aln  zap  hax  buc  cen  pam  soq  tar  lus
              hep  dot  ket  cab  tec  bar  sig
            ==
  ++  scem                                              ::  2396 scheme
    %+  cook  cass
    ;~(plug alf (star ;~(pose aln lus hep dot)))
  ::
  ++  smeg  (cook crip (plus pcar))                     ::  2396 segment
  ++  tock  (cook crip (plus pcok))                     ::  6265 cookie-value
  ++  tosk  ;~(pose tock (ifix [doq doq] tock))         ::  6265 cookie-value
  ++  toke  (cook crip (plus ptok))                     ::  2616 token
  ++  thor                                              ::  2396 host+port
    %+  cook  |*({* *} [+<+ +<-])
    ;~  plug
      thos
      ;~((bend) (easy ~) ;~(pfix col dim:ag))
    ==
  ++  thos                                              ::  2396 host, no local
    ;~  plug
      ;~  pose
        %+  stag  %&
        %+  sear                                        ::  LL parser weak here
          |=  a/(list @t)
          =+  b=(flop a)
          ?>  ?=(^ b)
          =+  c=(end 3 1 i.b)
          ?.(&((gte c 'a') (lte c 'z')) ~ [~ u=b])
        (most dot dlab)
      ::
        %+  stag  %|
        =+  tod=(ape:ag ted:ab)
        %+  bass  256
        ;~(plug tod (stun [3 3] ;~(pfix dot tod)))
      ==
    ==
  ++  yque                                              ::  query ending
    ;~  pose
      ;~(pfix wut yquy)
      (easy ~)
    ==
  ++  yquy                                              ::  query
    ;~  pose                                            ::  proper query
      %+  more
        ;~(pose pam sem)
      ;~(plug fque ;~(pose ;~(pfix tis fquu) (easy '')))
    ::
      %+  cook                                          ::  funky query
        |=(a/tape [[%$ (crip a)] ~])
      (star pque)
    ==
  ++  zest                                              ::  2616 request-uri
    ;~  pose
      (stag %& (cook |=(a/purl a) auri))
      (stag %| ;~(plug apat yque))
    ==
  --
++  epur                                                ::  url+header parser
  =>(urlp |=(a/cord `(unit purl)`(rush a auri)))
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bH, names etc                ::
::
++  clan                                                ::  ship to rank
  |=  who/ship  ^-  rank
  =+  wid=(met 3 who)
  ?:  (lte wid 1)   %czar
  ?:  =(2 wid)      %king
  ?:  (lte wid 4)   %duke
  ?:  (lte wid 8)   %earl
  ?>  (lte wid 16)  %pawn
::
++  glam                                                ::  carrier names
  |=  zar/@pD  ^-  tape
  (weld "galaxy " (scow %p zar))
::
++  gnom                                                ::  ship display name
  |=  {{our/@p now/@da} him/@p}  ^-  @t
  =+  yow=(scot %p him)
  =+  pax=[(scot %p our) %name (scot %da now) yow ~]
  =+  woy=.^(@t %a pax)
  ?:  =(%$ woy)  yow
  (rap 3 yow ' ' woy ~)
::
++  gnow
  |=  {who/@p gos/gcos}  ^-  @t
  ?-    -.gos
      $czar                 (rap 3 '|' (rap 3 (glam who)) '|' ~)
      $king                 (rap 3 '_' p.gos '_' ~)
      $earl                 (rap 3 ':' p.gos ':' ~)
      $pawn                 ?~(p.gos %$ (rap 3 '.' u.p.gos '.' ~))
      $duke
    ?:  ?=($anon -.p.gos)  %$
    %+  rap  3
    ^-  (list @)
    ?-    -.p.gos
        $punk  ~['"' q.p.gos '"']
        ?($lord $lady)
      =+  ^=  nad
          =+  nam=`name`s.p.p.gos
          %+  rap  3
          :~  p.nam
              ?~(q.nam 0 (cat 3 ' ' u.q.nam))
              ?~(r.nam 0 (rap 3 ' (' u.r.nam ')' ~))
              ' '
              s.nam
          ==
      ?:(=(%lord -.p.gos) ~['[' nad ']'] ~['(' nad ')'])
    ==
  ==
++  pale                                                ::  filter peers
  |=  {hid/bowl fun/$-(sink ?)}
  (skim (~(tap by sup.hid)) fun)
::
++  prix                                                ::  filter gate
  |=  pax/path  |=  sink  ^-  ?
  ?~  pax  &  ?~  r.+<  | 
  &(=(i.pax i.r.+<) $(pax t.pax, r.+< t.r.+<))
::
++  prey  |=({pax/path hid/bowl} (pale hid (prix pax))) ::  prefix 
++  hunt                                                ::  first of unit dates
  |=  {one/(unit @da) two/(unit @da)}
  ^-  (unit @da)
  ?~  one  two
  ?~  two  one
  ?:((lth u.one u.two) one two)
::
++  mole                                                ::  new to old sky
  |=  ska/$-(* (unit (unit)))
  |=  a/*
  ^-  (unit)
  =+  b=(ska a)
  ?~  b  ~
  ?~  u.b  ~
  [~ u.u.b]
::
++  myle                                                ::  new to old sky
  |=  ska/$-({* *} (unit (unit)))
  ^-  $-({* *} (unit))
  |=  a/{* *}
  ^-  (unit)
  =+  b=(ska a)
  ?~  b  ~
  ?~  u.b  ~
  [~ u.u.b]
::
++  pack                                                ::  light path encoding
  |=  {a/term b/path}  ^-  knot
  %+  rap  3  :-  (wack a)
  (turn b |=(c/knot (cat 3 '_' (wack c))))
::
++  puck                                                ::  light path decoding
  =+  fel=(most cab (sear wick urt:ab))
  |=(a/knot `(unit {p/term q/path})`(rush a fel))
::
++  saxo                                                ::  autocanon
  |=  who/ship
  ^-  (list ship)
  =+  dad=(sein who)
  [who ?:(=(who dad) ~ $(who dad))]
::
++  sein                                                ::  autoboss
  |=  who/ship  ^-  ship
  =+  mir=(clan who)
  ?-  mir
    $czar  ~zod
    $king  (end 3 1 who)
    $duke  (end 4 1 who)
    $earl  (end 5 1 who)
    $pawn  (end 4 1 who)
  ==
::
++  team                                                ::  our / our moon
  |=  {our/@p him/@p}
  ?|  =(our him)
      &(?=($earl (clan him)) =(our (sein him)))
  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bI, Arvo structures          ::
::
++  acru                                                ::  asym cryptosuite
          $_  ^?  |%                                    ::  opaque object
          ++  as  ^?                                    ::  asym ops
            |%  ++  seal  |~({a/pass b/@ c/@} *@)       ::  encrypt to a
                ++  sign  |~({a/@ b/@} *@)              ::  certify as us
                ++  sure  |~({a/@ b/@} *(unit @))       ::  authenticate from us
                ++  tear  |~  {a/pass b/@}              ::  accept from a 
                          *(unit {p/@ q/@})             ::
            --                                          ::
          ++  de  |~({a/@ b/@} *(unit @))               ::  symmetric de, soft
          ++  dy  |~({a/@ b/@} *@)                      ::  symmetric de, hard
          ++  en  |~({a/@ b/@} *@)                      ::  symmetric en
          ++  ex  ^?                                    ::  export
            |%  ++  fig  *@uvH                          ::  fingerprint
                ++  pac  *@uvG                          ::  default passcode
                ++  pub  *pass                          ::  public key
                ++  sec  *ring                          ::  private key
            --                                          ::
          ++  nu  ^?                                    ::  reconstructors
             |%  ++  pit  |~({a/@ b/@} ^?(..nu))        ::  from [width seed]
                 ++  nol  |~(a/@ ^?(..nu))              ::  from naked ring
                 ++  com  |~(a/@ ^?(..nu))              ::  from naked pass
             --                                         ::
          --                                            ::
++  aeon  @ud                                           ::
++  agon  (map {p/ship q/desk} {p/@ud q/@ud r/waks})    ::  mergepts
++  ankh                                                ::  fs node (new)
          $:  fil/(unit {p/lobe q/cage})                ::  file
              dir/(map @ta ankh)                        ::  folders
          ==                                            ::
++  apex  {p/@uvI q/(map @ta @uvI) r/(map @ta $~)}      ::  node report (old)
++  ares  (unit {p/term q/(list tank)})                 ::  possible error
++  bale                                                ::  driver state
  |*  a/_*                                              ::  %jael keys type
  $:  {our/ship now/@da eny/@uvJ byk/beak}              ::  base info
      {usr/iden dom/(list @t)}                          ::  req user, domain
      key/a                                             ::  secrets from %jael
  ==                                                    ::
++  iden  knot:?($~ @ta)                                ::  username
++  sec-move                                            ::  driver effect
  $%  {$send p/hiss}                                    ::  http out
      {$show p/purl}                                    ::  direct user to url
      {$give p/httr}                                    ::  respond immediately
      {$redo $~}                                        ::  restart request qeu
  ==                                                    ::
++  ball  @uw                                           ::  statement payload
++  bait  {p/skin q/@ud r/dove}                         ::  fmt nrecvd spec
++  beam  {{p/ship q/desk r/case} s/path}               ::  global name
++  beak  {p/ship q/desk r/case}                        ::  garnish with beak
++  bird                                                ::  packet in travel
          $:  gom/soap                                  ::  message identity
              mup/@ud                                   ::  pktno in msg
              nux/@ud                                   ::  xmission count
              lys/@da                                   ::  last sent
              pac/rock                                  ::  packet data
          ==                                            ::
++  bitt  (map bone (pair ship path))                   ::  incoming subs
++  blob                                                ::  fs blob
          $%  {$delta p/lobe q/{p/mark q/lobe} r/page}  ::  delta on q
              {$direct p/lobe q/page}                   ::  immediate
          ==                                            ::
++  boat  (map (pair bone wire) (trel bean ship path))  ::  outgoing subs
++  boon                                                ::  fort output
          $%  {$beer p/ship}                            ::  gained ownership
              {$bier p/ship q/deyd}                     ::  unsigned deyd
              {$cake p/sock q/soap r/coop s/duct}       ::  e2e message result
              {$coke p/sock q/soap r/cape s/duct}       ::  message result
              {$mead p/lane q/rock}                     ::  accept packet
              {$milk p/sock q/soap r/*}                 ::  accept message
              {$mulk p/sock q/soap r/*}                 ::  e2e pass message
              {$ouzo p/lane q/rock}                     ::  transmit packet
              {$wine p/sock q/tape}                     ::  notify user
          ==                                            ::
++  bowl                                                ::  standard app state
        $:  $:  our/ship                                ::  host
                src/ship                                ::  guest
                dap/term                                ::  agent
            ==                                          ::  
            $:  wex/boat                                ::  outgoing subs
                sup/bitt                                ::  incoming subs
            ==                                          ::
            $:  ost/bone                                ::  opaque cause
                act/@ud                                 ::  change number
                eny/@uvJ                                ::  entropy
                now/@da                                 ::  current time
                byk/beak                                ::  load source
        ==  ==                                          ::
++  bray  {p/life q/(unit life) r/ship s/@da}           ::  our parent us now
++  brow  {p/@da q/@tas}                                ::  browser version
++  buck  {p/mace q/wyll}                               ::  all security data
++  cake  {p/sock q/skin r/@}                           ::  top level packet
++  cape                                                ::  end-to-end result
          $?  $good                                     ::  delivered
              $dead                                     ::  rejected
          ==                                            ::
++  care  ?($u $v $w $x $y $z)                          ::  clay submode
++  case                                                ::  ship desk case spur
          $%  {$da p/@da}                               ::  date
              {$tas p/@tas}                             ::  label
              {$ud p/@ud}                               ::  number
          ==                                            ::
++  chum  @uvI                                          ::  hashed passcode
++  clot                                                ::  symmetric record
          $:  yed/(unit {p/hand q/code})                ::  outbound
              heg/(map hand code)                       ::  proposed
              qim/(map hand code)                       ::  inbound
          ==                                            ::
++  coal  *                                             ::  untyped vase
++  code  @uvI                                          ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  {$& p/twig}                               ::  transform
              {$| p/(list @tas)}                        ::  alter
          ==                                            ::
++  chum  @uvI                                          ::  hashed passcode
++  claw                                                ::  startup chain
          $:  joy/(unit coal)                           ::  local context
              ran/(unit coal)                           ::  arguments
              pux/(unit path)                           ::  execution path
              jiv/(unit coal)                           ::  app configuration
              kyq/(unit coal)                           ::  app customization
              gam/(unit coal)                           ::  app image
          ==                                            ::
++  clip  (each @if @is)                                ::  client IP
++  coal  *                                             ::  untyped vase
++  code  @uvI                                          ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  {$& p/twig}                                ::  transform
              {$| p/(list @tas)}                        ::  alter
          ==                                            ::
++  coop  (unit ares)                                   ::  e2e ack
++  cred                                                ::  credential
          $:  hut/hart                                  ::  client host
              aut/(jug @tas @t)                         ::  client identities
              orx/oryx                                  ::  CSRF secret
              acl/(unit @t)                             ::  accept-language
              cip/(each @if @is)                        ::  client IP
              cum/(map @tas *)                          ::  custom dirt
          ==                                            ::
++  deyd  {p/@ q/step r/?}                              ::  sig stage fake?
++  dome                                                ::  project state
          $:  ank/ankh                                  ::  state
              let/@ud                                   ::  top id
              hit/(map @ud tako)                        ::  changes by id
              lab/(map @tas @ud)                        ::  labels
          ==                                            ::
++  dore                                                ::  foreign contact
          $:  wod/road                                  ::  connection to
              wyl/wyll                                  ::  inferred mirror
              caq/clot                                  ::  symmetric key state
          ==                                            ::
++  dove  {p/@ud q/(map @ud @)}                         ::  count hash 13-blocks
++  epic                                                ::  FCGI parameters
          $:  qix/(map @t @t)                           ::  query
              ced/cred                                  ::  client credentials
              bem/beam                                  ::  original path
              but/path                                  ::  ending
          ==                                            ::
++  flap  @uvH                                          ::  network packet id
++  flow                                                ::  packet connection
          $:  rtt/@dr                                   ::  decaying avg rtt
              wid/@ud                                   ::  logical wdow msgs
          ==                                            ::
++  gank  (each vase (list tank))                       ::  abstract result
++  gilt  {@tas *}                                      ::  presumed gift
++  gens  {p/lang q/gcos}                               ::  general identity
++  germ                                                ::  merge style
          $?  $init                                     ::  new desk
              $this                                     ::  ours with parents
              $that                                     ::  hers with parents
              $fine                                     ::  fast forward
              $meet                                     ::  orthogonal files
              $mate                                     ::  orthogonal changes
              $meld                                     ::  force merge
          ==                                            ::
++  gcos                                                ::  id description
          $%  {$czar $~}                                ::  8-bit ship
              {$duke p/what}                            ::  32-bit ship
              {$earl p/@t}                              ::  64-bit ship
              {$king p/@t}                              ::  16-bit ship
              {$pawn p/(unit @t)}                       ::  128-bit ship
          ==                                            ::
++  govt  path                                          ::  country+postcode
++  hand  @uvH                                          ::  128-bit hash
++  hart  {p/? q/(unit @ud) r/host}                     ::  http sec+port+host
++  hate  {p/purl q/@p r/moth}                          ::  semi-cooked request
++  heir  {p/@ud q/mess r/(unit love)}                  ::  status+headers+data
++  hiss  {p/purl q/moth}                               ::  outbound request
++  hist  {p/@ud q/(list @t)}                           ::  depth texts
++  hole  @t                                            ::  session identity
++  hort  {p/(unit @ud) q/host}                         ::  http port+host
++  host  (each (list @t) @if)                          ::  http host
++  hoke  %+  each   {$localhost $~}                    ::  local host
          ?($.0.0.0.0 $.127.0.0.1)                      ::
++  httq                                                ::  raw http request
          $:  p/meth                                    ::  method
              q/@t                                      ::  unparsed url
              r/(list {p/@t q/@t})                      ::  headers
              s/(unit octs)                             ::  body
          ==                                            ::
++  httr  {p/@ud q/mess r/(unit octs)}                  ::  raw http response
++  httx                                                ::  encapsulated http
          $:  p/?                                       ::  https?
              q/clip                                    ::  source IP
              r/httq                                    ::
          ==                                            ::
++  kite  {p/care q/case r/ship s/desk t/spur}          ::  parsed global name
++  json                                                ::  normal json value
          $@  $~                                        ::  null
          $%  {$a p/(list json)}                        ::  array
              {$b p/?}                                  ::  boolean
              {$o p/(map @t json)}                      ::  object
              {$n p/@ta}                                ::  number
              {$s p/@t}                                 ::  string
          ==                                            ::
++  lamb                                                ::  short path
          $%  {$& p/@tas}                               ::  auto
              {$| p/twig}                               ::  manual
          ==                                            ::
++  lane                                                ::  packet route
          $%  {$if p/@da q/@ud r/@if}                   ::  IP4/public UDP/addr
              {$is p/@ud q/(unit lane) r/@is}           ::  IPv6 w+alternates
              {$ix p/@da q/@ud r/@if}                   ::  IPv4 provisional
          ==                                            ::
++  lang  @ta                                           ::  IETF lang as code
++  lark  {p/(unit @tas) q/lawn}                        ::  parsed command
++  lass  ?($0 $1 $2)                                   ::  power increment
++  lath  $%                                            ::  pipeline stage
              {$0 p/lass q/lamb r/(list cone) s/twig}   ::  command
              {$1 p/twig}                               ::  generator
              {$2 p/twig}                               ::  filter
          ==                                            ::
++  lawn  (list lath)                                   ::
++  lice  {p/ship q/buck}                               ::  full license
++  life  @ud                                           ::  regime number
++  lint  (list rock)                                   ::  fragment array
++  lobe  @uvI                                          ::  blob ref
++  love  $%                                            ::  http response
              {$ham p/manx}                             ::  html node
              {$mid p/mite q/octs}                      ::  mime-typed data
              {$raw p/httr}                             ::  raw http response
              {$wan p/wain}                             ::  text lines
              {$zap p/@ud q/(list tank)}                ::  status+error
          ==                                            ::
++  maki  {p/@ta q/@ta r/@ta s/path}                    ::
++  mace  (list {p/life q/ring})                        ::  private secrets
++  math  (map @t (list @t))                            ::  semiparsed headers
++  meal                                                ::  payload
          $%  {$back p/cape q/flap r/@dr}               ::  acknowledgment
              {$buck p/coop q/flap r/@dr}               ::  e2e ack
              {$bond p/life q/path r/@ud s/*}           ::  message
              {$bund p/life q/path r/@ud s/*}           ::  e2e message
              {$carp p/@ q/@ud r/@ud s/flap t/@}        ::  skin+inx+cnt+hash
              {$fore p/ship q/(unit lane) r/@}          ::  forwarded packet
          ==                                            ::
++  mess  (list {p/@t q/@t})                            ::  raw http headers
++  meta                                                ::  path metadata
          $%  {$& q/@uvI}                               ::  hash
              {$| q/(list @ta)}                         ::  dir
          ==                                            ::
++  meth                                                ::  http methods
          $?  $conn                                     ::  CONNECT
              $delt                                     ::  DELETE
              $get                                      ::  GET
              $head                                     ::  HEAD
              $opts                                     ::  OPTIONS
              $post                                     ::  POST
              $put                                      ::  PUT
              $trac                                     ::  TRACE
          ==                                            ::
++  mime  {p/mite q/octs}                               ::  mimetyped data
++  mite  (list @ta)                                    ::  mime type
++  miso                                                ::  ankh delta
          $%  {$del $~}                                 ::  delete
              {$ins p/cage}                             ::  insert
              {$dif p/cage}                             ::  mutate from diff
              {$mut p/cage}                             ::  mutate from raw
          ==                                            ::
++  misu                                                ::  computed delta
          $%  {$del $~}                                 ::  delete
              {$ins p/cage}                             ::  insert
              {$dif p/lobe q/cage}                      ::  mutate from diff
          ==                                            ::
++  mizu  {p/@u q/(map @ud tako) r/rang}                ::  new state
++  moar  {p/@ud q/@ud}                                 ::  normal change range
++  moat  {p/case q/case r/path}                        ::  change range
++  mood  {p/care q/case r/path}                        ::  request in desk
++  moth  {p/meth q/math r/(unit octs)}                 ::  http operation
++  name  {p/@t q/(unit @t) r/(unit @t) s/@t}           ::  first mid+nick last
++  newt  ?($boot $kick $mess $slay $wake)              ::  lifecycle events
++  nori                                                ::  repository action
          $%  {$& p/soba}                               ::  delta
              {$| p/@tas}                               ::  label
          ==                                            ::
++  nuri                                                ::  repository action
          $%  {$& p/suba}                               ::  delta
              {$| p/@tas}                               ::  label
          ==                                            ::
++  octs  {p/@ud q/@t}                                  ::  octet-stream
++  oryx  @t                                            ::  CSRF secret
++  page  (cask *)                                      ::  untyped cage
++  pail  ?($none $warm $cold)                          ::  connection status
++  pipe                                                ::  secure channel
          $:  out/(unit (pair hand bill:rights:^jael))  ::  outbound key
              inn/(map hand bill:rights:^jael)          ::  inbound keys
              cur/(unit life)                           ::  their version
              sax/(list ship)                           ::  their ancestry
              pub/will:pki:^jael                        ::  their public keys
          ==                                            ::
++  plan  (trel view (pair @da (unit @dr)) path)        ::  subscription
++  plea  {p/@ud q/{p/? q/@t}}                          ::  live prompt
++  plop  blob                                          ::  unvalidated blob
++  pork  {p/(unit @ta) q/(list @t)}                    ::  fully parsed url
++  pred  {p/@ta q/@tas r/@ta $~}                       ::  proto-path
++  prod  {p/prom q/tape r/tape}                        ::  prompt
++  prom  ?($text $pass $none)                          ::  format type
++  purf  (pair purl (unit @t))                         ::  url with fragment
++  purl  {p/hart q/pork r/quay}                        ::  parsed url
++  putt                                                ::  outgoing message
          $:  ski/snow                                  ::  sequence acked+sent
              wyv/(list rock)                           ::  packet list XX gear
          ==                                            ::
++  pyre                                                ::  cascade stash
          $:  p/(map {p/path q/path r/coal} coal)       ::  by path
              q/(map {p/path q/@uvI r/coal} coal)       ::  by source hash
              r/(map {p/* q/coal} coal)                 ::  by (soft) twig
          ==                                            ::
++  quay  (list {p/@t q/@t})                            ::  parsed url query
++  quri                                                ::  request-uri
          $%  {$& p/purl}                               ::  absolute
              {$| p/pork q/quay}                        ::  relative
          ==                                            ::
++  race                                                ::  inbound stream
          $:  did/@ud                                   ::  filled sequence
              dod/?                                     ::  not processing
              bum/(map @ud ares)                        ::  nacks
              mis/(map @ud {p/cape q/lane r/flap s/(unit)}) ::  misordered
          ==                                            ::
++  rank  ?($czar $king $duke $earl $pawn)              ::  ship width class
++  rang  $:  hut/(map tako yaki)                       ::
              lat/(map lobe blob)                       ::
          ==                                            ::
++  rill                                                ::  outbound stream
          $:  sed/@ud                                   ::  sent
              san/(map @ud duct)                        ::  outstanding
          ==                                            ::
++  riot  (unit rant)                                   ::  response+complete
++  road                                                ::  secured oneway route
          $:  exp/@da                                   ::  expiration date
              lun/(unit lane)                           ::  route to friend
              lew/wyll                                  ::  wyll of friend
          ==                                            ::
++  rock  @uvO                                          ::  packet
++  rout  {p/(list host) q/path r/oryx s/path}          ::  http route (new)
++  rump  {p/care q/case r/@tas s/path}                 ::  relative path
++  saba  {p/ship q/@tas r/moar s/dome}                 ::  patch+merge
++  sack  {p/ship q/ship}                               ::  incoming [our his}
++  scar                                                ::  opaque duct
          $:  p/@ud                                     ::  bone sequence
              q/(map duct bone)                         ::  by duct
              r/(map bone duct)                         ::  by bone
          ==                                            ::
++  sufi                                                ::  domestic host
          $:  hoy/(list ship)                           ::  hierarchy
              val/wund                                  ::  private keys
              law/wyll                                  ::  server wyll
              seh/(map hand {p/ship q/@da})             ::  key cache
              hoc/(map ship dore)                       ::  neighborhood
          ==                                            ::
++  salt  @uv                                           ::  entropy
++  seal                                                ::  auth conversation
          $:  whu/(unit ship)                           ::  client identity
              pul/purl                                  ::  destination url
              wit/?                                     ::  wait for partner
              foy/(unit {p/ship q/hole})                ::  partner to notify
              pus/(unit @ta)                            ::  password
          ==                                            ::
++  sect  ?($black $blue $red $orange $white)           ::  banner
++  shed                                                ::  packet flow
          $:  $:  rtt/@dr                               ::  smoothed rtt
                  rto/@dr                               ::  retransmit timeout
                  rtn/(unit @da)                        ::  next timeout
                  rue/(unit @da)                        ::  last heard from
              ==                                        ::
              $:  nus/@ud                               ::  number sent
                  nif/@ud                               ::  number live
                  nep/@ud                               ::  next expected
                  caw/@ud                               ::  logical window
                  cag/@ud                               ::  congest thresh
              ==                                        ::
              $:  diq/(map flap @ud)                    ::  packets sent
                  pyz/(map soup @ud)                    ::  message+unacked
                  puq/(qeu {p/@ud q/soul})              ::  packet queue
              ==                                        ::
          ==                                            ::
++  skit  {p/(unit @ta) q/(list @ta) r/(list @ta)}      ::  tracking path
++  skin  ?($none $open $fast $full)                    ::  encoding stem
++  snow  {p/@ud q/@ud r/(set @ud)}                     ::  window exceptions
++  soap  {p/{p/life q/life} q/path r/@ud}              ::  statement id
++  soup  {p/path q/@ud}                                ::  new statement id
++  soul                                                ::  packet in travel
          $:  gom/soup                                  ::  message identity
              nux/@ud                                   ::  xmission count
              liv/?                                     ::  deemed live
              lys/@da                                   ::  last sent
              pac/rock                                  ::  packet data
          ==                                            ::
++  soba  (list {p/path q/miso})                        ::  delta
++  sock  {p/ship q/ship}                               ::  outgoing [from to]
++  spur  path                                          ::  ship desk case spur
++  step  {p/bray q/gens r/pass}                        ::  identity stage
++  suba  (list {p/path q/misu})                        ::  delta
++  tako  @                                             ::  yaki ref
++  tick  @ud                                           ::  process id
++  toro  {p/@ta q/nori}                                ::  general change
++  town                                                ::  all security state
          $:  lit/@ud                                   ::  imperial modulus
              any/@                                     ::  entropy
              urb/(map ship sufi)                       ::  all keys and routes
              fak/?                                     ::
          ==                                            ::
++  tube  {p/@ta q/@ta r/@ta s/path}                    ::  canonical path
++  tutu  *                                             ::  presumed type
++  yaki  {p/(list tako) q/(map path lobe) r/tako t/@da} ::  commit
++  view  ?($u $v $w $x $y $z)                          ::  view mode
++  waks  (map path woof)                               ::  list file states
++  what                                                ::  logical identity
          $%  {$anon $~}                                ::  anonymous
              {$lady p/whom}                            ::  female person ()
              {$lord p/whom}                            ::  male person []
              {$punk p/sect q/@t}                       ::  opaque handle ""
          ==                                            ::
++  whom  {p/@ud q/govt r/sect s/name}                  ::  year+govt+id
++  woof  $@  $know                                     ::  udon transform
              {$chan (list {$@(@ud {p/@ud q/@ud})})}    ::
++  wund  (list {p/life q/ring r/acru})                 ::  mace in action
++  wyll  (list deyd)                                   ::  certificate
++  zuse  %310                                          ::  hoon+zuse kelvin
::          ::
::::        ::::  this will become `%york`, vane structures.
  ::          ::
++  gift-ames                                           ::  out result <-$
          $%  {$drop $~}                                ::  drop packet
              {$hear p/lane q/@}                        ::  receive packet
              {$east p/sock q/*}                        ::  network response
              {$init p/@p}                              ::  report install
              {$mass p/mass}                            ::  memory usage
              {$send p/lane q/@}                        ::  transmit packet
              {$waft p/ship q/path r/*}                 ::  response message
              {$wart p/sock q/@tas r/path s/*}          ::  network request
              {$went p/ship q/cape}                     ::  reaction message
              {$woot p/ship q/path r/coop}              ::  e2e reaction message
          ==                                            ::
++  kiss-ames                                           ::  in request ->$
          $%  ::  {$born p/@p q/@pG r/?}                ::  ticket birth
              {$barn $~}                                ::  new unix process
              {$crud p/@tas q/(list tank)}              ::  error with trace
              {$cash p/@p q/buck}                       ::  civil license
              ::  {$funk p/@p q/@p r/@}                 ::  symtix from/to/key
              {$hear p/lane q/@}                        ::  receive packet
              {$hole p/lane q/@}                        ::  packet failed
              {$junk p/@}                               ::  entropy
              {$kick p/@da}                             ::  wake up
              {$make p/(unit @t) q/@ud r/@ s/?}         ::  wild license
              {$sith p/@p q/@uw r/?}                    ::  imperial generator
              {$wake $~}                                ::  timer activate
              {$want p/sock q/path r/*}                 ::  send message
              {$wegh $~}                                ::  report memory
              {$wont p/sock q/path r/*}                 ::  e2e send message
          ==                                            ::
++  card-ames                                           ::  out cards
          $%  {$went p/sack q/path r/@ud s/coop}        ::  response confirm 
              {$west p/sack q/path r/@ud s/*}           ::  network request
          ==                                            ::
++  note-ames                                           ::  out request $->
          $%  {$c card-ames}                            ::  to %clay
              {$e card-ames}                            ::  to %eyre
              {$g card-ames}                            ::  to %gall
          ==                                            ::  
++  gift-xmas                                           ::  
          $%  {$east p/*}                               ::  response message
              {$home p/lane q/@}                        ::  process forward
              {$send p/lane q/@}                        ::  send packet
              {$rest p/coop}                            ::  acknowledgment
          ==
++  kiss-xmas                                           ::  in request ->$
          $%  {$hear p/lane q/@}                        ::
              {$mess p/ship q/path r/*}                 ::  send message
              {$gear p/farm:pki:^jael q/(map life ring)}::  general update
              {$wake $~}                                ::
          ==                                            ::
++  card-xmas                                           ::  out cards
          $%  {$west p/ship q/path r/*}                 ::  network request
          ==                                            ::
++  note-xmas                                           ::  out request $->
          $%  {$c $west p/ship q/path r/*}              ::  to %clay
              {$e $west p/ship q/path r/*}              ::  to %eyre
              {$g $west p/ship q/path r/*}              ::  to %gall
              $:  $j                                    ::  to %jael
                  $%  {$line p/ship q/@da r/code}       ::
                      {$link p/ship q/@da r/code}       ::
                      {$meet p/farm:pki:^jael}          ::
                      {$view p/ship}                    ::  
                      {$west p/ship q/path r/*}         ::  to %gall
          ==  ==  ==                                    ::
::
::::    %behn
  ::
++  gift-behn                                           ::  out result <-$
          $%  {$mass p/mass}                            ::  memory usage
              {$wake $~}                                ::  wakey-wakey
          ==                                            ::
++  kiss-behn                                           ::  in request ->$
          $%  {$rest p/@da}                             ::  cancel alarm
              {$wait p/@da}                             ::  set alarm
              {$wake $~}                                ::  timer activate
              {$wegh $~}                                ::  report memory
          ==                                            ::
::
::::    %clay
  ::
++  khan                                                ::
          $:  fil/(unit (unit cage))                    ::  XX see khan-to-soba
              dir/(unit (map @ta (unit khan)))          ::
          ==                                            ::
++  mode  (list {path (unit mime)})                     ::
++  riff  {p/desk q/(unit rave)}                        ::  request+desist
::
::  Response to a request.
::
::  Includes {case case desk} of original request, plus the path of the request,
::  and the data there.
::
++  rant                                                ::  namespace binding
          $:  p/{p/care q/case r/@tas}                  ::  clade release book
              q/path                                    ::  spur
              r/cage                                    ::  data
          ==                                            ::
::
::  A request.
::
::  %sing requests a single piece of data, %next requests to be notified when
::  something changes, and %many starts a subscription to a range of versions.
::
++  rave                                                ::  general request
          $%  {$sing p/mood}                            ::  single request
              {$next p/mood}                            ::  next version
              {$many p/? q/moat}                        ::  change range
          ==                                            ::
::::                                                    ::
++  gift-clay                                           ::  out result <-$
          $%  {$ergo p/@tas q/mode}                     ::  version update
              {$hill p/(list @tas)}                     ::  mount points
              {$mack p/(unit tang)}                     ::  ack
              {$mass p/mass}                            ::  memory usage
              {$mere p/(each (set path) (pair term tang))}  ::  merge result
              {$note p/@tD q/tank}                      ::  debug message
              {$ogre p/@tas}                            ::  delete mount point
              {$writ p/riot}                            ::  response
          ==                                            ::
++  kiss-clay                                           ::  in request ->$
          $%  {$boat $~}                                ::  pier rebooted
              {$drop p/@p q/@tas}                       ::  cancel pending merge
              {$info p/@p q/@tas r/nori}                ::  internal edit
              {$init p/@p}                              ::  report install
              {$into p/@tas q/? r/mode}                 ::  external edit
              {$merg p/@p q/@tas r/@p s/@tas t/case u/germ}  ::  merge desks
              {$mont p/@tas q/@p r/@tas s/path}         ::  mount to unix
              {$ogre p/$@(@tas beam)}                   ::  delete mount point
              {$warp p/sock q/riff}                     ::  file request
              {$wegh $~}                                ::  report memory
              {$went p/sack q/path r/@ud s/coop}        ::  response confirm 
              {$west p/sack q/path r/@ud s/*}           ::  network request
          ==                                            ::
::
::::
  ::
++  blew  {p/@ud q/@ud}                                 ::  columns rows
++  belt                                                ::  old belt
  $%  {$aro p/?($d $l $r $u)}                           ::  arrow key
      {$bac $~}                                         ::  true backspace
      {$ctl p/@c}                                       ::  control-key
      {$del $~}                                         ::  true delete
      {$met p/@c}                                       ::  meta-key
      {$ret $~}                                         ::  return
      {$txt p/(list @c)}                                ::  utf32 text
  ==                                                    ::
++  blit                                                ::  old blit
  $%  {$bel $~}                                         ::  make a noise
      {$clr $~}                                         ::  clear the screen
      {$hop p/@ud}                                      ::  set cursor position
      {$lin p/(list @c)}                                ::  set current line
      {$mor $~}                                         ::  newline
      {$sag p/path q/*}                                 ::  save to jamfile
      {$sav p/path q/@}                                 ::  save to file
      {$url p/@t}                                       ::  activate url
  ==                                                    ::
++  dill-belt                                           ::  new belt
  $%  {$aro p/?($d $l $r $u)}                           ::  arrow key
      {$bac $~}                                         ::  true backspace
      {$cru p/@tas q/(list tank)}                       ::  echo error
      {$ctl p/@}                                        ::  control-key
      {$del $~}                                         ::  true delete
      {$hey $~}                                         ::  refresh
      {$met p/@}                                        ::  meta-key
      {$ret $~}                                         ::  return
      {$rez p/@ud q/@ud}                                ::  resize, cols, rows
      {$txt p/(list @c)}                                ::  utf32 text
      {$yow p/gill}                                     ::  connect to app
  ==                                                    ::
++  dill-blit                                           ::  new blit
  $%  {$bel $~}                                         ::  make a noise
      {$clr $~}                                         ::  clear the screen
      {$hop p/@ud}                                      ::  set cursor position
      {$mor p/(list dill-blit)}                         ::  multiple blits
      {$pro p/(list @c)}                                ::  show as cursor+line
      {$qit $~}                                         ::  close console
      {$out p/(list @c)}                                ::  send output line
      {$sag p/path q/*}                                 ::  save to jamfile
      {$sav p/path q/@}                                 ::  save to file
      {$url p/@t}                                       ::  activate url
  ==                                                    ::
++  flog                                                ::  sent to %dill
  $%  {$crud p/@tas q/(list tank)}                      ::
      {$heft $~}                                        ::
      {$text p/tape}                                    ::
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
++  gill  (pair ship term)                              ::  general contact
::::
++  gift-dill                                           ::  out result <-$
  $%  {$bbye $~}                                        ::  reset prompt
      {$blit p/(list blit)}                             ::  terminal output
      {$burl p/@t}                                      ::  activate url
      {$init p/@p}                                      ::  set owner
      {$logo $~}                                        ::  logout
      {$mass p/mass}                                    ::  memory usage
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
++  kiss-dill                                           ::  in request ->$
  $%  {$belt p/belt}                                    ::  terminal input
      {$blew p/blew}                                    ::  terminal config
      {$boot p/*}                                       ::  weird %dill boot
      {$crud p/@tas q/(list tank)}                      ::  error with trace
      {$flog p/flog}                                    ::  wrapped error
      {$flow p/@tas q/(list gill)}                      ::  terminal config
      {$hail $~}                                        ::  terminal refresh
      {$heft $~}                                        ::  memory report
      {$hook $~}                                        ::  this term hung up
      {$harm $~}                                        ::  all terms hung up
      {$init p/ship}                                    ::  after gall ready
      {$tick p/@p q/@p}                                 ::  initial ticket
      {$noop $~}                                        ::  no operation
      {$talk p/tank}                                    ::
      {$text p/tape}                                    ::
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
::
::::    %eyre
  ::
++  gram                                                ::  inter-ship message
  =+  fuz={path @ud *}                                  ::  ames format
  $?  {{$lon $~} p/hole}                                ::  login request
      {{$aut $~} p/hole}                                ::  login reply
      {{$hat $~} p/hole q/hart}                         ::  login redirect
      {{$get $~} p/@uvH q/{? clip httq}}                ::  remote request
      {{$got $~} p/@uvH q/httr}                         ::  remote response
      {{$gib $~} p/@uvH}                                ::  remote cancel
  ==                                                    ::
::::                                                    ::
++  kiss-eyre                                           ::  in request ->$
          $%  {$born $~}                                ::  new unix process
              {$crud p/@tas q/(list tank)}              ::  XX rethink
              {$hiss p/(unit iden) q/mark r/cage}       ::  outbound user req
              {$init p/@p}                              ::  report install
              {$serv p/$@(desk beam)}                   ::  set serving root
              {$them p/(unit hiss)}                     ::  outbound request
              {$they p/@ud q/httr}                      ::  inbound response
              {$chis p/? q/clip r/httq}                 ::  IPC inbound request
              {$this p/? q/clip r/httq}                 ::  inbound request
              {$thud $~}                                ::  inbound cancel
              {$wegh $~}                                ::  report memory
              {$went p/sack q/path r/@ud s/coop}        ::  response confirm 
              {$west p/sack q/fuz:gram}                 ::  network request
          ==                                            ::
++  gift-eyre                                           ::  out result <-$
          $%  {$mass p/mass}                            ::  memory usage
              {$mack p/(unit tang)}                     ::  message ack
              {$sigh p/cage}                            ::  marked http response
              {$thou p/httr}                            ::  raw http response
              {$thus p/@ud q/(unit hiss)}               ::  http request+cancel
              {$veer p/@ta q/path r/@t}                 ::  drop-through
              {$vega p/path}                            ::  drop-through
          ==                                            ::
::
::::    %ford
  ::
++  hood                                                ::  assembly plan
  $:  zus/@ud                                           ::  zuse kelvin
      sur/(list hoof)                                   ::  structures
      lib/(list hoof)                                   ::  libraries
      fan/(list horn)                                   ::  resources
      src/(list hoop)                                   ::  program
  ==                                                    ::
++  hoof  (pair term (unit (pair case ship)))           ::  resource reference
++  hoop                                                ::  source in hood
  $%  {$& p/twig}                                       ::  direct twig
      {$| p/beam}                                       ::  resource location   
  ==                                                    ::
++  hops   {pre/(unit tyke) pof/(unit {p/@ud q/tyke})}  ::  XX late-bound path
++  horn                                                ::  resource tree
  $%  {$ape p/twig}                                     ::  /~  twig by hand
      {$arg p/twig}                                     ::  /$  argument
      {$alt p/(list horn)}                              ::  /|  options
      {$dep p/horn}                                     ::  /#  insert dephash
      {$dub p/term q/horn}                              ::  /=  apply face
      {$fan p/(list horn)}                              ::  /.  list
      {$for p/(list (pair spur horn))}                  ::  /,  switch by path
      {$hel p/horn}                                     ::  /%  propagate args
      {$lin p/(list mark) q/horn}                       ::  /&  translates
      {$man p/(map knot horn)}                          ::  /*  hetero map
      {$nap p/horn}                                     ::  /_  homo map
      {$now p/horn}                                     ::  DEPRECATED
      {$nod p/term q/horn}                              ::  /_  @  list by odor
      {$saw p/twig q/horn}                              ::  /;  operate on
      {$see p/hops q/horn}                              ::  /:  relative to
      {$sic p/twig q/horn}                              ::  /^  cast
      {$toy p/? q/mark}                                 ::  /mark/  static/hook
  ==                                                    ::
++  milk  (trel ship desk silk)                         ::  sourced silk
++  silk                                                ::  construction layer
  $^  {p/silk q/silk}                                   ::  cons
  $%  {$$ p/cage}                                       ::  literal
      {$alts p/(list silk)}                             ::  options
      {$bake p/mark q/coin r/beam}                      ::  local synthesis
      {$bunt p/mark}                                    ::  example of mark
      {$call p/silk q/silk}                             ::  slam
      {$cast p/mark q/silk}                             ::  translate
      {$core p/beam}                                    ::  build program
      {$diff p/silk q/silk}                             ::  diff
      {$dude p/(trap tank) q/silk}                      ::  error wrap
      {$file p/beam}                                    ::  from clay
      {$flag p/(set $@(@uvH beam)) q/silk}              ::  add dependencies
      {$join p/mark q/silk r/silk}                      ::  merge
      {$mash p/mark q/milk r/milk}                      ::  annotate
      {$mute p/silk q/(list (pair wing silk))}          ::  mutant
      {$pact p/silk q/silk}                             ::  patch
      {$plan p/beam q/coin r/hood}                      ::  structured assembly
      {$reef $~}                                        ::  kernel reef
      {$ride p/twig q/silk}                             ::  silk thru twig
      {$tabl p/(list (pair silk silk))}                 ::  list
      {$vale p/mark q/*}                                ::  validate
      {$volt p/(cask *)}                                ::  unsafe add type
  ==                                                    ::
::::
++  bilk  (pair beak silk)                              ::  sourced request
++  gage                                                ::  recursive cage+tang
  $%  {$& p/cage}                                       ::  success
      {$| p/tang}                                       ::  error
      {$tabl p/(list (pair gage gage))}                 ::  table of results
  ==
++  gift-ford                                           ::  out result <-$
          $%  {$made p/@uvH q/gage}                     ::  computed result
              {$mass p/mass}                            ::  memory usage
              {$news p/@uvH}                            ::  fresh depends
          ==                                            ::
++  kiss-ford                                           ::  in request ->$
          $%  {$exec p/@p q/(unit bilk)}                ::  make / kill
              {$wasp p/@p q/{@uvH ?}}                   ::  depends ask / kill
              {$wegh $~}                                ::  report memory
              {$wipe p/@p $~}                           ::  clear cache
          ==                                            ::
::
::::    %gall
  ::
++  gall
  ^?  |%
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
  ++  cush  (pair term club)                            ::  internal kiss
  ++  dude  term                                        ::  server identity
  ++  scup  (pair ship desk)                            ::  autoupdate
  ++  well  (pair desk term)                            ::
  ++  suss  (trel dude @tas @da)                        ::  config report
  ::::                                                  ::
  ++  kiss                                              ::  incoming request
    $%  {$conf p/dock q/culm}                           ::  configure app
        {$init p/ship}                                  ::  set owner
        {$deal p/sock q/cush}                           ::  full transmission
        {$went p/sack q/path r/@ud s/coop}              ::  response confirm 
        {$west p/sack q/path r/@ud s/*}                 ::  network request
        {$wegh $~}                                      ::  report memory
    ==                                                  ::
  ++  gift                                              ::  outgoing result
    $%  {$mass p/mass}                                  ::  memory usage
        {$onto p/(each suss tang)}                      ::  about agent
        {$rend p/path q/*}                              ::  network request
        {$unto p/cuft}                                  ::
        {$mack p/(unit tang)}                           ::  message ack
    ==                                                  ::
  --
::
::::    %arvo
  ::
++  gift-arvo                                           ::  out result <-$
  $?  gift-ames
      gift-clay
      gift-dill
      gift-eyre
      gift-ford
      gift:gall
      gift-behn
  ==
++  kiss-arvo                                           ::  in request ->$
  $?  kiss-ames
      kiss-clay
      kiss-dill
      kiss-eyre
      kiss-ford
      kiss:gall
      kiss-behn
  ==
++  note-arvo                                           ::  out request $->
  $?  {@tas $meta vase}
  $%  {$a kiss-ames}
      {$b kiss-behn}
      {$c kiss-clay}
      {$d kiss-dill}
      {$e kiss-eyre}
      {$f kiss-ford}
      {$g kiss:gall}
  ==  ==
++  sign-arvo                                           ::  in result $<-
  $%  {$a gift-ames}
      {$b gift-behn}
      {$c gift-clay}
      {$d gift-dill}
      {$e gift-eyre}
      {$f gift-ford}
      {$g gift:gall}
      {$j gift:moves:^jael}
  ==
--
