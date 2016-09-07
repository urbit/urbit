::    %jael, secret 
::
::::  /hoon/jael
  ::
!?  164
!:  ::
|=  pit/vase   
=>  =~
::  private structures                                  ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  jael-state                                          ::  all crypto state
  $:  ver/$0                                            ::  %jael version 
      nav/jael-objective                                ::  all universal state
  ::  nix/jael-subjective                               ::  all derived state
  ==                                                    ::
++  jael-objective                                      ::  all universal state
  $:  urb/jael-urbit                                    ::  all urbit state
      web/(map (list @ta) jael-web-domain)              ::  all web state
  ==                                                    ::
++  jael-web-domain                                     ::  per foreign app
  $:  sec/(map @t jael-web-app)                         ::  client per api key
      usr/(map @ta jael-web-user)                       ::  direct user info
  ==                                                    ::
++  jael-web-app                                        ::  local app
  $:  key/(unit (pair @da @))                           ::  API key
      tok/(map @t (pair @da @))                         ::  token by username
  ==                                                    ::
++  jael-web-user                                       ::  per-user secrets
  $:  pas/(unit @t)                                     ::  password
      dey/(unit @t)                                     ::  display name
  ==                                                    ::
++  jael-urbit                                          ::  objective urbit
  $:  pub/gree                                          ::  all public state
      pry/(map ship jael-ship)                          ::  all private state
  ==                                                    ::
++  jael-ship                                           ::  objective by ship
  $:  rel/(map ship jael-friend)                        ::  relationships
      own/(map life ring)                               ::  private keys
      vew/(set duct)                                    ::  watchers
  ==                                                    ::
++  jael-friend                                         ::  relationship 
  $:  luf/(unit life)                                   ::  life as known to
      lab/(nap jael-right)                              ::  promises to
      vow/(set duct)                                    ::  watchers
  ==                                                    ::
++  jael-inference                                      ::  learning result
  $%  {$hard p/ship}                                    ::  rough update to
      {$meet p/ship}                                    ::  first contact with
      {$soft p/ship}                                    ::  soft update to
      {$sign p/ship q/gree}                             ::  return signature
  ==
++  jael-right                                          ::  urbit commitment
  $%  {$block p/pile}                                   ::  address block
      {$email p/(set @ta)}                              ::  email addresses
      {$entry p/(map hand (pair @da code))}             ::  symmetric keys
      {$final p/(map ship @uvG)}                        ::  tickets
      {$fungi p/(map term @ud)}                         ::  fungibles
      {$funny p/(map term *)}                           ::  extended 
      {$hello p/(set term)}                             ::  usernames
      {$lived p/life}                                   ::  PKI commitment
  ==                                                    ::
++  jael-task                                           ::  operations on
  $%  {$give p/ship q/(nap jael-right)}                 ::  add rights
      {$line p/ship q/@da r/code}                       ::  outbound symkey
      {$link p/ship q/@da r/code}                       ::  inbound symkey
      {$meet p/gree}                                    ::  integrate truth
      {$over p/ship q/jael-task}                        ::  mirror operation
      {$pall p/ship q/life}                             ::  our life acked
      {$ring p/? q/(map chip (pair @ta @t)) r/ring}     ::  update private key
      {$take p/ship q/(nap jael-right)}                 ::  subtract rights
      {$vain $~}                                        ::  watch self
      {$vest $~}                                        ::  watch assets
      {$view p/ship}                                    ::  watch urbit
      {$vile p/(list @ta)}                              ::  watch website
      {$west p/ship q/path r/*}                         ::  remote request
      {$wink p/@ta q/@t r/(unit (pair @da @))}          ::  set API key
      {$wonk p/@ta q/@t r/@t s/(unit (pair @da @))}     ::  set API token
  ==                                                    ::
++  jael-report-them                                    ::  report on neighbor
  $:  gur/grue                                          ::  certificate
      lab/(nap jael-right)                              ::  our promises to
      own/(nap jael-right)                              ::  our promises from
  ==                                                    ::
++  jael-report-self                                    ::  report on self
  $:  gur/grue                                          ::  certificate
      war/(map life ring)                               ::  private keys
  ==                                                    ::
++  jael-report-cash                                    ::  neighbors/assets
  $:  has/(map ship (nap jael-right))                   ::
  ==                                                    ::
++  jael-report-paid                                    ::  asset diff
  $:  dif/(list (trel ship ? (nap jael-right)))         ::  who, +/-, what
  ==                                                    ::
::                                                      ::
++  jael-gift                                           ::  output
  $?  {$cash jael-report-cash}                          ::  asset dump
      {$clue jael-report-them}                          ::  channel dump
      {$paid jael-report-paid}                          ::  asset update
      {$self jael-report-self}                          ::  self dump
      {$well jael-web-domain}                           ::  service update
  ==                                                    ::
++  jael-message                                        ::  p2p message
  $%  {$hail p/(nap jael-right)}                        ::  re/set rights
      {$ping $~}                                        ::  ping
      {$seed p/gree}                                    ::  propagate
  ==                                                    ::
++  jael-effect                                         ::  propagation effect
  $%  {$cold p/ship q/life}                             ::  breach to life
      {$helo p/ship}                                    ::  intro neighbor
      {$sign p/ship q/life}                             ::  added signature
      {$sure p/ship q/life}                             ::  signature confirmed
      {$warm p/ship q/life}                             ::  advance to life
      {$yell p/gree}                                    ::  propagate
  ==                                                    ::
++  meet                                                ::  merge pkis
  |=  {via/@p new/gree old/gree}
  =+  wen=(~(tap by new))
  |^  ^-  (pair (list jael-effect) gree)
      ?~  wen  [~ old]
      =+  mor=$(wen t.wen)
      =+  dis=(boat(old p.mor) i.wen)
      [(weld p.dis p.mor) q.dis]
  ::                                                    ::
  ++  boat                                              ::  merge per ship
    |=  {who/ship gur/grue}
    =+  [num=1 rug=((bond |.(*grue)) (~(get by old) who))]
    =|  pre/(unit lama)
    |-  ^-  (pair (list jael-effect) gree)
    ::
    ::  lives in gur/grue are 1 through n
    ::
    ?:  (gth num p.gur)  [~ old]
    ::
    ::  lod is the old deed, wyn is the new deed
    ::
    =+  [lod=(~(get by q.rug) num) wyn=(~(get by q.gur) num)]
    ::
    ::  if no new deed, or new deed matches old deed
    ::
    ?:  |(?=($~ wyn) =(wyn lod))
      ::
      ::  there's nothing to learn, move forward
      ::
      $(num +(num), pre lod)
    ::
    ::  check all signatures in 
    ::
    ::
    ::  if there is an old deed
    ::
    ?^  lod
      ::
      ::  merge old and new 
      ::
    =^  wax  ^-  lama

    ?^  lod
      
    ::

  ::                                                    ::
  ++  look                                              ::  get public key
    |=  myn/mind 
    ^-  (unit @)
    |^  ((bond |.((find(old new) myn))) (find myn))
    ++  find
      ^-  (unit @)
      %+  biff  (~(get by old) who.myn)
      |=  gur/grue
      %+  biff  (~(get by q.gur) lyf.myn)
      |=(lama `pub)
    --
  ::
  ++  good                                              ::  check signature
    |=  {myn/mind ash/@ val/@}
    ^-  %&
    =*  pub

    =*  pub  
      %.  (~(get by 
  --

++  move  {p/duct q/{$gift jael-gift}}                  ::  local move
--                                                      ::
.  ==                                                   ::
=|  lex/jael-state                                      ::  kernel state
|=  {now/@da eny/@ ski/sley}                            ::  current invocation
=<  |%                                                  ::  vane interface
    ++  call                                            ::  request
      |=  $:  hen/duct
              hic/(hypo (hobo jael-task))
          ==
      =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard jael-task) p.q.hic)))
      ^-  {p/(list move) q/_..^$}
      =^  did  lex  (^call hen q.hic)
      [did ..^$]
    ::
    ++  doze                                            ::  sleep
      |=  {now/@da hen/duct}
      ^-  (unit @da)
      ~
    ::
    ++  load                                            ::  upgrade
      |=  old/jael-state
      ^+  ..^$
      ~&  %jael-reload
      ..^$(lex old)
    ::
    ++  scry
      |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
      ^-  (unit (unit cage))
      !!
    ::
    ++  stay  lex
    ++  take                                            ::  accept response
      |=  {tea/wire hen/duct hin/(hypo sign-arvo)}
      ^-  {p/(list move) q/_..^$}
      [~ ..^$]
    --
|%
++  call
  |=  {hen/duct tac/jael-task}
  ^-  {(list move) jael-state}
  ?+    -.tac  !!
      $give
    =<  abet
    =<  abet
    (give:(unto:(from our) p.tac) q.tac)
  ::
      $link
    =*  ryt  `jael-right`[%entry [[(shaf %hand r.tac) q.tac r.tac] ~ ~]]
    =<  abet
    =<  abet
    (give:(unto:(from our) p.tac) `(nap jael-right)`[ryt ~ ~])
  ::
      $line
    =*  ryt  `jael-right`[%entry [[(shaf %hand r.tac) q.tac r.tac] ~ ~]]
    =<  abet
    =<  abet
    (give:(unto:(from p.tac) our) `(nap jael-right)`[ryt ~ ~])
  ::
      $meet
    =^  fur  pub.urb.nav.lex  (~(meet da pub.urb.nav.lex) p.tac)
    [~ lex]
  ::
      $over
    $(our p.tac, tac q.tac)
  ::
      $pall
    =<  abet
    =<  abet
    (pall:(unto:(from our) p.tac) q.tac)
  ==
::
++  meld                                      
  |=  {new/gree old/gree}
  =+  
  =<  work
  |%  ++  abet
        
      ++  work
        =+  wen=(~(tap by new))
        |-  ^-  {(list jael-inference
  ++  
  =+  wen=(~(tap by new))
  |-  ^-  {gree gree}
  ?~  wen  [~ old]

  =+  mor=$(wen t.wen)
   


++  da                                                  ::  pedigree core
  |_  {via/ship ped/gree}
  ++  meet
    |=  new/gree
    =+  wen=(~(tap by new))
    |-  ^-  {(list jael-inference) gree}
    ?~  wen  [~ ped]
    =+  mor=$(wen t.wen)


     
  --
::
++  from
  |=  rex/ship
  =+  :*  nex=*(list move)
          ((bond |.(*jael-ship)) (~(get by pry.urb.nav.lex) rex))
      ==
  |%
  ++  abet  
    ^-  {(list move) jael-state}
    :-  (flop nex)
    lex(pry.urb.nav (~(put by pry.urb.nav.lex) rex `jael-ship`+<+))
  ::
  ++  unto
    |=  pal/ship
    =+  ((bond |.(*jael-friend)) (~(get by rel) pal))
    |%
    ++  abet                                            ::  resolve
      ..unto(rel (~(put by rel) pal `jael-friend`+<))
    ::
    ++  give
      |=  lab/(nap jael-right)
      ^+  +>
      !!
    ::
    ++  pall
      |=  lyf/life
      ?>  |(?=($~ luf) =(u.luf lyf) =(
      +>(luf `lyf)
      %=    +>
          luf
        ?~  luf  `lyf
        ?:  =(u.luf 
      ==
    --
  --
--
