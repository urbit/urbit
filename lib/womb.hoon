::                                                      ::  ::
::::  /hoon/womb/lib                                    ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/+    talk, old-phon
=,  wired
=,  title
=,  womb:jael
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
:: |%
:: ++  foil                                                ::  ship allocation map
::   |*  mold                                              ::  entry mold
::   $:  min/@u                                            ::  minimum entry
::       ctr/@u                                            ::  next allocated
::       und/(set @u)                                      ::  free under counter
::       ove/(set @u)                                      ::  alloc over counter
::       max/@u                                            ::  maximum entry
::       box/(map @u +<)                                   ::  entries
::   ==                                                    ::
:: --                                                      ::
::                                                      ::
::::                                                    ::
  ::                                                    ::
|%                                                      ::
:: ++  managed                                             ::  managed plot
::   |*  mold                                              ::
::   %-  unit                                              ::  unsplit
::   %+  each  +<                                          ::  subdivided
::   mail                                                  ::  delivered
:: ::                                                      ::
:: ++  divided                                             ::  get division state
::   |*  (managed)                                         ::
::   ?-  +<                                                ::
::     $~      ~                                           ::  unsplit
::     {$~ $| *}  ~                                        ::  delivered
::     {$~ $& *}  (some p.u.+<)                            ::  subdivided
::   ==                                                    ::
:: ::                                                      ::
:: ++  moon  (managed _!!)                                 ::  undivided moon
:: ::
:: ++  planet                                              ::  subdivided planet
::   (managed (lone (foil moon)))                          ::
:: ::                                                      ::
:: ++  star                                                ::  subdivided star
::   (managed (pair (foil moon) (foil planet)))            ::
:: ::                                                      ::
:: ++  galaxy                                              ::  subdivided galaxy
::   (managed (trel (foil moon) (foil planet) (foil star)))::
:: ::                                                      ::
++  ticket  @G                                          ::  old 64-bit ticket
++  passcode  @uvH                                      ::  128-bit passcode
++  passhash  @uwH                                      ::  passocde hash
++  mail  @t                                            ::  email address
++  balance                                             ::  invitation balance
  $:  planets/@ud                                       ::  planet count
      stars/@ud                                         ::  star count
      owner/mail                                        ::  owner's email
      history/(list mail)                               ::  transfer history
  ==                                                    ::
:: ++  property                                            ::  subdivided plots
::   $:  galaxies/(map @p galaxy)                          ::  galaxy
::       planets/(map @p planet)                           ::  star
::       stars/(map @p star)                               ::  planet
::   ==                                                    ::
++  invite                                              ::
  $:  who/mail                                          ::  who to send to
      pla/@ud                                           ::  planets to send
      sta/@ud                                           ::  stars to send
      wel/welcome                                       ::  welcome message
  ==                                                    ::
++  welcome                                             ::  welcome message
  $:  intro/tape                                        ::  in invite email
      hello/tape                                        ::  as talk message
  ==                                                    ::
++  stat  (pair live dist)                              ::  external info
++  live  ?($cold $seen $live)                          ::  online status
++  dist                                                ::  allocation
  $%  {$free $~}                                        ::  unallocated
      {$owned p/mail}                                   ::  granted, status
      {$split p/(map ship stat)}                        ::  all given ships
  ==                                                    ::
::                                                      ::
++  ames-tell                                           ::  .^ a+/=tell= type
  |^  {p/(list elem) q/(list elem)}                     ::
  ++  elem  $^  {p/elem q/elem}                         ::
            {term p/*}                                  ::  underspecified
  --                                                    ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  part  {$womb $1 pith}                               ::  womb state
++  pith                                                ::  womb content
  $:  boss/(unit ship)                                  ::  outside master
::       bureau/(map passhash balance)                     ::  active invitations
::       office/property                                   ::  properties managed
      recycling/(map ship @)                            ::  old ticket keys
  ==                                                    ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  arvo structures
++  invite-j  {who/mail pla/@ud sta/@ud}                ::  invite data
++  balance-j  {who/mail pla/@ud sta/@ud}               ::  balance data
++  womb-task                                           ::  manage ship %fungi
  $%  {$claim aut/passcode her/@p tik/ticket}           ::  convert to %final
      {$bonus tid/passcode pla/@ud sta/@ud}             ::  supplement passcode
      {$invite tid/passcode inv/invite-j}               ::  alloc to passcode
      {$reinvite aut/passcode tid/passcode inv/invite-j}::  move to another
  ==                                                    ::
++  card                                                ::
  $%  {$flog wire flog:dill}                           ::
      {$info wire @p @tas nori:clay}                   ::  fs write (backup)
      :: {$wait $~}                                     :: delay acknowledgment
      {$diff gilt}                                      :: subscription response
      {$poke wire dock pear}                            ::  app RPC
      {$next wire p/ring}                               ::  update private key
      {$tick wire p/@pG q/@p}                           ::  save ticket
      {$knew wire p/ship q/wyll:ames}                        ::  learn will (old pki)
      {$jaelwomb wire task:womb}                        ::  manage rights
  ==                                                    ::
++  pear                                                ::
  $%  {$email mail tape wall}                           ::  send email
      {$womb-do-ticket ship}                            ::  request ticket
      {$womb-do-claim ship @p}                          ::  issue ship
      {$drum-put path @t}                               ::  log transaction
  ==                                                    ::
++  gilt                                                :: scry result
  $%  {$ships (list ship)}                              ::
      {$womb-balance balance}                           ::
      {$womb-balance-all (map passhash mail)}           ::
      {$womb-stat stat}                                 ::
::       {$womb-stat-all (map ship stat)}                  ::
      {$womb-ticket-info passcode ?($fail $good $used)} ::
  ==
++  move  (pair bone card)                              ::  user-level move
::
++  transaction                                         ::  logged poke
  $%  {$report her/@p wyl/wyll:ames}
      {$claim aut/passcode her/@p}
      {$recycle who/mail him/knot tik/knot}
      {$bonus tid/cord pla/@ud sta/@ud}
      {$invite tid/cord inv/invite}
      {$reinvite aut/passcode inv/invite}
  ==
--
|%
++  ames-grab                                           :: XX better ames scry
  |=  {a/term b/ames-tell}  ^-  *
  =;  all  (~(got by all) a)
  %-  ~(gas by *(map term *))
  %-  zing
  %+  turn  (weld p.b q.b)
  |=  b/elem:ames-tell  ^-  (list {term *})
  ?@  -.b  [b]~
  (weld $(b p.b) $(b q.b))
::
++  murn-by
  |*  {a/(map) b/$-(* (unit))}
  ^-  ?~(a !! (map _p.n.a _(need (b q.n.a))))
  %-  malt
  %+  murn  (~(tap by a))
  ?~  a  $~
  |=  _c=n.a  ^-  (unit _[p.n.a (need (b q.n.a))])
  =+  d=(b q.c)
  ?~(d ~ (some [p.c u.d]))
::
++  neis  |=(a/ship ^-(@u (rsh (dec (xeb (dec (xeb a)))) 1 a)))  ::  postfix
::
--
::                                                    ::  ::
::::                                                  ::  ::
  ::                                                  ::  ::
=+  cfg=[can-claim=& can-recycle=&]                   ::  temporarily disabled
=+  [replay=| stat-no-email=|]                        ::  XX globals
|=  {bowl:gall part}                                 ::  main womb work
|_  moz/(list move)
++  abet                                              ::  resolve
  ^-  (quip move *part)
  [(flop moz) +>+<+]
::
++  teba                                              ::  install resolved
  |=  a/(quip move *part)  ^+  +>
  +>(moz (flop -.a), +>+<+ +.a)
::
++  emit  |=(card %_(+> moz [[ost +<] moz]))          ::  return card
++  emil                                              ::  return cards
  |=  (list card)
  ^+  +>
  ?~(+< +> $(+< t.+<, +> (emit i.+<)))
::
++  ames-last-seen                                    ::  last succesful ping
  |=  a/ship  ~+  ^-  (unit time)
  ?:  =(a our)  (some now)
  %-  (hard (unit time))
  ~|  ames-look+/(scot %p our)/tell/(scot %da now)/(scot %p a)
  %+  ames-grab  %rue
  .^(ames-tell %a /(scot %p our)/tell/(scot %da now)/(scot %p a))
::
++  jael-scry
  |*  {typ/mold pax/path}  ^-  typ
  .^(typ %j (welp /(scot %p our)/womb/(scot %da now) pax))
::
++  jael-pas-balance
  |=  pas/passcode  ^-  (unit balance)
  %+  bind  (jael-scry (unit balance-j) /balance/(scot %uv pas)/womb-balance)
  |=  a/balance-j  ^-  balance
  =/  hiz/(list mail)  ~  :: XX track history in jael
  [pla.a sta.a who.a hiz]
::
::
++  peek-x-shop                                       ::  available ships
  |=  tyl/path  ^-  (unit (unit {$ships (list @p)}))
  =;  a   ~&  peek-x-shop+[tyl a]  a
  =;  res/(list ship)  (some (some [%ships res]))
  :: XX redundant parse?
  =+  [typ nth]=~|(bad-path+tyl (raid tyl /[typ=%tas]/[nth=%ud]))
  (jael-scry (list ship) /shop/[typ]/(scot %ud nth)/ships)
::
++  get-live                                          ::  last-heard time ++live
  |=  a/ship  ^-  live
  =+  rue=(ames-last-seen a)
  ?~  rue  %cold
  ?:((gth (sub now u.rue) ~m5) %seen %live)
::
::
++  stats-ship                                        ::  inspect ship
  |=  who/@p  ^-  stat
  :-  (get-live who)
  =/  man  (jael-scry (unit mail) /stats/(scot %p who)/womb-owner)
  ?~  man  [%free ~]
  ?:  stat-no-email  [%owned '']
  [%owned u.man]
::
++  peek-x-stats                                      ::  inspect ship/system
  |=  tyl/path
  ?^  tyl
    ?>  |(=(our src) =([~ src] boss))                   ::  privileged info
    :: XX redundant parse?
    =+  who=~|(bad-path+tyl (raid tyl /[who=%p]))
    ``womb-stat+(stats-ship who)
  !!  ::  XX meaningful and/or useful in sein-jael model?
::   ^-  (unit (unit {$womb-stat-all (map ship stat)}))
::   =.  stat-no-email  &                      ::  censor adresses
::   :^  ~  ~  %womb-stat-all
::   %-  ~(uni by (~(urn by planets.office) stat-planet))
::   %-  ~(uni by (~(urn by stars.office) stat-star))
::   (~(urn by galaxies.office) stat-galaxy)
::
++  peek-x-balance                                     ::  inspect invitation
  |=  tyl/path
  ^-  (unit (unit {$womb-balance balance}))
  :: XX redundant parse?
  =+  pas=~|(bad-path+tyl (raid tyl /[pas=%uv]))
  %-  some
  %+  bind  (jael-pas-balance pas)
  |=(a/balance [%womb-balance a])
::
++  parse-ticket
  |=  {a/knot b/knot}  ^-  {him/@ tik/@}
  [him=(rash a old-phon) tik=(rash b old-phon)]
::
++  check-old-ticket
  |=  {a/ship b/@pG}  ^-  (unit ?)
  %+  bind   (~(get by recycling) (sein a))
  |=  key/@  ^-  ?
  =(b `@p`(end 6 1 (shaf %tick (mix a (shax key)))))
::
::
++  peek-x-ticket
  |=  tyl/path
  ^-  (unit (unit {$womb-ticket-info passcode ?($fail $good $used)}))
  ?.  ?=({@ @ $~} tyl)  ~|(bad-path+tyl !!)
  =+  [him tik]=(parse-ticket i.tyl i.t.tyl)
  %+  bind  (check-old-ticket him tik)
  |=  gud/?
  :+  ~  %womb-ticket-info
  =+  pas=`passcode`(end 7 1 (sham %tick him tik))
  :-  pas
  ?.  gud  %fail
  ?^  (jael-pas-balance pas)  %used
  %good
::
++  peer-scry-x                                        ::  subscription like .^
  |=  tyl/path
  =<  abet
  =+  gil=(peek-x tyl)
  ~|  tyl
  ?~  gil  ~|(%block-stub !!)
  ?~  u.gil  ~|(%bad-path !!)
  (emit %diff u.u.gil)
::
++  peek-x                                             ::  stateless read
  |=  tyl/path  ^-  (unit (unit gilt))
  ~|  peek+x+tyl
  ?~  tyl  ~
  ?+  -.tyl  ~
  ::  /shop/planets/@ud   (list @p)    up to 3 planets
  ::  /shop/stars/@ud     (list @p)    up to 3 stars
  ::  /shop/galaxies/@ud  (list @p)    up to 3 galaxies
    $shop  (peek-x-shop +.tyl)
  ::  /stats                          general stats dump
  ::  /stats/@p                       what we know about @p
    $stats  (peek-x-stats +.tyl)
  ::  /balance/passcode                invitation status
    $balance  (peek-x-balance +.tyl)
  ::  /ticket/ship/ticket              check ticket usability
    $ticket  (peek-x-ticket +.tyl)
  ==
::
++  poke-manage-old-key                               ::  add to recyclable tickets
  |=  {a/ship b/@}
  =<  abet
  ?>  |(=(our src) =([~ src] boss))                   ::  privileged
  .(recycling (~(put by recycling) a b))
::
++  email                                             ::  send email
  |=  {wir/wire adr/mail msg/tape}  ^+  +>
  ?:  replay  +>                      ::  dont's send email in replay mode
  ~&  do-email+[adr msg]
  ::~&([%email-stub adr msg] +>)
  (emit %poke womb+[%mail wir] [our %gmail] %email adr "Your Urbit Invitation" [msg]~)
::
++  log-transaction                                   ::  logged poke
  |=  a/transaction  ^+  +>
  ?:  replay  +>
  (emit %poke /womb/log [our %hood] %drum-put /womb-events/(scot %da now)/hoon (crip <eny a>))
::
++  poke-replay-log                                   ::  rerun transactions
  |=  a/(list {eny/@uvJ pok/transaction})
  ?~  a  abet
  ~&  womb-replay+-.pok.i.a
  =.  eny  eny.i.a
  =.  replay  &
  %_    $
      a  t.a
      +>
    ?-  -.pok.i.a
      $claim     (teba (poke-claim +.pok.i.a))
      $bonus    (teba (poke-bonus +.pok.i.a))
      $invite    (teba (poke-invite +.pok.i.a))
      $report    (teba (poke-report +.pok.i.a))
      $recycle   (teba (poke-recycle +.pok.i.a))
      $reinvite  (teba (poke-reinvite +.pok.i.a))
    ==
  ==
::
++  poke-bonus                                        ::  expand invitation
  |=  {tid/cord pla/@ud sta/@ud}
  =<  abet
  =.  log-transaction  (log-transaction %bonus +<)
  ?>  |(=(our src) =([~ src] boss))                   ::  priveledged
  =/  pas  ~|(bad-invite+tid `passcode`(slav %uv tid))
  (emit %jaelwomb / %bonus pas pla sta)
::
++  poke-invite                                       ::  create invitation
  |=  {tid/cord inv/invite}
  =<  abet
  =.  log-transaction  (log-transaction %invite +<)
  ?>  |(=(our src) =([~ src] boss))                   ::  priveledged
  =+  pas=~|(bad-invite+tid `passcode`(slav %uv tid))
  =.  emit  (emit %jaelwomb / %invite pas [who pla sta]:inv)
  (email /invite who.inv intro.wel.inv)
::
++  poke-reinvite                                     ::  split invitation
  |=  {aut/passcode inv/invite}                       ::  further invite
  =<  abet
  =.  log-transaction  (log-transaction %reinvite +<)
  ?>  =(src src)                                      ::  self-authenticated
  =/  pas/@uv  (end 7 1 (shaf %pass eny))
  =.  emit  (emit %jaelwomb / %reinvite aut pas [who pla sta]:inv)
  (email /invite who.inv intro.wel.inv)
::
++  poke-obey                                         ::  set/reset boss
  |=  who/(unit @p)
  =<  abet
  ?>  =(our src)                                      ::  me only
  .(boss who)
::
++  poke-save                                         ::  write backup
  |=  pax/path
  =<  abet
  ?>  =(our src)                                      ::  me only
  =+  pas=`@uw`(shas %back eny)
  ~&  [%backing-up pas=pas]
  =;  dif  (emit %info /backup [our dif])
  %+  foal:space:userlib
    (welp pax /jam-crub)
  [%jam-crub !>((en:crub:crypto pas (jam `part`+:abet)))]
::
++  poke-rekey                                        ::  extend wyll
  |=  $~
  =<  abet
  ?>  |(=(our src) =([~ src] boss))                   ::  privileged
  ::  (emit /rekey %next sec:ex:(pit:nu:crub 512 (shaz (mix %next (shaz eny)))))
  ~&  %rekey-stub  .
::
++  poke-report                                       ::  report wyll
  |=  {her/@p wyl/wyll:ames}                               ::
  =<  abet
  =.  log-transaction  (log-transaction %report +<)
  ?>  =(src src)                                      ::  self-authenticated
  (emit %knew /report her wyl)
::
++  poke-recycle                                      ::  save ticket as balance
  |=  {who/mail him-t/knot tik-t/knot}
  ?.  can-recycle.cfg  ~|(%ticket-recycling-offline !!)
  =<  abet
  =.  log-transaction  (log-transaction %recycle +<)
  ?>  =(src src)
  =+  [him tik]=(parse-ticket him-t tik-t)
  ?>  (need (check-old-ticket him tik))
  =+  pas=`passcode`(end 7 1 (sham %tick him tik))
::   ?^  (scry-womb-invite (shaf %pass pas))
::     ~|(already-recycled+[him-t tik-t] !!)
  =/  inv/{pla/@ud sta/@ud}
    ?+((clan him) !! $duke [0 1], $king [1 0])
  (emit %jaelwomb / %invite pas who inv)
::
::
:: ++  jael-claimed  'Move email here if an ack is necessary'
::
++  poke-claim                                        ::  claim plot, req ticket
  |=  {aut/passcode her/@p}
  ?.  can-claim.cfg  ~|(%ticketing-offline !!)
  =<  abet
  =.  log-transaction  (log-transaction %claim +<)
  ?>  =(src src)
  =/  bal  ~|(%bad-invite (need (jael-pas-balance aut)))
  =/  tik/ticket  (end 6 1 (shas %tick eny))
  =.  emit  (emit %jaelwomb / %claim aut her tik)
  :: XX event crashes work properly yes?
  (email /ticket owner.bal "Ticket for {<her>}: {<`@pG`tik>}")
--
