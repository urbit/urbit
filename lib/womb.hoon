::                                                      ::  ::
::::  /hoon/womb/lib                                    ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/+    talk, old-phon
=,  wired
=,  title
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  foil                                                ::  ship allocation map
  |*  a=mold                                             ::  entry mold
  $:  min/@u                                            ::  minimum entry
      ctr/@u                                            ::  next allocated
      und/(set @u)                                      ::  free under counter
      ove/(set @u)                                      ::  alloc over counter
      max/@u                                            ::  maximum entry
      box/(map @u a)                                    ::  entries
  ==                                                    ::
--                                                      ::
::                                                      ::
::::                                                    ::
  ::                                                    ::
|%                                                      ::
++  managed                                             ::  managed plot
  |*  mold                                              ::
  %-  unit                                              ::  unsplit
  %+  each  +<                                          ::  subdivided
  mail                                                  ::  delivered
::                                                      ::
++  divided                                             ::  get division state
  |*  (managed)                                         ::
  ?-  +<                                                ::
    $~      ~                                           ::  unsplit
    {$~ $| *}  ~                                        ::  delivered
    {$~ $& *}  (some p.u.+<)                            ::  subdivided
  ==                                                    ::
::                                                      ::
++  moon  (managed _!!)                                 ::  undivided moon
::
++  planet                                              ::  subdivided planet
  (managed (lone (foil moon)))                          ::
::                                                      ::
++  star                                                ::  subdivided star
  (managed (pair (foil moon) (foil planet)))            ::
::                                                      ::
++  galaxy                                              ::  subdivided galaxy
  (managed (trel (foil moon) (foil planet) (foil star)))::
::                                                      ::
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
++  client                                              ::  per email
  $:  sta/@ud                                           ::  unused star refs
      has/(set @p)                                      ::  planets owned
  ==                                                    ::
++  property                                            ::  subdivided plots
  $:  galaxies/(map @p galaxy)                          ::  galaxy
      planets/(map @p planet)                           ::  star
      stars/(map @p star)                               ::  planet
  ==                                                    ::
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
++  reference                                           ::  affiliate credit
  (unit (each @p mail))                                 ::  ship or email
::                                                      ::
++  reference-rate  2                                   ::  star refs per star
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
      bureau/(map passhash balance)                     ::  active invitations
      office/property                                   ::  properties managed
      hotel/(map (each ship mail) client)               ::  everyone we know
      recycling/(map ship @)                            ::  old ticket keys
  ==                                                    ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  arvo structures
++  card                                                ::
  $%  {$flog wire flog:dill}                           ::
      {$info wire @p @tas nori:clay}                   ::  fs write (backup)
      :: {$wait $~}                                     :: delay acknowledgment
      {$diff gilt}                                      :: subscription response
      {$poke wire dock pear}                            ::  app RPC
      {$next wire p/ring}                               ::  update private key
      {$tick wire p/@pG q/@p}                           ::  save ticket
      {$knew wire p/ship q/wyll:ames}                        ::  learn will (old pki)
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
      {$womb-stat-all (map ship stat)}                  ::
      {$womb-ticket-info passcode ?($fail $good $used)} ::
  ==
++  move  (pair bone card)                              ::  user-level move
::
++  transaction                                         ::  logged poke
  $%  {$report her/@p wyl/wyll:ames}
      {$release gal/@ud sta/@ud}
      {$release-ships (list ship)}
      {$claim aut/passcode her/@p}
      {$recycle who/mail him/knot tik/knot}
      {$bonus tid/cord pla/@ud sta/@ud}
      {$invite tid/cord ref/reference inv/invite}
      {$reinvite aut/passcode new/passcode inv/invite}
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
  ^+  ?~(a !! *(map _p.n.a _(need (b q.n.a))))
  %-  malt
  %+  murn  ~(tap by a)
  ?~  a  $~
  |=  _c=n.a  ^-  (unit _[p.n.a (need (b q.n.a))])
  =+  d=(b q.c)
  ?~(d ~ (some [p.c u.d]))
::
++  unsplit
  |=  a/(map ship (managed))  ^-  (list {ship *})
  %+  skim  ~(tap by a)
  |=({@ a/(managed)} ?=($~ a))
::
++  issuing
  |*  a/(map ship (managed))
  ^-  (list [ship _(need (divided (~(got by a))))])
  (sort ~(tap by (murn-by a divided)) lor)
::
++  issuing-under
  |*  {a/bloq b/ship c/(map @u (managed))}
  ^-  (list [ship _(need (divided (~(got by c))))])
  %+  turn  (sort ~(tap by (murn-by c divided)) lor)
  |*(d/{@u *} [(rep a b -.d ~) +.d])
++  cursor  (pair (unit ship) @u)
++  neis  |=(a/ship ^-(@u (rsh (dec (xeb (dec (xeb a)))) 1 a)))  ::  postfix
::
::  Create new foil of size
++  fo-init
  |=  a/bloq  ::  ^-  (foil *)
  [min=1 ctr=1 und=~ ove=~ max=(dec (bex (bex a))) box=~]
::
++  fo
  |_  (foil $@($~ *))
  ++  nth                                             ::  index
    |=  a/@u  ^-  (pair (unit @u) @u)
    ?:  (lth a ~(wyt in und))
      =+  out=(snag a (sort ~(tap in und) lth))
      [(some out) 0]
    =.  a  (sub a ~(wyt in und))
    |-  ^-  {(unit @u) @u}
    ?:  =(ctr +(max))  [~ a]
    ?:  =(0 a)  [(some ctr) a]
    $(a (dec a), +<.nth new)
  ::
  +-  fin  +<                                         ::  abet
  ++  new                                             ::  alloc
    ?:  =(ctr +(max))  +<
    =.  ctr  +(ctr)
    ?.  (~(has in ove) ctr)  +<
    new(ove (~(del in ove) ctr))
  ::
  +-  get                                             ::  nullable
    |=  a/@p  ^+  ?~(box ~ q.n.box)
    (fall (~(get by box) (neis a)) ~)
  ::
  +-  put
    |*  {a/@u b/*}  ^+  fin           ::  b/_(~(got by box))
    ~|  put+[a fin]
    ?>  (fit a)
    =;  adj  adj(box (~(put by box) a b))
    ?:  (~(has in box) a)  fin
    ?:  =(ctr a)  new
    ?:  (lth a ctr)
      ?.  (~(has in und) a)  fin
      fin(und (~(del in und) a))
    ?.  =(a ctr:new)    :: heuristic
      fin(ove (~(put in ove) a))
    =+  n=new(+< new)
    n(und (~(put in und.n) ctr))
  ::
  ++  fit  |=(a/@u &((lte min a) (lte a max)))        ::  in range
  ++  gud                                             ::  invariant
    ?&  (fit(max +(max)) ctr)
        (~(all in und) fit(max ctr))
        (~(all in ove) fit(min ctr))
        (~(all in box) |=({a/@u *} (fit a)))
        |-  ^-  ?
        ?:  =(min max)  &
        =-  &(- $(min +(min)))
        %+  gte  1              ::  at most one of
        ;:  add
          ?:(=(min ctr) 1 0)
          ?:((~(has in und) min) 1 0)
          ?:((~(has in ove) min) 1 0)
          ?:((~(has by box) min) 1 0)
        ==
    ==
  --
--
::                                                    ::  ::
::::                                                  ::  ::
  ::                                                  ::  ::
=+  cfg=[can-claim=& can-recycle=&]                   ::  temporarily disabled
=+  [replay=| stat-no-email=|]                        ::  XX globals
|=  {bowl:gall part}                                 ::  main womb work
|_  moz/(list move)
++  abet                                              ::  resolve
  ^-  (quip move part)
  [(flop moz) +>+<+]
::
++  teba                                              ::  install resolved
  |=  a/(quip move part)  ^+  +>
  +>(moz (flop -.a), +>+<+ +.a)
::
++  emit  |=(card %_(+> moz [[ost +<] moz]))          ::  return card
++  emil                                              ::  return cards
  |=  (list card)
  ^+  +>
  ?~(+< +> $(+< t.+<, +> (emit i.+<)))
::
::
++  take-n                                            ::  compute range
  |=  {{index/@u count/@u} get/$-(@u cursor)}
  ^-  (list ship)
  ?~  count  ~
  %+  biff  p:(get index)
  |=  a/ship  ^-  (list ship)
  [a ^$(index +(index), count (dec count))]
::
++  available                                         ::  enumerate free ships
  |=  all/(map ship (managed))  ^-  $-(@u cursor)
  =+  pur=(sort (turn (unsplit all) head) lth)
  =+  len=(lent pur)
  |=(a/@u ?:((gte a len) [~ (sub a len)] [(some (snag a pur)) a]))
::
:: foil cursor to ship cursor, using sized parent
++  prefix
  |=  {a/bloq b/@p {c/(unit @u) d/@u}}  ^-  cursor
  ?~  c  [c d]
  [(some (rep a b u.c ~)) d]
::
++  in-list                                           ::  distribute among options
  |*  {a/(list) b/@u}  ^+  [(snag *@ a) b]
  =+  c=(lent a)
  [(snag (mod b c) a) (div b c)]
::
++  ames-last-seen                                    ::  last succesful ping
  |=  a/ship  ~+  ^-  (unit time)
  ?:  =(a our)  (some now)
  %-  (hard (unit time))
  ~|  ames-look+/(scot %p our)/tell/(scot %da now)/(scot %p a)
  %+  ames-grab  %rue
  .^(ames-tell %a /(scot %p our)/tell/(scot %da now)/(scot %p a))
::
++  neighboured                                        ::  filter for connectivity
  |*  a/(list {ship *})  ^+  a
  %+  skim  a
  |=  {b/ship *}
  ?=(^ (ames-last-seen b))
::
++  shop-galaxies  (available galaxies.office)        ::  unassigned %czar
::
::  Stars can be either whole or children of galaxies
++  shop-stars                                        ::  unassigned %king
  |=  nth/@u  ^-  cursor
  =^  out  nth  %.(nth (available stars.office))
  ?^  out  [out nth]
  %+  shop-star   nth
  (neighboured (issuing galaxies.office))
::
++  shop-star                                         ::  star from galaxies
  |=  {nth/@u lax/(list {who/@p * * r/(foil star)})}  ^-  cursor
  ?:  =(~ lax)  [~ nth]
  =^  sel  nth  (in-list lax nth)
  (prefix 3 who.sel (~(nth fo r.sel) nth))
::
++  shop-planets                                      ::  unassigned %duke
  |=  nth/@u  ^-  cursor
  =^  out  nth  %.(nth (available planets.office))
  ?^  out  [out nth]
  =^  out  nth
    %+  shop-planet   nth
    (neighboured (issuing stars.office))
  ?^  out  [out nth]
  (shop-planet-gal nth (issuing galaxies.office))
::
++  shop-planet                                       ::  planet from stars
  |=  {nth/@u sta/(list {who/@p * q/(foil planet)})}  ^-  cursor
  ?:  =(~ sta)  [~ nth]
  =^  sel  nth  (in-list sta nth)
  (prefix 4 who.sel (~(nth fo q.sel) nth))
::
++  shop-planet-gal                                   ::  planet from galaxies
  |=  {nth/@u lax/(list {who/@p * * r/(foil star)})}  ^-  cursor
  ?:  =(~ lax)  [~ nth]
  =^  sel  nth  (in-list lax nth)
  %+  shop-planet   nth
  (neighboured (issuing-under 3 who.sel box.r.sel))
::
++  peek-x-shop                                       ::  available ships
  |=  tyl/path  ^-  (unit (unit {$ships (list @p)}))
  =;  a   ~&  peek-x-shop+[tyl a]  a
  =;  res  (some (some [%ships res]))
  =+  [typ nth]=~|(bad-path+tyl (raid tyl typ=%tas nth=%ud ~))
  :: =.  nth  (mul 3 nth)
  ?+  typ  ~|(bad-type+typ !!)
    $galaxies  (take-n [nth 3] shop-galaxies)
    $planets   (take-n [nth 3] shop-planets)
    $stars     (take-n [nth 3] shop-stars)
  ==
::
++  get-managed-galaxy  ~(got by galaxies.office)     ::  office read
++  mod-managed-galaxy                                ::  office write
  |=  {who/@p mod/$-(galaxy galaxy)}  ^+  +>
  =+  gal=(mod (get-managed-galaxy who))
  +>.$(galaxies.office (~(put by galaxies.office) who gal))
::
++  get-managed-star                                  ::  office read
  |=  who/@p  ^-  star
  =+  (~(get by stars.office) who)
  ?^  -  u
  =+  gal=(get-managed-galaxy (sein who))
  ?.  ?=({$~ $& *} gal)  ~|(unavailable-star+(sein who) !!)
  (fall (~(get by box.r.p.u.gal) (neis who)) ~)
::
++  mod-managed-star                                  ::  office write
  |=  {who/@p mod/$-(star star)}  ^+  +>
  =+  sta=(mod (get-managed-star who))                ::  XX double traverse
  ?:  (~(has by stars.office) who)
    +>.$(stars.office (~(put by stars.office) who sta))
  %+  mod-managed-galaxy  (sein who)
  |=  gal/galaxy  ^-  galaxy
  ?>  ?=({$~ $& *} gal)
  gal(r.p.u (~(put fo r.p.u.gal) (neis who) sta))
::
++  get-managed-planet                                ::  office read
  |=  who/@p  ^-  planet
  =+  (~(get by planets.office) who)
  ?^  -  u
  ?:  (~(has by galaxies.office) (sein who))
    =+  gal=(get-managed-galaxy (sein who))
    ?.  ?=({$~ $& *} gal)  ~|(unavailable-galaxy+(sein who) !!)
    (~(get fo q.p.u.gal) who)
  =+  sta=(get-managed-star (sein who))
  ?.  ?=({$~ $& *} sta)  ~|(unavailable-star+(sein who) !!)
  (~(get fo q.p.u.sta) who)
::
++  mod-managed-planet                                ::  office write
  |=  {who/@p mod/$-(planet planet)}  ^+  +>
  =+  pla=(mod (get-managed-planet who))              ::  XX double traverse
  ?:  (~(has by planets.office) who)
    +>.$(planets.office (~(put by planets.office) who pla))
  ?:  (~(has by galaxies.office) (sein who))
    %+  mod-managed-galaxy  (sein who)
    |=  gal/galaxy  ^-  galaxy
    ?>  ?=({$~ $& *} gal)
    gal(q.p.u (~(put fo q.p.u.gal) (neis who) pla))
  %+  mod-managed-star  (sein who)
  |=  sta/star  ^-  star
  ?>  ?=({$~ $& *} sta)
  sta(q.p.u (~(put fo q.p.u.sta) (neis who) pla))
::
++  get-live                                          ::  last-heard time ++live
  |=  a/ship  ^-  live
  =+  rue=(ames-last-seen a)
  ?~  rue  %cold
  ?:((gth (sub now u.rue) ~m5) %seen %live)
::
++  stat-any                                          ::  unsplit status
  |=  {who/@p man/(managed _!!)}  ^-  stat
  :-  (get-live who)
  ?~  man  [%free ~]
  ?:  stat-no-email  [%owned '']
  [%owned p.u.man]
::
++  stat-planet                                       ::  stat of planet
  |=  {who/@p man/planet}  ^-  stat
  ?.  ?=({$~ $& ^} man)  (stat-any who man)
  :-  (get-live who)
  =+  pla=u:(divided man)
  :-  %split
  %-  malt
  %+  turn  ~(tap by box.p.pla)
  |=({a/@u b/moon} =+((rep 5 who a ~) [- (stat-any - b)]))
::
++  stat-star                                         ::  stat of star
  |=  {who/@p man/star}  ^-  stat
  ?.  ?=({$~ $& ^} man)  (stat-any who man)
  :-  (get-live who)
  =+  sta=u:(divided man)
  :-  %split
  %-  malt
  %+  welp
    %+  turn  ~(tap by box.p.sta)
    |=({a/@u b/moon} =+((rep 5 who a ~) [- (stat-any - b)]))
  %+  turn  ~(tap by box.q.sta)
  |=({a/@u b/planet} =+((rep 4 who a ~) [- (stat-planet - b)]))
::
++  stat-galaxy                                       :: stat of galaxy
  |=  {who/@p man/galaxy}  ^-  stat
  ?.  ?=({$~ $& ^} man)  (stat-any who man)
  =+  gal=u:(divided man)
  :-  (get-live who)
  :-  %split
  %-  malt
  ;:  welp
    %+  turn  ~(tap by box.p.gal)
    |=({a/@u b/moon} =+((rep 5 who a ~) [- (stat-any - b)]))
  ::
    %+  turn  ~(tap by box.q.gal)
    |=({a/@u b/planet} =+((rep 4 who a ~) [- (stat-planet - b)]))
  ::
    %+  turn  ~(tap by box.r.gal)
    |=({a/@u b/star} =+((rep 3 who a ~) [- (stat-star - b)]))
  ==
::
++  stats-ship                                        ::  inspect ship
  |=  who/@p  ^-  stat
  ?-  (clan who)
    $pawn  !!
    $earl  !!
    $duke  (stat-planet who (get-managed-planet who))
    $king  (stat-star who (get-managed-star who))
    $czar  (stat-galaxy who (get-managed-galaxy who))
  ==
::
++  peek-x-stats                                      ::  inspect ship/system
  |=  tyl/path
  ?^  tyl
    ?>  |(=(our src) =([~ src] boss))                   ::  privileged info
    ``womb-stat+(stats-ship ~|(bad-path+tyl (raid tyl who=%p ~)))
  ^-  (unit (unit {$womb-stat-all (map ship stat)}))
  =.  stat-no-email  &                      ::  censor adresses
  :^  ~  ~  %womb-stat-all
  %-  ~(uni by (~(urn by planets.office) stat-planet))
  %-  ~(uni by (~(urn by stars.office) stat-star))
  (~(urn by galaxies.office) stat-galaxy)
::
++  peek-x-balance                                     ::  inspect invitation
  |=  tyl/path
  ?~  tyl
    ?>  |(=(our src) =([~ src] boss))                  ::  priveledged
    ``[%womb-balance-all (~(run by bureau) |=(balance owner))]
  ^-  (unit (unit {$womb-balance balance}))
  =+  pas=~|(bad-path+tyl (raid tyl pas=%uv ~))
  %-  some
  %+  bind  (~(get by bureau) (shaf %pass pas))
  |=(bal/balance [%womb-balance bal])
::
:: ++  old-phon    ;~(pfix sig fed:ag:hoon151)  :: library
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
  ?:  (~(has by bureau) (shaf %pass pas))  %used
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
  ::  /balance                         all invitations
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
++  poke-manage                                       ::  add to property
  |=  a/(list ship)
  =<  abet
  ?>  |(=(our src) =([~ src] boss))                   ::  privileged
  |-
  ?~  a  .
  ?+      (clan i.a)  ~|(bad-size+(clan i.a) !!)
        $duke
    ?.  (~(has by planets.office) i.a)
      $(a t.a, planets.office (~(put by planets.office) i.a ~))
    ~|(already-managing+i.a !!)
  ::
        $king
    ?.  (~(has by stars.office) i.a)
      $(a t.a, stars.office (~(put by stars.office) i.a ~))
    ~|(already-managing+i.a !!)
  ::
        $czar
    ?.  (~(has by galaxies.office) i.a)
      $(a t.a, galaxies.office (~(put by galaxies.office) i.a ~))
    ~|(already-managing+i.a !!)
  ==
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
      $release   (teba (poke-release +.pok.i.a))
      $recycle   (teba (poke-recycle +.pok.i.a))
      $reinvite  (teba (poke-reinvite +.pok.i.a))
      $release-ships  (teba (poke-release-ships +.pok.i.a))
    ==
  ==
::
++  poke-bonus                                        ::  expand invitation
  |=  {tid/cord pla/@ud sta/@ud}
  =<  abet
  =.  log-transaction  (log-transaction %bonus +<)
  ?>  |(=(our src) =([~ src] boss))                   ::  priveledged
  =/  pas  ~|(bad-invite+tid `passcode`(slav %uv tid))
  %_    .
      bureau
    %+  ~(put by bureau)  (shaf %pass pas)
    =/  bal  ~|(%bad-passcode (~(got by bureau) (shaf %pass pas)))
    bal(planets (add pla planets.bal), stars (add sta stars.bal))
  ==
::
++  poke-invite                                       ::  create invitation
  |=  {tid/cord ref/reference inv/invite}
  =<  abet
  =.  log-transaction  (log-transaction %invite +<)
  =.  hotel
    ?~  ref  hotel
    ?~  sta.inv  hotel
    %+  ~(put by hotel)  u.ref
    =+  cli=(fall (~(get by hotel) u.ref) *client)
    cli(sta +(sta.cli))
  (invite-from ~ tid inv)
::
++  invite-from                                       ::  traced invitation
  |=  {hiz/(list mail) tid/cord inv/invite}  ^+  +>
  ?>  |(=(our src) =([~ src] boss))                   ::  priveledged
  =+  pas=~|(bad-invite+tid `passcode`(slav %uv tid))
  ?:  (~(has by bureau) (shaf %pass pas))
    ~|([%duplicate-passcode pas who.inv replay=replay] !!)
  =.  bureau  (~(put by bureau) (shaf %pass pas) [pla.inv sta.inv who.inv hiz])
  (email /invite who.inv intro.wel.inv)
::
:: ++  coup-invite                                      ::  invite sent
::
++  poke-reinvite                                     ::  split invitation
  |=  {aut/passcode new/passcode inv/invite}                       ::  further invite
  =<  abet
  =.  log-transaction  (log-transaction %reinvite +<)
  ?>  =(src src)                                      ::  self-authenticated
  =+  ~|(%bad-passcode bal=(~(got by bureau) (shaf %pass aut)))
  =.  stars.bal  (sub stars.bal sta.inv)
  =.  planets.bal  (sub planets.bal pla.inv)
  =.  bureau  (~(put by bureau) (shaf %pass aut) bal)
  =+  tid=(scot %uv new)
  (invite-from [owner.bal history.bal] (scot %uv new) inv)
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
++  use-reference                                     ::  bonus stars
  |=  a/(each @p mail)  ^-  (unit _+>)
  ?.  (~(has by hotel) a)  ~
  =+  cli=(~(get by hotel) a)
  ?~  cli  ~
  ?.  (gte sta.u.cli reference-rate)  ~
  =.  sta.u.cli  (sub sta.u.cli reference-rate)
  `+>.$(hotel (~(put by hotel) a u.cli))
::
++  poke-do-ticket                                       ::  issue child ticket
  |=  her/ship
  =<  abet
  ?>  =(our (sein her))
  ?>  |(=(our src) =([~ src] boss))                   ::  privileged
  =+  tik=.^(@p %a /(scot %p our)/tick/(scot %da now)/(scot %p her))
  :: =.  emit  (emit /tick %tick tik her)
  (emit %poke /womb/tick [src %hood] [%womb-do-claim her tik]) :: XX peek result
::
++  needy
  |*  a/(each * tang)
  ?-  -.a
    $&  p.a
    $|  ((slog (flop p.a)) (mean p.a))
  ==
::
++  poke-do-claim                                     ::  deliver ticket
  |=  {her/ship tik/@p}
  =<  abet
  ^+  +>
  ?>  =(src (sein her))               ::  from the parent which could ticket
  =+  sta=(stats-ship her)
  ?>  ?=($cold p.sta)                 ::  a ship not yet started
  ?-    -.q.sta
      $free  !!                         ::  but allocated
      $owned                            ::  to an email
    (email /ticket p.q.sta "Ticket for {<her>}: {<`@pG`tik>}")
  ::
      $split                            ::  or ship distribution
    %.(+>.$ (slog leaf+"Ticket for {<her>}: {<`@pG`tik>}" ~))  ::  XX emit via console formally?
  ==
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
  ?:  (~(has by bureau) (shaf %pass pas))
    ~|(already-recycled+[him-t tik-t] !!)
  =+  bal=`balance`?+((clan him) !! $duke [1 0 who ~], $king [0 1 who ~])
  .(bureau (~(put by bureau) (shaf %pass pas) bal))
::
++  poke-claim                                        ::  claim plot, req ticket
  |=  {aut/passcode her/@p}
  ?.  can-claim.cfg  ~|(%ticketing-offline !!)
  =<  abet
  =.  log-transaction  (log-transaction %claim +<)
  ?>  =(src src)
  (claim-any aut her)
::
++  claim-any                                        ::  register
  |=  {aut/passcode her/@p}
  =;  claimed
    :: =.  claimed  (emit.claimed %wait $~)          :: XX delay ack
    (emit.claimed %poke /womb/tick [(sein her) %hood] [%womb-do-ticket her])
  =+  ~|(%bad-passcode bal=(~(got by bureau) (shaf %pass aut)))
  ?+    (clan her)  ~|(bad-size+(clan her) !!)
      $king
    =;  all  (claim-star.all owner.bal her)
    =+  (use-reference &+src)
    ?^  -  u   :: prefer using references
    =+  (use-reference |+owner.bal)
    ?^  -  u
    =.  stars.bal  ~|(%no-stars (dec stars.bal))
    +>.$(bureau (~(put by bureau) (shaf %pass aut) bal))
  ::
      $duke
    =.  planets.bal  ~|(%no-planets (dec planets.bal))
    =.  bureau  (~(put by bureau) (shaf %pass aut) bal)
    (claim-planet owner.bal her)
  ==
::
++  claim-star                                        ::  register
  |=  {who/mail her/@p}  ^+  +>
  %+  mod-managed-star  her
  |=  a/star  ^-  star
  ?^  a  ~|(impure-star+[her ?:(-.u.a %owned %split)] !!)
  (some %| who)
::
++  claim-planet                                      ::  register
  |=  {who/mail her/@p}  ^+  +>
  =.  hotel
    %+  ~(put by hotel)  |+who
    =+  cli=(fall (~(get by hotel) |+who) *client)
    cli(has (~(put in has.cli) her))
  %+  mod-managed-planet  her
  |=  a/planet  ^-  planet
  ?^  a  ~|(impure-planet+[her ?:(-.u.a %owned %split)] !!)
  (some %| who)
::
++  poke-release-ships                                ::  release specific
  |=  a/(list ship)
  =<  abet  ^+  +>
  =.  log-transaction  (log-transaction %release-ships +<)
  ?>  =(our src)                                      ::  privileged
  %+  roll  a
  =+  [who=*@p res=+>.$]
  |.  ^+  res
  ?+  (clan who)  ~|(bad-size+(clan who) !!)
    $king  (release-star who res)
    $czar  (release-galaxy who res)
  ==
::
++  poke-release                                      ::  release to subdivide
  |=  {gal/@ud sta/@ud}                               ::
  =<  abet  ^+  +>
  =.  log-transaction  (log-transaction %release +<)
  ?>  =(our src)                                      ::  privileged
  =.  +>
    ?~  gal  +>
    =+  all=(take-n [0 gal] shop-galaxies)
    ?.  (gth gal (lent all))
      (roll all release-galaxy)
    ~|(too-few-galaxies+[want=gal has=(lent all)] !!)
  ^+  +>
  ?~  sta  +>
  =+  all=(take-n [0 sta] shop-stars)
  ~&  got-stars+all
  %-  (slog leaf+"For issuing to proceed smoothly, immediately upon boot, ".
                 "each should |obey {<our>} to honor ticket requests." ~)
  ?.  (gth sta (lent all))
    (roll all release-star)
  ~|(too-few-stars+[want=sta has=(lent all)] !!)
::
++  release-galaxy                                    ::  subdivide %czar
  =+  [who=*@p res=.]
  |.  ^+  res
  %+  mod-managed-galaxy:res  who
  |=  gal/galaxy  ^-  galaxy
  ~&  release+who
  ?^  gal  ~|(already-used+who !!)
  (some %& (fo-init 5) (fo-init 4) (fo-init 3))
::
++  release-star                                      ::  subdivide %king
  =+  [who=*@p res=.]
  |.  ^+  res
  =.  res  
      %-  emit.res
      [%poke /womb/tick [(sein who) %hood] [%womb-do-ticket who]]
  %+  mod-managed-star:res  who
  |=  sta/star  ^-  star
  ~&  release+who
  ?^  sta  ~|(already-used+[who u.sta] !!)
  (some %& (fo-init 5) (fo-init 4))
--
