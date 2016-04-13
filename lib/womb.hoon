::                                                      ::  ::
::::  /hoon/womb/lib                                    ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/+    talk
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  foil                                                ::  ship allocation map
  |*  mold                                              ::  entry mold
  $:  min/@u                                            ::  minimum entry
      ctr/@u                                            ::  next allocated
      und/(set @u)                                      ::  free under counter
      ove/(set @u)                                      ::  alloc over counter
      max/@u                                            ::  maximum entry
      box/(map @u +<)                                   ::  entries
  ==                                                    ::
--                                                      ::
::   $:  min/@p
::       mid/@p
::       max/@p
::       box/(map @p (each mail ))
::   ==
  
:: |%
:: ++  ships  (list {min/@p mid/@p max/@p ish/(map @p state)})
:: ++  state  ?($given $split {$~ mail})

  
:: ++  
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
++  passcode  @pG                                       ::  64-bit passcode
++  mail  @ta                                           ::  email address
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
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  part  {$womb $0 pith}                               ::  womb state
++  pith                                                ::  womb content
  $:  boss/(unit @p)                                    ::  outside master
      bureau/(map passcode balance)                     ::  active invitations
      office/property                                   ::  properties managed
      hotel/(map mail client)                           ::  everyone we know
  ==                                                    ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  arvo structures
++  card                                                ::
  $%  {$flog wire flog}                                 ::
      ::{$wait $~}
      :: {$poke
      {$next p/ring}                                    ::  update private key
      {$tick p/@pG q/@p}                                ::  save ticket
  ==                                                    ::
++  move  (pair bone card)                              ::  user-level move
--
|%
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
++  unsplit
  |=  a/(map ship (managed))  ^-  (list {ship *})
  %+  skim  (~(tap by a))
  |=({@ a/(managed)} ?=($~ a))
::
++  issuing
  |*  a/(map ship (managed))
  ^-  (list {ship _(need (divided *~(got by a)))})
  (sort (~(tap by (murn-by a divided))) lor)
::
++  issuing-under
  |*  {a/bloq b/ship c/(map @u (managed))}
  ^-  (list {ship _(need (divided *~(got by c)))})
  %+  turn  (sort (~(tap by (murn-by c divided))) lor)
  |*(d/{@u *} [(rep a b -.d ~) +.d])
::
++  cursor  (pair (unit ship) @u)
++  neis  |=(a/ship ^-(@u (rsh (dec (xeb (xeb a))) 1 a)))  ::  postfix
::
::  Create new foil of size
++  fo-init
  |=  a/bloq  ::  ^-  (foil *)
  [min=1 ctr=1 und=~ ove=~ max=(dec (bex (bex a))) box=~]
::
++  fo
  |_  (foil)
  ++  get                                             ::  nth
    |=  a/@u  ^-  (pair (unit @u) @u)
    ?:  (lth a ~(wyt in und))
      =+  out=(snag a (sort (~(tap in und)) lth))
      [(some out) 0]
    =.  a  (sub a ~(wyt in und))
    |-  ^-  {(unit @u) @u}
    ?:  =(ctr +(max))  [~ a]
    ?:  =(0 a)  [(some ctr) a]
    $(a (dec a), +<.get new)
  ::
  +-  fin  +<                                         ::  abet
  ++  new                                             ::  alloc
    ?:  =(ctr +(max))  +<
    =.  ctr  +(ctr)
    ?.  (~(has in ove) ctr)  +<
    new(ove (~(del in ove) ctr))
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
  !:                                                  ::  ::
|=  {bowl part}                                       ::  main womb work
|_  moz/(list move)
++  abet                                              ::  resolve
  ^-  (quip move *part)
  [(flop moz) +>+<+]
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
::
++  shop-galaxies  (available galaxies.office)        ::  unassigned %czar
::
::  Stars can be either whole or children of galaxies
++  shop-stars                                        ::  unassigned %king
  |=  nth/@u  ^-  cursor
  =^  out  nth  %.(nth (available stars.office))
  ?^  out  [out nth]
  (shop-star nth (issuing galaxies.office))
::
++  shop-star                                         ::  star from galaxies
  |=  {nth/@u lax/(list {who/@p * * r/(foil star)})}  ^-  cursor
  ?:  =(~ lax)  [~ nth]
  =^  sel  nth  (in-list lax nth)
  (prefix 3 who.sel (~(get fo r.sel) nth))
::
++  shop-planets                                      ::  unassigned %duke
  |=  nth/@u  ^-  cursor
  =^  out  nth  %.(nth (available planets.office))
  ?^  out  [out nth]
  =^  out  nth  (shop-planet nth (issuing stars.office))
  ?^  out  [out nth]
  (shop-planet-gal nth (issuing galaxies.office))
::
++  shop-planet                                       ::  planet from stars
  |=  {nth/@u sta/(list {who/@p * q/(foil planet)})}  ^-  cursor
  ?:  =(~ sta)  [~ nth]
  =^  sel  nth  (in-list sta nth)
  (prefix 4 who.sel (~(get fo q.sel) nth))
::
++  shop-planet-gal                                   ::  planet from galaxies
  |=  {nth/@u lax/(list {who/@p * * r/(foil star)})}  ^-  cursor
  ?:  =(~ lax)  [~ nth]
  =^  sel  nth  (in-list lax nth)
  (shop-planet nth (issuing-under 3 who.sel box.r.sel))
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
  ?.  ?=({$~ $& *} gal)  ~|(unavailable-galaxy+(sein who) !!)
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
    (fall (~(get by box.q.p.u.gal) (neis who)) ~)
  =+  sta=(get-managed-star (sein who))
  ?.  ?=({$~ $& *} sta)  ~|(unavailable-star+(sein who) !!)
  (fall (~(get by box.q.p.u.sta) (neis who)) ~)
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
++  stats-ship                                        ::  inspect ship
  |=  who/@p  ^-  (unit (unit (cask _!!)))
  ~
::
++  peek-x-stats                                      ::  inspect ship/system
  |=  tyl/path
  ?^  tyl
    (stats-ship ~|(bad-path+tyl (raid tyl who=%p ~)))
  ^-  (unit (unit (cask _!!)))
  ~
::
++  peek-x-invite                                     ::  inspect invitation
  |=  tyl/path  ^-  (unit (unit {$womb-balance balance}))
  =+  pas=~|(bad-path+tyl (raid tyl pas=%p ~))
  %-  some
  %+  bind  (~(get by bureau) pas)
  |=(bal/balance [%womb-balance bal])
::
++  peek-x
  |=  tyl/path  ::  ^-  (unit (unit (pair mark *)))
  ~&  peek-x+tyl
  ?~  tyl  ~
  ?+  -.tyl  ~
  ::  /shop/planets/@ud   (list @p)    up to 3 planets
  ::  /shop/stars/@ud     (list @p)    up to 3 stars
  ::  /shop/galaxies/@ud  (list @p)    up to 3 galaxies 
    $shop  (peek-x-shop +.tyl)
  ::  /stats                          general stats dump
  ::  /stats/@p                       what we know about @p
    $stats  (peek-x-stats +.tyl)
  ::  /invite/passcode                invitation status  
    $invite  (peek-x-invite +.tyl)
  ==
::
++  poke-invite                                       ::  create invitation
  |=  {ref/reference inv/invite}
  =<  abet
  ?>  |(=(our src) =([~ src] boss))                   ::  me or boss
  .
::
++  poke-obey                                         ::  set/reset boss
  |=  who/(unit @p)
  =<  abet
  ?>  =(our src)                                      ::  me only
  .
::
++  poke-rekey                                        ::  extend will
  |=  $~
  =<  abet
  ?>  |(=(our src) =([~ src] boss))                   ::  privileged
  .
::
++  poke-report                                       ::  report will
  |=  {her/@p wyl/will}                               ::
  =<  abet
  ?>  =(src src)                                      ::  self-authenticated
  .
::
++  poke-claim                                        ::  claim plot, send ticket
  |=  {aut/@uvH her/@p}                               ::
  =<  abet
  ?>  =(src src)
  .
::
++  poke-release                                      ::  release to subdivide
  |=  {gal/@ud sta/@ud}                               ::
  =<  abet  ^+  +>
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
  %+  mod-managed-star:res  who
  |=  sta/star  ^-  star
  ~&  release+who
  ?^  sta  ~|(already-used+[who u.sta] !!)
  (some %& (fo-init 4) (fo-init 3))
::
++  poke-reinvite                                     ::  split invitation
  |=  $:  aut/@uvH                                    ::  hash w/passcode
          inv/invite                                  ::  further invite
      ==
  ?>  =(src src)                                      ::
  =<  abet
  .
--
