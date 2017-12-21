!:                                                      ::  /van/jael
::                                                      ::  %reference/0
^%
!?  150
::
::
::  %jael: secrets and promises.
::
::  todo:
::
::    - communication with other vanes:
::      - actually use %behn for expiring secrets
::      - report %ames propagation errors to user
::
::    - nice features:
::      - scry namespace
::      - task for converting invites to tickets
::
|=  pit/vase
=,  pki:jael
=,  rights:jael
=,  able:jael 
=,  title
=,  crypto
=,  jael
::                                                      ::::
::::                    # models                        ::  data structures
  ::                                                    ::::
::  the %jael state comes in two parts: absolute
::  and relative.
::
::  ++state-absolute is objective -- defined without
::  reference to our ship.  if you steal someone else's
::  private keys, we have a place to put them.  when
::  others make promises to us, we store them in the
::  same structures we use to make promises to others.
::
::  ++state-relative is subjective, denormalized and
::  derived.  it consists of all the state we need to
::  manage subscriptions efficiently.
::
=>  |%
++  state                                               ::  all vane state
  $:  ver/$0                                            ::  vane version
      yen/(set duct)                                    ::  raw observers
      urb/state-absolute                                ::  all absolute state
      sub/state-relative                                ::  all relative state
  ==                                                    ::
++  state-relative                                      ::  urbit metadata
  $:  $=  car                                           ::  secure channels
        %+  map  ship                                   ::  partner
        $:  yen/(set duct)                              ::  trackers
            det/channel                                 ::  channel state
        ==                                              ::
      $=  rel                                           ::  neighborhood
        $:  dad/_our                                    ::  parent
            cod/farm                                    ::  dependencies
            pyr/(set ship)                              ::  peers
            kyz/(set ship)                              ::  children
        ==                                              ::
      $=  bal                                           ::  balance sheet
        $:  yen/(set duct)                              ::  trackers
        ==                                              ::
      $=  own                                           ::  vault
        $:  yen/(set duct)                              ::  trackers
            lyf/life                                    ::  version
            jaw/(map life ring)                         ::  private keys
        ==                                              ::
  ==                                                    ::
++  state-absolute                                      ::  absolute urbit
  $:  pug/farm                                          ::  keys
      pry/(map ship (map ship safe))                    ::  promises
  ==                                                    ::
::                                                      ::
++  message                                             ::  p2p message
  $%  {$hail p/safe}                                    ::  reset rights
      {$meet p/farm}                                    ::  propagate pki
  ==                                                    ::
++  card                                                ::  i/o action
  (wind note gift)                                      ::
::                                                      ::
++  move                                                ::  output
  {p/duct q/card}                                       ::
--  ::
::                                                      ::::
::::                    # data                          ::  static data
  ::                                                    ::::
=>  |%
::                                                      ::  ++zeno
++  zeno                                                ::  boot fingerprints
  ::
  ::  in ++zeno we hardcode the fingerprints of galaxies
  ::  and the identities of their owners.  if the
  ::  fingerprint is 0, the galaxy can't be created.
  ::
  ::  we'll probably move at least the identity data
  ::  into urbit as it becomes more stable, but keeping
  ::  it in the source makes it very resilient.
  ::
  |=  who/ship
  ^-  @
  %+  snag  who
  ^-  (list @uw)
  :~  0w0                           ::    0, ~zod, urbit.org
      0w0                           ::    1, ~nec, Curtis Yarvin
      0w0                           ::    2, ~bud, Tlon Investor 1
      0w0                           ::    3, ~wes, Tlon Investor 2
      0w0                           ::    4, ~sev, Tlon Investor 2
      0w0                           ::    5, ~per, Tlon Investor 3
      0w0                           ::    6, ~sut, Tlon Investor 4
      0w0                           ::    7, ~let, Tlon Investor 4
      0w0                           ::    8, ~ful, Tlon Investor 4
      0w0                           ::    9, ~pen, Tlon Investor 4
      0w0                           ::   10, ~syt, Tlon Investor 4
      0w0                           ::   11, ~dur, Tlon Investor 4
      0w0                           ::   12, ~wep, Sam Putman
      0w0                           ::   13, ~ser, Tlon Investor 5
      0w0                           ::   14, ~wyl, Zimran Ahmed
      0w0                           ::   15, ~sun, Colin Smith
      0w0                           ::   16, ~ryp, Tlon Investor 6
      0w0                           ::   17, ~syx, Tlon Investor 6
      0w0                           ::   18, ~dyr, Tlon Investor 6
      0w0                           ::   19, ~nup, Tlon Investor 6
      0w0                           ::   20, ~heb, Tlon Investor 6
      0w0                           ::   21, ~peg, Tlon Investor 6
      0w0                           ::   22, ~lup, Tlon Investor 6
      0w0                           ::   23, ~dep, Tlon Investor 6
      0w0                           ::   24, ~dys, Mike Gogulski
      0w0                           ::   25, ~put, Tlon Investor 7
      0w0                           ::   26, ~lug, Tlon Investor 8
      0w0                           ::   27, ~hec, Tlon Investor 8
      0w0                           ::   28, ~ryt, Tlon Investor 8
      0w0                           ::   29, ~tyv, Tlon Investor 8
      0w0                           ::   30, ~syd, Curtis Yarvin
      0w0                           ::   31, ~nex, Prakhar Goel
      0w0                           ::   32, ~lun, Tlon Investor 9
      0w0                           ::   33, ~mep, Tlon Investor 9
      0w0                           ::   34, ~lut, Tlon Investor 9
      0w0                           ::   35, ~sep, Tlon Investor 9
      0w0                           ::   36, ~pes, Curtis Yarvin
      0w0                           ::   37, ~del, Kingdon Barrett
      0w0                           ::   38, ~sul, John Burnham
      0w0                           ::   39, ~ped, Jeremy Wall
      0w0                           ::   40, ~tem, Tlon Investor 10
      0w0                           ::   41, ~led, Nick Caruso
      0w0                           ::   42, ~tul, Curtis Yarvin
      0w0                           ::   43, ~met, Curtis Yarvin
      0w0                           ::   44, ~wen, Curtis Yarvin
      0w0                           ::   45, ~byn, Curtis Yarvin
      0w0                           ::   46, ~hex, James Torre
      0w0                           ::   47, ~feb, urbit.org
      0w0                           ::   48, ~pyl, Michael Hartl
      0w0                           ::   49, ~dul, Galen Wolfe-Pauly
      0w0                           ::   50, ~het, Galen Wolfe-Pauly
      0w0                           ::   51, ~mev, Curtis Yarvin
      0w0                           ::   52, ~rut, Curtis Yarvin
      0w0                           ::   53, ~tyl, Tlon Investor 11
      0w0                           ::   54, ~wyd, Curtis Yarvin
      0w0                           ::   55, ~tep, Curtis Yarvin
      0w0                           ::   56, ~bes, Curtis Yarvin
      0w0                           ::   57, ~dex, Jared Hance
      0w0                           ::   58, ~sef, Owen Rescher
      0w0                           ::   59, ~wyc, Galen Wolfe-Pauly
      0w0                           ::   60, ~bur, Galen Wolfe-Pauly
      0w0                           ::   61, ~der, Galen Wolfe-Pauly
      0w0                           ::   62, ~nep, Galen Wolfe-Pauly
      0w0                           ::   63, ~pur, Paul Driver
      0w0                           ::   64, ~rys, Charlie Cummings
      0w0                           ::   65, ~reb, Curtis Yarvin
      0w0                           ::   66, ~den, Michael Hartl
      0w0                           ::   67, ~nut, Curtis Yarvin
      0w0                           ::   68, ~sub, Curtis Yarvin
      0w0                           ::   69, ~pet, Curtis Yarvin
      0w0                           ::   70, ~rul, Curtis Yarvin
      0w0                           ::   71, ~syn, Pantera
      0w0                           ::   72, ~reg, Henry Ault
      0w0                           ::   73, ~tyd, Henry Ault
      0w0                           ::   74, ~sup, Henry Ault
      0w0                           ::   75, ~sem, Michael Livshin
      0w0                           ::   76, ~wyn, Anton Dyudin
      0w0                           ::   77, ~rec, Anton Dyudin
      0w0                           ::   78, ~meg, Anton Dyudin
      0w0                           ::   79, ~net, Anthony Martinez
      0w0                           ::   80, ~sec, Curtis Yarvin
      0w0                           ::   81, ~mul, Curtis Yarvin
      0w0                           ::   82, ~nym, Max Greer
      0w0                           ::   83, ~tev, Curtis Yarvin
      0w0                           ::   84, ~web, Ar Vicco
      0w0                           ::   85, ~sum, Philip Monk
      0w0                           ::   86, ~mut, Philip Monk
      0w0                           ::   87, ~nyx, Philip Monk
      0w0                           ::   88, ~rex, Tlon Investor 12
      0w0                           ::   89, ~teb, Michael Vassar
      0w0                           ::   90, ~fus, Tlon Corporation
      0w0                           ::   91, ~hep, urbit.org
      0w0                           ::   92, ~ben, urbit.org
      0w0                           ::   93, ~mus, urbit.org
      0w0                           ::   94, ~wyx, urbit.org
      0w0                           ::   95, ~sym, urbit.org
      0w0                           ::   96, ~sel, urbit.org
      0w0                           ::   97, ~ruc, urbit.org
      0w0                           ::   98, ~dec, urbit.org
      0w0                           ::   99, ~wex, Pax Dickinson
      0w0                           ::  100, ~syr, urbit.org
      0w0                           ::  101, ~wet, urbit.org
      0w0                           ::  102, ~dyl, urbit.org
      0w0                           ::  103, ~myn, urbit.org
      0w0                           ::  104, ~mes, urbit.org
      0w0                           ::  105, ~det, urbit.org
      0w0                           ::  106, ~bet, urbit.org
      0w0                           ::  107, ~bel, urbit.org
      0w0                           ::  108, ~tux, Tlon Investor 13
      0w0                           ::  109, ~tug, Philip Monk
      0w0                           ::  110, ~myr, urbit.org
      0w0                           ::  111, ~pel, urbit.org
      0w0                           ::  112, ~syp, urbit.org
      0w0                           ::  113, ~ter, urbit.org
      0w0                           ::  114, ~meb, urbit.org
      0w0                           ::  115, ~set, urbit.org
      0w0                           ::  116, ~dut, urbit.org
      0w0                           ::  117, ~deg, urbit.org
      0w0                           ::  118, ~tex, urbit.org
      0w0                           ::  119, ~sur, urbit.org
      0w0                           ::  120, ~fel, urbit.org
      0w0                           ::  121, ~tud, urbit.org
      0w0                           ::  122, ~nux, urbit.org
      0w0                           ::  123, ~rux, urbit.org
      0w0                           ::  124, ~ren, urbit.org
      0w0                           ::  125, ~wyt, urbit.org
      0w0                           ::  126, ~nub, urbit.org
      0w0                           ::  127, ~med, urbit.org
      0w0                           ::  128, ~lyt, Arthur Breitman
      0w0                           ::  129, ~dus, urbit.org
      0w0                           ::  130, ~neb, urbit.org
      0w0                           ::  131, ~rum, urbit.org
      0w0                           ::  132, ~tyn, urbit.org
      0w0                           ::  133, ~seg, urbit.org
      0w0                           ::  134, ~lyx, urbit.org
      0w0                           ::  135, ~pun, urbit.org
      0w0                           ::  136, ~res, urbit.org
      0w0                           ::  137, ~red, Alex Kravets
      0w0                           ::  138, ~fun, Aaron Beckerman
      0w0                           ::  139, ~rev, urbit.org
      0w0                           ::  140, ~ref, Matt Brubeck
      0w0                           ::  141, ~mec, urbit.org
      0w0                           ::  142, ~ted, urbit.org
      0w0                           ::  143, ~rus, Stephen Burnham
      0w0                           ::  144, ~bex, urbit.org
      0w0                           ::  145, ~leb, Justin LeBlanc
      0w0                           ::  146, ~dux, urbit.org
      0w0                           ::  147, ~ryn, urbit.org
      0w0                           ::  148, ~num, Tlon
      0w0                           ::  149, ~pyx, Katherine McFall
      0w0                           ::  150, ~ryg, Dan Haffey
      0w0                           ::  151, ~ryx, Tlon
      0w0                           ::  152, ~fep, Tlon
      0w0                           ::  153, ~tyr, Steve Dee
      0w0                           ::  154, ~tus, Tlon
      0w0                           ::  155, ~tyc, Tlon
      0w0                           ::  156, ~leg, Tlon
      0w0                           ::  157, ~nem, Jeremy Tunnell
      0w0                           ::  158, ~fer, Tlon
      0w0                           ::  159, ~mer, Tlon
      0w0                           ::  160, ~ten, Tlon
      0w0                           ::  161, ~lus, Tlon
      0w0                           ::  162, ~nus, Tlon
      0w0                           ::  163, ~syl, Tlon
      0w0                           ::  164, ~tec, Tlon
      0w0                           ::  165, ~mex, Tlon
      0w0                           ::  166, ~pub, Tlon
      0w0                           ::  167, ~rym, Tlon
      0w0                           ::  168, ~tuc, Tlon
      0w0                           ::  169, ~fyl, Tlon
      0w0                           ::  170, ~lep, Tlon
      0w0                           ::  171, ~deb, Tlon
      0w0                           ::  172, ~ber, Tlon
      0w0                           ::  173, ~mug, Tlon
      0w0                           ::  174, ~hut, Tlon
      0w0                           ::  175, ~tun, Tlon
      0w0                           ::  176, ~byl, Tlon
      0w0                           ::  177, ~sud, Tlon
      0w0                           ::  178, ~pem, Tlon
      0w0                           ::  179, ~dev, Tlon
      0w0                           ::  180, ~lur, Tlon
      0w0                           ::  181, ~def, Tlon
      0w0                           ::  182, ~bus, Tlon
      0w0                           ::  183, ~bep, Tlon
      0w0                           ::  184, ~run, Tlon
      0w0                           ::  185, ~mel, Tlon
      0w0                           ::  186, ~pex, Tlon
      0w0                           ::  187, ~dyt, Tlon
      0w0                           ::  188, ~byt, Tlon
      0w0                           ::  189, ~typ, Tlon
      0w0                           ::  190, ~lev, Tlon
      0w0                           ::  191, ~myl, Tlon
      0w0                           ::  192, ~wed, Tlon
      0w0                           ::  193, ~duc, Tlon
      0w0                           ::  194, ~fur, Tlon
      0w0                           ::  195, ~fex, Tlon
      0w0                           ::  196, ~nul, Matthew Liston
      0w0                           ::  197, ~luc, Tlon
      0w0                           ::  198, ~len, Tlon
      0w0                           ::  199, ~ner, Tlon
      0w0                           ::  200, ~lex, Michael Hartl
      0w0                           ::  201, ~rup, Owen Rescher
      0w0                           ::  202, ~ned, Tlon
      0w0                           ::  203, ~lec, Tlon
      0w0                           ::  204, ~ryd, Tlon
      0w0                           ::  205, ~lyd, Adam Bliss
      0w0                           ::  206, ~fen, Tlon
      0w0                           ::  207, ~wel, Tlon
      0w0                           ::  208, ~nyd, Tlon
      0w0                           ::  209, ~hus, Tlon
      0w0                           ::  210, ~rel, Tlon
      0w0                           ::  211, ~rud, Tlon
      0w0                           ::  212, ~nes, Tlon
      0w0                           ::  213, ~hes, Tlon Investor 14
      0w0                           ::  214, ~fet, Tlon
      0w0                           ::  215, ~des, Tlon
      0w0                           ::  216, ~ret, Tlon
      0w0                           ::  217, ~dun, Tlon
      0w0                           ::  218, ~ler, Tlon
      0w0                           ::  219, ~nyr, Ivan Matosevic
      0w0                           ::  220, ~seb, Tlon
      0w0                           ::  221, ~hul, Tlon
      0w0                           ::  222, ~ryl, Tlon
      0w0                           ::  223, ~lud, Tlon
      0w0                           ::  224, ~rem, Tlon
      0w0                           ::  225, ~lys, Tlon
      0w0                           ::  226, ~fyn, Stephen Burnham
      0w0                           ::  227, ~wer, Tlon
      0w0                           ::  228, ~ryc, Tlon
      0w0                           ::  229, ~sug, Tlon
      0w0                           ::  230, ~nys, Tlon
      0w0                           ::  231, ~nyl, Tlon
      0w0                           ::  232, ~lyn, Tlon
      0w0                           ::  233, ~dyn, Tlon
      0w0                           ::  234, ~dem, Tlon
      0w0                           ::  235, ~lux, Tlon Investor 15
      0w0                           ::  236, ~fed, Iceman
      0w0                           ::  237, ~sed, Tlon
      0w0                           ::  238, ~bec, Tlon
      0w0                           ::  239, ~mun, Tlon
      0w0                           ::  240, ~lyr, Tlon
      0w0                           ::  241, ~tes, Tlon
      0w0                           ::  242, ~mud, Ian Rowan
      0w0                           ::  243, ~nyt, Byrne Hobart
      0w0                           ::  244, ~byr, Tlon
      0w0                           ::  245, ~sen, Tlon
      0w0                           ::  246, ~weg, Tlon
      0w0                           ::  247, ~fyr, Anton Dyudin
      0w0                           ::  248, ~mur, Tlon
      0w0                           ::  249, ~tel, Tlon
      0w0                           ::  250, ~rep, Raymond Pasco
      0w0                           ::  251, ~teg, Tlon
      0w0                           ::  252, ~pec, Tlon
      0w0                           ::  253, ~nel, Tlon
      0w0                           ::  254, ~nev, Tlon
      0w0                           ::  255, ~fes, John Burnham
  ==
--  ::
::                                                      ::::
::::                    # light                         ::  light cores
  ::                                                    ::::
=>  |%
::                                                      ::  ++py
::::                    ## sparse/light                 ::  sparse range
  ::                                                    ::::
++  py
  ::  because when you're a star with 2^16 unissued
  ::  planets, a (set) is kind of lame...
  ::
  |_  a/pile
  ::                                                    ::  ++dif:py
  ++  dif                                               ::  add/remove a->b
    |=  b/pile
    ^-  (pair pile pile)
    [(sub(a b) a) (sub b)]
  ::                                                    ::  ++div:py
  ++  div                                               ::  allocate
    |=  b/@ud
    ^-  (unit (pair pile pile))
    =<  ?-(- $& [~ p], $| ~)
    |-  ^-  (each (pair pile pile) @u)
    ?:  =(0 b)
      [%& ~ a]
    ?~  a  [%| 0]
    =/  al  $(a l.a)
    ?-    -.al
        $&  [%& p.p.al a(l q.p.al)]
        $|
      =.  b  (^sub b p.al)
      =/  top  +((^sub q.n.a p.n.a))
      ?:  =(b top)
        [%& a(r ~) r.a]
      ?:  (lth b top)
        :+  %&  a(r ~, q.n (add p.n.a (dec b)))
        =.  p.n.a  (add p.n.a b)
        (uni(a r.a) [n.a ~ ~])
      =/  ar  $(a r.a, b (^sub b top))
      ?-    -.ar
          $&  [%& a(r p.p.ar) q.p.ar]
          $|  [%| :(add top p.al p.ar)]
      ==
    ==
  ::
  ++  gas                                               ::  ++gas:py
    |=  b/(list ship)  ^-  pile                         ::  insert list
    ?~  b  a
    $(b t.b, a (put i.b))
  ::                                                    ::  ++gud:py
  ++  gud                                               ::  validate
    =|  {bot/(unit ship) top/(unit ship)}
    |-  ^-  ?
    ?~  a  &
    ?&  (lte p.n.a q.n.a)
        ?~(top & (lth +(q.n.a) u.top))
        ?~(bot & (gth p.n.a +(u.bot)))
    ::
        ?~(l.a & (vor p.n.a p.n.l.a))
        $(a l.a, top `p.n.a)
    ::
        ?~(l.a & (vor p.n.a p.n.l.a))
        $(a r.a, bot `q.n.a)
    ==
  ::                                                    ::  ++int:py
  ++  int                                               ::  intersection
    |=  b/pile  ^-  pile
    ?~  a  ~
    ?~  b  ~
    ?.  (vor p.n.a p.n.b)  $(a b, b a)
    ?:  (gth p.n.a q.n.b)
      (uni(a $(b r.b)) $(a l.a, r.b ~))
    ?:  (lth q.n.a p.n.b)
      (uni(a $(b l.b)) $(a r.a, l.b ~))
    ?:  (gte p.n.a p.n.b)
      ?:  (lte q.n.a q.n.b)
        [n.a $(a l.a, r.b ~) $(a r.a, l.b ~)]
      [n.a(q q.n.b) $(a l.a, r.b ~) $(l.a ~, b r.b)]
    %-  uni(a $(r.a ~, b l.b))
    ?:  (lte q.n.a q.n.b)
      %-  uni(a $(l.b ~, a r.a))
      [n.b(q q.n.a) ~ ~]
    %-  uni(a $(l.a ~, b r.b))
    [n.b ~ ~]
  ::                                                    ::  ++put:py
  ++  put                                               ::  insert
    |=  b/ship  ^-  pile
    (uni [b b] ~ ~)
  ::                                                    ::  ++sub:py
  ++  sub                                               ::  subtract
    |=  b/pile  ^-  pile
    ?~  b  a
    ?~  a  a
    ?:  (gth p.n.a q.n.b)
      $(b r.b, l.a $(a l.a, r.b ~))
    ?:  (lth q.n.a p.n.b)
      $(b l.b, r.a $(a r.a, l.b ~))
    %-  uni(a $(a l.a, r.b ~))
    %-  uni(a $(a r.a, l.b ~))
    ?:  (gte p.n.a p.n.b)
      ?:  (lte q.n.a q.n.b)
        ~
      $(b r.b, a [[+(q.n.b) q.n.a] ~ ~])
    ?:  (lte q.n.a q.n.b)
      $(b l.b, a [[n.a(q (min q.n.a (dec p.n.b)))] ~ ~])
    %-  uni(a $(b r.b, a [[+(q.n.b) q.n.a] ~ ~]))
    $(b l.b, a [[n.a(q (min q.n.a (dec p.n.b)))] ~ ~])
  ::
  ++  tap
    =|  out/(list (pair ship ship))
    |-  ^+  out
    ?~  a  out
    $(a l.a, out [n.a $(a r.a)])
  ::                                                    ::  ++uni:py
  ++  uni                                               ::  merge two piles
    |=  b/pile
    ^-  pile
    ?~  b  a
    ?~  a  b
    ?.  (vor p.n.a p.n.b)  $(a b, b a)
    ?:  (lth +(q.n.b) p.n.a)
      $(b r.b, l.a $(a l.a, r.b ~))
    ?:  (lth +(q.n.a) p.n.b)
      $(b l.b, r.a $(a r.a, l.b ~))
    ?:  =(n.a n.b)  [n.a $(a l.a, b l.b) $(a r.a, b r.b)]
    ?:  (lth p.n.a p.n.b)
      ?:  (gth q.n.a q.n.b)
        $(b l.b, a $(b r.b))
      $(b l.b, a $(b r.b, a $(b r.a, r.a ~, q.n.a q.n.b)))
    ?:  (gth q.n.a q.n.b)
      $(a l.a, b $(a r.a, b $(a r.b, r.b ~, q.n.b q.n.a)))
    $(a l.a, b $(a r.a))
  --  ::py
::                                                      ::  ++ry
::::                    ## rights/light                 ::  rights algebra
  ::                                                    ::::
++  ry
  ::
  ::  we need to be able to combine rights, and
  ::  track changes by taking differences between them.
  ::
  ::  ++ry must always crash when you try to make it
  ::  do something that makes no sense.
  ::
  ::  language compromises: the type system can't enforce
  ::  that lef and ryt match, hence the asserts.
  ::
  |_  $:  ::  lef: old right
          ::  ryt: new right
          ::
          lef/rite
          ryt/rite
      ==
  ::                                                    ::  ++dif:ry
  ++  dif                                               ::  r->l: {add remove}
    ^-  (pair (unit rite) (unit rite))
    |^  ?-  -.lef
          $apple  ?>(?=($apple -.ryt) (table %apple p.lef p.ryt))
          $block  ?>(?=($block -.ryt) [~ ~])
          $email  ?>(?=($email -.ryt) (sable %email p.lef p.ryt))
          $final  ?>(?=($final -.ryt) (table %final p.lef p.ryt))
          $fungi  ?>(?=($fungi -.ryt) (noble %fungi p.lef p.ryt))
          $guest  ?>(?=($guest -.ryt) [~ ~])
          $hotel  ?>(?=($hotel -.ryt) (bible %hotel p.lef p.ryt))
          $jewel  ?>(?=($jewel -.ryt) (table %jewel p.lef p.ryt))
          $login  ?>(?=($login -.ryt) (sable %login p.lef p.ryt))
          $pword  ?>(?=($pword -.ryt) (ruble %pword p.lef p.ryt))
          $token  ?>(?=($token -.ryt) (ruble %token p.lef p.ryt))
          $urban  ?>(?=($urban -.ryt) (table %urban p.lef p.ryt))
        ==
    ::                                                  ::  ++bible:dif:ry
    ++  bible                                           ::  diff pile
      |*  {nut/@tas new/(map dorm pile) old/(map dorm pile)}
      =/  mor/_old
        %-  ~(rep by old)
        |=  {{cur/dorm fid/pile} acc/_^+(old ~)}
        =.  fid
          (~(sub py fid) (fall (~(get by new) cur) ~))
        ?~  fid  acc
        (~(put by acc) cur fid)
      ::
      =/  les/_new
        %-  ~(rep by new)
        |=  {{cur/dorm fid/pile} acc/_^+(new ~)}
        =.  fid
          (~(sub py fid) (fall (~(get by old) cur) ~))
        ?~  fid  acc
        (~(put by acc) cur fid)
      ::
      :-  ?~(mor ~ `[nut mor])
          ?~(les ~ `[nut les])
    ::                                                  ::  ++noble:dif:ry
    ++  noble                                           ::  diff map of @ud
      |*  {nut/@tas new/(map * @ud) old/(map * @ud)}
      ^-  (pair (unit rite) (unit rite))
      =/  mor/_old
        %-  ~(rep by old)
        |*  {{cur/* fid/@ud} acc/_^+(old ~)}
        =>  .(+< `_[[cur fid]=-.new acc=old]`+<)
        =.  fid
          (^sub fid (max fid (fall (~(get by new) cur) 0)))
        ?~  fid  acc
        (~(put by acc) cur fid)
      ::
      =/  les/_new
        %-  ~(rep by new)
        |*  {{cur/* fid/@ud} acc/_^+(new ~)}
        =>  .(+< `_[[cur fid]=-.old acc=new]`+<)
        =.  fid
          (^sub fid (max fid (fall (~(get by old) cur) 0)))
        ?~  fid  acc
        (~(put by acc) cur fid)
      ::
      :-  ?~(mor ~ `[nut mor])
          ?~(les ~ `[nut les])
    ::                                                  ::  ++ruble:dif:ry
    ++  ruble                                           ::  diff map of maps
      |*  {nut/@tas new/(map * (map)) old/(map * (map))}
      =/  mor/_old
        %-  ~(rep by old)
        |*  {{cur/* fid/(map)} acc/_^+(old ~)}
        =>  .(+< `_[[cur fid]=n.-.new acc=old]`+<)
        =.  fid
          (~(dif by ,.fid) (fall (~(get by new) cur) ~))
        ?~  fid  acc
        (~(put by acc) cur fid)
      ::
      =/  les/_new
        %-  ~(rep by new)
        |*  {{cur/* fid/(map)} acc/_^+(new ~)}
        =>  .(+< `_[[cur fid]=n.-.old acc=new]`+<)
        =.  fid
          (~(dif by ,.fid) (fall (~(get by old) cur) ~))
        ?~  fid  acc
        (~(put by acc) cur fid)
      ::
      :-  ?~(mor ~ `[nut mor])
          ?~(les ~ `[nut les])
    ::                                                  ::  ++sable:dif:ry
    ++  sable                                           ::  diff set
      |*  {nut/@tas new/(set) old/(set)}
      =/  mor  (~(dif in new) old)
      =/  les  (~(dif in old) new)
      :-  ?~(mor ~ `[nut mor])
          ?~(les ~ `[nut les])
    ::                                                  ::  ++table:dif:ry
    ++  table                                           ::  diff map
      |*  {nut/@tas new/(map) old/(map)}
      ^-  (pair (unit rite) (unit rite))
      =/  ped  (~(dep by old) new)
      :-  ?~(p.ped ~ `[nut p.ped])
          ?~(q.ped ~ `[nut q.ped])
    --  ::dif
  ::                                                    ::  ++sub:ry
  ++  sub                                               ::  l - r
    ^-  (unit rite)
    =/  vid  dif
    ?>(?=($~ q.vid) p.vid)
  ::                                                    ::  ++add:ry
  ++  uni                                               ::  lef new, ryt old
    ^-  rite
    |^  ?-  -.lef
          $apple  ?>(?=($apple -.ryt) [%apple (table p.lef p.ryt)])
          $block  ?>(?=($block -.ryt) [%block ~])
          $email  ?>(?=($email -.ryt) [%email (sable p.lef p.ryt)])
          $final  ?>(?=($final -.ryt) [%final (table p.lef p.ryt)])
          $fungi  ?>(?=($fungi -.ryt) [%fungi (noble p.lef p.ryt)])
          $guest  ?>(?=($guest -.ryt) [%guest ~])
          $hotel  ?>(?=($hotel -.ryt) [%hotel (bible p.lef p.ryt)])
          $jewel  ?>(?=($jewel -.ryt) [%jewel (table p.lef p.ryt)])
          $login  ?>(?=($login -.ryt) [%login (sable p.lef p.ryt)])
          $pword  ?>(?=($pword -.ryt) [%pword (ruble p.lef p.ryt)])
          $token  ?>(?=($token -.ryt) [%token (ruble p.lef p.ryt)])
          $urban  ?>(?=($urban -.ryt) [%urban (table p.lef p.ryt)])
        ==
    ::                                                  ::  ++bible:uni:ry
    ++  bible                                           ::  union pile
      |=  {new/(map dorm pile) old/(map dorm pile)}
      ^+  new
      %-  (~(uno by old) new)
      |=  (trel dorm pile pile)
      (~(uni py q) r)
    ::                                                  ::  ++noble:uni:ry
    ++  noble                                           ::  union map of @ud
      |=  {new/(map term @ud) old/(map term @ud)}
      ^+  new
      %-  (~(uno by old) new)
      |=  (trel term @ud @ud)
      (add q r)
    ::                                                  ::  ++ruble:uni:ry
    ++  ruble                                           ::  union map of maps
      |=  {new/(map site (map @t @t)) old/(map site (map @t @t))}
      ^+  new
      %-  (~(uno by old) new)
      |=  (trel site (map @t @t) (map @t @t))
      %-  (~(uno by q) r)
      |=  (trel @t @t @t)
      ?>(=(q r) r)
    ::                                                  ::  ++sable:uni:ry
    ++  sable                                           ::  union set
      |*  {new/(set) old/(set)}
      ^+  new
      (~(uni in old) new)
    ::                                                  ::  ++table:uni:ry
    ++  table                                           ::  union map
      |*  {new/(map) old/(map)}
      ^+  new
      %-  (~(uno by old) new)
      |=  (trel _p.-<.new _q.->.new _q.->.new)
      ?>(=(q r) r)
    --  ::uni
  --  ::ry
::                                                      ::  ++up
::::                    ## wallet^light                 ::  wallet algebra
  ::                                                    ::::
++  up
  ::  a set of rites is stored as a tree (++safe), sorted
  ::  by ++gor on the stem, balanced by ++vor on the stem.
  ::  (this is essentially a ++map with stem as key, but
  ::  ++map doesn't know how to link stem and bulb types.)
  ::  the goal of the design is to make it easy to add new
  ::  kinds of rite without a state adapter.
  ::
  ::  wallet operations always crash if impossible;
  ::  %jael has no concept of negative rights.
  ::
  ::  performance issues: ++differ and ++splice, naive.
  ::
  ::  external issues: much copy and paste from ++by.  it
  ::  would be nice to resolve this somehow, but not urgent.
  ::
  ::  language issues: if hoon had an equality test
  ::  that informed inference, ++expose could be
  ::  properly inferred, eliminating the ?>.
  ::
  |_  pig/safe
  ::                                                    ::  ++delete:up
  ++  delete                                            ::  delete right
    |=  ryt/rite
    ^-  safe
    ?~  pig
      ~
    ?.  =(-.ryt -.n.pig)
      ?:  (gor -.ryt -.n.pig)
        [n.pig $(pig l.pig) r.pig]
      [n.pig l.pig $(pig r.pig)]
    =/  dub  ~(sub ry n.pig ryt)
    ?^  dub  [u.dub l.pig r.pig]
    |-  ^-  safe
    ?~  l.pig  r.pig
    ?~  r.pig  l.pig
    ?:  (vor -.n.l.pig -.n.r.pig)
      [n.l.pig l.l.pig $(l.pig r.l.pig)]
    [n.r.pig $(r.pig l.r.pig) r.r.pig]
  ::                                                    ::  ++differ:up
  ++  differ                                            ::  delta pig->gob
    |=  gob/safe
    ^-  bump
    |^  [way way(pig gob, gob pig)]
    ++  way
      %-  intern(pig ~)
      %+  skip  linear(pig gob)
      |=(rite (~(has in pig) +<))
    --
  ::                                                    ::  ++exists:up
  ++  exists                                            ::  test presence
    |=  tag/@tas
    !=(~ (expose tag))
  ::                                                    ::  ++expose:up
  ++  expose                                            ::  typed extract
    |=  tag/@tas
    ^-  (unit rite)
    ?~  pig  ~
    ?:  =(tag -.n.pig)
      [~ u=n.pig]
    ?:((gor tag -.n.pig) $(pig l.pig) $(pig r.pig))
  ::                                                    ::  ++insert:up
  ++  insert                                            ::  insert item
    |=  ryt/rite
    ^-  safe
    ?~  pig
      [ryt ~ ~]
    ?:  =(-.ryt -.n.pig)
      ?:  =(+.ryt +.n.pig)
        pig
      [~(uni ry ryt n.pig) l.pig r.pig]
    ?:  (gor -.ryt -.n.pig)
      =+  nex=$(pig l.pig)
      =.  l.pig  nex
      ?>  ?=(^ l.pig)
      ?:  (vor -.n.pig -.n.l.pig)
        [n.pig l.pig r.pig]
      [n.l.pig l.l.pig [n.pig r.l.pig r.pig]]
    =+  nex=$(pig r.pig)
    =.  r.pig  nex
    ?>  ?=(^ r.pig)
    ?:  (vor -.n.pig -.n.r.pig)
      [n.pig l.pig r.pig]
    [n.r.pig [n.pig l.pig l.r.pig] r.r.pig]
  ::                                                    ::  ++intern:up
  ++  intern                                            ::  insert list
    |=  lin/(list rite)
    ^-  safe
    ?~  lin  pig
    =.  pig  $(lin t.lin)
    (insert i.lin)
  ::                                                    ::  ++linear:up
  ++  linear                                            ::  convert to list
    =|  lin/(list rite)
    |-  ^+  lin
    ?~  pig  ~
    $(pig r.pig, lin [n.pig $(pig l.pig)])
  ::                                                    ::  ++redact:up
  ++  redact                                            ::  conceal secrets
    |-  ^-  safe
    ?~  pig  ~
    :_  [$(pig l.pig) $(pig r.pig)]
    =*  rys  n.pig
    ^-  rite
    ?+    -.rys  rys
        $apple
      [%apple (~(run by p.rys) |=(@ (mug +<)))]
    ::
        $final
      [%final (~(run by p.rys) |=(@ (mug +<)))]
    ::
        $login
      [%login ~]
    ::
        $pword
      :-  %pword
      %-  ~(run by p.rys)
      |=  (map @ta @t)
      (~(run by +<) |=(@t (fil 3 (met 3 +<) '*')))
    ::
        $jewel
      [%jewel (~(run by p.rys) |=(@ (mug +<)))]
    ::
        $token
      :-  %token
      %-  ~(run by p.rys)
      |=((map @ta @) (~(run by +<) |=(@ (mug +<))))
    ::
        $urban
      [%urban (~(run by p.rys) |=({@da code:ames} [+<- (mug +<+)]))]
    ==
  ::                                                    ::  ++remove:up
  ++  remove                                            ::  pig minus gob
    |=  gob/safe
    ^-  safe
    =/  buv  ~(tap by gob)
    |-  ?~  buv  pig
        $(buv t.buv, pig (delete i.buv))
  ::                                                    ::  ++splice:up
  ++  splice                                            ::  pig plus gob
    |=  gob/safe
    ^-  safe
    =/  buv  ~(tap by gob)
    |-  ?~  buv  pig
        $(buv t.buv, pig (insert i.buv))
  ::                                                    ::  ++update:up
  ++  update                                            ::  arbitrary change
    |=  del/bump
    ^-  safe
    (splice(pig (remove les.del)) mor.del)
  --
::                                                      ::  ++we
::::                    ## will^light                   ::  will functions
  ::                                                    ::::
++  we
  |_  pub/will
  ::                                                    ::  ++collate:we
  ++  collate                                           ::  sort by version
    |=  ord/$-({{life cert} {life cert}} ?)
    ^-  (list (pair life cert))
    (sort ~(tap by pub) ord)
  ::                                                    ::  ++current:we
  ++  current                                           ::  current number
    ^-  (unit life)
    (bind instant |=((pair life cert) p))
  ::                                                    ::  ++forward:we
  ++  forward                                           ::  sort oldest first
    (collate |=({a/{life *} b/{life *}} (lth -.a -.b)))
  ::                                                    ::  ++instant:we
  ++  instant                                           ::  current cert
    ^-  (unit (pair life cert))
    =+  reverse
    ?~(- ~ `i)
  ::                                                    ::  ++reverse:we
  ++  reverse                                           ::  sort latest first
    (collate |=({a/{life *} b/{life *}} (gth -.a -.b)))
  --
--
::                                                      ::::
::::                    #  heavy                        ::  heavy engines
  ::                                                    ::::
=>  |%
::                                                      ::  ++of
::::                    ## main^heavy                   ::  main engine
  ::                                                    ::::
++  of
  ::  this core handles all top-level %jael semantics,
  ::  changing state and recording moves.
  ::
  ::  logically we could nest the ++su and ++ur cores
  ::  within it, but we keep them separated for clarity.
  ::  the ++curd and ++cure arms complete relative and
  ::  absolute effects, respectively, at the top level.
  ::
  ::  a general pattern here is that we use the ++ur core
  ::  to generate absolute effects (++change), then invoke
  ::  ++su to calculate the derived effect of these changes.
  ::
  ::  arvo issues: should be merged with the top-level
  ::  vane interface when that gets cleaned up a bit.
  ::
  =|  moz/(list move)
  =|  $:  ::  sys: system context
          ::
          $=  sys
          $:  ::  now: current time
              ::  eny: unique entropy
              ::
              now/@da
              eny/@e
          ==
          ::  all vane state
          ::
          state
      ==
  ::  lex: all durable state
  ::  moz: pending actions
  ::
  =*  lex  ->
  |%
  ::                                                    ::  ++abet:of
  ++  abet                                              ::  resolve
    [(flop moz) lex]
  ::                                                    ::  ++burb:of
  ++  burb                                              ::  per ship
    |=  who/ship
    ~(able ~(ex ur urb) who)
  ::                                                    ::  ++call:of
  ++  call                                              ::  invoke
    |=  $:  ::  hen: event cause
            ::  tac: event data
            ::
            hen/duct
            tac/task
        ==
    ^+  +>
    ?-    -.tac
    ::
    ::  destroy promises
    ::    {$burn p/ship q/safe}
    ::
        $burn
      (cure abet:abet:(deal:(burb our) p.tac [~ q.tac]))
    ::
    ::  remote update
    ::    {$hail p/ship q/remote}
    ::
        $hail
      (cure abet:abet:(hail:(burb p.tac) our q.tac))
    ::
    ::  initialize vane
    ::    {$init p/code q/arms}
    ::
        $init
      (cure abet:abet:(make:(burb our) now.sys eny.sys p.tac q.tac))
    ::
    ::  create promises
    ::    {$mint p/ship q/safe}
    ::
        $mint
      (cure abet:abet:(deal:(burb our) p.tac [q.tac ~]))

    ::
    ::  move promises
    ::    {$move p/ship q/ship r/safe}
    ::
        $move
      =.  +>  (cure abet:abet:(deal:(burb our) p.tac [~ r.tac]))
      =.  +>  (cure abet:abet:(deal:(burb our) q.tac [r.tac ~]))
      +>
    ::
    ::  public-key update
    ::    {$meet p/(unit (unit ship)) q/farm}
    ::
        $meet
      (cure abet:(~(meet ur urb) p.tac q.tac))
    ::
    ::  cancel all trackers from duct
    ::    {$nuke $~}
    ::
        $nuke
      %_  +>
        yen          (~(del in yen) hen)
        yen.bal.sub  (~(del in yen.bal.sub) hen)
        yen.own.sub  (~(del in yen.own.sub) hen)
        car.sub      %-  ~(run by car.sub)
                     |=  {yen/(set duct) det/channel}
                     [(~(del in yen) hen) det]
      ==
    ::
    ::  extend our certificate with a new private key
    ::    {$next p/bull}
    ::
        $next
      (cure abet:abet:(next:(burb our) eny.sys p.tac))
    ::
    ::  open secure channel
    ::    {$veil p/ship}
    ::
        $veil
      (curd abet:(~(veil ~(feed su urb sub) hen) p.tac))
    ::
    ::  watch private keys
    ::    {$vein $~}
    ::
        $vein
      (curd abet:~(vein ~(feed su urb sub) hen))
    ::
    ::  monitor assets
    ::    {$vest $~}
    ::
        $vest
      (curd abet:~(vest ~(feed su urb sub) hen))
    ::
    ::  monitor all
    ::    {$vine $~}
    ::
        $vine
      +>(yen (~(put in yen) hen))
    ::
    ::  authenticated remote request
    ::    {$west p/ship q/path r/*}
    ::
        $west
      ?>  =(~ q.tac)
      =+  mes=((hard message) r.tac)
      ?-    -.mes
      ::
      ::  reset remote rights
      ::    {$hail p/safe}
      ::
          $hail
        (cure abet:abet:(hail:(burb p.tac) our [%| p.mes]))
      ::
      ::  share certificates
      ::    {$meet p/farm}
      ::
          $meet
        (cure abet:(~(meet ur urb) ``p.tac p.mes))
      ==
    ==
  ::                                                    ::  ++curd:of
  ++  curd                                              ::  relative moves
    |=  {moz/(list move) sub/state-relative}
    +>(sub sub, moz (weld (flop moz) ^moz))
  ::                                                    ::  ++cure:of
  ++  cure                                              ::  absolute edits
    |=  {hab/(list change) urb/state-absolute}
    ^+  +>
    (curd(urb urb) abet:(~(apex su urb sub) hab))
  --
::                                                      ::  ++su
::::                    ## relative^heavy               ::  subjective engine
  ::                                                    ::::
++  su
      ::  the ++su core handles all derived state,
      ::  subscriptions, and actions.
      ::
      ::  ++feed:su registers subscriptions, and also
      ::  drives certificate propagation when a %veil
      ::  (secure channel) subscription is created.
      ::
      ::  ++feel:su checks if a ++change should notify
      ::  any subscribers.
      ::
      ::  ++fire:su generates outgoing network messages.
      ::
      ::  ++form:su generates the actual report data.
      ::
  =|  moz/(list move)
  =|  $:  state-absolute
          state-relative
      ==
  ::  moz: moves in reverse order
  ::  urb: absolute urbit state
  ::  sub: relative urbit state
  ::
  =*  urb  -<
  =*  sub  ->
  |%
  ::                                                    ::  ++abet:su
  ++  abet                                              ::  resolve
    [(flop moz) sub]
  ::                                                    ::  ++apex:su
  ++  apex                                              ::  apply changes
    |=  hab/(list change)
    ^+  +>
    ?~  hab  +>
    %=    $
        hab  t.hab
        +>
      ?-  -.i.hab
        $rite  (paid +.i.hab)
        $fact  (said +.i.hab)
      ==
    ==
  ::                                                    ::  ++exec:su
  ++  exec                                              ::  mass gift
    |=  {yen/(set duct) cad/card}
    =/  noy  ~(tap in yen)
    |-  ^+  ..exec
    ?~  noy  ..exec
    $(noy t.noy, moz [[i.noy cad] moz])
  ::                                                    ::  ++feed:su
  ++  feed                                              ::  subscribe to view
    |_  ::  hen: subscription source
        ::
        hen/duct
    ::                                                  ::  ++veil:feed:su
    ++  veil                                            ::  secure channel
      |=  who/ship
      ^+  ..feed
      ::
      ::  send initial pki sync as needed
      ::
      =.  ..feed  (open hen who)
      =/  ruc  (~(get by car) who)
      =/  rec
        ?~  ruc
           [`yen/(set duct)`[hen ~ ~] det=(veil:form who)]
         u.ruc(yen (~(put in yen.u.ruc) hen))
      %_  ..feed
        moz  [[hen %give %veil det.rec] moz]
        car  (~(put by car) who rec)
      ==
    ::                                                  ::  ++vein:feed:su
    ++  vein                                            ::  private keys
      %_  ..feed
        moz      [[hen %give %vein [lyf jaw]:own] moz]
        yen.own  (~(put in yen.own) hen)
      ==
    ::                                                  ::  ++vest:feed:su
    ++  vest                                            ::  balance
      %_  ..feed
        moz      [[hen %give %vest %& vest:form] moz]
        yen.bal  (~(put in yen.bal) hen)
      ==
    --
  ::                                                    ::  ++feel:su
  ++  feel                                              ::  update tracker
    |%
    ::                                                  ::  ++veal:feel:su
    ++  veal                                            ::  kick subfarm
      ^+  ..feel
      =/  cod  veal:form
      ?:(=(cod.rel cod) ..feel ..feel(cod.rel cod))
    ::                                                  ::  ++veil:feel:su
    ++  veil                                            ::  kick secure channel
      |=  who/ship
      ^+  ..feel
      =/  ruc  (~(get by car) who)
      ?~  ruc  ..feel
      =/  det  (veil:form who)
      ?:  =(det det.u.ruc)  ..feel
      =.  car  (~(put by car) who [yen.u.ruc det])
      (exec yen.u.ruc [%give %veil det])
    ::                                                  ::  ++vein:feel:su
    ++  vein                                            ::  kick private keys
      ^+  ..feel
      =/  yam  vein:form
      ?:  =(yam +.own)  ..feel
      (exec(+.own yam) yen.own [%give %vein +.own])
    ::                                                  ::  ++vest:feel:su
    ++  vest                                            ::  kick balance
      |=  hug/action
      ^+  ..feel
      ?:  =([~ ~] +.q.hug)  ..feel
      ::
      ::  notify all local listeners
      ::
      =.  ..feel  (exec yen.bal [%give %vest %| p.hug q.hug])
      ::
      ::  pig: purse report for partner
      ::
      ?.  ?=($| -.q.hug)  ..feel
      =*  pig  (~(lawn ur urb) our p.hug)
      %_    ..feel
          moz  :_  moz
        [*duct %pass /vest/(scot %p p.hug) %x %mess p.hug /j %hail pig]
      ==
    --
  ::                                                    ::  ++fire:su
  ++  fire                                              ::  propagate keys
    |_  hec/farm
    ++  home                                            ::  ++home:su
      |=  who/ship                                      ::  to ship
      %_    ..fire
          moz
        :_  moz
        [*duct %pass /meet/(scot %p who) %x %mess who /j [%meet hec]]
      ==
    ::                                                  ::  ++flow:su
    ++  flow                                            ::  to set of ships
      |=  tar/(set ship)
      =+  rot=~(tap in (~(del in tar) our))
      |-  ^+  ..fire
      ?~  rot  ..fire
      $(rot t.rot, ..fire (home i.rot))
    ::                                                  ::  ++spam:su
    ++  spam                                            ::  to list of sets
      |=  {via/(unit ship) jax/(list (set ship))}
      ^+  ..fire
      =-  (flow ?~(via - (~(del in -) u.via)))
      |-  ^-  (set ship)
      ?~(jax ~ (~(uni in i.jax) $(jax t.jax)))
    --
  ::                                                    ::  ++form:su
  ++  form                                              ::  generate reports
    |%
    ::                                                  ::  ++veal:form:su
    ++  veal                                            ::  public dependencies
      =|  sea/(set ship)
      =|  out/farm
      =/  mor  `(set ship)`[our ~ ~]
      |-  ^-  farm
      ?:  =(~ mor)  out
      ::
      ::  nex: all wills to add
      ::
      =/  nex
        =/  rom  ~(tap in mor)
        |-  ^-  farm
        ?~  rom  ~
        %+  ~(put by $(rom t.rom))
          i.rom
        (~(got by pug.urb) i.rom)
      ::
      ::  wit: all new ships in these wills
      ::
      =.  sea  (~(uni in sea) mor)
      =/  wit
        =|  wit/(set ship)
        =/  fem  ~(tap by nex)
        |-  ^+  wit
        ?~  fem  wit
        =.  wit  $(fem t.fem)
        =/  naw  ~(tap by q.i.fem)
        |-  ^+   wit
        ?~  naw  wit
        =.  wit  $(naw t.naw)
        =*  dad  dad.doc.dat.q.i.naw
        ?:  (~(has in sea) dad)  wit
        (~(put in wit) dad)
      ::
      ::  repeat, flushing output
      ::
      $(mor wit, out (~(uni by out) nex))
    ::                                                  ::  ++veil:form:su
    ++  veil                                            ::  channel report
      |=  who/ship
      ^-  channel
      ::
      ::  pub: will of who
      ::  exp: promises from our to who
      ::  imp: promises from who to our
      ::  out: symmetric key from our to who
      ::  inn: symmetric keys from who to our
      ::
      =/  pub
        ^-  will
        =-  ?~(- ~ u.-)
        (~(get by pug.urb) who)
      ::
      =/  exp
        ^-  safe
        =-  ?~(- ~ u.-)
        (~(get by (~(got by pry.urb) our)) who)
      ::
      =/  imp
        ^-  safe
        =-  ?~(- ~ u.-)
        %.  our
        ~(get by (fall (~(get by pry.urb) who) *(map ship safe)))
      ::
      =*  out
        ^-  (unit (pair hand bill))
        =+  (~(expose up exp) %urban)
        ?~  -  ~
        ?>  ?=($urban -.u.-)
        =*  pam  p.u.-
        ?~  pam  ~
        ::  arbitrarily select root node of the map
        ::
        `n.pam
      ::
      =*  inn
          =+  (~(expose up imp) %urban)
          ^-  (map hand bill)
          ?~  -  ~
          ?>  ?=($urban -.u.-)
          p.u.-
      ::
      ^-  channel
      [out inn ~(current we pub) (~(dads ur urb) who) pub]
    ::                                                  ::  ++vein:form:su
    ++  vein                                            ::  private key report
      ^-  (pair life (map life ring))
      (~(lean ur urb) our)
    ::                                                  ::  ++vest:form:su
    ++  vest                                            ::  balance report
      ^-  balance
      :-  ::
          ::  raw: all our liabilities by ship
          ::  dud: delete liabilities to self
          ::  cul: mask secrets
          ::
          =*  raw  =-(?~(- ~ u.-) (~(get by pry.urb) our))
          =*  dud  (~(del by raw) our)
          =*  cul  (~(run by dud) |=(safe ~(redact up +<)))
          cul
      ::
      ::  fub: all assets by ship
      ::  veg: all nontrivial assets, secrets masked
      ::
      =/  fub
        ^-  (list (pair ship (unit safe)))
        %+  turn
          ~(tap by pry.urb)
        |=  (pair ship (map ship safe))
        [p (~(get by q) our)]
      =*  veg
        |-  ^-  (list (pair ship safe))
        ?~  fub  ~
        =+  $(fub t.fub)
        ?~(q.i.fub - [[p.i.fub ~(redact up u.q.i.fub)] -])
      ::
      (~(gas by *(map ship safe)) veg)
    --
  ::                                                    ::  ++open:su
  ++  open                                              ::  make secure channel
    |=  $:  hen/duct
            who/ship
        ==
    ^+  +>
    ::
    ::  a one-time operation to create a secure channel
    ::
    ?:  (~(has by car) who)  +>
    ::
    ::  initial propagation: ourself and dependencies, plus
    ::  all capital ships if meeting a child.
    ::
    =*  hec  ^-  farm
      ?.  (~(has in kyz.rel) who)  cod.rel
      =-  (~(uni by cod.rel) -)
      %-  ~(gas by *farm)
      %+  skim  ~(tap by pug.urb)
      |=({who/ship *} (lth who 65.536))
    ::
    (~(home fire hec) who)
  ::                                                    ::  ++paid:su
  ++  paid                                              ::  track asset change
    |=  $:  ::  rex: promise from
            ::  pal: promise to
            ::  del: change to existing
            ::  bur: changes to symmetric keys
            ::
            rex/ship
            pal/ship
            del/bump
        ==
    ^+  +>
    =*  bur  ?|  (~(exists up mor.del) %urban)
                 (~(exists up les.del) %urban)
             ==
    ::  ignore empty delta; keep secrets out of metadata
    ::
    ?:  =([~ ~] del)  +>
    =.  del  [~(redact up mor.del) ~(redact up les.del)]
    ?.  =(our pal)
      ::
      ::  track promises we made to others
      ::
      ?.  =(our rex)  +>
      ::
      ::  track liabilities
      ::
      =.  +>  (vest:feel pal %& del)
      ::
      ::  track secure channels
      ::
      ?.  bur  +>
      (veil:feel pal)
    ::
    ::  track private keys
    ::
    =?  +>  (~(exists up mor.del) %jewel)
      vein:feel
    ::
    ::  track changes in secure channels
    ::
    ?.  bur  +>
    (veil:feel rex)
  ::                                                    ::  ++said:su
  ++  said                                              ::  track cert change
    |=  $:  ::  rex: ship whose will has changed
            ::  vie: change authorized by
            ::  lyf: modified/created version
            ::  gan: modification
            ::
            rex/ship
            vie/(unit (unit ship))
            lyf/life
            gan/growth
        ==
    ::  lip: this change as its own farm
    ::
    =/  lip  ^-  farm
      =-  [[rex -] ~ ~]
      ^-  will
      =-  [[lyf -] ~ ~]
      ^-  cert
      ?-    -.gan
      ::
      ::  add a new certificate to this will
      ::    {$step p/cert}
      ::
          $step  p.gan
      ::
      ::  add a new signature to this certificate
      ::    {$sign p/mind q/@}
      ::
          $sign
        :-  dat:(~(got by (~(got by pug.urb) rex)) lyf)
        =-  [- ~ ~]
        [who.p.gan lyf.p.gan q.gan]
      ==
    ::
    ::  if our subfarm may have changed, reset it
    ::
    =?  +>.$  |(=(our rex) (~(has by cod.rel) rex))
      veal:feel
    ::
    ::  if a new deed, reset parent
    ::
    =?  dad.rel  &(=(our rex) ?=($step -.gan))
      dad.doc.dat.p.gan
    ::
    ::  kick secure channels
    ::
    =.  +>.$  (veil:feel rex)
    ::
    ::  if we signed a will for someone else, send it home
    ::
    ?:  &(=([~ ~] vie) !=(our rex))
      (~(home fire lip) rex)
    ::
    ::  if first certificate, add to neighbor lists
    ::
    =?  +>.$  &(?=($step -.gan) =(1 lyf))
      =?  kyz.rel  =(our dad.doc.dat.p.gan)
        (~(put in kyz.rel) rex)
      =?  pyr.rel  =((clan rex) (clan our))
        (~(put in pyr.rel) rex)
      +>.$
    ::
    ::  propagate new data as appropriate
    ::
    %+  ~(spam fire lip)
      ?~(vie ~ ?~(u.vie ~ `u.u.vie))
    ^-  (list (set ship))
    ::
    ::  if our will has changed, send to parents and kids;
    ::  if a new deed has been added, also to pals
    ::
    ?:  =(our rex)
      :*  [dad.rel ~ ~]
          kyz.rel
          ?.(=(%step -.gan) ~ [pyr.rel ~])
      ==
    ::
    ::  forward star and galaxy updates to parents and kids
    ::
    ?.  (lth rex 65.536)
      ~
    :*  [dad.rel ~ ~]
        kyz.rel
        ~
    ==
  --
::                                                      ::  ++ur
::::                    ## absolute^heavy               ::  objective engine
  ::                                                    ::::
++  ur
      ::  the ++ur core handles primary, absolute state.
      ::  it is the best reference for the semantics of
      ::  the urbit pki.
      ::
      ::  it is absolutely verboten to use [our] in ++ur.
      ::
  =|  hab/(list change)
  =|  state-absolute
  ::
  ::  hab: side effects, reversed
  ::  urb: all urbit state
  ::
  =*  urb  -
  |%
  ::                                                    ::  ++abet:ur
  ++  abet                                              ::  resolve
    [(flop hab) `state-absolute`urb]
  ::                                                    ::  ++boss:ur
  ++  boss                                              ::  parent
    |=  who/ship
    ^-  ship
    -:(dads who)
  ::
  ++  dads                                              ::  ++dads:ur
    |=  who/ship                                        ::  lineage
    ^-  (list ship)
    =/  ryg  (~(get by pug) who)
    ?~  ryg  (saxo who)
    =/  dad  dad.doc.dat.q:(need ~(instant we u.ryg))
    [who ?:(=(who dad) ~ $(who dad))]
  ::
  ++  lawn                                              ::  ++lawn:ur
    |=  {rex/ship pal/ship}                             ::  debts, rex to pal
    ^-  safe
    (lawn:~(able ex rex) pal)
  ::                                                    ::  ++leak:ur
  ++  leak                                              ::  private key
    |=  rex/ship
    ^-  (pair life ring)
    =/  lyn  lean:~(able ex rex)
    [p.lyn (~(got by q.lyn) p.lyn)]
  ::                                                    ::  ++lean:ur
  ++  lean                                              ::  private keys
    |=  rex/ship
    ^-  (pair life (map life ring))
    lean:~(able ex rex)
  ::                                                    ::  ++make:ur
  ++  make                                              ::  initialize urbit
    |=  $:  ::  now: date
            ::  eny: entropy
            ::  gen: bootstrap ticket
            ::  nym: self-description
            ::
            now/@da
            eny/@e 
            gen/@pG
            nym/arms
        ==
    ^+  +>
    ::  key: generated key
    ::  bul: initial bull
    ::
    =/  key  (ypt:scr (mix our %jael-make) gen)
    =*  doc  `bull`[(sein our) & nym]
    ::
    ::  register generator as login secret
    ::
    =.  +>.$  abet:(deal:~(able ex our) our [[[%login [gen ~ ~]] ~ ~] ~])
    ::
    ::  initialize hierarchical property
    ::
    =.  +>.$  
      =-  abet:(deal:~(able ex our) our - ~)
      ^-  safe
      %-  intern:up
      ^-  (list rite)
      =/  mir  (clan our)
      ?+    mir  ~
          $czar
        :~  [%fungi [%usr 255] ~ ~]
            [%hotel [[our 3] [1 255] ~ ~] ~ ~]
        ==
          $king
        :~  [%fungi [%upl 65.535] ~ ~]
            [%hotel [[our 4] [1 65.535] ~ ~] ~ ~]
        ==
          $duke
        :~  [%hotel [[our 5] [1 0xffff.ffff] ~ ~] ~ ~]
        ==
      ==
    ::
    ::  create initial communication secrets
    ::
    ?:  (lth our 256)
      ::
      ::  create galaxy with generator as seed
      ::
      abet:(next:~(able ex our) key doc)
    ::
    ::  had: key handle
    ::  ryt: initial right
    ::
    =/  key  (ypt:scr (mix our %jael-make) gen)  
    =*  had  (shaf %hand key)
    =*  ryt  `rite`[%urban [had (add ~m1 now) key] ~ ~]
    ::
    ::  register initial symmetric key from ticket
    ::
    =.  +>.$  abet:(hail:~(able ex (sein our)) our %& [ryt ~ ~])
    ::
    ::  create initial private key and certificate
    ::
    abet:(next:~(able ex our) (mix eny key) doc)
  ::                                                    ::  ++meet:ur
  ++  meet                                              ::  calculate merge
    |=  $:  ::  vie: authenticated source
            ::  cod: transmitted certificates
            ::
            vie/(unit (unit ship))
            cod/farm
        ==
    ^+  +>
    =+  lec=~(tap by cod)
    |-  ^+  ..meet
    ?~  lec  ..meet
    %=  $
      lec     t.lec
      ..meet  abet:(grow:~(able ex p.i.lec) vie cod q.i.lec)
    ==
  ::                                                    ::  ++ex:ur
  ++  ex                                                ::  server engine
    ::  shy: private state
    ::  rug: domestic will
    ::
    =|  $:  shy/(map ship safe)
            rug/will
        ==
    =|  ::  rex: server ship
        ::
        rex/ship
    |%
    ::                                                  ::  ++abet:ex:ur
    ++  abet                                            ::  resolve
      %_  ..ex
        pry  (~(put by pry) rex shy)
        pug  (~(put by pug) rex rug)
      ==
    ::                                                  ::  ++able:ex:ur
    ++  able                                            ::  initialize
      %_  .
        shy  (fall (~(get by pry) rex) *(map ship safe))
        rug  (fall (~(get by pug) rex) *will)
      ==
    ::                                                  ::  ++deal:ex:ur
    ++  deal                                            ::  alter rights
      |=  {pal/ship del/bump}
      ^+  +>
      =/  gob  (fall (~(get by shy) pal) *safe)
      =*  hep  (~(update up gob) del)
      %_  +>.$
        shy  (~(put by shy) pal hep)
        hab  [[%rite rex pal del] hab]
      ==
    ::
    ++  hail                                            ::  ++hail:ex:ur
      |=  {pal/ship rem/remote}                         ::  report rights
      ^+  +>
      =/  gob  (fall (~(get by shy) pal) *safe)
      =/  yer  ^-  (pair bump safe)
        ?-  -.rem
          $&  [[p.rem ~] (~(splice up gob) p.rem)]
          $|  [(~(differ up gob) p.rem) p.rem]
        ==
      %_  +>.$
        shy  (~(put by shy) pal q.yer)
        hab  [[%rite rex pal p.yer] hab]
      ==
    ::                                                  ::  ++lean:ex:ur
    ++  lean                                            ::  private keys
      ^-  (pair life (map life ring))
      ::
      ::  lyf: latest life of
      ::  lab: promises by rex
      ::  par: promises by rex, to rex
      ::  jel: %jewel rights
      ::
      =/  lyf  `life`(need ~(current we (~(got by pug) rex)))
      =*  lab  (~(got by pry) rex)
      =*  par  (~(got by lab) rex)
      =/  jel  `rite`(need (~(expose up par) %jewel))
      ?>  ?=($jewel -.jel)
      [lyf p.jel]
    ::                                                  ::  ++lawn:ex:ur
    ++  lawn                                            ::  liabilities to pal
      |=  pal/ship
      ^-  safe
      =-(?~(- ~ u.-) (~(get by shy) pal))
    ::                                                  ::  ++next:ex:ur
    ++  next                                            ::  advance private key
      |=  {eny/@e doc/bull}
      ^+  +>
      ::  loy: live keypair
      ::  rig: private key
      ::  ryt: private key as right
      ::  pub: public key
      ::  cet: unsigned certificate
      ::  wyl: initial will
      ::  hec: initial will as farm
      ::
      =/  loy  (pit:nu:crub 512 eny)
      =*  rig  sec:ex:loy
      =*  ryt  `rite`[%jewel [1 rig] ~ ~]
      =*  pub  pub:ex:loy
      =*  cet  `cert`[[doc pub] ~]
      =*  wyl  `will`[[1 cet] ~ ~]
      =*  hec  `farm`[[rex wyl] ~ ~]
      =.  +>.$  (deal rex [[ryt ~ ~] ~])
      =.  ..ex  (meet [~ ~] hec)
      +>.$
    ::                                                  ::  grow:ex:ur
    ++  grow                                            ::  merge wills
      |=  $:  ::  vie: data source
              ::  cod: merge context
              ::  gur: input will
              ::
              vie/(unit (unit ship))
              cod/farm
              gur/will
          ==
      ?:  |(=(~ gur) =(gur rug))  ..grow
      |^  ^+  ..grow
          ::
          ::  wap: ascending list of new certs
          ::  pre: previous deed
          ::
          =/  wap  ~(forward we gur)
          ?~  wap  ..grow
          =/  pre
            ^-  (unit deed)
            ?~  (dec p.i.wap)  ~
            `dat:(~(got by rug) (dec p.i.wap))
          ::
          ::  merge each life
          ::
          |-  ^+  ..grow
          ::
          ::  hub: changes
          ::  lub: merged deed
          ::
          =+  [hub lub]=[p q]:(grow-mate p.i.wap q.i.wap pre)
          ?~  t.wap  ..grow
          ?>  =(p.i.t.wap +(p.i.wap))
          %=  $
            wap  t.wap
            pre  `dat.lub
            rug  (~(put by rug) p.i.wap lub)
            hab  (weld (flop hub) hab)
          ==
      ::                                                ::  grow-lick/ex:ur
      ++  grow-lick                                     ::  check signature
        |=  {pub/pass ash/@ val/@}
        ^-  ?
        =+  ver=(sure:as:(com:nu:crub pub) *code:ames val)
        ?~  ver  |
        =(ash u.ver)
      ::                                                ::  grow-like/ex:ur
      ++  grow-like                                     ::  verify signature
        |=  {myn/mind ash/@ val/@}
        ^-  ?
        =:  ..able  able(rex who.myn)
            gur     (fall (~(get by cod) who.myn) *will)
          ==
        (grow-lick (grow-look lyf.myn) ash val)
      ::                                                ::  grow-look/ex:ur
      ++  grow-look                                     ::  load public key
        |=  lyf/life
        ^-  @
        ::
        ::  cascade search over old and new, new first
        ::
        |^  %-  (bond |.((need grow-look-find)))
            grow-look-find(rug gur)
        ::                                              ::  grow-look-find:ex:ur
        ++  grow-look-find                              ::
          ^-  (unit @)
          ::
          ::  crash if this life is revoked
          ::
          ?<  (~(has by rug) +(lyf))
          %+  biff  (~(get by rug) lyf)
          |=(cert `pub.dat)
        --
      ::                                                ::  grow-mate/ex:ur
      ++  grow-mate                                     ::  merge lives
        |=  $:  ::  num: life we're merging
                ::  new: new deed
                ::  pre: previous deed
                ::  eld: old deed
                ::
                num/@ud
                new/cert
                pre/(unit deed)
            ==
        =+  :*  eld=`(unit cert)`(~(get by rug) num)
            ==
        ^-  (pair (list change) cert)
        ::
        ::  enforce artificial scarcity in lives
        ::
        ?>  (lte num 9)
        ::
        ::  if no new information, do nothing
        ::
        ?:  |(=(eld `new))
          ?>  ?=(^ eld)
          [~ u.eld]
        ::
        ::  ash: hash of deed content
        ::  def: our default parent
        ::  dad: our declared parent
        ::  mir: our rank
        ::
        =/  ash  (sham %urbit rex num dat.new)
        =/  def  (sein rex)
        =*  dad  dad.doc.dat.new
        =/  mir  (clan rex)
        ?>  ?:  |(=(num 1) =(%earl mir) =(%pawn mir))
              ::
              ::  first parent must be default;
              ::  comets and moons may not migrate
              ::
              =(def dad)
            ::
            ::  all others may migrate to parent of same rank
            ::
            =((clan def) (clan dad))
        ::
        ::  if we have an old deed at this life, merge new signatures
        ::
        ?:  ?=(^ eld)
          ::
          ::  deed data must be identical
          ::
          ?>  =(dat.new dat.u.eld)
          ::
          ::  sow: all new signatures
          ::
          =+  sow=`(list (trel ship life @))`~(tap by syg.new)
          |-  ^-  (pair (list change) cert)
          ?~  sow  [~ u.eld]
          ::
          ::  mor: all further edits
          ::  och: old signature for this signer
          ::
          =+  mor=$(sow t.sow)
          =+  och=(~(get by syg.q.mor) p.i.sow)
          ::
          ::  ignore obsolete/equivalent signature
          ::
          ?.  |(?=($~ och) (gth q.i.sow p.u.och))
            mor
          ::
          ::  verify and merge added signature
          ::
          ?>  (grow-like [p q]:i.sow ash r.i.sow)
          :_  q.mor(syg (~(put by syg.q.mor) p.i.sow [q r]:i.sow))
          :_  p.mor
          `change`[%fact rex vie num `growth`[%sign [[p q] r]:i.sow]]
        ::
        ::  non-initial deeds must be signed by previous
        ::
        ?>  ?|  ?=($~ pre)
                =+  laz=(~(got by syg.new) rex)
                ?>  =(p.laz (dec num))
                (grow-lick pub.u.pre ash q.laz)
            ==
        ::
        ::  initial fingerprint for galaxy is hardcoded
        ::
        ?>  ?|  !=(%czar mir)
                !=(~ pre)
                ~|  [%czar (shaf %zeno pub.dat.new) (zeno rex)]
                =((shaf %zeno pub.dat.new) (zeno rex))
            ==
        ::
        ::  check the parent has signed, if necessary
        ::
        ?>  ?|  ::
                ::  no parent signature for existing, non-moon urbits
                ::
                ?&  ?=(^ pre)
                    =(dad.doc.u.pre dad)
                    !=(%earl mir)
                ==
                ::
                ::  no parent signature for initial galaxy
                ::
                ?&  =(%czar mir)
                    =(~ pre)
                ==
                ::
                ::  the deed is homemade or sent by owner
                ::
                &(?=(^ vie) |(?=($~ u.vie) =(u.u.vie rex)))
                ::
                ::  check valid parent signature
                ::
                =+  par=(~(got by syg.new) dad)
                (grow-like [dad p.par] ash q.par)
            ==
        =-  [[%fact rex p.- num %step q.-]~ q.-]
        ^-  (pair (unit (unit ship)) cert)
        ::
        ::  the new deed is complete; report it
        ::
        ?:  (~(has by syg.new) dad)
          [vie new]
        ::
        ::  the new deed needs a parent signature; try to add it
        ::
        :-  [~ ~]
        ::
        ::  pev: life and ring of parent
        ::  val: new signature
        ::
        =/  pev  (leak dad)
        =*  val  (sign:as:(nol:nu:crub q.pev) *@ ash)
        new(syg (~(put by syg.new) dad [p.pev val]))
  --  --
    --
++  neon
  |=  our/ship
  ^-  (vane task gift sign note state state)
  =|  lex/state
  |%
  ++  load  |=(state +>)
  ++  stay  lex
  ++  plow
    =|  $:  now/@da
            eny/@e
            sky/roof
        ==
    |%
    ++  doze  ~
    ++  peek
      |=  $:  lyc/(unit (set ship))
              car/term
              bem/beam
          ==
      ^-  (unit (unit (cask vase)))
      ::
      ::  XX: needs review
      ::
      ?.  &(=(p.bem our) =(r.bem `case`[%da now]))  ~
      %-  some
      ?.  =(%$ car)  ~
      %+  bind  (~(scry of [now eny] lex) q.bem s.bem)
      |=(a/gilt [-.a (slot 3 (spec !>(a)))])
    ::
    ++  spin
      =|  $:  hen/duct
              moz/(list (pair duct (wind note gift)))
          ==
      |%
      ++  call  
        |=  tac/task
        ^+  +>
        =^  did  lex  abet:(~(call of [now eny] lex) hen tac)
        +>.$(moz (weld moz did))
      ::
      ++  take  
        |=  {tea/wire hin/sign}
        +>
      --
    --
  --
--
::                                                      ::::
::::                    #  vane                         ::  interface
  ::                                                    ::::
::
::  lex: all durable %jael state
::
=|  lex/state
|=  $:  ::
        ::  now: current time
        ::  eny: unique entropy
        ::  ski: namespace resolver
        ::
        now/@da
        eny/@e
        ski/sley
    ==
|%
::                                                      ::  ++call
++  call                                                ::  request
  |=  $:  ::  hen: cause of this event
          ::  hic: event data
          ::
          hen/duct
          hic/(hypo (hobo task))
      ==
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard task) p.q.hic)))
  ^-  {p/(list move) q/_..^$}
  =^  did  lex  abet:~(call of [now eny] lex)
  [did ..^$]
::                                                      ::  ++doze
++  doze                                                ::  await
  |=  $:  ::  now: current time
          ::  hen: cause (XX why we need this?)
          ::
          now/@da
          hen/duct
      ==
  ^-  (unit @da)
  ~
::                                                      ::  ++load
++  load                                                ::  upgrade
  |=  $:  ::  old: previous state
          ::
          old/state
      ==
  ^+  ..^$
  ..^$(lex old)
::                                                      ::  ++scry
++  scry                                                ::  inspect
  |=  $:  ::  fur: event security
          ::  ren: access mode
          ::  why: owner
          ::  syd: desk (branch)
          ::  lot: case (version)
          ::  tyl: rest of path
          ::
          fur/(unit (set monk))
          ren/@tas
          why/shop
          syd/desk
          lot/coin
          tyl/spur
      ==
  ^-  (unit (unit cage))
  ~
::                                                      ::  ++stay
++  stay                                                ::  preserve
  lex
::                                                      ::  ++take
++  take                                                ::  accept
  |=  $:  ::  tea: order
          ::  hen: cause
          ::  hin: result
          ::
          tea/wire
          hen/duct
          hin/(hypo sign-arvo)
      ==
  ^-  {p/(list move) q/_..^$}
  [~ ..^$]
--
