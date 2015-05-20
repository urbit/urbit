::                                                      ::  ::
::::  /hook/core/acto/ape                               ::  ::  dependencies
  ::                                                    ::  ::
/-  *sole                                               ::  structures
/+  sole                                                ::  libraries
::                                                      ::  ::
::::                                                    ::  ::  structures
  !:                                                    ::  ::
=>  |%                                                  ::  board logic
    ++  board  ,@                                       ::  one-player bitfield
    ++  point  ,[x=@ y=@]                               ::  coordinate
    ++  game   ,[who=? box=board boo=board]             ::  game state
    ++  icon   |=(? ?:(+< 'X' 'O'))                     ::  display at
    ++  bo                                              ::  per board
      |_  bud=board                                     ::
      ++  get  |=(point =(1 (cut 0 [(off +<) 1] bud)))  ::  get point
      ++  off  |=(point (add x (mul 3 y)))              ::  bitfield address
      ++  set  |=(point (con bud (bex (off +<))))       ::  set point
      ++  win  %-  lien  :_  |=(a=@ =(a (dis a bud)))   ::  test for win
               (rip 4 0wl04h0.4A0Aw.4A00s.0e070)        ::  with bitmasks
      --                                                ::
    ++  go                                              ::  per game
      |_  game                                          ::
      ++  at  |_  point                                 ::  per point
              ++  g  +>+<                               ::  game
              ++  k  !|(x o)                            ::  ok move
              ++  m  ?.(k [| g] [& g:t:?:(who y p)])    ::  move
              ++  o  (~(get bo boo) +<)                 ::  old at o
              ++  p  .(boo (~(set bo boo) +<))          ::  play at o
              ++  t  .(who !who)                        ::  take turn
              ++  v  ?:(x (icon &) ?:(o (icon |) '.'))  ::  view
              ++  x  (~(get bo box) +<)                 ::  old at x
              ++  y  .(box (~(set bo box) +<))          ::  play at x
              --                                        ::
      ++  res  ?:  ~(win bo box)  `"{~[(icon &)]} wins" ::  result
               ?:  ~(win bo boo)  `"{~[(icon |)]} wins" ::
               ?:  =(511 (con boo box))  `"tie :-("  ~  ::
      ++  row  |=  y=@   :~  (add y '1')                ::  print row
                   ' '  ~(v at y 0)                     ::
                   ' '  ~(v at y 1)                     ::
                   ' '  ~(v at y 2)                     ::
               ==                                       ::
      ++  tab  ~["+ 1 2 3" (row 0) (row 1) (row 2)]     ::  print table
      --                                                ::
    --                                                  ::
::                                                      ::  ::
::::                                                    ::  ::  server
  ::                                                    ::  ::
=>  |%                                                  ::  arvo structures
    ++  axle  ,[%1 eye=face but=tube gam=game]          ::  agent state
    ++  axon  $?(axle [%0 eye=face gam=game])           ::  historical state
    ++  card  $%  [%diff lime]                          ::  update
                  [%quit ~]                             ::  cancel
                  [%peer wire dock path]                ::  subscribe
                  [%pull wire dock ~]
              ==                                        ::
    ++  face  (pair (list ,@c) (map bone sole-share))   ::  interface
    ++  lime  $%  [%sole-effect sole-effect]            ::  :sole update
                  [%octo-game game]                     ::  :octo update
              ==                                        ::
    ++  move  (pair bone card)                          ::  cause and action
    ++  mote  (pair ship ,?)                            ::  remote binding
    ++  tube  (unit (pair ,? mote))                     ::  alive, remote
    --                                                  ::
=>  |%                                                  ::  parsers
    ++  colm  (cook |=(a=@ (sub a '1')) (shim '1' '3')) ::  row or column
    ++  come  ;~(plug colm ;~(pfix fas colm))           ::  coordinate
    ++  comb  (pick come ;~(pfix sig (punt comp)))      ::  all command input
    ++  comp  ;~(plug fed:ag ;~(pfix ace (flag %x %o))) ::  login command
    ++  cope  |=(? ?:(+< (stag %| (cold ~ sig)) comb))  ::  with wait mode
    --                                                  ::
|_  [hid=hide moz=(list move) axle]                     ::  per agent
++  et                                                  ::
  |_  [from say=sole-share]                             ::  per console client
  ++  abet  +>(q.eye (~(put by q.eye) ost say))         ::  continue
  ++  amok  +>(q.eye (~(del by q.eye) ost))             ::  discontinue
  ++  beep  (emit %bel ~)                               ::  bad user
  ++  cusp  (cope wait)                                 ::  parsing rule
  ++  delt  |=  cal=sole-change                         ::  input line change
            =^  cul  say  (remit:sole cal good)         ::
            ?~  cul  (park:abet(p.eye buf.say) | ~)     ::
            abet:beep:(emit det/u.cul)                  ::
  ++  emit  |=  fec=sole-effect  ^+  +>                 ::  send effect
            +>(moz [[ost %diff %sole-effect fec] moz])  ::  
  ++  emil  |=  fex=(list sole-effect)                  ::  send effects
            ?~(fex +> $(fex t.fex, +> (emit i.fex)))    ::
  ++  good  |=((list ,@c) -:(rose (tufa +<) cusp))      ::  valid input
  ++  kick  |=  point                                   ::  move command
            =^  dud  gam  ~(m ~(at go gam) +<)          ::
            ?.  dud  abet:beep  =+  mus=~(res go gam)   ::
            (park:abet(gam ?^(mus *game gam)) %2 mus)   ::
  ++  line  =^  cal  say  (transmit:sole set/p.eye)     ::  update command
            (emit %det cal)                             ::
  ++  make  =+  dur=(rust (tufa p.eye) comb)            ::
            ?~  dur  abet:beep                          ::
            =.  +  line(p.eye ~)                        ::
            ?-(+<.dur & (kick +>.dur), | (plan +>.dur)) ::
  ++  mean  |=((unit tape) ?~(+< +> (emit txt/+<+)))    ::  optional message
  ++  play  |=  lev=?(%0 %1 %2)                         ::  update by level
            ?-(lev %0 +>, %1 line, %2 line:show:prom)   ::
  ++  plow  |=  [lev=?(%0 %1 %2) mus=(unit tape)]       ::  complete print
            abet:(mean:(play lev) mus)                  ::
  ++  prom  %^  emit  %pro  %&  :-  %octo               ::  update prompt
            ?:  wait  "(their turn) "                   ::
            ": {~[(icon who.gam)]} to move (row/col): " ::  
  ++  plan  |=  mut=(unit mote)                         ::  peer command
            ?~  mut  ?~(but abet:beep stop:abet)        ::
            ?^(but abet:beep (link:abet u.mut))         ::
  ++  rend  (turn `wall`~(tab go gam) |=(tape txt/+<))  ::  table print
  ++  show  (emit %mor rend)                            ::  update board
  ++  sole  ~(. cs say)                                 ::  console library
  ++  wait  &(?=(^ but) !=(q.q.u.but who.gam))          ::  waiting turn
  ++  work  |=  act=sole-action                         ::  console input
            ?:(?=(%det -.act) (delt +.act) make)        ::
  --                                                    ::
++  abet  [(flop moz) .(moz ~)]                         ::  resolve core
++  dump  |=(mov=move %_(+> moz [mov moz]))             ::  send move
++  dish  |=(cad=card (dump 0 cad))                     ::  forward move
++  flet  |=(from ~(. et +< (~(got by q.eye) ost)))     ::  in old client
++  fret  |=(from ~(. et +< *sole-share))               ::  in new client
++  like  |=(xir=ship |*(* [/octo [xir %octo] +<]))     ::  to friend
++  link  |=  mot=mote  %+  dish(but `[| mot])  %peer   ::  subscribe to friend
          ((like p.mot) /octo/net/[?:(q.mot %x %o)])    ::  
++  pals  %+  turn  (pale hid (prix /sole))  |=  sink   ::  per console 
          [[p=p.+< q=q.+<] r=(~(got by q.eye) p.+<)]    ::
++  park  |=  [lev=?(%0 %1 %2) mus=(unit tape)]         ::  update all
          =.  +>  ?:(=(%2 lev) push +>)                 ::  
          =+  pals                                      ::
          |-  ^+  +>.^$  ?~  +<  +>.^$                  ::
          $(+< t.+<, +>.^$ (~(plow et i.+<) lev mus))   ::
++  push  =+  pey=(pale hid (prix /octo))  |-  ^+  +>   ::  update friends
          ?~(pey +> $(pey t.pey, +> (sell p.i.pey)))    ::
++  sell  |=(ost=bone (dump ost %diff %octo-game gam))  ::  update friend
++  stop  (dish(but ~) pull/((like +>-.but) ~))         ::  cancel subscribe
::                                                      :::::::::::::::
::::                                                    ::  ::       ::  hooks
  ::                                                    :::::::::::::::
++  diff-octo-game                                      ::  friend update
          |=  [then gam=game]  =<  abet                 ::
          ?.  &(?=([~ %& *] but) =(src p.q.u.but))  +>  ::
          ?:  =(^gam gam)  +>                           ::
          (park(gam gam) %2 ~)                          ::
++  peer-octo-net                                       ::  urbit peer
          |=  [from pax=path]  =<  abet                 ::
          =+  who==(%x -.pax)                           ::
          ?^  but  (park %2 ~)                          ::
          (park:(link src !who) %2 `"net from {<src>}") ::
++  peer-octo-web                                       ::  web peer
          |=  [from pax=path]  =<  abet                 ::
          ~&  [%peer-web +<]                            ::
          ?^  but  (park %2 ~)                          ::
          %+  park(but `[%& src !who.gam])  %2          ::
          `"web from {<src>}"                           ::
++  peer-sole                                           ::  console subscribe
          |=  [from pax=path]  =<  abet                 ::
          (plow:(fret +<-) %2 ~)                        ::
++  poke-sole-action                                    ::  console input
          |=  [from act=sole-action]  =<  abet          ::
          (work:(flet +<-) act)                         ::
++  poke-octo-move                                      ::
          |=  [from wha=point]  =<  abet                ::
          =^  dud  gam  ~(m ~(at go gam) wha)           ::
          ?>  dud  =+  mus=~(res go gam)                ::
          (park(gam ?^(mus *game gam)) %2 mus)          ::
++  prep  |=  [from old=(unit ,[(list move) axon])]     ::  initialize
          =<  abet  ?~  old  +>                         ::
          =<  (park %2 ~)                               ::
          ?-  -.+>.old                                  ::
            %1  +>(+<+ u.old)                           ::
            %0  +>(eye.+< eye.+>.old, gam.+< gam.+>.old)::
          ==                                            ::
++  pull-octo                                           ::
          |=  [from *]  =<  abet                        ::
          (park(but ~) %2 `"dropped")                   ::
++  pull-sole                                           ::  disconnect console
          |=  [from *]  =<  abet                        ::
          amok:(flet +<-)                               ::
++  quit-octo                                           ::  unlinked by friend
          |=([then ~] abet:(park(but ~) %0 `"removed")) ::
++  reap-octo                                           ::  linked to friend
          |=  [then saw=(unit tang)]  =<  abet          ::
          ?>  ?=([~ %| *] but)                          ::
          ?^  saw  (park:stop %0 `"fail to {<src>}")    ::
          (park(p.u.but %&) %0 `"link to {<src>}")      ::
--
