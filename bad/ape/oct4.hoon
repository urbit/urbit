::                                                      ::  ::
::::  /hoon+oct4/ape                                    ::::::  dependencies
  ::                                                    ::  ::
/?  310                                                 ::  arvo version
/-  sole, oct4                                          ::  structures
/+  sole, oct4                                          ::  libraries
[. sole oct4]                                           ::  ::
::                                                      ::  ::
::::                                                    ::::::  interfaces
  !:                                                    ::  ::
=>  |%                                                  ::
    ++  axle  {eye/face rem/(unit ship) gam/game}       ::  agent state
    ++  card  $%  {$diff lime}                          ::  update
                  {$quit $~}                            ::  cancel
                  {$peer wire dock path}                ::  subscribe
                  {$poke wire dock pear}                ::  send move
                  {$pull wire dock $~}                  ::  unsubscribe
              ==                                        ::
    ++  face  (map bone sole-share)                     ::  console state
    ++  lime  $%  {$sole-effect sole-effect}            ::  :sole update
                  {$oct4-update play}                   ::  :oct4 update
              ==                                        ::
    ++  move  (pair bone card)                          ::  cause and action
    ++  pear  {$oct4-move point}                        ::  outgoing move
    --                                                  ::
::                                                      ::  ::
::::                                                    ::::::  past state
  ::                                                    ::  ::
=>  |%                                                  ::  
    ++  agon    (unit {(list move) axon})               ::  boot argument
    ++  axon     $%({$1 axle} {$0 axle-0})              ::  all states
    ++  axle-0  {eye/face gam/game-0}                   ::  old axle
    ++  game-0  {who/? box/board boo/board}             ::  old game
    ++  wake    |=  axon  :-  %1  ?-  +<-  $1  +<+      ::  coarse upgrade
                $0  [eye ~ [who ~^~ ~ box boo]:gam]:+<+ ::
    ==  --                                              ::
::                                                      ::  ::
::::                                                    ::::::  parsers
  ::                                                    ::  ::
=>  |%                                                  ::
    ++  colm  (cook |=(a/@ (sub a '1')) (shim '1' '3')) ::  row or column
    ++  come  ;~(plug colm ;~(pfix fas colm))           ::  coordinate
    ++  comb  (pick come ;~(pfix sig (punt fed:ag)))    ::  all command input
    ++  cope  |=(? ?:(+< (stag %| (cold ~ sig)) comb))  ::  with wait mode
    --                                                  ::
::                                                      ::  ::
::::                                                    ::::::  process core
  ::                                                    ::  ::
|_  $:  bowl                                            ::  
        moz/(list move)                                 ::  pending actions
        {$1 axle}                                       ::  process state v1
    ==                                                  ::
::                                                      ::  ::
::::                                                    ::::::  process tools
  ::                                                    ::  ::
++  abet  [(flop moz) .(moz ~)]                         ::  resolve
++  bike  $+(_. _+>)                                    ::  self-transformer
++  dish  |=(cad/card %_(+> moz [[ost cad] moz]))       ::  request
++  done  (echo eels |=(_. (dish:+< %quit ~)))          ::  cancel everyone
++  echo  |=  {all/(list sink) fun/bike}  =+  old=+>+<- ::  publish to all
          |-  ^+  +>.^$  ?~  all  +>.^$(+<- old)        ::
          =>  .(ost p.i.all, src q.i.all)               ::
          $(all t.all, +>.^$ (fun +>.^$))               ::
++  eels  (~(tap by sup))                               ::  all clients
++  elfs  (prey /oct4 +<-)                              ::  network clients
++  elks  (prey /sole +<-)                              ::  console clients
++  emit  |=(lime (dish %diff +<))                      ::  publish
++  flap  |=  {net/bike con/bike}                       ::  update all clients
          (echo:(echo elks con) elfs net)               ::
++  here  ~(. go src gam)                               ::  game core
::                                                      ::  ::
::::                                                    ::::::  server logic
  ::                                                    ::  ::
++  fail  ?:(soul (fect %bel ~) ~|(%invalid-move !!))   ::  user error
++  fect  |=(sole-effect (emit %sole-effect +<))        ::  update console
++  fact  |=(play (emit %oct4-update +<))               ::  update partner
++  hail  |=(? ?^(rem +> tame(gam (hey:here +<))))      ::  toggle subscriber
++  harp  |=(game ?:(=(gam +<) +> wild(gam +<)))        ::  update game
++  heal  |=  old/axon  =.  +>+<+>  (wake old)          ::  complete update
          =-  +>.$(gam -)  ?.  !=(1 +<-)  gam           ::
          (muy:here (turn eels |=(sink q)))             ::
++  hear  |=(play ?-(+<- $| (word +<+), $& (harp +<+))) ::  network update
++  kick  |=  point  =^  dud  gam  ~(m at:here +<)      ::
          ?.(dud fail wild:?~(rem kind (send +>-)))     ::
++  kind  =+(res:here ?~(- + (word(gam new:here) ->)))  ::  move result
++  plan  |=  (unit ship)  ?~  +<  stop(gam new:here)   ::  link+unlink
          ?^(rem fail link(rem +<))                     ::
++  plot  |=  (each point (unit ship))                  ::  apply command
          ?-(+<- $& (kick +<+), $| (plan +<+))          ::
++  like  |*(* [/oct4 [+.rem dap] +<])                  ::  friend message
++  link  (dish peer+(like /oct4))                      ::  subscribe to friend
++  prom  (fect %pro %& %oct4 stat)                     ::  update prompt
++  rend  (turn `wall`tab:here |=(tape txt++<))         ::  table print
++  sawn  (hail(eye (~(del by eye) ost)) |)             ::  console unsubscribe
++  seen  (hail(eye (~(put by eye) ost *sole-share)) &) ::  console subscribe
++  send  |=(point (dish poke+(like %oct4-move +<)))    ::  send move
++  show  prom:(fect %mor rend)                         ::  update console
++  soul  =+((~(get by sup) ost) ?=([~ * %sole *] -))   ::  is console
++  stat  (weld ?~(rem ~ "@{(scow p+u.rem)}") voy:here) ::  prompt line
++  stop  ?~(rem done wild:(dish pull+(like ~)))        ::  unsubscribe
++  tame  (flap |=(_. (fact:+< &/gam)) |=(_. prom:+<))  ::  light update
++  wild  (flap |=(_. (fact:+< &/gam)) |=(_. show:+<))  ::  full update
++  with  |=(? (word(rem ?:(+< rem ~)) "{<[+< src]>}")) ::  
++  word  |=  txt/tape  %+  flap                        ::  game message
          |=(_+> (fact:+< |/txt))                       ::
          |=(_+> (fect:+< txt+txt))                     ::
::                                                      ::  ::
::::                                                    ::::::  console UI
  ::                                                    ::  ::
++  work                                                ::  console action
  |=  act/sole-action                                   ::  
  =+  say=(~(got by eye) ost)                           ::
  |^  ?+(-.act abet $det (delt +.act), $ret dive)       ::
  ++  abet  ..work(eye (~(put by eye) ost say))         ::  resolve
  ++  cusp  (cope !ept:here)                            ::  parsing rule
  ++  delt  |=  cal/sole-change                         ::  edit command line
            =^  cul  say  (~(remit sole say) cal good)  ::
            ?~(cul abet fail:(fect:abet det+u.cul))     ::
  ++  dive  =+  (rust (tufa buf.say) (punt comb))       ::  apply command line
            ?~(- fail ?~(-> show (plot:wipe ->+)))      ::
  ++  good  |=((list @c) -:(rose (tufa +<) cusp))       ::  validate input
  ++  wipe  =^  cal  say  (~(transmit sole say) set+~)  ::  clear line
            (fect:abet %det cal)                        ::
  --                                                    ::
::                                                      ::  ::
::::                                                    ::::::  arvo handlers
  ::                                                    ::  ::
++  reap-oct4  |=({* (unit)} abet:(with =(~ +<+)))      ::  linked to friend
++  coup-oct4  |=({* (unit)} abet:?~(+<+ +> fail))      ::  move acknowledge
++  diff-oct4-update  |=({* play} abet:(hear +<+))      ::  network update
++  peer-oct4  |=(* abet:tame:(hail &))                 ::  urbit subscribe
++  peer-sole  |=(* abet:show:seen)                     ::  console subscribe
++  poke-sole-action  |=(sole-action abet:(work +<))    ::  console input
++  poke-oct4-move  |=(point abet:wild:(kick +<))       ::  urbit move
++  prep  |=(agon abet:?~(+< +> (heal +<+>)))           ::  load state
++  pull-oct4  |=(* abet:(hail |))                      ::  urbit unsubscribe
++  pull-sole  |=(* abet:sawn)                          ::  console unsubscribe
++  quit-oct4  |=(* abet:?~(rem +> wild(rem ~)))        ::  unlinked by friend
--
