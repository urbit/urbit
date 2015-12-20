::                                                      ::  ::
::::  /hoon#oct3/ape                                    ::::::  dependencies
  ::                                                    ::  ::
/?  310                                                 ::  arvo version
/-  sole, oct3                                          ::  structures
/+  sole, oct3                                          ::  libraries
[. sole oct3]                                           ::  ::
::                                                      ::  ::
::::                                                    ::::::  interfaces
  !:                                                    ::  ::
=>  |%                                                  ::
    ++  axle  {eye+face gam+game}                       ::  agent state
    ++  card  $%  {$diff lime}                          ::  update
                  {$quit $~}                            ::  cancel
              ==                                        ::
    ++  face  (map bone sole-share)                     ::  console state
    ++  lime  $%  {$sole-effect sole-effect}            ::  :sole update
                  {$oct3-update play}                   ::  :oct3 update
              ==                                        ::
    ++  move  (pair bone card)                          ::  cause and action
    --                                                  ::
::                                                      ::  ::
::::                                                    ::::::  past state
  ::                                                    ::  ::
=>  |%                                                  ::  
    ++  axon    $%({$1 axle} {$0 axle-0})               ::  all states
    ++  axle-0  {eye+face gam+game-0}                   ::  old axle
    ++  game-0  {who+? box+board boo+board}             ::  old game
    ++  wake    |=  axon  :-  %1  ?-  +<-  $1  +<+      ::  coarse upgrade
                $0  [eye [who ~^~ ~ box boo]:gam]:+<+   ::
    ==  --                                              ::
::                                                      ::  ::
::::                                                    ::::::  parsers
  ::                                                    ::  ::
=>  |%                                                  ::
    ++  colm  (cook |=(a+@ (sub a '1')) (shim '1' '3')) ::  row or column
    ++  come  ;~(plug colm ;~(pfix fas colm))           ::  coordinate
    ++  cope  |=(? ?:(+< (stag %| (cold ~ sig)) come))  ::  with wait mode
    --                                                  ::
::                                                      ::  ::
::::                                                    ::::::  process core
  ::                                                    ::  ::
|_  $:  bowl                                            ::
        moz+(list move)                                 ::  pending actions
        {$1 axle}                                       ::  process state v1
    ==                                                  ::
::                                                      ::  ::
::::                                                    ::::::  process tools
  ::                                                    ::  ::
++  abet  [(flop moz) .(moz ~)]                         ::  resolve
++  bike  $+(_. _+>)                                    ::  self-transformer
++  dish  |=(cad+card %_(+> moz [[ost cad] moz]))       ::  request
++  echo  |=  {all+(list sink) fun+bike}  =+  old=+>+<- ::  publish to all
          |-  ^+  +>.^$  ?~  all  +>.^$(+<- old)        ::
          =>  .(ost p.i.all, src q.i.all)               ::
          $(all t.all, +>.^$ (fun +>.^$))               ::
++  eels  (~(tap by sup))                               ::  all clients
++  elfs  (prey /oct3 +<-)                              ::  network clients
++  elks  (prey /sole +<-)                              ::  console clients
++  flap  |=  {net+bike con+bike}                       ::  update all clients
          (echo:(echo elks con) elfs net)               ::
++  here  ~(. go src gam)                               ::  game core
::                                                      ::  ::
::::                                                    ::::::  server logic
  ::                                                    ::  ::
++  fail  ?:(soul (fect %bel ~) ~|(%invalid-move !!))   ::  user error
++  fect  |=(sole-effect (dish %diff %sole-effect +<))  ::  update console
++  fact  |=(play (dish %diff %oct3-update +<))         ::  update partner
++  hail  |=(? tame(gam (hey:here +<)))                 ::  toggle subscriber
++  heal  |=  old+axon  =.  +>+<+>  (wake old)          ::  complete update
          =-  +>.$(gam -)  ?.  !=(1 +<-)  gam           ::
          (muy:here (turn eels |=(sink q)))             ::
++  kick  |=  point  =^  dud  gam  ~(m at:here +<)      ::
          ?.(dud fail wild:kind)                        ::
++  kind  =+(res:here ?~(- + (word(gam new:here) ->)))  ::  move result
++  prom  (fect %pro %& %oct3 voy:here)                 ::  update prompt
++  rend  (turn `wall`tab:here |=(tape txt#+<))         ::  table print
++  sawn  (hail(eye (~(del by eye) ost)) |)             ::  console unsubscribe
++  seen  (hail(eye (~(put by eye) ost *sole-share)) &) ::  console subscribe
++  show  prom:(fect %mor rend)                         ::  update console
++  soul  =+((~(get by sup) ost) ?=([~ * %sole *] -))   ::  is console
++  tame  (flap |=(_. (fact:+< &/gam)) |=(_. prom:+<))  ::  light update
++  wild  (flap |=(_. (fact:+< &/gam)) |=(_. show:+<))  ::  full update
++  word  |=  txt+tape  %+  flap                        ::  game message
          |=(_+> (fact:+< |/txt))                       ::
          |=(_+> (fect:+< txt#txt))                     ::
::                                                      ::  ::
::::                                                    ::::::  console UI
  ::                                                    ::  ::
++  work                                                ::  console action
  |=  act+sole-action                                   ::  
  =+  say=(~(got by eye) ost)                           ::
  |^  ?+(-.act abet $det (delt +.act), $ret dive)       ::
  ++  abet  ..work(eye (~(put by eye) ost say))         ::  resolve
  ++  cusp  (cope !ept:here)                            ::  parsing rule
  ++  delt  |=  cal+sole-change                         ::  edit command line
            =^  cul  say  (~(remit sole say) cal good)  ::
            ?~(cul abet fail:(fect:abet det#u.cul))     ::
  ++  dive  =+  (rust (tufa buf.say) (punt come))       ::  apply command line
            ?~(- fail ?~(-> show (kick:wipe ->+)))      ::
  ++  good  |=((list @c) -:(rose (tufa +<) cusp))       ::  validate input
  ++  wipe  =^  cal  say  (~(transmit sole say) set#~)  ::  clear line
            (fect:abet %det cal)                        ::
  --                                                    ::
::                                                      ::  ::
::::                                                    ::::::  arvo handlers
  ::                                                    ::  ::
++  peer-oct3  |=(* abet:tame:(hail &))                 ::  urbit subscribe
++  peer-sole  |=(* abet:show:seen)                     ::  console subscribe
++  poke-sole-action  |=(sole-action abet:(work +<))    ::  console input
++  poke-oct3-move  |=(point abet:wild:(kick +<))       ::  urbit move
++  prep  |=  (unit (pair (list move) axon))            ::  update self
          abet:?~(+< +> wild:(heal +<+>))               ::
++  pull-oct3  |=(* abet:(hail |))                      ::  urbit unsubscribe
++  pull-sole  |=(* abet:sawn)                          ::  console unsubscribe
--
