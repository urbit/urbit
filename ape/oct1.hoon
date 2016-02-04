::                                                      ::  ::
::::  /hoon+oct1/ape                                    ::::::  dependencies
  ::                                                    ::  ::
/?  310                                                 ::  arvo version
/-  sole, oct1                                          ::  structures
/+  sole, oct1                                          ::  libraries
[. sole oct1]                                           ::  ::
::                                                      ::  ::
::::                                                    ::::::  interfaces
  !:                                                    ::  ::
=>  |%                                                  ::
    ++  axon  {$0 eye/face gam/game}                    ::  agent state
    ++  card  {$diff $sole-effect sole-effect}          ::  update
    ++  face  (map bone sole-share)                     ::  console state
    ++  move  (pair bone card)                          ::  cause and action
    --                                                  ::
::                                                      ::  ::
::::                                                    ::::::  parsers
  ::                                                    ::  ::
=>  |%                                                  ::
    ++  colm  (cook |=(a/@ (sub a '1')) (shim '1' '3')) ::  row or column
    ++  come  ;~(plug colm ;~(pfix fas colm))           ::  coordinate
    --                                                  ::
::                                                      ::  ::
::::                                                    ::::::  process core
  ::                                                    ::  ::
|_  $:  bowl                                            ::  system state
        moz/(list move)                                 ::  pending actions
        axon                                            ::  server state, v0
    ==                                                  ::
::                                                      ::  ::
::::                                                    ::::::  process tools
  ::                                                    ::  ::
++  abet  [(flop moz) .(moz ~)]                         ::  resolve
++  bike  $-(_. _+>)                                    ::  self-transformer
++  dish  |=(cad/card %_(+> moz [[ost cad] moz]))       ::  request
++  eels  (~(tap by sup))                               ::  all clients
++  echo  |=  {all/(list sink) fun/bike}  =+  old=+>+<- ::
          |-  ^+  +>.^$  ?~  all  +>.^$(+<- old)        ::
          =.  ost  p.i.all                              ::
          $(all t.all, +>.^$ (fun +>.^$))               ::
++  flap  |=(con/bike (echo eels con))                  ::  update all clients
++  here  ~(. go gam)                                   ::  game core
::                                                      ::  ::
::::                                                    ::::::  process logic
  ::                                                    ::  ::
++  fail  (fect %bel ~)                                 ::  user error
++  fect  |=(sole-effect (dish %diff %sole-effect +<))  ::  update console
++  kick  |=  point  =^  dud  gam  ~(m at:here +<)      ::
          ?.(dud fail wild:kind)                        ::
++  kind  =+(res:here ?~(- + (word(gam new:here) ->)))  ::  move result
++  prom  (fect %pro %& %oct1 voy:here)                 ::  update prompt
++  rend  (turn `wall`tab:here |=(tape txt++<))         ::  table print
++  sawn  .(eye (~(del by eye) ost))                    ::  console unsubscribe
++  seen  .(eye (~(put by eye) ost *sole-share))        ::  console subscribe
++  show  prom:(fect %mor rend)                         ::  update console
++  tame  (flap |=(_. prom:+<))                         ::  light update
++  wild  (flap |=(_. show:+<))                         ::  full update
++  word  |=(tape (flap |=(_+> (fect:+< txt++>+<))))    ::
::                                                      ::  ::
::::                                                    ::::::  process UI
  ::                                                    ::  ::
++  work                                                ::  console action
  |=  act/sole-action                                   ::  
  =+  say=(~(got by eye) ost)                           ::
  |^  ?+(-.act abet $det (delt +.act), $ret dive)       ::
  ++  abet  ..work(eye (~(put by eye) ost say))         ::  resolve
  ++  delt  |=  cal/sole-change                         ::  edit command line
            =^  cul  say  (~(remit sole say) cal good)  ::
            ?~(cul abet fail:(fect:abet det+u.cul))     ::
  ++  dive  =+  (rust (tufa buf.say) (punt come))       ::  apply command line
            ?~(- fail ?~(-> show (kick:wipe ->+)))      ::
  ++  good  |=((list @c) -:(rose (tufa +<) come))       ::  validate input
  ++  wipe  =^  cal  say  (~(transmit sole say) set+~)  ::  clear line
            (fect:abet %det cal)                        ::
  --                                                    ::
::                                                      ::  ::
::::                                                    ::::::  arvo handlers
  ::                                                    ::  ::
++  peer-sole  |=(* abet:show:seen)                     ::  console subscribe
++  prep  |=  (unit (pair (list move) axon))            ::  update self
          abet:?~(+< +> wild(+<+ +<+))                  ::
++  poke-sole-action  |=(sole-action abet:(work +<))    ::  console input
++  pull-sole  |=(* abet:sawn)                          ::  console unsubscribe
--
