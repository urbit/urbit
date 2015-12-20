::                                                      ::  ::
::::  /hoon#oct2/ape                                    ::::::  dependencies
  ::                                                    ::  ::
/?  310                                                 ::  arvo version
/-  sole, oct2                                          ::  structures
/+  sole, oct2                                          ::  libraries
[. sole oct2]                                           ::  ::
::                                                      ::  ::
::::                                                    ::::::  interfaces
  !:                                                    ::  ::
=>  |%                                                  ::
    ++  axle  {eye+face gam+game}                       ::  agent state
    ++  card  $%  {$diff lime}                          ::  update
                  {$quit $~}                            ::  cancel
              ==                                        ::
    ++  face  (map bone sole-share)                     ::  console state
    ++  lime  {$sole-effect sole-effect}                ::  :sole update
    ++  move  (pair bone card)                          ::  cause and action
    --                                                  ::
::                                                      ::  ::
::::                                                    ::::::  past state
  ::                                                    ::  ::
=>  |%                                                  ::  
    ++  axon    $%({$1 axle} {$0 axle-0})               ::  all states
    ++  axle-0  {eye+face gam+game-0}                   ::  old axle
    ++  game-0  {who+? box+board boo+board}             ::  old game
    ++  wake    |=  axon  :-  %1  ?-  +<-  $1  +<+      ::  rough upgrade
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
++  flap  |=(con+bike (echo (~(tap by sup)) con))       ::  update all clients
++  here  ~(. go src gam)                               ::  game core
::                                                      ::  ::
::::                                                    ::::::  server logic
  ::                                                    ::  ::
++  fail  (fect %bel ~)                                 ::  user error
++  fect  |=(sole-effect (dish %diff %sole-effect +<))  ::  update console
++  heal  |=  old+axon  =.  +>+<+>  (wake old)          ::  complete update
          =-  +>.$(gam -)  ?.  !=(1 +<-)  gam           ::
          (muy:here (turn (~(tap by sup)) |=(sink q)))  ::
++  kick  |=  point  =^  dud  gam  ~(m at:here +<)      ::
          ?.(dud fail wild:kind)                        ::
++  kind  =+(res:here ?~(- + (word(gam new:here) ->)))  ::  move result
++  hail  |=(? tame(gam (hey:here +<)))                 ::  toggle subscriber
++  prom  (fect %pro %& %oct2 voy:here)                 ::  update prompt
++  rend  (turn `wall`tab:here |=(tape txt#+<))         ::  table print
++  sawn  (hail(eye (~(del by eye) ost)) |)             ::  console unsubscribe
++  seen  (hail(eye (~(put by eye) ost *sole-share)) &) ::  console subscribe
++  show  prom:(fect %mor rend)                         ::  update console
++  tame  (flap |=(_. prom:+<))                         ::  light update
++  wild  (flap |=(_. show:+<))                         ::  full update
++  word  |=(tape (flap |=(_+> (fect:+< txt#+>+<))))    ::
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
++  peer-sole  |=(* abet:show:seen)                     ::  console subscribe
++  poke-sole-action  |=(sole-action abet:(work +<))    ::  console input
++  prep  |=  (unit (pair (list move) axon))            ::  update self
          abet:?~(+< +> wild:(heal +<+>))               ::
++  pull-sole  |=(* abet:sawn)                          ::  console unsubscribe
--
