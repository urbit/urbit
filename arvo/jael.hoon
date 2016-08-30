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
      nix/jael-subjective                               ::  all derived state
  ==                                                    ::
++  jael-objective                                      ::  all universal state
  $:  urb/jael-urbit                                    ::  all urbit state
      web/(map @ta jael-domain)                         ::  all DNS state
  ==                                                    ::
++  jael-domain                                         ::  per foreign app
  $:  sec/(map @t jael-app)                             ::  security tokens
  ==                                                    ::
++  jael-app                                            ::  local app
  $:  key/(unit (pair @da @))                           ::  API key
      tok/(map @t jael-web-token)                       ::  token by username
  ==                                                    ::
++  jael-web-token                                      ::  per-user secrets
  $:  pas/(unit @t)                                     ::  password
  ==                                                    ::
++  jael-urbit                                          ::  objective urbit
  $:  pub/gree                                          ::  all public state
      pry/(map ship jael-ship)                          ::  all private state
  ==                                                    ::
++  jael-ship                                           ::  objective by ship
  $:  ney/(map ship life)                               ::  neighborhood
      lab/(map ship (nap jael-right))                   ::  commitments
      own/(map life ring)                               ::  private keys
  ==                                                    ::
++  jael-right                                          ::  urbit commitment
  $%  {$block p/pile}                                   ::  address block
      {$email p/(set @ta)}                              ::  email addresses
      {$entry p/(map hand (pair @da code))}             ::  symmetric keys
      {$final p/(map ship @uvG)}                        ::  tickets
      {$fungi p/(map term @ud)}                         ::  fungibles
      {$hello p/(set term)}                             ::  usernames
      {$vague p/(map term *)}                           ::  extended 
  ==                                                    ::
++  jael-task                                           ::  operations on
  $%  {$give p/ship q/(nap jael-right)}                 ::  issue rights to
      {$line p/ship q/@da r/code}                       ::  outbound symkey
      {$link p/ship q/@da r/code}                       ::  inbound symkey
      {$meet p/gree}                                    ::  integrate truth
      {$over p/ship q/jael-task}                        ::  mirror operation
      {$ring p/ring}                                    ::  update private key
      {$take p/ship q/(nap jael-right)}                 ::  revoke rights to
      {$view p/ship}                                    ::  watch urbit
      {$west p/ship q/path r/*}                         ::  remote request
      {$wkey p/@ta q/@t r/(unit (pair @da @))}          ::  set API key
      {$wtok p/@ta q/@t r/@t s/(unit (pair @da @))}     ::  set API token
      {$wvue p/@ta}                                     ::  watch website
  ==                                                    ::
++  jael-gift                                           ::  output
  $%  {$clue p/pipe}                                    ::  secure channel
      {$wclu p/jael-domain}                             ::  secure channel
  ==                                                    ::
++  jael-message                                        ::  p2p message
  $%  {$wake (each (pair rank @ud) (list @p))}          ::  activate
      {$hail p/(nap jael-right)}                        ::  update rights
      {$germ p/gree}                                    ::  propagate
      {$ping $~}                                        ::  ping
  ==                                                    ::
++  jael-subjective                                     ::  derived state
  $:  lam/(map ship duct)                               ::  urbit observers
      haz/(map ship (nap jael-right))                   ::  commmitments to us
      nem/(map term ship)                               ::  usernames issued
      red/(map @ta ship)                                ::  emails issued
  ==                                                    ::
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
      !!
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
      |=  {fur/(unit (set monk)) ren/@tas who/ship syd/desk lot/coin tyl/path}
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
++  foo  %bar
--
