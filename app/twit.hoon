::  Twitter daemon
::
::::  /hook/core/twit/app
  ::
/+    twitter, talk
::
::::  ~fyr
  ::
|%
++  twit-path                                           ::  valid peer path
  $%  ::  {$home $~}                                         ::  home timeline
      {$user p/@t $~}                                    ::  user's tweets
      {$post p/@taxuv $~}                             ::  status of status
  ==
::
++  axle                                                ::  app state
  $:  $0
      out/(map @uvI (each {knot cord} stat))           ::  sent tweets
      ran/(map path {p/@ud q/@da})                     ::  polls active
      fed/(jar path stat)                               ::  feed cache
  ==
::
++  gift                                                ::  subscription action
  $%  {$quit $~}                                         ::  terminate
      {$diff gilt}                                      ::  send data
  ==
++  gilt  
  $%  {$twit-feed p/(list stat)}                        ::  posts in feed
      {$twit-stat p/stat}                               ::  tweet accepted
      {$ares term (list tank)}
  ==
::
++  move  {bone card}
++  card                                                ::  arvo request
  $?  gift
  $%  {$hiss wire {$~ $~} api-call}                     ::  api request
      {$poke wire dock $talk-command command:talk}      ::
      {$wait wire p/@da}                                ::  timeout
  ==  ==
::
++  api-call  {response-mark $twit-req {endpoint quay}} :: full hiss payload
++  response-mark  ?($twit-status $twit-feed)           :: sigh options
++  sign                                                ::  arvo response
  $%  {$e $thou p/httr}                                 ::  HTTP result
      {$t $wake $~}                                      ::  timeout ping
  ==
::
::  XX =*
++  stat      stat:twitter                              ::  recieved tweet
++  command   command:twitter                           ::  incoming command
++  endpoint  endpoint:reqs:twitter                     ::  outgoing target
++  param  param:reqs:twitter                           ::  twit-req paramters
++  print  print:twitter                                ::  their serialization
++  parse  parse:twitter                                ::  and deserialization
::
:: ++  twit  args:reqs:twitter                             ::  arugment types
:: ++  twir  parse:twitter                                 ::  reparsers
:: ++  twip  print:twitter                                 ::  printers
--
!:
::::
  ::
|_  {bowl axle}
::
++  cull                                                ::  remove seen tweets
  |=  {pax/path rep/(list stat)}  ^+  rep
  =+  pev=(silt (turn (~(get ja fed) pax) |=(stat id)))
  (skip rep |=(stat (~(has in pev) id)))
::
++  done  [*(list move) .]
++  dely                                                ::  next polling timeout
  |=  pax/path
  ^-  {(unit time) _ran}
  =+  cur=(~(get by ran) pax)
  =+  tym=(add now (mul ~s8 (bex ?~(cur 0 p.u.cur))))
  :: ~&  dely/`@dr`(sub tym now)
  ?:  &(?=(^ cur) (gte tym q.u.cur) (gth q.u.cur now))
    [~ ran]
  [`tym (~(put by ran) pax ?~(cur 0 (min 5 +(p.u.cur))) tym)]
::
++  wait                                                ::  ensure poll by path
  |=  {pax/path mof/(list move)}
  =^  tym  ran  (dely pax)
  :_  +>.$
  ?~  tym  
    :: ~&  no-wait/ran
    mof
  :: ~&  will-wait/u.tym
  :-  [ost %wait pax u.tym]
  mof
::
++  poke-twit-do                                        ::  recieve request
  |=  {usr/knot act/command}
  ^+  [*(list move) +>]
  ?-    -.act
      $post
    =:  out  (~(put by out) p.act %& usr q.act)
        ran  (~(del by ran) /peer/home)
      ==
    %+  wait  /peer/home
    =+  req=[%twit-req `endpoint`stat-upda+[%status q.act]~ ~]
    [ost %hiss post+(dray ~[%uv] p.act) `usr %twit-status req]~
  ==
::
++  wake-peer
  |=  {pax/path $~}  ^+  done
  ~&  twit-wake+peer+pax
  :_  +>.$
  ?.  (~(has by ran) peer+pax)                           ::  ignore if retracted
    ~
  =+  =>  |=({a/bone @ b/path} [b a])
      pus=(~(gas ju *(jug path bone)) (turn (~(tap by sup)) .))
  ?~  (~(get ju pus) pax)
    ~
  ~&  peer-again+[pax ran]
  (pear | our pax)
::
::  XX parse from stack trace?
:: ++  sigh-429                          ::  Rate-limit
::   |=  {pax/path hit/httr}
::   =.  ran  (~(put by ran) pax 6 now)
::   =+  lim=%.(%x-rate-limit-reset ;~(biff ~(get by (mo q.hit)) poja ni:jo))
::   =+  tym=?~(lim (add ~m7.s30 now) (add ~1970.1.1 (mul ~s1 u.lim)))
::   ~&  retrying-in+`@dr`(sub tym now)
::   :_(+>.$ [ost %wait pax tym]~)
::
++  sigh-twit-status-post                               ::  post acknowledged
  |=  {wir/wire rep/stat}  ^+  done
  =+  (raid wir mez=%uv ~)
  =.  out  (~(put by out) mez %| rep)
  :_  +>.$
  =+  pax=/[who.rep]/status/(rsh 3 2 (scot %ui id.rep))
  :-  (show-url [& ~ &+/com/twitter] `pax ~)
  (spam pax (tweet-good rep))
::
++  sigh-twit-feed-peer                                 ::  feed data
  |=  {wir/path rep/(list stat)}
  :: ~&  got-feed+[(scag 5 (turn rep |=(stat id))) fed]
  =+  ren=(cull wir rep)                       ::  new messages
  ?~  ren
    (wait peer+wir ~)                              ::  pump polling
  :: ~&  spam-feed+ren
  =:  ran  (~(del by ran) peer+wir)                    ::  clear poll delay
      fed  (~(put by fed) wir rep)              ::  saw last message
    ==
  (wait peer+wir (spam wir [%diff twit-feed+(flop ren)] ~))
  ::
++  sigh-mean                       ::  Err
  |=  {pax/path tan/tang}
  =+  ^-  git/gift
      =+  err='' ::%.(q:(need r.hit) ;~(biff poja mean:twir))  :: XX parse?
      :^  %diff  %ares  %bad-http
      tan
      :: [leaf/"HTTP Code {<p.hit>}" (turn (need err) mean:render:twit)]
  ?+    pax  [[ost git]~ +>.$]
    {$post @ $~}
      [(spam pax git ~) +>.$]
  ==
++  tweet-good  |=(rep/stat `(list gift)`~[[%diff %twit-stat rep] [%quit ~]]) 
++  peer  |=(pax/path :_(+> (pear & src pax)))       ::  accept subscription
++  pear                              ::  poll, possibly returning current data
  |=  {ver/? @ pax/path}
  ^-  (list move)
  ?.  ?=(twit-path pax)
    ~|([%missed-path pax] !!)
  =>  .(pax `twit-path`pax)
  ?:  ?=($post -.pax)
    ?.  ver  ~
    =+  (raid +.pax mez=%uv ~) 
    =+  sta=(~(get by out) mez)
    ?.  ?=({$~ $| ^} sta)                                ::  post not received
      ~
    ~[[ost %diff %twit-stat p.u.sta] [ost %quit ~]]
  =+  ole=(~(get ja fed) pax)
  :_  ^-  (list move)
      ?.  ver  ~
      ?~  ole  ~
      [ost %diff %twit-feed (flop ole)]~
  =-  `move`[ost %hiss peer+pax `~ -]
  ~!  print
  =+  opt=?~(ole ~ ['since_id' (tid:print id.i.ole)]~)
  ?-    -.pax
    $user  [%twit-feed twit-req+[`endpoint`stat-user+[(to-sd p.pax)]~ opt]]
::     %home  [%twit-home stat-home+~ opt]
  ==
::
++  to-sd                                               ::  parse user name/numb
  |=  a/knot  ^-  sd:param
  ~|  [%not-user a]
  %+  rash  a
  ;~(pose (stag %user-id dem) (stag %screen-name user:parse))
::
:: ++  pull                                                ::  release subscription
::   |=  ost/bone
::   ?.  (~(has by sup) ost)  `+>.$      ::  XX should not occur
::   =+  [his pax]=(~(got by sup) ost)
::   ?:  (lth 1 ~(wyt in (~(get ju pus) pax)))
::     `+>.$
::   =:  ran  (~(del by ran) [%peer pax])
::       fed  (~(del by fed) pax)
::     ==
::   `+>.$
::
++  spam                                                ::  send by path
  |=  {a/path b/(list gift)}  ^-  (list move)
  %-  zing  ^-  (list (list move))
  %+  turn  (~(tap by sup))
  |=  {ost/bone @ pax/path}
  ?.  =(pax a)  ~
  (turn b |=(c/gift [ost c]))
::
++  show-url  ~(said-url talk `bowl`+<-)
--
