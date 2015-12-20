::  Twitter daemon
::
::::  /hook#core#twit#app
  ::
/-    twitter
/+    twitter, talk
::
::::  ~fyr
  ::
[twitter .]
|%
++  twit-path                                           ::  valid peer path
  $%  ::  [%home $~]                                    ::  home timeline
      {$user p/@t $~}                                   ::  user's tweets
      {$post p/span $~}                             ::  status of status
  ==
::
++  axle                                                ::  app state
  $:  $0
      kes/(map span keys:twit-do)                       ::  auth
      out/(map @uvI (each {span cord} stat))          ::  sent tweets
      ran/(map path {p/@ud q/@da})                     ::  polls active
      fed/(jar path stat)                               ::  feed cache
  ==
::
++  gift                                                ::  subscription action
  $%  {$quit $~}                                        ::  terminate
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
  $%  {$them path $~ u/hiss}                            ::  HTTP request
      {$poke wire dock $talk-command command:talk}      ::
      {$wait path p/@da}                                ::  timeout
  ==  ==
::
++  sign                                                ::  arvo response
  $%  {$e $thou p/httr}                                 ::  HTTP result
      {$t $wake $~}                                     ::  timeout ping
  ==
::
++  stat  twit-stat                                     ::  recieved tweet
--
!:
::::
  ::
|_  {bowl axle}
++  any-auth  ?~(kes (auth) (auth p.n.kes))             ::  use any keys
++  auth                                                ::  build API door
  |=  a/span
  ~|  [%no-auth a] 
  ~(. twit (~(got by kes) a) now `@`eny)
::
++  cull                                                ::  remove seen tweets
  |=  {pax/path rep/(list stat)}  ^+  rep
  =+  pev=(sa (turn (~(get ja fed) pax) |=(stat id)))
  (skip rep |=(stat (~(has in pev) id)))
::
++  done  [*(list move) .]
++  dely                                                ::  next polling timeout
  |=  pax/path
  ^-  {(unit time) _ran}
  =+  cur=(~(get by ran) pax)
  =+  tym=(add now (mul ~s8 (bex ?~(cur 0 p.u.cur))))
  :: ~&  dely#`@dr`(sub tym now)
  ?:  &(?=(^ cur) (gte tym q.u.cur) (gth q.u.cur now))
    [~ ran]
  [`tym (~(put by ran) pax ?~(cur 0 (min 5 +(p.u.cur))) tym)]
::
++  wait                                                ::  ensure poll by path
  |=  {pax/path mof/(list move)}
  =^  tym  ran  (dely pax)
  :_  +>.$
  ?~  tym  
    :: ~&  no-wait#ran
    mof
  :: ~&  will-wait#u.tym
  :-  [ost %wait pax u.tym]
  mof
::
++  poke-twit-do                                        ::  recieve request
  |=  act/twit-do
  ^+  [*(list move) +>]
  ?-    -.q.act
      $auth
    :-  [(print "authed @{(trip p.act)}")]~
    +>.$(kes (~(put by kes) p.act p.q.act))             ::  XX verify key
      $post
    =:  out  (~(put by out) p.q.act %& p.act q.q.act)
        ran  (~(del by ran) /peer#home)
      ==
    %+  wait  /peer#home
    =+  mez=(stat-upda:(auth p.act) [%status q.q.act]~ ~)
    [ost %them /post#(scot %uv p.q.act) ~ mez]~
  ==
::
++  wake-peer
  |=  {pax/path ~}  ^+  done
  ~&  twit-wake#peer#pax
  :_  +>.$
  ?.  (~(has by ran) peer#pax)                           ::  ignore if retracted
    ~
  =+  =>  |=({a/bone @ b/path} [b a]) 
      pus=(~(gas ju *(jug path bone)) (turn (~(tap by sup)) .))
  ?~  (~(get ju pus) pax)
    ~
  ~&  peer-again#[pax ran]
  (pear | our pax)
::
++  thou
  |=  {pax/path hit/httr}  ^+  done
  ?+    p.hit  ~|([%unknown-code p.hit] !!)
      429                           ::  Rate-limit
    =.  ran  (~(put by ran) pax 6 now)
    =+  lim=%.(%x-rate-limit-reset ;~(biff ~(get by (mo q.hit)) poja ni:jo))
    =+  tym=?~(lim (add ~m7.s30 now) (add ~1970.1.1 (mul ~s1 u.lim)))
    ~&  retrying-in#`@dr`(sub tym now)
    :_(+>.$ [ost %wait pax tym]~)
  ::
      200                           ::  OK
    =+  jon=(need (poja q:(need r.hit)))
    :: ~&  twit-resp#%.(jon ?+(-.jon !! %o stat:twir, %a (ar:jo stat:twir)))
    ?+    pax  ~|([%http-missed pax] !!)
        {$post @ $~}                                    ::  post acknowledged
      =+  ^=  rep
          ~|  [%bad-post jon]
          (need %.(jon stat:twir))
      =.  out  (~(put by out) (slav %uv i.t.pax) %| rep)
      :_  +>.$
      =+  pax=/[who.rep]/status#(rsh 3 2 (scot %ui id.rep))
      :-  (print (earn [& ~ `/com#twitter] `pax ~))
      (spam pax (tweet-good rep))
    ::
        {$peer *}                                     ::  feed data
      =+  ^=  rep
          ~|  [%bad-feed jon]
          (need %.(jon (ar:jo stat:twir)))
      :: ~&  got-feed#[(scag 5 (turn rep |=(stat id))) fed]
      =+  ren=(cull t.pax rep)                       ::  new messages
      ?~  ren
        (wait pax ~)                              ::  pump polling
      :: ~&  spam-feed#ren
      =:  ran  (~(del by ran) pax)                    ::  clear poll delay
          fed  (~(put by fed) t.pax rep)              ::  saw last message
        ==
      (wait pax (spam t.pax [%diff twit-feed#(flop ren)] ~))
    ==
  ::
      ?($400 $401 $403 $404)            ::  Err
    =+  ^-  git/gift
        =+  err=%.(q:(need r.hit) ;~(biff poja mean:twir))
        :^  %diff  %ares  %bad-http
        [leaf#"HTTP Code {<p.hit>}" (turn (need err) mean:twip)]
    ?+    pax  [[ost git]~ +>.$]
      {$post @ ~}
        [(spam pax git ~) +>.$]
    ==
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
    =+  sta=(~(get by out) (slav %uv p.pax))
    ?.  ?=({$~ $| ^} sta)                                ::  post not received
      ~
    ~[[ost %diff %twit-stat p.u.sta] [ost %quit ~]]
  =+  ole=(~(get ja fed) pax)
  :_  ^-  (list move)
      ?.  ver  ~
      ?~  ole  ~
      [ost %diff %twit-feed (flop ole)]~
  =-  `move`[ost %them peer#pax ~ `hiss`-]
  =+  opt=?~(ole ~ ['since_id' (lutt:twit id.i.ole)]~)
  =+  aut=any-auth
  ?-    -.pax
    $user  (stat-user:aut [(to-sd p.pax)]~ opt)
::     $home  (stat-home:auth ~ opt)
  ==
::
++  to-sd                                               ::  parse user name#numb
  |=  a/span  ^-  sd:twit
  ~|  [%not-user a]
  %+  rash  a
  ;~(pose (stag %user-id dem) (stag %screen-name user:twir))
::
:: ++  pull                                           ::  release subscription
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
++  print
  |=  mes/tape 
  [ost %poke / [our %talk] (said:^talk our %twit now eny leaf#mes ~)]
--
