::  Twitter daemon
::
::::  /hoon/twit/app
  ::
/-    plan-acct
/+    twitter, talk
::
::::  ~fyr
  ::
=,  eyre
=,  html
|%
++  twit-path                                           ::  valid peer path
  $%  {$cred $~}                                        ::  credential info
      {$home p/@t $~}                                   ::  home timeline
      {$user p/@t $~}                                   ::  user's tweets
      {$post p/@taxuv $~}                               ::  status of status
  ==
::
++  axle                                                ::  app state
  $:  $0
      out/(map @uvI (each {knot cord} stat))            ::  sent tweets
      ran/(map path {p/@ud q/@da})                      ::  polls active
      fed/(jar path stat)                               ::  feed cache
      ced/(unit (pair @da json))                        ::  credentials
  ==
::
++  gift                                                ::  subscription action
  $%  {$quit $~}                                         ::  terminate
      {$diff gilt}                                      ::  send data
  ==
++  gilt  
  $%  {$twit-feed p/(list stat)}                        ::  posts in feed
      {$twit-post p/stat}                               ::  tweet accepted
      {$ares term (list tank)}                          ::  error
      {$json json}                                      ::  unspecialized
  ==
::
++  move  {bone card}
++  card                                                ::  arvo request
  $?  gift
  $%  {$hiss wire (unit iden) api-call}                 ::  api request
      {$poke wire app-message}                          ::
      {$wait wire p/@da}                                ::  timeout
  ==  ==
::
++  api-call  {response-mark $twit-req {endpoint quay}} :: full hiss payload
++  response-mark  ?($twit-post $twit-feed $twit-cred)  :: sigh options
++  app-message
  $?  {{ship $talk} $talk-command command:talk}         ::  chat message
      {{ship $hood} $write-plan-account iden plan-acct} :: registration
  ==                                                    ::
++  sign                                                ::  arvo response
  $%  {$e $thou p/httr}                                 ::  HTTP result
      {$t $wake $~}                                      ::  timeout ping
  ==
::
::  XX =*
++  stat      post:twitter                              ::  recieved tweet
++  command   command:twitter                           ::  incoming command
++  endpoint  endpoint:reqs:twitter                     ::  outgoing target
++  param  param:reqs:twitter                           ::  twit-req paramters
++  print  print:twitter                                ::  their serialization
::
--
::
::::
  ::
|_  {bowl axle}
::
++  prep
  |=  a/(unit axle)  ^-  (quip move +>)
  ?^  a  [~ +>(+<+ u.a)]
  (peer-scry-x /cred)
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
++  wait-new                                            ::  poll with min delay
  |=  {pax/path mof/(list move)}
  (wait(ran (~(del by ran) pax)) pax mof)
::
++  wait                                                ::  ensure poll by path
  |=  {pax/path mof/(list move)}  ^+  done
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
  |=  {usr/iden act/command}  ^+  done
  ?-    -.act
      $post
    =.  out  (~(put by out) p.act %& usr q.act)
    %+  wait-new  /peer/home/[usr]
    =+  req=[%twit-req `endpoint`update+[%status q.act]~ ~]
    [ost %hiss post+(dray ~[%uv] p.act) `usr %twit-post req]~
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
  (pear | `~. pax) ::(user-from-path pax))
::
++  sigh-recoverable-error                              ::  Rate-limit
  |=  {pax/path $429 $rate-limit lim/(unit @da)}
  =.  ran  (~(put by ran) pax 6 now)
  =+  tym=?~(lim (add ~m7.s30 now) (add ~1970.1.1 (mul ~s1 u.lim)))
  ~&  retrying-in+`@dr`(sub tym now)
  :_(+>.$ [ost %wait pax tym]~)
::
++  sigh-twit-cred-scry-cred  sigh-twit-cred-cred       :: alias
++  sigh-twit-cred-cred
  |=  {wir/wire acc/plan-acct raw/json}  ^+  done
  ?>  ?=($~ wir)
  =+  pax=`twit-path`cred+wir
  :_  +>.$(ced `[now raw])
  :-  [ost %poke pax [our %hood] %write-plan-account ~.twitter acc]
  (spam-with-scry-x pax json+raw)
::
++  sigh-twit-post-post                                ::  status acknowledged
  |=  {wir/wire rep/stat}  ^+  done
  =+  (raid wir mez=%uv ~)
  =.  out  (~(put by out) mez %| rep)
  :_  +>.$
  =+  pax=/[who.rep]/status/(rsh 3 2 (scot %ui id.rep))
  :-  (show-url [& ~ &+/com/twitter] `pax ~)
  (spam-with-scry-x post+wir twit-post+rep)
::
++  sigh-twit-feed                                      ::  feed data
  |=  {wir/wire rep/(list stat)}  ^+  done
  ?>  ?=({?($peer $scry) *} wir)
  =*  pax  t.wir
  :: ~&  got-feed+[(scag 5 (turn rep |=(stat id))) fed]
  =+  ren=(cull pax rep)                       ::  new messages
  =.  rep  (weld ren (~(get ja fed) pax))
  =.  fed  (~(put by fed) pax rep)             ::  save full list
  ?:  ?=($scry -.wir)
    [(spam scry+x+pax [%diff twit-feed+(flop rep)] [%quit ~] ~) +>.$]
  ?~  ren
    (wait wir ~)                              ::  pump polling
  :: ~&  spam-feed+ren
  (wait-new wir (spam pax [%diff twit-feed+(flop ren)] ~))
::
++  sigh-tang                       ::  Err
  |=  {pax/path tan/tang}  ^+  done
  ~&  sigh-tang+pax
  %-  (slog (flop tan))
  =+  ^-  git/gift
      =+  err='' ::%.(q:(need r.hit) ;~(biff de-json mean:reparse:twitter))  :: XX parse?
      :^  %diff  %ares  %bad-http
      tan
      :: [leaf/"HTTP Code {<p.hit>}" (turn (need err) mean:render:twit)]
  ?+    pax  [[ost git]~ +>.$]
    {$post @ $~}
      [(spam pax git ~) +>.$]
  ==
::
:: ++  user-to-path  |=(a/(unit iden) ?~(a '~' (scot %ta u.a)))
:: ++  user-from-path
::   |=  pax/path  ^-  {(unit iden) path}
::   ~|  %bad-user
::   ?~  pax  ~|(%empty-path !!)
::   ~|  i.pax
::   ?:  =('~' i.pax)  [~ t.pax]
::   [`(slav %ta i.pax) t.pax]
::
::
++  compat  |=({usr/(unit iden) req/(unit iden)} ?~(req & =(usr req)))
::
::  .^(twit-feed %gx /=twit=/~/home/urbit_test)
::  .^(twit-stat %gx /=twit=/~./post/0vv0old.0post.hash0.0000)
++  peek
  |=  {ren/care pax/path}  ^-  (unit (unit gilt))
  ?>  ?=($x ren)  ::  others unsupported
  =+  usr=`~.  ::   =^  usr  pax  (user-from-path pax)
  ?.  ?=(twit-path pax)
    ~|([%missed-path pax] !!)
  =+  gil=(pear-scry pax)
  ?-  -.gil
    $none  ~
    $part  ~      ::  stale data
    $full  ``p.gil
  ==
::
++  peer-scry-x
  |=  pax/path  ^+  done
  :_  +>
  =+  pek=(peek %x pax)
  ?^  pek
    ?~  u.pek  ~|(bad-scry+x+pax !!)
    ~[[ost %diff u.u.pek] [ost %quit ~]]
  =+  usr=`~.  ::   =^  usr  pax  (user-from-path pax)
  ?.  ?=(twit-path pax)
    ~|([%missed-path pax] !!)
  =+  hiz=(pear-hiss pax)
  ?~  hiz  ~                          :: already in flight
  ::?>  (compat usr -.u.hiz)                  ::  XX better auth
  [ost %hiss scry+pax usr +.u.hiz]~  
::
++  peer  |=(pax/path :_(+> (pear & `~. pax)))       ::  accept subscription
++  pear                              ::  poll, possibly returning current data
  |=  {ver/? usr/(unit iden) pax/path}
  ^-  (list move)
  ?.  ?=(twit-path pax)
    ~|([%missed-path pax] !!)
  =+  gil=(pear-scry pax)
  %+  welp
    ^-  (list move)
    ?:  ?=($full -.gil)  ~       :: permanent result
    =+  hiz=(pear-hiss pax)
    ?~  hiz  ~
    ::?>  (compat usr -.u.hiz)                  ::  XX better auth
    [ost %hiss peer+pax usr +.u.hiz]~
  ^-  (list move)
  ?.  ver  ~
  ?-  -.gil
    $none  ~
    $part  [ost %diff p.gil]~
    $full  ~[[ost %diff p.gil] [ost %quit ~]]
  ==
::
++  pear-scry
  |=  pax/twit-path  ^-  $%({$none $~} {$part p/gilt} {$full p/gilt})
  ?-    -.pax
      $post
    =+  (raid +.pax mez=%uv ~)
    =+  sta=(~(get by out) mez)
    ?.  ?=({$~ $| *} sta)
      [%none ~]
    [%full twit-post+p.u.sta]
  ::
      ?($user $home)
    [%part twit-feed+(flop (~(get ja fed) pax))]
  ::
      $cred
    ?~  ced  [%none ~]
    ?:  (gth now (add p.u.ced ~m1))   ::  stale
      [%none ~]
    [%full %json q.u.ced]
  ==
::
++  pear-hiss
  |=  pax/twit-path  ^-  (unit {(unit iden) api-call})
  ?-    -.pax
      $post  ~                        :: future/unacked
      $cred
    `[`~. %twit-cred twit-req+[test-login+~ ['skip_status'^%t]~]]
  ::
      $user
    =+  ole=(~(get ja fed) pax)
    =+  opt=?~(ole ~ ['since_id' (tid:print id.i.ole)]~)
    `[`~. [%twit-feed twit-req+[posts-by+[(to-sd p.pax)]~ opt]]]
  ::
      $home
    =+  ole=(~(get ja fed) pax)
    =+  opt=?~(ole ~ ['since_id' (tid:print id.i.ole)]~)
    `[`p.pax [%twit-feed twit-req+[timeline+~ opt]]]
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
++  spam-with-scry-x                                    :: recieve final
  |=  {a/path b/gilt}  ^-  (list move)
  =+  mof=~[[%diff b] [%quit ~]]
  (weld (spam a mof) (spam scry+x+a mof))
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
