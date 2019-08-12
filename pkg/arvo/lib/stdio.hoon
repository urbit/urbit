::  Standard input/output functions.
::
::  These are all asynchronous computations, which means they produce a
::  form:(async A) for some type A.  You can always tell what they
::  produce by checking their first three lines.
::
::  Functions with the word "raw" in their name are for internal use
::  only because they carry a high salmonella risk.  More specifcally,
::  improper use of them may result in side effects that the tapp
::  runtime doesn't know about and can't undo in case the transaction
::  fails.
::
/-  tapp-sur=tapp
/+  async
|*  [poke-data=mold out-peer-data=mold]
=/  tapp-sur  (tapp-sur poke-data out-peer-data)
=,  card=card:tapp-sur
=,  sign=sign:tapp-sur
=,  contract=contract:tapp-sur
=+  (async sign card contract)
|%
::
::  Raw power
::
++  send-raw-card
  |=  =card
  =/  m  (async ,~)
  ^-  form:m
  |=  =async-input
  [[card]~ ~ ~ %done ~]
::
::  Add or remove a contract
::
++  set-raw-contract
  |=  [add=? =contract]
  =/  m  (async ,~)
  ^-  form:m
  |=  =async-input
  =/  delta=contract-delta:async
    ?.(add [%lose ~] [%gain ost.bowl.async-input])
  [~ ~ (my [contract delta] ~) %done ~]
::
::  Send effect on current bone
::
++  send-effect
  |=  =card
  =/  m  (async ,~)
  ^-  form:m
  ;<  =bone  bind:m
    |=  =async-input
    [~ ~ ~ %done ost.bowl.async-input]
  (send-effect-on-bone bone card)
::
::  Send effect on particular bone
::
++  send-effect-on-bone
  |=  [=bone =card]
  =/  m  (async ,~)
  ^-  form:m
  |=  async-input
  [~ [bone card]~ ~ %done ~]
::
::    ----
::
::  Scry from the namespace.
::
::    Direct scrys are impossible in a tapp, so this routes around that.
::
++  scry
  |*  result-type=mold
  |=  =path
  =/  m  (async ,result-type)
  ;<  ~  bind:m  (send-raw-card %scry path)
  |=  =async-input
  :^  ~  ~  ~
  ?~  in.async-input
    [%wait ~]
  ?.  ?=(%scry-result -.sign.u.in.async-input)
    [%fail %expected-scry-result >got=-.sign< ~]
  [%done (result-type result.sign.u.in.async-input)]
::
::    ----
::
::  Outgoing HTTP requests
::
++  send-request
  |=  =request:http
  =/  m  (async ,~)
  ^-  form:m
  =/  =card
    [%request / request *outbound-config:iris]
  ;<  ~  bind:m  (send-raw-card card)
  (set-raw-contract & %request ~)
::
++  send-hiss
  |=  =hiss:eyre
  =/  m  (async ,~)
  ^-  form:m
  (send-request (hiss-to-request:html hiss))
::
::  Wait until we get an HTTP response or cancelation
::
++  take-response-raw
  =/  m  (async (unit client-response:iris))
  ^-  form:m
  |=  =async-input
  :^  ~  ~  ~
  ?~  in.async-input
    [%wait ~]
  =*  sign  sign.u.in.async-input
  ::  fail on anything other than an http-response
  ::
  ?.  ?=(%http-response -.sign)
    [%fail %expected-http-response >got=-.sign< ~]
  ?-  -.response.sign
  ::  ignore progress notifications
  ::
      %progress
    [%wait ~]
  ::
      %cancel
    [%done ~]
  ::
      %finished
    [%done (some response.sign)]
  ==
::  Wait until we get an HTTP response or cancelation and unset contract
::
++  take-maybe-response
  =/  m  (async (unit client-response:iris))
  ^-  form:m
  ;<  rep=(unit client-response:iris)  bind:m
    take-response-raw
  ;<  ~  bind:m  (set-raw-contract | %request ~)
  (pure:m rep)
::
::  Wait until we get an HTTP response and unset contract
::
++  take-response
  =/  m  (async (unit client-response:iris))
  ^-  form:m
  ;<  rep=(unit client-response:iris)  bind:m
    take-maybe-response
  ?^  rep
    (pure:m rep)
  |=  =async-input
  [~ ~ ~ %fail %http-canceled ~]
::
::  Wait until we get an HTTP response or cancelation and unset contract
::
++  take-maybe-sigh
  =/  m  (async (unit httr:eyre))
  ^-  form:m
  ;<  rep=(unit client-response:iris)  bind:m
    take-maybe-response
  ?~  rep
    (pure:m ~)
  ::  XX s/b impossible
  ::
  ?.  ?=(%finished -.u.rep)
    (pure:m ~)
  (pure:m (some (to-httr:iris +.u.rep)))
::
::  Wait until we get an HTTP response and unset contract
::
++  take-sigh
  =/  m  (async ,httr:eyre)
  ^-  form:m
  ;<  rep=(unit httr:eyre)  bind:m  take-maybe-sigh
  ?^  rep
    (pure:m u.rep)
  |=  =async-input
  [~ ~ ~ %fail %http-canceled ~]
::
::  Extract body from raw httr
::
++  extract-httr-body
  |=  =httr:eyre
  =/  m  (async ,cord)
  ^-  form:m
  ?.  =(2 (div p.httr 100))
    (async-fail %httr-error >p.httr< >+.httr< ~)
  ?~  r.httr
    (async-fail %expected-httr-body >httr< ~)
  (pure:m q.u.r.httr)
::
::  Parse cord to json
::
++  parse-json
  |=  =cord
  =/  m  (async ,json)
  ^-  form:m
  =/  json=(unit json)  (de-json:html cord)
  ?~  json
    (async-fail %json-parse-error ~)
  (pure:m u.json)
::
::  Fetch json at given url
::
++  fetch-json
  |=  url=tape
  =/  m  (async ,json)
  ^-  form:m
  =/  =hiss:eyre
    :*  purl=(scan url auri:de-purl:html)
        meth=%get
        math=~
        body=~
    ==
  ;<  ~           bind:m  (send-hiss hiss)
  ;<  =httr:eyre  bind:m  take-sigh
  ;<  =cord       bind:m  (extract-httr-body httr)
  (parse-json cord)
::
::    ----
::
::  Incoming HTTP requests
::
++  bind-route-raw
  |=  [=binding:eyre =term]
  =/  m  (async ,~)
  ^-  form:m
  (send-raw-card [%connect / binding term])
::
++  take-bound
  =/  m  (async ?)
  ^-  form:m
  |=  =async-input
  :^  ~  ~  ~
  ?~  in.async-input
    [%wait ~]
  =*  sign  sign.u.in.async-input
  ?.  ?=(%bound -.sign)
    [%fail %expected-bound >got=-.sign< ~]
  [%done success.sign]
::
++  bind-route
  |=  [=binding:eyre =term]
  =/  m  (async ?)
  ^-  form:m
  ;<  ~  bind:m  (bind-route-raw binding term)
  take-bound
::
::    ----
::
::  Identity is immutable
::
::    XX should be statefully cycled
::
++  get-identity
  =/  m  (async ,@p)
  ^-  form:m
  |=  =async-input
  [~ ~ ~ %done our.bowl.async-input]
::
::  Entropy is always increasing
::
++  get-entropy
  =/  m  (async ,@uvJ)
  ^-  form:m
  |=  =async-input
  [~ ~ ~ %done eny.bowl.async-input]
::
::    ----
::
::  Time is what keeps everything from happening at once
::
++  get-time
  =/  m  (async ,@da)
  ^-  form:m
  |=  =async-input
  [~ ~ ~ %done now.bowl.async-input]
::
::  Set a timer
::
++  send-wait
  |=  at=@da
  =/  m  (async ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-raw-card %wait /note/(scot %da at) at)
  (set-raw-contract & %wait at)
::
::  Wait until we get a wake event
::
++  take-wake-raw
  =/  m  (async ,@da)
  ^-  form:m
  |=  =async-input
  :^  ~  ~  ~
  ?~  in.async-input
    [%wait ~]
  ?.  ?=(%wake -.sign.u.in.async-input)
    [%fail %expected-wake >got=-.sign.u.in.async-input< ~]
  ?~  wire.u.in.async-input
    [%fail %expected-wake-time ~]
  =/  at=(unit @da)  (slaw %da i.wire.u.in.async-input)
  ?~  at
    [%fail %expected-wake-time-da >wire< ~]
  [%done u.at]
::
::  Wait until we get a wake event and unset contract
::
++  take-wake
  =/  m  (async ,~)
  ^-  form:m
  ;<  at=@da  bind:m  take-wake-raw
  (set-raw-contract | %wait at)
::
::  Wait until time
::
++  wait
  |=  until=@da
  =/  m  (async ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-wait until)
  take-wake
::
::  Wait until time then start new computation
::
++  wait-effect
  |=  until=@da
  =/  m  (async ,~)
  ^-  form:m
  (send-effect %wait /effect/(scot %da until) until)
::
::  Cancel computation if not done by time
::
++  set-timeout
  |*  computation-result=mold
  =/  m  (async ,computation-result)
  |=  [when=@da computation=form:m]
  ^-  form:m
  ;<  ~  bind:m  (send-wait when)
  |=  =async-input
  =*  loop  $
  ?:  ?&  ?=([~ * %wake *] in.async-input)
          =(/(scot %da when) wire.u.in.async-input)
      ==
    [~ ~ (my [[%wait when] [%lose ~]] ~) %fail %async-timeout ~]
  =/  c-res  (computation async-input)
  ?.  ?=(%cont -.next.c-res)
    c-res
  c-res(self.next ..loop(computation self.next.c-res))
::
::    ----
::
::  Apps
::
++  poke-app
  |=  [[her=ship app=term] =poke-data]
  =/  m  (async ,~)
  ^-  form:m
  =/  =wire  /(scot %p her)/[app]
  (send-effect %poke wire [her app] poke-data)
::
++  peer-app
  |=  [[her=ship app=term] =path]
  =/  m  (async ,~)
  ^-  form:m
  =/  =wire  (weld /(scot %p her)/[app] path)
  (send-effect %peer wire [her app] path)
::
++  pull-app
  |=  [[her=ship app=term] =path]
  =/  m  (async ,~)
  ^-  form:m
  =/  =wire  (weld /(scot %p her)/[app] path)
  (send-effect %pull wire [her app] ~)
::
++  quit-app
  |=  [[her=ship app=term] =path]
  =/  m  (async ,~)
  ^-  form:m
  =/  =wire  (weld /(scot %p her)/[app] path)
  (send-effect %quit wire [her app] ~)
::
::    ----
::
::  Handle subscriptions
::
::  Get bones at particular path; for internal use only
::
++  get-bones-on-path
  |=  =the=path
  =/  m  (async ,(list bone))
  ^-  form:m
  |=  =async-input
  :^  ~  ~  ~
  :-  %done
  %+  murn  ~(tap by sup.bowl.async-input)
  |=  [ost=bone her=ship =sub=path]
  ^-  (unit bone)
  ?.  =(the-path sub-path)
    ~
  `ost
::
::  Give a result to subscribers on particular path
::
++  give-result
  |=  [=path =out-peer-data]
  =/  m  (async ,~)
  ^-  form:m
  ;<  bones=(list bone)  bind:m  (get-bones-on-path path)
  |-  ^-  form:m
  =*  loop  $
  ?~  bones
    (pure:m ~)
  ;<  ~  bind:m  (send-effect-on-bone i.bones %diff out-peer-data)
  loop(bones t.bones)
::
::    ----
::
::  Handle domains
::
++  install-domain
  |=  =turf
  =/  m  (async ,~)
  ^-  form:m
  (send-effect %rule / %turf %put turf)
--
