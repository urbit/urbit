::
/-  *notify, resource, hark-store, post
/+  default-agent, verb, dbug, group
::
|%
+$  card  card:agent:gall
::
+$  provider-state  (map term provider-entry)
+$  provider-entry
  $:  notify-endpoint=@t
      binding-endpoint=@t
      auth-token=@t
      clients=(map ship (unit @t))
      =whitelist
  ==
::
+$  client-state
  $:  providers=(jug @p term)
  ==
::
+$  state-0
  $:  %0
      =provider-state
      =client-state
  ==
::
+$  versioned-state
  $%  state-0
  ==
::
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
=<
  |_  =bowl:gall
  +*  this  .
      def  ~(. (default-agent this %|) bowl)
      do   ~(. +> bowl)
  ::
  ++  on-init
    :_  this
    [%pass /hark %agent [our.bowl %hark-store] %watch /updates]~
  ::
  ++  on-save   !>(state)
  ++  on-load
    |=  =old=vase
    ^-  (quip card _this)
::    `this(state *state-0)
    =/  old  !<(versioned-state old-vase)
    ?-  -.old
        %0
      `this(state old)
    ==
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    |^
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %notify-provider-action  (handle-provider-action !<(provider-action vase))
        %notify-client-action    (handle-client-action !<(client-action vase))
      ==
    [cards this]
    ::
    ++  handle-provider-action
      |=  act=provider-action
      ^-  (quip card _state)
      ?-  -.act
          %add
        ?>  (team:title our.bowl src.bowl)
        =/  new-entry=provider-entry
          :*  notify.act
              binding.act
              auth-token.act
              ~
              whitelist.act
          ==
        [~ state(provider-state (~(put by provider-state) service.act new-entry))]
      ::
          %remove
        ?>  (team:title our.bowl src.bowl)
        =/  entry=(unit provider-entry)  (~(get by provider-state) service.act)
        ?~  entry
          ~|("no such service: {<service.act>}" !!)
        :_  state(provider-state (~(del by provider-state) service.act))
        %+  turn  ~(tap by clients.u.entry)
        |=  [who=@p *]
        ^-  card
        (leave-client who service.act)
      ::
          %client-join
        =/  entry=(unit provider-entry)  (~(get by provider-state) service.act)
        ?~  entry
          ~|("no such service: {<service.act>}" !!)
        ?.  (is-whitelisted:do src.bowl u.entry)
          ~|("permission denied" !!)
        =.  clients.u.entry  (~(put by clients.u.entry) src.bowl ~)
        :_  state(provider-state (~(put by provider-state) service.act u.entry))
        :~  %:  register-binding:do
                service.act
                u.entry
                binding-endpoint.u.entry
                src.bowl
                address.act
            ==
            (watch-client src.bowl service.act)
        ==
      ::
          %client-leave
        =/  entry=(unit provider-entry)  (~(get by provider-state) service.act)
        ?~  entry
          ~|("no such service: {<service.act>}" !!)
        ?.  (is-client:do src.bowl u.entry)
          ~|("permission denied" !!)
        =/  client-info=(unit @t)  (~(got by clients.u.entry) src.bowl)
        =.  clients.u.entry        (~(del by clients.u.entry) src.bowl)
        :_  state(provider-state (~(put by provider-state) service.act u.entry))
        ?~  client-info
          [(leave-client src.bowl service.act)]~
        :~  %:  remove-binding:do
                service.act
                u.entry
                src.bowl
                binding-endpoint.u.entry
                u.client-info
            ==
            (leave-client src.bowl service.act)
        ==
      ==
    ::
    ++  handle-client-action
      |=  act=client-action
      ^-  (quip card _state)
      ?>  (team:title our.bowl src.bowl)
      ?-  -.act
          %connect-provider
        =/  =wire  /connect/(scot %p who.act)
        =.  providers.client-state
          (~(put ju providers.client-state) who.act service.act)
        :_  state
        [(poke-provider wire who.act %client-join service.act address.act) ~]
      ::
          %remove-provider
        =/  =wire  /remove/(scot %p who.act)
        =.  providers.client-state
          (~(del ju providers.client-state) who.act service.act)
        :_  state
        [(poke-provider wire who.act %client-leave service.act) ~]
      ==
    --
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path  (on-watch:def path)
        [%notify @ ~]
      =*  service  i.t.path
      ?.  (~(has ju providers.client-state) src.bowl service)
        ~|("permission denied" !!)
      `this
    ==
  ::
  ++  on-leave
    |=  =path
    ^-  (quip card _this)
    `this
  ::
  ++  on-peek   on-peek:def
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  wire  (on-agent:def wire sign)
    ::
    ::  subscription from client to their own hark-store
    ::
        [%hark ~]
      ?+  -.sign  (on-agent:def wire sign)
          %fact
        ?.  ?=(%hark-update p.cage.sign)
          [~ this]
        =+  !<(hark-update=update:hark-store q.cage.sign)
        =/  notes=(list notification)  (filter-notifications:do hark-update)
        ?~  notes
          `this
        ::  only send the last one, since hark accumulates notifcations
        =/  =update  [%notification `notification`(snag 0 (flop notes))]
        :_  this
        [(give-update:do update)]~
      ::
          %kick
        :_  this
        [%pass /hark %agent [our.bowl %hark-store] %watch /updates]~
      ==
    ::
    ::  subscription from provider to client
    ::
        [%notify @ @ ~]
      =/  who      (slav %p i.t.wire)
      =*  service  i.t.t.wire
      ?+  -.sign  (on-agent:def wire sign)
          %fact
        ?>  ?=(%notify-update p.cage.sign)
        =+  !<(=update q.cage.sign)
        ?-  -.update
            %notification
          =/  entry=(unit provider-entry)  (~(get by provider-state) service)
          ?~  entry
            `this
          :_  this
          [(send-notification u.entry who notification.update)]~
        ==
      ::
          %kick
        :_  this
        [(watch-client who service)]~
      ::
          %watch-ack
        ?~  p.sign
          `this
        %-  (slog u.p.sign)
        `this
      ==
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  wire  (on-arvo:def wire sign-arvo)
        [%register-binding @ @ @ ~]
      =/  who=@p   (slav %p i.t.wire)
      =/  service  i.t.t.wire
      ::
      ?>  ?=(%iris -.sign-arvo)
      ?>  ?=(%http-response +<.sign-arvo)
      ?>  ?=(%finished -.client-response.sign-arvo)
      ?>  ?=(^ full-file.client-response.sign-arvo)
      =/  =mime-data:iris  u.full-file.client-response.sign-arvo
      ?>  =('application/json' type.mime-data)
      =/  jon=json
        (fall (rush (@t q.data.mime-data) apex:de-json:html) *json)
      =/  [sid=@t message=@t]
        %.  jon
        %-  ot:dejs:format
        :~  sid+so:dejs:format
            message+so:dejs:format
        ==
      ::
      =/  entry=(unit provider-entry)  (~(get by provider-state) service)
      ?~  entry
        `this
      =.  clients.u.entry  (~(put by clients.u.entry) who `sid)
      :-  ~
      this(provider-state (~(put by provider-state) service u.entry))
    ::
        [%remove-binding *]
      `this
    ::
        [%send-notification *]
      `this
    ==
  ::
  ++  on-fail   on-fail:def
  --
|_  bowl=bowl:gall
::
++  filter-notifications
  |=  =update:hark-store
  ^-  (list notification)
  ?+  -.update  ~
      %more
    (zing (turn more.update filter-notifications))
  ::
      %added
    ?-  -.index.update
        %graph
      ?:  =(`%graph-validator-dm mark.index.update)
        ?.  ?=(%graph -.contents.notification.update)
          ~
        %+  turn  list.contents.notification.update
        |=  =post:post
        ^-  notification
        [graph.index.update index.post]
      ?:  =(`%graph-validator-chat mark.index.update)
        =/  hid  (group-is-hidden graph.index.update)
        ?~  hid  ~
        ?.  u.hid  ~
        ?.  ?=(%graph -.contents.notification.update)
          ~
        %+  turn  list.contents.notification.update
        |=  =post:post
        ^-  notification
        [graph.index.update index.post]
      ~
    ::
        %group  ~
    ==
  ==
::
++  group-is-hidden
  |=  =resource:resource
  ^-  (unit ?)
  =/  grp=(unit group:group)  (~(scry-group group bowl) resource)
  ?~  grp  ~
  `hidden.u.grp
::
++  is-whitelisted
  |=  [who=@p entry=provider-entry]
  ^-  ?
  |^
  ?|  public.whitelist.entry
      =(our.bowl who)
      is-kid
      (~(has in users.whitelist.entry) who)
      in-group
  ==
  ::
  ++  is-kid
    ?&  kids.whitelist.entry
        =(our.bowl (sein:title our.bowl now.bowl who))
    ==
  ::
  ++  in-group
    =/  gs  ~(tap in groups.whitelist.entry)
    |-
    ?~  gs  %.n
    ?:  (~(is-member group bowl) who i.gs)
      %.y
    $(gs t.gs)
  --
::
++  is-client
  |=  [who=@p entry=provider-entry]
  ^-  ?
  (~(has by clients.entry) who)
::
++  post-form
  |=  [=wire url=@t auth=@t params=(list [@t @t])]
  ^-  card
  =/  data
    %+  roll
      %+  sort  params
      |=  [[p=@t @t] [q=@t @t]]
      (aor p q)
    |=  [[p=@t q=@t] out=_url]
    (cat 3 out (cat 3 p q))
  =/  hmac-sig  (hmac-sha1t:hmac:crypto auth data)
  =/  b64-sig   (en:base64:mimes:html (met 3 hmac-sig) (swp 3 hmac-sig))
  =/  headers
    :~  ['X-Twilio-Signature' b64-sig]
        ['Content-Type' 'application/x-www-form-urlencoded']
    ==
  =/  form-data  (build-form-data params)
  =/  =request:http
    [%'POST' url headers `[(met 3 form-data) form-data]]
  [%pass wire %arvo %i %request request *outbound-config:iris]
::
++  build-form-data
  |=  data=(list [@t @t])
  ^-  @t
  %+  roll  data
  |=  [[p=@t q=@t] out=@t]
  ?:  =(out '')
    (cat 3 p (cat 3 '=' q))
  (cat 3 out (cat 3 '&' (cat 3 p (cat 3 '=' q))))
::
++  send-notification
  |=  [entry=provider-entry who=@p =notification]
  ^-  card
  =/  params=(list [@t @t])
    :~  identity+(rsh [3 1] (scot %p who))
        ship+(rsh [3 1] (scot %p entity.resource.notification))
        graph+name.resource.notification
        :-  %node
        %+  roll  index.notification
        |=  [in=@ out=@t]
        (cat 3 (cat 3 out '/') (scot %ud in))
    ==
  %:  post-form
      /send-notification/(scot %uv (sham eny.bowl))
      notify-endpoint.entry
      auth-token.entry
      params
  ==
::
++  register-binding
  |=  [service=term entry=provider-entry url=@t who=@p address=@t]
  ^-  card
  =/  params=(list [@t @t])
    :~  identity+(rsh [3 1] (scot %p who))
        bindingtype+'apn'
        address+address
        action+'add'
    ==
  %:  post-form
      /register-binding/(scot %p who)/[service]/(scot %uv (sham eny.bowl))
      binding-endpoint.entry
      auth-token.entry
      params
  ==
::
++  remove-binding
  |=  [service=term entry=provider-entry who=@p url=@t sid=@t]
  ^-  card
  =/  params=(list [@t @t])
    :~  sid+sid
        action+'remove'
    ==
  %:  post-form
      /remove-binding/(scot %p who)/[service]/(scot %uv (sham eny.bowl))
      binding-endpoint.entry
      auth-token.entry
      params
  ==
::
++  watch-client
  |=  [who=@p service=term]
  ^-  card
  =/  =wire  /notify/(scot %p who)/[service]
  [%pass wire %agent [who %notify] %watch /notify/[service]]
::
++  leave-client
  |=  [who=@p service=term]
  ^-  card
  =/  =wire  /notify/(scot %p who)/[service]
  [%pass wire %agent [who %notify] %leave ~]
::
++  poke-provider
  |=  [=wire who=@p act=provider-action]
  ^-  card
  [%pass wire %agent [who %notify] %poke %notify-provider-action !>(act)]
::
++  give-update
  |=  =update
  ^-  card
  =/  paths=(list path)
    %+  turn  ~(tap by sup.bowl)
    |=  [duct =ship =path]
    path
  [%give %fact paths %notify-update !>(update)]
::
--
