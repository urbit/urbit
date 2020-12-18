::  contact-view [landscape]:
::
::  sets up contact JS client and combines commands
::  into semantic actions for the UI
::
/-
    inv=invite-store,
    *contact-hook,
    *metadata-store,
    *metadata-hook,
    pull-hook,
    push-hook
/+  *server, *contact-json, default-agent, dbug, verb,
    grpl=group, mdl=metadata, resource,
    group-store
::
|%
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      ~
  ==
::
+$  card  card:agent:gall
--
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this       .
      contact-core  +>
      cc         ~(. contact-core bowl)
      def        ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    :~  [%pass /updates %agent [our.bowl %contact-store] %watch /updates]
        (contact-poke:cc [%create /~/default])
        (contact-poke:cc [%add /~/default our.bowl *contact])
        :*  %pass  /srv  %agent  [our.bol %file-server]
            %poke  %file-server-action
            !>([%serve-dir /'~groups' /app/landscape %.n %.y])
        ==
    ==
  ::
  ++  on-save   !>(state)
  ++  on-load
    |=  old-vase=vase
    ^-  (quip card _this)
    =/  old  ((soft state-0) q.old-vase)
    ?^  old  [~ this]
    :_  this(state [%0 ~])
    :~  [%pass / %arvo %e %disconnect [~ /'~groups']]
        [%pass / %arvo %e %connect [~ /'contact-view'] %contact-view]
        :*  %pass  /srv  %agent  [our.bol %file-server]
            %poke  %file-server-action
            !>([%serve-dir /'~groups' /app/landscape %.n %.y])
        ==
    ==
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    ?+  mark  (on-poke:def mark vase)
        %json                 [(poke-json:cc !<(json vase)) this]
        %contact-view-action
      [(poke-contact-view-action:cc !<(contact-view-action vase)) this]
    ::
        %handle-http-request
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      :_  this
      %+  give-simple-payload:app  eyre-id
      %+  require-authorization:app  inbound-request
      poke-handle-http-request:cc
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    ?:  ?=([%http-response *] path)  [~ this]
    ?.  =(/primary path)  (on-watch:def path)
    [[%give %fact ~ %json !>((update-to-json [%initial all-scry:cc]))]~ this]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
       %poke-ack
      ?.  ?=([%join-group %ship @ @ ~] wire)
        (on-agent:def wire sign)
      ?^  p.sign
        (on-agent:def wire sign)
      :_  this
      (joined-group:cc t.wire)
    ::
        %kick
      [[%pass / %agent [our.bol %contact-store] %watch /updates]~ this]
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %contact-update
        =/  update=json  (update-to-json !<(contact-update q.cage.sign))
        [[%give %fact ~[/primary] %json !>(update)]~ this]
      ==
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=(%bound +<.sign-arvo)
      (on-arvo:def wire sign-arvo)
    [~ this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
++  grp  ~(. grpl bol)
++  md  ~(. mdl bol)
++  poke-json
  |=  jon=json
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  (poke-contact-view-action (json-to-view-action jon))
::
++  poke-contact-view-action
  |=  act=contact-view-action
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  ?-  -.act
      %create
    =/  rid=resource
      [our.bol name.act]
    =/  =path
      (en-path:resource rid)
    ;:  weld
      :~  (group-poke [%add-group rid policy.act %.n])
          (group-poke [%add-members rid (sy our.bol ~)])
          (group-push-poke %add rid)
          (contact-poke [%create path])
          (contact-hook-poke [%add-owned path])
      ==
      (create-metadata path title.act description.act)
      ?.  ?=(%invite -.policy.act)
        ~
      %+  turn
        ~(tap in pending.policy.act)
      |=  =ship
      (send-invite our.bol %contacts rid ship '')
    ==
  ::
      %join
    =/  =cage
      :-  %group-update
      !>  ^-  update:group-store
      [%add-members resource.act (sy our.bol ~)]
    =/  =wire
      [%join-group (en-path:resource resource.act)]
    [%pass wire %agent [entity.resource.act %group-push-hook] %poke cage]~
  ::
      %invite
    =*  rid  resource.act
    =/  =group  (need (scry-group:grp rid))
    :-  (send-invite entity.rid %contacts rid ship.act text.act)
    ?.  ?=(%invite -.policy.group)  ~
    ~[(add-pending rid ship.act)]
  ::
      %delete
    ~
  ::
      %remove
    =/  rid=resource
      (de-path:resource path.act)
    :~  (group-poke %remove-members rid (sy ship.act ~))
        (contact-poke [%remove path.act ship.act])
    ==
  ::
      %share
    ::  determine whether to send to our contact-hook or foreign
    ::  send contact-action to contact-hook with %add action
    [(share-poke recipient.act [%add path.act ship.act contact.act])]~
  ::
      %groupify
    =/  =path
      (en-path:resource resource.act)
    %+  weld
      :~  (group-poke %expose resource.act ~)
          (contact-poke [%create path])
          (contact-hook-poke [%add-owned path])
      ==
    (create-metadata path title.act description.act)
  ==
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =+  url=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.url)
    ?~  back-path
      ''
    i.back-path
  ?+  site.url  not-found:gen
      [%'contact-view' @ *]
    =/  =path  (flop t.t.site.url)
    ?~  path  not-found:gen
    =/  contact  (contact-scry `^path`(snoc (flop t.path) name))
    ?~  contact  not-found:gen
    ?~  avatar.u.contact  not-found:gen
    ?-  -.u.avatar.u.contact
        %url   [[307 ['location' url.u.avatar.u.contact]~] ~]
        %octt
      =/  max-3-days  ['cache-control' 'max-age=259200']
      =/  content-type  ['content-type' content-type.u.avatar.u.contact]
      [[200 [content-type max-3-days ~]] `octs.u.avatar.u.contact]
    ==
  ==
::
++  joined-group
  |=  =path
  ^-  (list card)
  =/  rid=resource
    (de-path:resource path)
  :~  (group-pull-poke [%add entity.rid rid])
      (contact-hook-poke [%add-synced entity.rid path])
      (sync-metadata entity.rid path)
  ==
::
::  +utilities
::
++  add-pending
  |=  [rid=resource =ship]
  ^-  card
  =/  app=term
    ?:  =(our.bol entity.rid)
      %group-store
    %group-push-hook
  =/  =cage
    :-  %group-update
    !>  ^-  action:group-store
    [%change-policy rid %invite %add-invites (sy ship ~)]
  [%pass / %agent [entity.rid app] %poke cage]
::
++  send-invite
  |=  =invite:inv
  ^-  card
  =/  =cage
    :-  %invite-action
    !>  ^-  action:inv
    [%invite %contacts (shaf %invite-uid eny.bol) invite]
  [%pass / %agent [recipient.invite %invite-hook] %poke cage]
::
++  contact-poke
  |=  act=contact-action
  ^-  card
   [%pass / %agent [our.bol %contact-store] %poke %contact-action !>(act)]
::
++  contact-hook-poke
  |=  act=contact-hook-action
  ^-  card
  [%pass / %agent [our.bol %contact-hook] %poke %contact-hook-action !>(act)]
::
++  share-poke
  |=  [=ship act=contact-action]
  ^-  card
  [%pass / %agent [ship %contact-hook] %poke %contact-action !>(act)]
::
++  group-poke
  |=  act=action:group-store
  ^-  card
  [%pass / %agent [our.bol %group-store] %poke %group-action !>(act)]
::
++  group-push-poke
  |=  act=action:push-hook
  ^-  card
  [%pass / %agent [our.bol %group-push-hook] %poke %push-hook-action !>(act)]
::
++  group-proxy-poke
  |=  act=action:group-store
  ^-  card
  [%pass / %agent [entity.resource.act %group-push-hook] %poke %group-update !>(act)]
::
++  group-pull-poke
  |=  act=action:pull-hook
  ^-  card
  [%pass / %agent [our.bol %group-pull-hook] %poke %pull-hook-action !>(act)]
::
++  metadata-poke
  |=  act=metadata-action
  ^-  card
  [%pass / %agent [our.bol %metadata-store] %poke %metadata-action !>(act)]
::
++  metadata-hook-poke
  |=  act=metadata-hook-action
  ^-  card
  [%pass / %agent [our.bol %metadata-hook] %poke %metadata-hook-action !>(act)]
::
++  sync-metadata
  |=  [=ship =path]
  ^-  card
  (metadata-hook-poke %add-synced ship path)
::
++  create-metadata
  |=  [=path title=@t description=@t]
  ^-  (list card)
  =/  =metadata
    %*  .  *metadata
        title         title
        description   description
        date-created  now.bol
        creator       our.bol
    ==
  :~  (metadata-poke [%add path [%contacts path] metadata])
      (metadata-hook-poke [%add-owned path])
  ==
::
++  all-scry
  ^-  rolodex
  .^(rolodex %gx /(scot %p our.bol)/contact-store/(scot %da now.bol)/all/noun)
::
++  contact-scry
  |=  pax=path
  ^-  (unit contact)
  =.  pax
    ;:  weld
      /(scot %p our.bol)/contact-store/(scot %da now.bol)/contact
      pax
      /noun
    ==
  .^((unit contact) %gx pax)
--
