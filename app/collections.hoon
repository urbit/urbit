::
::::  /app/collections/hoon
  ::
/?  309
/-  hall
/+  collections
::
::  cols:
::
::    run collections-item renderer on children of /web/collections
::    combine with a bunted config in a +collection structure defined in
::    /lib/collections because the top level collection has no config file
::
::    whenever any of the clay files that compose this renderer change, this app
::    will recompile and the +prep arm will fire. we then check which files
::    changed and notify the corresponding hall circle of that change
::
/=  cols
  /^  collection:collections
  /;  |=  a=(map knot item:collections)
      [*config:collections a]
  /:  /===/web/collections  /_  /collections-item/
::
=,  collections
=,  space:userlib
::
::  state:
::
::    stores the collection built by above by :cols so that we can compare old
::    and new versions whenever the rendered data changes
::
~%  %landscape  ..^is  ~
|_  [bol=bowl:gall sta=state]
::
::  +this: app core subject
::
++  this  .
::
::  +prep:
::
::    on initial boot, create top level hall circle for collections, called %c
::
::    on subsequent compiles, call +ta-update:ta on the old collection data,
::    then update state to store the new collection data
::
++  prep
  ~/  %land-prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?~  old
    :_  this
    ;:  welp
      =<  ta-done
      (~(ta-hall-create-circle ta ~ bol) /c 'collections')
    ::
      :~  [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]
          [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]
          [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]
        ::
      ::    ?.  =(%duke (clan:title our.bol))
      ::      ~
      ::    :_  ~
      ::    :*  ost.bol  %poke  /client-action  [our.bol %hall]
      ::        %hall-action  %source  %inbox  &
      ::        (sy [[(^sein:title our.bol) %urbit-meta] ~]~)
      ::    ==
      ==
    ==
  ?-    -.u.old
      %0
    =/  mow=(list move)
      =<  ta-done
      (~(ta-update ta ~ bol) col.u.old cols)
    :-  mow
    %=  this
      sta  [%0 cols str.u.old]
    ==
  ==
::
::  +mack:
::
::    recieve acknowledgement for permissions changes, print error if it failed
::
++  mack
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  (mean u.err)
::
::  +coup: recieve acknowledgement for poke, print error if it failed
::
++  coup
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  (mean u.err)
::
::  +poke-collections-action:
::
::    the main interface for creating and deleting collections and items
::
++  poke-collections-action
  ~/  %coll-poke-collections-action
  |=  act=action
  ^-  (quip move _this)
  ?:  =(who.act our.bol)
    :_  this
    =<  ta-done
    (~(ta-act ta ~ bol) act)
  ::  forward poke if its not meant for us
  ::
  :_  this
  :_  ~
  :*  ost.bol  %poke
      /forward-collections-action
      [who.act %collections]
      %collections-action  act
  ==
::
::  +poke-json
::
::    utility for setting whether or not to display the onboarding page
::
++  poke-json
  ~/  %coll-poke-json
  |=  jon=json
  ^-  (quip move _this)
  ?:  ?=([%o [[%onboard %b ?] ~ ~]] jon)
    :_  this
    =<  ta-done
    (~(ta-write ta ~ bol) /web/landscape/onboard/json [%json !>(jon)])
  [~ this]
::
::  +poke-collections-command
::
++  poke-collections-command
  |=  cod=command
  ^-  (quip move _this)
  ?-    -.cod
      %chat-invite
    :_  this
    :-  :*  ost.bol
            %poke
            /permit
            [our.bol %hall]
            %hall-action
            %permit
            nom.cod
            %.y
            who.cod
        ==
    %+  turn  ~(tap in who.cod)
    |=  guy=@p
    ^-  move
    :*  ost.bol
        %poke
        /invite
        [our.bol %hall]
        %hall-action
        %phrase
        aud=(sy [guy %i] ~)
        ses=[%inv & our.bol nom.cod]~
    ==
    ::
      %collection-invite
    :_  this
    :-  :*  ost.bol
            %poke
            /permit
            [our.bol %hall]
            %hall-action
            %permit
            nom.cod
            %.y
            who.cod
        ==
    %+  turn  ~(tap in who.cod)
    |=  guy=@p
    ^-  move
    :*  ost.bol
        %poke
        /invite
        [our.bol %hall]
        %hall-action
        %phrase
        aud=(sy [guy %i] ~)
        ses=[%app col.cod [%inv & our.bol nom.cod]]~
    ==
  ==
::
::  +peer:
::
++  peer
  |=  wir=wire
  ^-  (quip move _this)
  ::~&  peer-prize+str.sta
  :_  this
  [ost.bol %diff %collections-prize str.sta]~
::
::  +reap: recieve acknowledgement for peer, retry on failure
::
++  reap
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ::~&  reap+[wir =(~ err)]
  ?~  err
    ::  XX send message to users inbox
    [~ this]
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
      %circles
    :_  this
    [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]~
  ::
      %inbox
    :_  this
    [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]~
  ::
      %invites
    :_  this
    [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]~
  ::
      %our
    ?<  ?=(~ t.wir)
    :_  this
    [ost.bol %peer /our/[i.t.wir] [our.bol %hall] /circle/[i.t.wir]/config]~
  ==
::
::  +quit:
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
      %circles
    :_  this
    [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]~
  ::
      %inbox
    :_  this
    [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]~
  ::
      %invites
    :_  this
    [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]~
  ::
      %our
    ?<  ?=(~ t.wir)
    :_  this
    [ost.bol %peer /our/[i.t.wir] [our.bol %hall] /circle/[i.t.wir]/config]~
  ==
::
::  +diff-hall-prize:
::
++  diff-hall-prize
  |=  [wir=wire piz=prize:hall]
  ^-  (quip move _this)
  ::
  ::
  ::~&  prize+[wir piz]
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
  ::  %circles: subscribe to the configuration of each of our circles
  ::
      %circles
    ?>  ?=(%circles -.piz)
    =/  noms=(set name:hall)  (~(dif in cis.piz) (sy ~[%inbox %i %public]))
    :_  this(our-circles.str.sta (~(uni in our-circles.str.sta) noms))
    ^-  (list move)
    %+  turn  ~(tap in noms)
    |=  nom=name:hall
    ^-  move
    [ost.bol %peer /our/[nom] [our.bol %hall] /circle/[nom]/config]
  ::
  ::  %inbox: fill inbox config, messages and remote configs with prize data
  ::
      %inbox
    ?>  ?=(%circle -.piz)
    :-  ~
    %=    this
        con.inbox.str.sta  `loc.cos.piz
    ::
        env.inbox.str.sta  nes.piz
    ::
        circles.str.sta
      %-  ~(uni in circles.str.sta)
      ^-  (map circle:hall (unit config:hall))
      (~(run by rem.cos.piz) |=(a=config:hall `a))
    ==
  ::
  ::  %invites: fill invite messages with prize data
  ::
      %invites
    ?>  ?=(%circle -.piz)
    :-  ~
    %=  this
      invites.str.sta  nes.piz
    ==
  ::
  ::  %our:
  ::
      %our
    ?>  ?=(%circle -.piz)
    =/  nom=name:hall  &2:wir
    ::  XX todo: send rumor or let config-change handle it?
    ::
    :-  ~
    %=    this
        circles.str.sta
      (~(put by circles.str.sta) [our.bol nom] `loc.cos.piz)
    ::
      our-circles.str.sta  (~(put in our-circles.str.sta) nom)
    ==
  ==
::
::  +diff-hall-rumor
::
++  diff-hall-rumor
  |=  [wir=wire rum=rumor:hall]
  ^-  (quip move _this)
  ::~&  rumor+[wir rum]
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  =;  upd=[mow=(list move) sta=_this]
    :_  sta.upd
    %+  welp  mow.upd
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [=bone *]
    [bone %diff %hall-rumor rum]
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
  ::  %circles:
  ::
      %circles
    ::~&  %circles
    ?>  ?=(%circles -.rum)
    =?    dms.str.sta
        ?&  (is-dm-circle our.bol cir.rum)
          !(~(has by dms.str.sta) cir.rum)
        ==
      (~(put by dms.str.sta) cir.rum our.bol ~)
    ?:  add.rum
      :_  this(our-circles.str.sta (~(put in our-circles.str.sta) cir.rum))
      [ost.bol %peer /our/[cir.rum] [our.bol %hall] /circle/[cir.rum]/config]~
    :_  this(our-circles.str.sta (~(del in our-circles.str.sta) cir.rum))
    [ost.bol %pull /our/[cir.rum] [our.bol %hall] ~]~
  ::
  ::  %inbox:
  ::
      %inbox
    ?>  ?=(%circle -.rum)
    ?+  -.rum.rum
      ::~&  inbox-unprocessed-rumor+rum.rum
      [~ this]
    ::
    ::  %remove:
    ::
        %remove
      ::~&  %inbox-remove
        ::~&  %removed-story
      [~ this]
    ::
    ::  %gram: inbox has recieved messages
    ::
        %gram
      ::~&  %inbox-gram
      ::  XX TODO: handle stack trace message when foreign circle is killed?
      ::
      ?:  (is-dm-circle src.rum.rum)
        =/  dms=[ini=ship env=(list envelope:hall)]
          (~(got by dms.str.sta) nom.src.rum.rum)
        =.  env.dms  [nev.rum.rum env.dms]
        :-  ~
        %=  this
          env.inbox.str.sta  [nev.rum.rum env.inbox.str.sta]
          dms.str.sta        (~(put by dms.str.sta) nom.src.rum.rum dms)
        ==
      :-  ~  ::(send-rumor [%new-msg %inbox nev.rum.rum])
      this(env.inbox.str.sta [nev.rum.rum env.inbox.str.sta])
    ::
    ::  %config: inbox config has changed
    ::
        %config
      =*  circ  cir.rum.rum
      ?+  -.dif.rum.rum
        ::~&  inbox-unprocessed-config+dif.rum.rum
        [~ this]
      ::
      ::  %remove: circle has been erased
      ::
          %remove
        ::~&  %inbox-config-remove
        :-  ~  ::(send-rumor %config-change cir.rum.rum ~)
        %=    this
            circles.str.sta
          (~(del by circles.str.sta) cir.rum.rum)
        ==
      ::
      ::  %source: the sources of our inbox have changed
      ::
          %source
        ::~&  %inbox-config-source
        ?.  =(circ [our.bol %inbox])
          [~ this]
        ::  we've added a source to our inbox
        ::
        ?>  ?=(^ con.inbox.str.sta)
        ?:  add.dif.rum.rum
          =/  conf=config:hall
            %=  u.con.inbox.str.sta
              src  (~(put in src.u.con.inbox.str.sta) src.dif.rum.rum)
            ==
          :-  ~  ::(send-rumor %config-change [our.bol %inbox] `conf)
          %=    this
              con.inbox.str.sta  `conf
          ::
              circles.str.sta
            ?:  (~(has by circles.str.sta) cir.src.dif.rum.rum)
              circles.str.sta
            (~(put by circles.str.sta) cir.src.dif.rum.rum ~)
          ==
        ::  we've removed a source from our inbox
        ::
        =/  conf=config:hall
          %=  u.con.inbox.str.sta
            src  (~(del in src.u.con.inbox.str.sta) src.dif.rum.rum)
          ==
        ::~&  inbox+conf
        :-  ~  ::(send-rumor %config-change [our.bol %inbox] `conf)
        %=    this
            con.inbox.str.sta  `conf
        ::
            circles.str.sta
          ?:  =(our.bol hos.cir.src.dif.rum.rum)
            circles.str.sta
          (~(del by circles.str.sta) cir.src.dif.rum.rum)
        ==
      ::
      ::  %full: recieved a full config update for one of our sources
      ::
          %full
        ::~&  %inbox-config-full
        =*  conf  cof.dif.rum.rum
        :-  ~  ::(send-rumor %config-change circ `conf)
        %=  this
          circles.str.sta  (~(put by circles.str.sta) circ `conf)
        ==
      ::
      ::  %read: recieved a read reciept
      ::
          %read
        ?.  =([our.bol %inbox] cir.rum.rum)
          [~ this]
        ?>  ?=(^ con.inbox.str.sta)
        :-  ~
        %=  this
          red.u.con.inbox.str.sta  red.dif.rum.rum
        ==
      ==
    ==
  ::
  ::  %invites:
  ::
      %invites
    ::~&  %invites
    ?>  ?=(%circle -.rum)
    ?>  ?=(%gram -.rum.rum)
    ?+  -.sep.gam.nev.rum.rum
      [~ this(invites.str.sta [nev.rum.rum invites.str.sta])]
    ::
        %inv
      =/  circ=circle:hall  cir.sep.gam.nev.rum.rum
      ?:  (is-dm-circle circ)
        =/  who=(set ship)  (sy (rash nom.circ (more dot fed:ag)))
        =/  act=poke  [%hall-action %newdm who]
        :-  [ost.bol %poke /join-dm [our.bol %hall] act]~
        %=  this
          invites.str.sta  [nev.rum.rum invites.str.sta]
          dms.str.sta      (~(put by dms.str.sta) nom.circ hos.circ ~)
        ==
      =/  act=poke  [%hall-action %source %inbox & (sy [circ ~] ~)]
      :-  ~
      this(invites.str.sta [nev.rum.rum invites.str.sta])
    ==
  ::
  ::  %our:
  ::
      %our
    ?>  ?=(%circle -.rum)
    ?+  -.rum.rum
      ::~&  our-unprocessed-rumor+rum.rum
      [~ this]
    ::
    ::  %remove:
    ::
        %remove
        ::~&  %our-remove
      [~ this]
    ::
    ::  %config:
    ::
        %config
      =*  circ  cir.rum.rum
      =*  diff  dif.rum.rum
      ?+  -.diff
        ::~&  our-unprocessed-config+diff
        [~ this]
      ::
      ::  %full: recieved a full config update for one of our sources
      ::
          %full
        ::~&  %our-config-full
        =*  conf  cof.dif.rum.rum
        :-  ~  ::(send-rumor %config-change circ `conf)
        %=  this
          circles.str.sta  (~(put by circles.str.sta) circ `conf)
        ==
      ::
      ::  %read: recieved a read reciept
      ::
          %read
        =/  conf  (need (~(got by circles.str.sta) circ))
        =.  red.conf  red.dif.rum.rum
        :-  ~
        %=  this
          circles.str.sta  (~(put by circles.str.sta) circ `conf)
        ==
      ==
    ==
  ==
::
::  +send-rumor: send a rumor to all subscribers
::
++  send-rumor
  |=  rum=rumor
  ::~&  send-rumor+rum
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /primary bol)
  |=  [=bone *]
  [bone %diff %collections-rumor rum]
::
::  +poke-noun: debugging stuff
::
++  poke-noun
  |=  a=@tas
  ^-  (quip move _this)
  ?+    a
    [~ this]
  ::
      %check-all-subs
    ~&  'here are all incoming subs'
    ~&  ^-  (list (pair ship path))
        %+  turn  ~(tap by sup.bol)
        |=  [b=bone s=ship p=path]
        ^-  (pair ship path)
        [s p]
    [~ this]
  ::
      %print-state
    ~&  str.sta
    [~ this]
  ::
      %rebuild-subs
    :_  this
    :~  [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]
        [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]
        [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]
    ==
  ==
::
++  is-dm-circle
  |=  circ=circle:hall
  ^-  ?
  ?=  ^
  (rush nom.circ (more dot fed:ag))
--
