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
      :*  [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]
          [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]
          [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]
        ::
          ?.  =(%duke (clan:title our.bol))
            ~
          :_  ~
          :*  ost.bol  %poke  /client-action  [our.bol %hall]
              %hall-action  %source  %inbox  &
              (sy [[(^sein:title our.bol) %urbit-meta] ~]~)
          ==
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
::  +peer:
::
++  peer
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %diff %collections-prize str.sta]~
::
::  +reap: recieve acknowledgement for peer, retry on failure
::
++  reap
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ~&  reap+[wir =(~ err)]
  ?~  err
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
    [~ this]
  ==
::
::  +quit: 
::
++  quit
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ~&  quit+[wir =(~ err)]
  ?~  err
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
    [~ this]
  ==
::
::  +diff-hall-prize:
::
++  diff-hall-prize
  |=  [wir=wire piz=prize:hall]
  ^-  (quip move _this)
  ::
  ::
  ~&  prize+[wir piz]
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
    =/  circs=(map circle:hall (unit config:hall))
      ^-  (map circle:hall (unit config:hall))
      %-  ~(rep in noms)
      |=  [n=name:hall out=(map circle:hall (unit config:hall))]
      ^-  (map circle:hall (unit config:hall))
      (~(put by out) [[our.bol n] ~])
    ::
    :_  this(our-circles.str.sta (~(uni in our-circles.str.sta) circs))
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
    %=  this
      con.inbox.str.sta    `loc.cos.piz
      env.inbox.str.sta    nes.piz
      sub-circles.str.sta  (~(run by rem.cos.piz) |=(a=config:hall `a))
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
    :-  ~
    %=  this
      our-circles.str.sta  
        (~(put by our-circles.str.sta) [our.bol nom] `loc.cos.piz)
    ==
  ==
::
::  +diff-hall-rumor
::
++  diff-hall-rumor
  |=  [wir=wire rum=rumor:hall]
  ^-  (quip move _this)
  ~&  rumor+[wir rum]
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
  ::  %circles:
  ::
      %circles
    ?>  ?=(%circles -.rum)
    =/  circ=circle:hall  [our.bol cir.rum]
    ?:  add.rum
      :_  this(our-circles.str.sta (~(put by our-circles.str.sta) circ ~))
      [ost.bol %peer /our/[cir.rum] [our.bol %hall] /circle/[cir.rum]/config]~
    :_  this(our-circles.str.sta (~(del by our-circles.str.sta) circ))
    :-  [ost.bol %pull /our/[cir.rum] [our.bol %hall] ~]
    (send-rumor %circle-change %our circ ~)
  ::
  ::  %inbox:
  ::
      %inbox
    ?>  ?=(%circle -.rum)
    ?+  -.rum.rum
      ~&  unprocessed-rumor+rum.rum
      [~ this]
    ::
    ::  %remove:
    ::
        %remove
        ~&  %removed-story
      [~ this]
    ::
    ::  %gram: inbox has recieved messages
    ::
        %gram
      :-  (send-rumor [%new-msg %inbox nev.rum.rum])
      this(env.inbox.str.sta [nev.rum.rum env.inbox.str.sta])
    ::
    ::  %config: inbox config has changed
    ::
        %config
      =*  circ  cir.rum.rum
      ?+  -.dif.rum.rum
        ~&  unprocessed-config+dif.rum.rum
        [~ this]
      ::
      ::  %source: the sources of our inbox have changed
      ::
          %source
        ?.  =(circ [our.bol %inbox])
          [~ this]
        ::  we've added a source to our inbox  
        ::
        ?:  add.dif.rum.rum
          ?>  ?=(^ con.inbox.str.sta)
          =/  conf=config:hall
            %=  u.con.inbox.str.sta
              src 
              (~(put by src.u.con.inbox.str.sta) src.dif.rum.rum)
            ==
          :-  (send-rumor %circle-change %our [our.bol %inbox] `conf)
          %=  this
            con.inbox.str.sta  `conf
          ::
            sub-circles.str.sta 
              (~(put by sub-circles.str.sta) cir.src.dif.rum.rum ~)
          ==
        ::  we've removed a source from our inbox  
        ::
        :-  (send-rumor %circle-change %sub cir.src.dif.rum.rum ~)
        %=  this
          sub-circles.str.sta 
            (~(del by sub-circles.str.sta) cir.src.dif.rum.rum)
        ==
      ::
      ::  %full: recieved a full config update for one of our sources
      ::
          %full
        =*  conf  cof.dif.rum.rum
        :-  (send-rumor %circle-change %sub circ `conf)
        %=  this
          sub-circles.str.sta  (~(put by sub-circles.str.sta) circ `conf)
        ==
      ==
    ==
  ::
  ::  %invites:
  ::
      %invites
    ?>  ?=(%circle -.rum)
    ?>  ?=(%gram -.rum.rum)
    :-  (send-rumor [%new-msg %invites nev.rum.rum])
    this(invites.str.sta [nev.rum.rum invites.str.sta])
  ::
  ::  %our:
  ::
      %our
    ?>  ?=(%circle -.rum)
    ?+  -.rum.rum
      ~&  unprocessed-rumor+rum.rum
      [~ this]
    ::
    ::  %remove:
    ::
        %remove
        ~&  %removed-story
      [~ this]
    ::
    ::  %config:
    ::
        %config
      =*  circ  cir.rum.rum
      =*  diff  dif.rum.rum
      ?+  -.diff
        ~&  unprocessed-config+diff
        [~ this]
      ::
      ::  %full: recieved a full config update for one of our sources
      ::
          %full
        =*  conf  cof.dif.rum.rum
        :-  (send-rumor %circle-change %our circ `conf)
        %=  this
          our-circles.str.sta  (~(put by our-circles.str.sta) circ `conf)
        ==
      ==
    ==
  ==
::
::  +send-rumor: send a rumor to all subscribers
::
++  send-rumor
  |=  rum=rumor
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /data bol)
  |=  [=bone *]
  [bone %diff %collections-rumor rum]
::
::  +poke-noun: debugging stuff
::
++  poke-noun
  |=  a=@t
  ^-  (quip move _this)
  ?:  =(a 'check all subs')
    ~&  'here are all incoming subs'
    ~&  ^-  (list (pair ship path))
        %+  turn  ~(tap by sup.bol)
        |=  [b=bone s=ship p=path]
        ^-  (pair ship path)
        [s p]
    [~ this]
  ::
  ?:  =(a 'print state')
    ~&  str.sta
    [~ this]
  [~ this]
--






















