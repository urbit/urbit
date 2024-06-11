/@  dm-diff
/@  groupchat-diff
/@  messenger-diff
^-  kook:neo
=>
|%
++  send-invites
  |=  [invites=(set ship) location=pith]
  %+  turn
    ~(tap in invites)
  |=  =ship
  :-  location
  =/  provider  ~[p/ship %home %messenger]
  ~&  provider
  [%poke groupchat-diff/!>([%invite ship provider])]
--
::
|%
++  state  pro/%sig
++  poke  (sy %dm-diff %groupchat-diff %messenger-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%t |]
      [pro/%dm (sy %dm-diff ~)]
      :-  [|/%t |]
      [pro/%groupchat (sy %groupchat-diff ~)]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    [~ sig/!>(~)]
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ~&  >>  stud
    ?+    stud  !!
        %dm-diff
      ~&  >>>  'got dm diff'
      =/  poke  !<(dm-diff vax)
      ?>  =(%invited -.poke)
      :_  state
      :~  :-  (welp here.bowl ~[%dms p/ship.src.bowl])
          [%make %dm `dm-diff/vax ~]
      ==
    ::
        %groupchat-diff
      =/  poke  !<(groupchat-diff vax)
      ?+    -.poke  !!
          %invited
        :_  state
        :~  :-  (welp here.bowl ~[%groupchats p/ship.src.bowl (rear host.poke)])
            [%make %groupchat `groupchat-diff/vax ~]
        ==
      ==
    ::
        %messenger-diff
      ?>  =(our ship.src):bowl
      =/  poke  !<(messenger-diff vax)
      ?-    -.poke
          %new-dm
        =/  provider  ~[p/partner.poke %home %messenger]
        :_  state
        :~  :-  (welp here.bowl ~[%dms p/partner.poke])
            [%make %dm `dm-diff/!>([%initiate partner.poke provider]) ~]
        ==
      ::
          %new-groupchat
        =/  location  
          (welp here.bowl ~[%groupchats p/our.bowl t/name.poke])
        :_  state
        :-  [location [%make %groupchat ~ ~]]
        (send-invites invites.poke location)
      ::
          %invite-to-groupchat
        =/  location  
          (welp here.bowl ~[%groupchats p/our.bowl t/name.poke])
        :_  state
        (send-invites invites.poke location)
      ==
    ==
  --
--