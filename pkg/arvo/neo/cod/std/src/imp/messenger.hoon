/@  dm-diff
/@  groupchat-diff
/@  messenger-diff
^-  kook:neo
|%
++  state  pro/%sig
++  poke  (sy %dm-diff %groupchat-diff ~)
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
    ?+    stud  !!
        %dm-diff
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
        :_  state
        :~  :-  (welp here.bowl ~[%groupchats p/our.bowl t/name.poke])
            [%make %groupchat ~ ~]
        ==
      ::
          %invite-to-groupchat
        =/  provider  ~[p/ship.poke %home %messenger]
        :_  state
        :~  :-  (welp here.bowl ~[%groupchats p/our.bowl t/name.poke])
            [%poke groupchat-diff/!>([%invite ship.poke provider])]
        ==
      ==
    ==
  --
--