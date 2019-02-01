/+  *test, collections
::
/=  app        /:  /===/app/collections  /!noun/
/=  gall-vane  /:  /===/sys/vane/gall    /!noun/
::
=,  gall
=>
|%
++  gall-args  [~1234.5.7 0vagine *slyt]
::
++  hype
  |=  tak=task:able:gall
  ^-  (hypo (hobo task:able:gall))
  [-:!>(tak) tak]
::
++  gall-gate  (gall-vane !>(..zuse))
--
::
|%
::
++  test-prep
  =/  prep-bone  20
  =/  our-ship   ~marzod
  =/  bol
    %*  .  *bowl:gall
      our  our-ship
      ost  prep-bone
    ==
  ::
  =/  moves  -:(~(prep app bol *state:collections) ~)
  ;:  weld
    %+  expect-eq
    !>  %-  sort
        :_  aor
        ^-  (list move:collections)
        :~  :*  prep-bone  %poke  /col-hall-action  [our-ship %hall]
                %hall-action  %create  %c  'collections'  %journal
            ==
          ::
            :*  prep-bone  %poke  /col-hall-action  [our-ship %hall]
                %hall-action  %source  %inbox  &  (sy [[our-ship %c] ~]~)
            ==
          ::
            :*  prep-bone  %peer  /circles  [our-ship %hall]
                /circles/[(scot %p our-ship)]
            ==
          ::
            :*  prep-bone  %peer  /inbox  [our-ship %hall]
                /circle/inbox/config/grams
            ==
          ::
            :*  prep-bone  %peer  /invites  [our-ship %hall]
                /circle/i/grams
            ==
        ==
    !>  (sort moves aor)
    ::
    =.  our-ship  ~davtyr-nimren
    =.  bol  bol(our our-ship)
    ::
    =/  moves  -:(~(prep app bol *state:collections) ~)
    %+  expect-eq
    !>  %-  sort
        :_  aor
        ^-  (list move:collections)
        :~  :*  prep-bone  %poke  /col-hall-action  [our-ship %hall]
                %hall-action  %create  %c  'collections'  %journal
            ==
          ::
            :*  prep-bone  %poke  /col-hall-action  [our-ship %hall]
                %hall-action  %source  %inbox  &  (sy [[our-ship %c] ~]~)
            ==
          ::
            :*  prep-bone  %peer  /circles  [our-ship %hall]
                /circles/[(scot %p our-ship)]
            ==
          ::
            :*  prep-bone  %peer  /inbox  [our-ship %hall]
                /circle/inbox/config/grams
            ==
          ::
            :*  prep-bone  %peer  /invites  [our-ship %hall]
                /circle/i/grams
            ==
          ::
        ==
    !>  (sort moves aor)
  ==
::
::++  test-load-app
::  =/  gall-door  (gall-gate gall-args)
::  =/  init  (call:gall-door [/test]~ (hype %init ~marzod))
::  %+  expect-eq
::  !>
::  !>
::
--
