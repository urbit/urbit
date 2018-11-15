::
::
/?  309
::
/-  hall
/+  collections, hall-json
::
|_  piz=prize:collections
++  grow
  |%
  ++  json
    =,  enjs:format
    ^-  ^json
    %-  pairs
    :~  :-  %inbox
        %-  pairs
        :~  [%config ?~(con.inbox.piz ~ (conf:enjs:hall-json u.con.inbox.piz))]
            [%envelopes %a (turn env.inbox.piz enve:enjs:hall-json)]
        ==
      ::
        :+  %our-circles  %a
        %+  turn  ~(tap by our-circles.piz)
        |=  [cir=circle:hall con=(unit config:hall)]
        %-  pairs
        :~  [%circle (circ:enjs:hall-json cir)]
            [%config ?~(con ~ (conf:enjs:hall-json u.con))]
        ==
      ::
        :+  %sub-circles  %a
        %+  turn  ~(tap by our-circles.piz)
        |=  [cir=circle:hall con=(unit config:hall)]
        %-  pairs
        :~  [%circle (circ:enjs:hall-json cir)]
            [%config ?~(con ~ (conf:enjs:hall-json u.con))]
        ==
      ::
        [%invites %a (turn invites.piz enve:enjs:hall-json)]
    ==
  --
::
++  grab
  |%
  ++  noun  prize:collections
  --
--
