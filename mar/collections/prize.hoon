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
    %+  frond  %landscape
    %+  frond  %prize
    %-  pairs
    :~  :-  %inbox
        %-  pairs
        :~  [%config ?~(con.inbox.piz ~ (conf:enjs:hall-json u.con.inbox.piz))]
            [%messages %a (turn env.inbox.piz enve:enjs:hall-json)]
        ==
      ::
        :+  %circles  %a
        %+  turn  ~(tap by circles.piz)
        |=  [cir=circle:hall con=(unit config:hall)]
        %-  pairs
        :~  [%circle (circ:enjs:hall-json cir)]
            [%config ?~(con ~ (conf:enjs:hall-json u.con))]
        ==
      ::
        :+  %circles-our  %a
        %+  turn  ~(tap in our-circles.piz)
        |=  nom=name:hall
        [%s nom]
      ::
        :-  %dms
        %-  pairs
        %+  turn  ~(tap by dms.piz)
        |=  [nom=name:hall ini=@p env=(list envelope:hall)]
        :-  nom
        %-  pairs
        :~  [%initiator (ship ini)]
            [%messages %a (turn env enve:enjs:hall-json)]
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
