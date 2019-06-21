::
::
/?  309
::
/-  hall
/+  chat, hall-json
::
|_  num=(list [circle:hall @ud])
++  grow
  |%
  ++  json
    =,  enjs:format
    ^-  ^json
    %+  frond  %chat
    %-  pairs
    :~
      ::
      ::  %config
        :-  %numbers
        :-  %a
        %+  turn  num
        |=  [cir=circle:hall len=@ud]
        ^-  json
        %-  pairs
        :~
          [%circle (circ:enjs:hall-json cir)]
          [%length (numb len)]
        ==
      ::
      ::  %noop
      [*@t *^json]
    ==
  --
::
++  grab
  |%
  ++  noun  (list [circle:hall @ud])
  --
--
