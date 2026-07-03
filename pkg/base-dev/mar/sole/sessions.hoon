::
::::  /hoon/sessions/sole/mar
  ::
/?    310
/-    sole
::
::::
  ::
=,  sole
|_  sessions=(set sole-id)
::
++  grad  %noun
++  grab                                                ::  convert from
  |%
  ++  noun  (set sole-id)                               ::  clam from %noun
  --
++  grow                                                ::  convert to
  |%
  ++  noun  sessions
  ++  json
    =,  enjs:format
    ^-  ^json
    :-  %a
    %+  turn  ~(tap in sessions)
    |=  id=sole-id
    %-  pairs
    :~  ship+(ship who.id)
        session+s+ses.id
    ==
  --
--
