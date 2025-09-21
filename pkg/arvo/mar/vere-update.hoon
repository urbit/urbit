|_  [cur=vere next=(unit vere)]
++  en-vere
  |=  v=vere
  %-  pairs:enjs:format
  :-  [%non s+non.v]
  :-  [%rev (path:enjs:format rev.v)]
  %+  turn  kel.v
  |=  w=weft
  [lal.w (numb:enjs:format num.w)]
::
++  grab
  |%
  ++  noun  ,[cur=vere next=(unit vere)]
  --
++  grow
  |%
  ++  json
    %-  pairs:enjs:format
    :-  [%cur (en-vere cur)]
    ?~  next  ~
    :-  [%next (en-vere u.next)]  ~
  --
++  grad  %noun
--
