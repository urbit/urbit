::
::::  /hoon/jump/kiln/mar
  ::
/-  h=hood
|_  jum=jump:h
::
++  grow
  |%
  ++  noun  jum
  ++  json
    =,  enjs:format
    |^  ^-  ^json
    ?-    -.jum
        %add
      %+  frond  'add'
      (pairs ['old' (en-dock old.jum)] ['new' (en-dock new.jum)] ~)
    ::
        %yea
      %+  frond  'yea'
      (pairs ['old' (en-dock old.jum)] ['new' (en-dock new.jum)] ~)
    ::
        %nay
      %+  frond  'nay'
      (pairs ['old' (en-dock old.jum)] ['new' (en-dock new.jum)] ~)
    ::
        %all
      %+  frond  'all'
      :-  %a
      %+  turn  ~(tap by all.jum)
      |=  [old=dock new=dock]
      (pairs ['old' (en-dock old)] ['new' (en-dock new)] ~)
    ==
    ++  en-dock
      |=  =dock
      (pairs ['ship' s+(scot %p p.dock)] ['desk' s+q.dock] ~)
    --
  --
++  grab
  |%
  ++  noun  jump:h
  --
++  grad  %noun
--
