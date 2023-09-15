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
    %+  frond  -.jum
    ?-    -.jum
        %add  (pairs ['old' (en-dock old.jum)] ['new' (en-dock new.jum)] ~)
        %yea  (pairs ['old' (en-dock old.jum)] ['new' (en-dock new.jum)] ~)
        %nay  (pairs ['old' (en-dock old.jum)] ['new' (en-dock new.jum)] ~)
        %all
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
