::
::::  /hoon/sync-update/kiln/mar
  ::
/-  h=hood
|_  upd=sync-update:h
::
++  grow
  |%
  ++  noun  upd
  ++  json
    =,  enjs:format
    |^  ^-  ^json
    ?-    -.upd
        %new
      %+  frond  'new'
      (pairs ['for' (en-sync-record for.upd)] ['rev' (numb rev.upd)] ~)
    ::
        %done
      %+  frond  'done'
      (pairs ['for' (en-sync-record for.upd)] ['rev' (numb rev.upd)] ~)
    ::
        %drop
      %+  frond  'drop'
      (pairs ['for' (en-sync-record for.upd)] ['rev' (numb rev.upd)] ~)
    ::
        %pending
      %+  frond  'pending'
      :-  %a
      %+  turn  ~(tap by pending.upd)
      |=  [for=sync-record:h rev=@ud]
      %-  pairs
      :~  ['for' (en-sync-record for)]
          ['rev' (numb rev)]
      ==
    ==
    ++  en-sync-record
      |=  sync-record:h
      %-  pairs
      :~  ['syd' s+syd]
          ['her' s+(scot %p her)]
          ['sud' s+sud]
      ==
    --
  --
++  grab
  |%
  ++  noun  sync-update:h
  --
++  grad  %noun
--
