/-  *publish
|_  upd=update
++  grab
  |%
  ++  noun  update
  --
++  grow
  |%
  ++  noun  upd
  ++  json
    =,  enjs:format
    %+  frond  -.upd
    ::
    ?-  -.upd
        %invite
      %-  pairs
      :~  [%who (ship who.upd)]
          [%add b+add.upd]
          [%coll s+col.upd]
          [%title s+title.upd]
      ==
    ::
        %unread
      %-  pairs
      :~  [%add b+add.upd]
          :+  %posts
            %a
          %+  turn  ~(tap in keys.upd)
          |=  [who=@p coll=@tas post=@tas]
          ^-  ^json
          %-  pairs
          :~  [%who (ship who)]
              [%coll s+coll]
              [%post s+post]
          ==
      ==
    ::
    ==
    ::
  --
--
