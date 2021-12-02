/-  column
|%
++  dejs
  =,  dejs:format
  |%
  ++  id    (se %uv)
  ++  ship  (se %p)
  ++  ref
    %-  of
    :~  feed+ship
        twit+so
    ==
  ++  diff
    ^-  $-(json diff:^column)
    %-  of
    :~  push-ref+ref
        pop-ref+ul
        replies+bo
        reversed+bo
    ==
  ++  column
    ^-  $-(json ^column)
    %-  ot
    :~  id+id
        ref+ref
        reversed+bo
        replies+bo
        hist+(ar ref)
    ==
  --
++  enjs
  =,  enjs:format
  |%
  ++  id  (cork (cury scot %uv) (lead %s))
  ++  ref
    |=  r=ref:^column
    %+  frond  -.r
    ?-  -.r
      %feed  s+(scot %p ship.r)
      %twit  s+id.r
    ==
  ::
  ++  column
    |=  col=^column
    %-  pairs
    :~  id+s+(scot %uv id.col)
        ref+(ref ref.col)
        reversed+b+reversed.col
        replies+b+replies.col
        hist+a+(turn hist.col ref)
    ==
  ::
  ++  diff
    |=  d=diff:^column
    %+  frond  -.d
    ?-  -.d
      %push-ref  (ref p.d)
      %pop-ref    ~
    ::
      ?(%replies %reversed)  b+p.d
    ==
  --
--


