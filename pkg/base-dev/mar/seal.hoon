|_  both=[must=(list perm:gall) may=(list perm:gall)]
++  grow
  |%
  ++  mime  `^mime`[/text/x-seal (as-octs:mimes:html hoon)]
  ++  noun  both
  ++  hoon
    ^-  @t
    |^  %-  crip
        %-  of-wall:format
        :-  ":-"
        %+  weld
          %+  enlist  1
          (enhoon must.both)
        %+  enlist  0
        (enhoon may.both)
    ::
    ++  enlist
      |=  [dep=@ud taz=wall]
      ^-  wall
      =+  pre=(reap (mul dep 2) ' ')
      ?~  taz  [(snoc pre '~')]~
      :-  :(weld pre ":~  " i.taz)
      %-  snoc  :_  (weld pre "==")
      (turn t.taz |=(t=tape :(weld pre "    " t)))
    ::
    ++  enhoon
      |=  pez=(list perm:gall)
      ^-  wall
      (turn pez |=(=perm:gall <perm>))
    --
  ++  txt   (to-wain:format hoon)
  --
++  grab
  |%
  ++  noun  ,[(list perm:gall) (list perm:gall)]
  ++  mime
    |=  [=mite len=@ud tex=@]
    ~_  tex
    !<([(list perm:gall) (list perm:gall)] (slap !>(~) (ream tex)))
  --
++  grad  %noun
--
