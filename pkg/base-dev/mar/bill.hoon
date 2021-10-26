|_  bil=(list dude:gall)
++  grow
  |%
  ++  mime  `^mime`[/text/x-bill (as-octs:mimes:html hoon)]
  ++  noun  bil
  ++  hoon
    ^-  @t
    |^  (crip (of-wall:format (wrap-lines (spit-duz bil))))
    ::
    ++  wrap-lines
      |=  taz=wall
      ^-  wall
      ?~  taz  ["~"]~
      :-  (weld ":~  " i.taz)
      %-  snoc  :_  "=="
      (turn t.taz |=(t=tape (weld "    " t)))
    ::
    ++  spit-duz
      |=  duz=(list dude:gall)
      ^-  wall
      (turn duz |=(=dude:gall ['%' (trip dude)]))
    --
  ++  txt   (to-wain:format hoon)
  --
++  grab
  |%
  ++  noun  (list dude:gall)
  ++  mime
    |=  [=mite len=@ud tex=@]
    ~_  tex
    !<((list dude:gall) (slap !>(~) (ream tex)))
  --
++  grad  %noun
--
