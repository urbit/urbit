/-  *bill
|_  bil=bill
++  grow
  |%
  ++  mime  `^mime`[/text/x-bill (as-octs:mimes:html hoon)]
  ++  noun  bil
  ++  hoon
    ^-  @t
    |^  (crip (of-wall:format (wrap-lines (zing (turn bil spit-chit)))))
    ::
    ++  wrap-lines
      |=  taz=wall
      ^-  wall
      ?~  taz  ["~"]~
      :-  (weld ":~  " i.taz)
      %-  snoc  :_  "=="
      (turn t.taz |=(t=tape (weld "    " t)))
    ::
    ++  spit-chit
      |=  =chit
      ^-  wall
      ?-  -.chit
        %apes  [":-  %apes" (wrap-lines (spit-duz duz.chit))]
        %fish  [":-  %fish" (wrap-lines (spit-duz duz.chit))]
      ==
    ::
    ++  spit-duz
      |=  duz=(list dude:gall)
      ^-  wall
      (turn duz |=(=dude:gall "%{<dude>}"))
    --
  ++  txt   (to-wain:format hoon)
  --
++  grab
  |%
  ++  noun  bill
  ++  mime
    |=  [=mite len=@ud tex=@]
    ~_  tex
    !<(bill (slap !>(~) (ream tex)))
  --
++  grad  %noun
--
