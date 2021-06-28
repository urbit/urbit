=>  |%
    +$  bill  (list chit)
    +$  chit
      $%  [%apes duz=(list dude:gall)]
          [%fish duz=(list dude:gall)]
      ==
    --
|_  bil=bill
++  grow
  |%
  ++  mime  [/text/x-bill (as-octs:mimes:html hoon)]
  ++  noun  bil
  ++  hoon
    |^  (crip (wrap-chits (turn bil spit-chit)))
    ::
    ++  wrap-chits
      =/  res=tape  ":~  "
      |=  taz=(list tape)
      ^-  tape
      ?:  =(~ taz)  "~"
      |-  ^+  res
      ?~  taz  (weld res "==\0a")
      $(taz t.taz, res :(weld res "    " i.taz "\0a"))
    ::
    ++  spit-chit
      |=  =chit
      ^-  tape
      ?-  -.chit
        %apes  (weld (spit-tag %apes) (spit-list duz.chit))
        %fish  (weld (spit-tag %fish) (spit-list duz.chit))
      ==
    ::
    ++  spit-tag  |=(tag=term ":-  {<tag>}\0a")
    ++  spit-list
      =/  res=tape  ":~  "
      |=  duz=(list dude:gall)
      ^-  tape
      ?:  =(~ duz)  "~"
      |-  ^+  res
      ?~  duz  (weld res "==\0a")
      $(duz t.duz, res :(weld res "    " (trip i.duz) "\0a"))
    --
  ++  txt   (to-wain:format hoon)
  --
++  grab
  |%
  ++  noun  bill
  ++  mime
    |=  [=mite len=@ud tex=@]
    !<(bill (slap !>(~) (ream tex)))
  --
++  grad  %noun
--
