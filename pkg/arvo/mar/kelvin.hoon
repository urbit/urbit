=/  weft  ,[lal=@tas num=@ud]  ::  TODO remove after merge
|_  kel=weft
++  grow
  |%
  ++  mime  `^mime`[/text/x-kelvin (as-octs:mimes:html hoon)]
  ++  noun  kel
  ++  hoon  (crip "{<[lal num]:kel>}\0a")
  ++  txt   (to-wain:format hoon)
  --
++  grab
  |%
  ++  noun  weft
  ++  mime
    |=  [=mite len=@ud tex=@]
    !<(weft (slap !>(~) (ream tex)))
  --
++  grad  %noun
--
