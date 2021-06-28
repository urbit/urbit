=>  |%
    +$  kelvin  [lal=@tas num=@ud]
    --
|_  kel=kelvin
++  grow
  |%
  ++  mime  [/text/x-kelvin (as-octs:mimes:html hoon)]
  ++  noun  kel
  ++  hoon  (crip "{<[lal num]:kel>}\0a")
  ++  txt   (to-wain:format hoon)
  --
++  grab
  |%
  ++  noun  kelvin
  ++  mime
    |=  [=mite len=@ud tex=@]
    !<(kelvin (slap !>(~) (ream tex)))
  --
++  grad  %noun
--
