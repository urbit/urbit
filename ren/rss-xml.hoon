::  Simple rss transformer
::
::::  /hoon/rss-xml/ren
  ::
/?    310
/=  sum  /%  /&front&/|(/front/ /~[~])
/=  kid  /^  :(map knot knot cord)
         /%  /_  /front/
/$    |=({bem/beam *} [our=p.bem tub=(slag 1 (flop s.bem))])
!:
::::  ~fyr, ~tasfyn-partyv
  ::
::~&  [sum=sum kid=kid]
::  Link from relative path
=+  hok=.^(hart %e /(scot %p our)/host/real)
=+  ref=|=(a/path (earn hok `(weld tub a) ~))
=/  atrs
  |=  a/(map cord cord)
  :*  title=(fall (~(get by a) %title) '')
      preview=(fall (~(get by a) %preview) '')
      author=(fall (~(get by a) %author) '')
      date=(fall (~(get by a) %date) '')
  ==
::
%-  crip  %-  poxo
;rss(version "2.0")
::   ;raw: *{(turn (wash 0^80 >% .<) |=(a/tape ;l:"{a}"))}
  ;channel
    ;*  =/  a  (atrs sum)
        ;=
          ;title: {(trip title.a)}
          ;link: {(ref /)}
          ;description: {(trip preview.a)}
        ==
    ;*  %+  turn  (~(tap by kid))
        |=  {nom/@t som/(map knot cord)}
        =/  a  (atrs som)
        ;item
          ;link: {(ref /[nom])}
          ;title: {(trip title.a)}
          ;author: {(trip author.a)}
          ;description: {(trip preview.a)}
          ;*  %-  drop
              %+  bind  (slaw %da date.a)
              |=  b/@da  ^-  manx
              ;date:"{(dust (yore b))}"
        ==
  ==
==
