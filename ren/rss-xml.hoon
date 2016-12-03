::  Simple rss transformer
::
::::  /hoon/rss-xml/ren
  ::
/?    310
/=  sum  /&snip&elem&/|(/elem/ /~[;div;])
/=  kid  /^  (map knot {marl marl})
         /_  /&snip&/elem/
/$    |=({bem/beam *} [our=p.bem tub=(slag 1 (flop s.bem))])
::
::::  ~fyr, ~tasfyn-partyv
  ::
::~&  [sum=sum kid=kid]
::  Link from relative path
=,  ^eyre
=,  html
=+  hok=.^(hart %e /(scot %p our)/host/real)
=+  ref=|=(a/path (en-purl hok `(weld tub a) ~))
::  urb:front attrs confuse RSS validators, readers
=+  no-meta=|=(a/marl ^+(a ?~(a ~ ?.(?=($meta n.g.i.a) a $(a t.a)))))
::
%-  crip  %-  print
;rss(version "2.0")
  ;channel
    ;title: *{hed.sum}
    ;link: {(ref /)}
    ;description: *{(no-meta tal.sum)}
    ;*  %+  turn  (~(tap by kid))
        |=  {nom/@t hed/marl tal/marl}
        ;item
          ;title: *{hed}
          ;description: *{(no-meta tal)}
          ;link: {(ref /[nom])}
        ==
  ==
==
