::  Simple rss transformer
::
::::  /hoon/rss-xml/ren
  ::
/?    314
/=  sum  /snip/
/^  kid=(map span ,[marl marl])
    /_  /snip/
/$    |=([bem=beam but=path] [our=p.bem tub=(slag 1 (flop but))])
::
::::  ~fyr
  ::
::  Link from relative path
=+  hok=`hart`[& ~ `/org/urbit/(rsh 3 1 (scot %p our))]
=+  ref=|=(a=path (earn hok `(weld tub a) ~))
::  urb:front attrs confuse RSS validators, readers
=+  no-meta=|=(a=marl `_a`?~(a ~ ?.(?=(%meta n.g.i.a) a $(a t.a))))
::
%-  crip  %-  poxo
;rss(version "2.0")
  ;channel
    ;title: *{hed.sum}
    ;link: {(ref /)}
    ;description: *{(no-meta tal.sum)}
    ;*  %+  turn  (~(tap by kid))
        |=  [nom=@t hed=marl tal=marl]
        ;item
          ;title: *{hed}
          ;description: *{(no-meta tal)}
          ;link: {(ref /[nom])}
        ==
  ==
==
