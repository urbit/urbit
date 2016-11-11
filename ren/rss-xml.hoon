::  Simple rss transformer
::
::::  /hoon/rss-xml/ren
  ::
/?    310
/=  our  /$  |=({bem/beam *} p.bem)
/=  top
  /.  /=  pax  /$  |=({bem/beam *} (slag (lent /web) (flop s.bem)))
      /=  inf  /%  /&front&/|(/front/ /~[~])
  ==
/=  kid  /^  :(map knot knot cord)
         /%  /_  /front/
!:
::::  ~fyr, ~tasfyn-partyv
  ::
|%
++  relative-link
  =/  external-host
    ~+(.^(hart %e /(scot %p our)/host/real))
  |=  a/path  ^-  tape
  (earn external-host `(weld pax.top a) ~)
::
++  parse-front
  |=  a/(map cord cord)
  :*  title=(fall (~(get by a) %title) '')
      preview=(fall (~(get by a) %preview) '')
      author=(fall (~(get by a) %author) '')
      date=(fall (~(get by a) %date) '')
  ==
--
::
::::
  ::
%-  crip  %-  poxo
;rss(version "2.0")
  ;channel
    ;*  =/  a  (parse-front inf.top)
        ;=
          ;title: {(trip title.a)}
          ;link: {(relative-link /)}
          ;description: {(trip preview.a)}
        ==
    ;*  %+  turn  (~(tap by kid))
        |=  {fyl/@t inf/(map knot cord)}
        =/  a  (parse-front inf)
        ;item
          ;link: {(relative-link /[fyl])}
          ;title: {(trip title.a)}
          ;author: {(trip author.a)}
          ;description: {(trip preview.a)}
          ;*  %-  drop
              %+  bind  (slaw %da date.a)
              |=  b/@da  ^-  manx
              [/'pubDate' ;/((dust (yore b))) ~]
        ==
  ==
==
