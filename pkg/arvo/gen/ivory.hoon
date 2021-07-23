::  Produce an ivory pill
::
::::  /hoon/ivory/gen
  ::
/?    310
/+  pill
::
::::
  !:
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=$@(~ [top=path ~])
        ~
    ==
:-  %noun
^-  pill:pill
=/  sys=path
  ?^  arg  top.arg
  /(scot %p p.bec)/[q.bec]/(scot %da now)/sys
=/  lib
  (welp (flop (tail (flop sys))) /lib)
::
|^  =/  ver
      =/  sub  *(trap vase)
      =.  sub  (build-sys sub %hoon)
      =.  sub  (build-sys sub %arvo)
      =.  sub  (build-sys sub %lull)
      =.  sub  (build-sys sub %zuse)
      =.  sub  (build-lib sub & %ethereum)
      =.  sub  (build-lib sub & %azimuth)
      (build-lib sub | %vere)
    =/  nok  !.
      =>  *[ver=(trap vase) ~]
      !=  q:$:ver
    ivory/[nok ver ~]
::
++  build-sys
  |=  [sub=(trap vase) nam=term]  ^-  (trap vase)
  ~>  %slog.[0 leaf+"ivory: building /sys/{(trip nam)}"]
  (swat sub (rain /sys/[nam]/hoon .^(@t cx+(welp sys /[nam]/hoon))))
::
++  build-lib
  |=  [sub=(trap vase) imp=? nam=term]  ^-  (trap vase)
  ~>  %slog.[0 leaf+"ivory: building /lib/{(trip nam)}"]
  =/  hun=hoon
    %+  mist  /lib/[nam]/hoon
    .^(@t cx+(welp lib /[nam]/hoon))
  ?.  imp  (swat sub hun)
  (swel sub [%ktts nam hun])
::  +mist: +rain but skipping past ford runes
::
++  mist
  |=  [bon=path txt=@]
  ^-  hoon
  =+  vas=vast
  ~|  bon
  %+  scan  (trip txt)
  %-  full
  =;  fud
    (ifix [;~(plug gay fud) gay] tall:vas(wer bon))
  %-  star
  ;~  pose  vul
    %+  ifix  [fas (just `@`10)]
    (star ;~(less (just `@`10) next))
  ==
::  +swel: +swat but with +slop
::
++  swel
  |=  [tap=(trap vase) gen=hoon]
  ^-  (trap vase)
  =/  gun  (~(mint ut p:$:tap) %noun gen)
  =>  [tap=tap gun=gun]
  |.  ~+
  =/  pro  q:$:tap
  [[%cell p.gun p:$:tap] [.*(pro q.gun) pro]]
--
