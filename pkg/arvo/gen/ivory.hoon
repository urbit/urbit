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
      (build-lib sub %vere)
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
  |=  [sub=(trap vase) nam=term]  ^-  (trap vase)
  ~>  %slog.[0 leaf+"ivory: building /lib/{(trip nam)}"]
  (swat sub (rain /lib/[nam]/hoon .^(@t cx+(welp lib /[nam]/hoon))))
--
