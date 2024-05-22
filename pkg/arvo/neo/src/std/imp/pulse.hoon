/@  pulse
/@  pulse-diff
::  sender ship namesapce (~bus)
::  /messages/1
::  host ship namespace
::  /chat/foo
::  /chat/foo/messages/1 :: symlink to /+bus/messages/1
::
::  /~zod/chat/foo/messages/1 <- []
::  /~zod/chat/foo/messages/1 <-
::  /~zod/chat/foo/messages/1 <-
::
::  /~bus/subs/foo -> /~zod/chat/foo
::
::  [/~bus/subs/foo/messages/1 %make ]
::  possibly optimisticaly update, then forward note to foreign ship
::
::  %make
:: ^-  kook:neo
=>
|%
++  behn
  |=  our=@p
  ^-  pith:neo
  [p/our #/$/behn]
++  wait
  |=  [=bowl:neo sta=pulse]
  ^-  card:neo
  [(behn our.bowl) %poke %behn-req !>([%wait (add now.bowl freq.sta)])]
++  rest
  |=  [=bowl:neo sta=pulse]
  ^-  card:neo
  [(behn our.bowl) %poke %behn-req !>([%rest (add [last freq]:sta)])]
::
--
|%
++  state  pro/%pulse
++  poke   %pulse-diff
  
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  +*  sta  !<(pulse state-vase)
  ++  poke
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    =/  sta  sta
    |^
    ?+    stud  !!
        %behn-res
      =.  count.sta  +(count.sta)
      =/  next  (wait bowl sta)  
      =.  last.sta  now.bowl
      [next^~ pulse/!>(sta)]
    ::
        %pulse-diff
      =+  !<(diff=pulse-diff vase)
      ?-  -.diff
          %reset  `!>(sta(count 0))
          %freq
        =/  stop  (rest bowl sta)
        =.  freq.sta  freq.diff
        =.  last.sta  now.bowl
        [stop^~ pulse/!>(sta)]
      ==
    ==
  --
  ::
  ++  init
    |=  old=(unit pail:neo)
    ?>  ?=(^ old)
    =+  !<(sta=pulse q.u.old)
    =.  last.sta  now.bowl
    [(wait bowl sta)^~ pulse/!>(sta)]
  ::
  --
--
