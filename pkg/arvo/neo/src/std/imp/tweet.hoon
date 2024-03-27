/@  tweet
/@  tweet-diff
|%
++  state  %tweet
++  poke
  %-  ~(gas in *(set stud:neo))
  :~  %tweet-diff
  ==
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<(tweet state-vase)  
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    =/  sta  sta
    ?>  ?=(%tweet-diff stud)
    =+  !<(diff=tweet-diff vax)
    :-  ~
    !>  ^-  tweet
    ?-  -.diff
      %add-fave  sta(favs (~(put in favs.sta) ship.src.bowl))
      %del-fave  sta(favs (~(del in favs.sta) ship.src.bowl))
    ==
  ::
  ++  init
    |=  vas=(unit vase)
    ^-  (quip card:neo vase)
    =+  !<(=tweet (need vas))
    `!>(tweet)
  --
--
