/@  tweet
/@  tweet-diff
|%
++  state  pro/%tweet
++  poke
  %-  ~(gas in *(set stud:neo))
  :~  %tweet-diff
  ==
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  +*  sta  !<(tweet state-vase)  
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  sta  sta
    ?>  ?=(%tweet-diff stud)
    =+  !<(diff=tweet-diff vax)
    :-  ~
    :-  %tweet
    !>  ^-  tweet
    ?-  -.diff
      %add-fave  sta(favs (~(put in favs.sta) ship.src.bowl))
      %del-fave  sta(favs (~(del in favs.sta) ship.src.bowl))
    ==
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need pal)
  --
--
