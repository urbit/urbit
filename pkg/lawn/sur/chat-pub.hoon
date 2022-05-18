|%
++  ord  ((on @da writ) lte)
+$  writ  [seal memo]
+$  seal
  $:  =time
      feel=(jug term ship)
  ==
+$  memo
  $:  author=ship
      sent=@da
      content=@t
  ==
+$  chan-wave
  $%  [%add writ]
      [%del =ship =time]
      [%add-feel =ship =time =term]
      [%del-feel =ship =time =term]
  ==
+$  meta-wave
  $%  [%image @t]
      [%title @t]
      [%description @t]
  ==
+$  wave
  $%  [%chan =chan-wave]
      [%meta =meta-wave]
  ==
+$  rock
  $:  =chan
      =meta
  ==
+$  chan  ((mop @da writ) lte)
+$  meta
  $:  title=@t
      description=@t
      image=@t
  ==
++  wash
  |=  [=rock wav=wave]
  |^  ^+  rock
  ?-  -.wav
    %chan  rock(chan (wash-chan +.wav))
    %meta  rock(meta (wash-meta +.wav))
  ==
  ++  wash-chan
    |=  wav=chan-wave
    |^  ^+  chan.rock
    ?-    -.wav
        %add  (put:ord chan.rock [time +]:wav)
        %del
      ::  only the post's author can delete it
      ::
      =/  author  author:(need (get:ord chan.rock time.wav))
      ?>  =(ship.wav author)
      (del:ord chan.rock time.wav)
    ::
        %add-feel  (wash-feel & [ship time term]:wav)
        %del-feel  (wash-feel | [ship time term]:wav)
    ==
    ++  wash-feel
      |=  [add=? =ship =time =term]
      =/  =writ  (need (get:ord chan.rock time))
      ?:  add
        writ(feel (~(put ju feel.writ) term ship))
      writ(feel (~(del ju feel.writ) term ship))
    --
  ::
  ++  wash-meta
    |=  wav=meta-wave
    ^+  meta.rock
    ?-  -.wav
      %image        meta.rock(image +.wav)
      %title        meta.rock(title +.wav)
      %description  meta.rock(description +.wav)
   ==
  --
--
