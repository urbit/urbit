=>  |%
    ++  ord  ((on @da post) lte)
    --
|%
+$  writ  [seal memo]
+$  seal
  $:  =time
      feel=(jug term ship)
  ==
+$  chan-wave
  $%  [%add writ]
      [%del =time]
      [%add-feel =time =term =ship]
      [%del-feel =time =term =ship]
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
+$  chan  ((mop @da post) lte)
+$  memo  [author=ship sent=@da content=@t]
+$  meta
  $:  title=@t
      description=@t
      image=@t
  ==
++  wash
  |=  [=rock wav=wave]
  |-  ^+  rock
  ?-  -.wav
    %chan  rock(chan (wash-chan +.wav))
    %meta  rock(meta (wash-meta +.wav))
  ==
  ++  wash-chan
    |=  wav=chan-wave
    |^  ^+  chan.rock
    ?-    -.wav
      %add  (put:ord chan.rock [time +]:wav)
      %del  (del:ord chan.rock time.wav)
      %add-feel  (wash-feel & [time term ship]:wav)
      %del-feel  (wash-feel | [time term ship]:wav)
    ==
    ++  wash-feel
      |=  [add=? =time =term =ship]
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
