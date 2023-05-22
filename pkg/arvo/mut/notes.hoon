/-  shrub, note
|%
+$  card  card:shrub
+$  delta
  $%  [%add =note]
      [%del ~]
  ==
+$  diff  (pair iota delta)
--
|_  [=bowl:shrub notes=(map iota note)]
++  mult  &
++  diff  ^diff
+$  rock  note
++  poke
  |=  [id=iota del=delta]
  ^-  (list card:shrub)
  ::  ?>  (do-permissions src.bowl)
  =-  ~[-]
  ^-  card:shrub
  :-  %give
  :-  id
  ?-  -.del
    %add  [%add /(scot %p p.our.bowl)/base/(scot %da now.bowl)/sig/note/hoon !>(note.del)]
    %del  [%del ~]
==
--
