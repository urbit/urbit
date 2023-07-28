/+  *generators
:-  %ask
|=  $:  [now=@da eny=@uvJ bec=beak]
        [syd=desk her=ship sud=desk ~]
        hard=_|
    ==
?:  hard  (produce %kiln-change-publisher syd her sud)
=/  m1
  'This will tell desk subscribers to switch update source.'
=/  m2
  'If you enter the wrong ship/desk, you will not be \
  /able to correct it.'
=/  m3
  leaf+"Are you sure you want to tell subscribers to get ".
  "updates for {<syd>} from {<her>}/{(trip sud)}?"
%+  print  m3
%+  print  m2
%+  print  m1
%+  prompt  [%& %prompt "(y/N) "]
|=  in=tape
?.  |(=("y" in) =("Y" in) =("yes" in))
  no-product
(produce %kiln-change-publisher syd her sud)
