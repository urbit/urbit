/+  *generators
:-  %ask
|=  $:  ^
        [syd=desk her=ship sud=desk ~]
        hard=_|
    ==
?:  hard  (produce %kiln-jump-propose syd her sud)
=/  msg
  leaf+"Are you sure you want to tell subscribers to get ".
  "updates for {<syd>} from {<her>}/{(trip sud)}?"
%+  print  msg
%+  prompt  [%& %prompt "(y/N) "]
|=  in=tape
?.  |(=("y" in) =("Y" in) =("yes" in))
  no-product
(produce %kiln-jump-propose syd her sud)
