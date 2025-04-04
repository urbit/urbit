/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
^-  thread:spider
=>  |%  +$  out  $%  [%sage =sage:mess:ames]
                     [%tune (pair spar:ames (unit roar:ames))]
                     [%page (pair spar:ames (unit (unit page)))]
                 ==
    --
::
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =spar:ames] arg)
;<  ~     bind:m  (meta:strandio /meta spar)
;<  =out  bind:m  (take-message:strandio /meta)
;<  ~     bind:m  (yawn:strandio /meta spar)
=/  =sage:mess:ames
  ?-  -.out
    %sage  sage.out
    %tune  :-  p.+.out
           ?~  q.+.out  ~
           ?~  q.dat.u.q.+.out  ~
           u.q.dat.u.q.+.out
    %page  :-  p.+.out
           ?~  q.+.out    ~
           ?~  u.q.+.out  ~
           u.u.q.+.out
  ==
?~  q.sage
  (pure:m !>([leaf+"... empty response ..." ~]))
?>  ?=(%message p.q.sage)
%-  pure:m  !>
?+  -.q.q.sage  !!
  %clos  ["flow in closing? " "{<;;(? +.q.q.sage)>}"]
  %gone  ["flow is corked?" "%.y"]
  %line  ["migration line: " "{<;;(@ud +.q.q.sage)>}"]
  %lods  ["unacked payloads: " "{<;;(@ud +.q.q.sage)>}"]
  %next  ["next payload: " "{<;;(@ud +.q.q.sage)>}"]
  %last  ["last acked: " "{<;;(@ud +.q.q.sage)>}"]
  %naxp  ["message nacked? " "{<;;(? +.q.q.sage)>}"]
  %curr  ["current message: " "{<;;(@ud +.q.q.sage)>}"]
==
