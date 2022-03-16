=>
|%
++  msgs  (map term $)
++  gall  [=msgs > [=agents body]:(by-msgs msgs)]
++  by-msgs
  |=  msgs/msgs
  |%
  ++  mesg    |=  app/term  (by msgs gut: term _!!)
  ++  move    [app=term : con=(mesg app)]
  ++  bond    |=  app/term  |-  $-((mesg app) [(list move) %])
  ++  agents  (map =term (bond term))
  ++  body
    $|
    ++  start  $-([nom=term cod=code hed=code] gall)
    ++  send   $-(=move [(list move) gall])
    --
  --
--
^-  gall
=<  [msgs agents garl]
=|  =msgs
=|  =agents:(by-msgs msgs)
^?
|%  self
++  start
  |=  [nom/term cod/code hed/code]
  ^-  gall
  ?:  (by msgs has: nom)
    ~&  already-loaded=nom
    [msgs agents self]
  =/  typ  !*  zuse  $  hed
  ?~  typ
    ~&  bad-typ=hed
    [msgs agents self]
  =.  msgs  (by msgs put: nom u.typ)
  =/  bon  !*  zuse  (bond:(by-msgs msgs) app)  cod
  ?~  bon
    ~&  bad-bond=cod
    [msgs agents self]
  =.  agents  (by agents put: nom u.bon)
  [msgs agents self]
::
++  send
  |=  mov/move
  ^-  gall
  =/  [out nex]  (by agents gut: app.mov : con.mov)
  =.  agents  (by agents put: app.mov nex)
  ::  depth first rather than breadth first; bf would
  ::  require a queue but this is still illustrative.
  (roll move gall nex [msgs agents self] |=([mov gal] (send:gal mov)))
--

=/  hi  ^-  [@ : (vect - @)]  [1 2]
|-
?:  =(0 -.hi)
  %done
%(hi [(dec -.hi) tel.+.hi])

=/  dojo  ...
=/  hood  ...
%.  [%dojo dojo]~
|=  ag/[term > (bond -)]
^+  .
=?  ag  ?=(%dojo -.i.ags)
  [%hood hood]
..%
