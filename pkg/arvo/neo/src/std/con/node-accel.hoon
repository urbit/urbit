/@  node
/@  accel-diff
/-  _/manx-utils
:-  [%node %accel-diff]
|=  nod=node
^-  accel-diff
=/  mu  ~(. manx-utils nod)
=/  a  (vol:mu "a")
=/  b  (vol:mu "b")
::
=/  a-path  (rush a stap)
=/  b-path  (rush b stap)
::  check that if user typed anything at all
::  to the ref textboxes,
::  they were valid paths
?:  &(=(~ a-path) !=('' a))
    ~|  "not a valid path {<a>}"
    !!
?:  &(=(~ b-path) !=('' b))
    ~|  "not a valid path {<b>}"
    !!
:*  %new
    (slav %ud (got:mu %row))
    (slav %ud (got:mu %col))
    (vol:mu "code")
    ?~(a-path ~ `(pave:neo u.a-path))
    ?~(b-path ~ `(pave:neo u.b-path))
==
