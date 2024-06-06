/@  node          ::  manx
/@  counter-diff  ::  [%inc ~]
::  import /lib/manx-utils, which helps us work with XML
/-  manx-utils
::  declare this is a conversion from node to counter-diff
:-  [%node %$ %counter-diff]
|=  =node
^-  counter-diff
::  initiate the manx-utils door with node
=/  mu  ~(. manx-utils node)
::
::  got:mu gets an attribute from the manx by its name
::  in this case, the =head specified in /con/number-htmx
::  we expect the head from the manx to be %inc,
::  but we could add more terms to that type union...
=/  head  (?(%inc) (got:mu %head))
::
::  return the [%inc ~] poke
::  if we wanted to handle multiple pokes,
::  we'd switch on the type of head here
[head ~]
