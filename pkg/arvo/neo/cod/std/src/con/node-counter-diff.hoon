/@  node
/@  counter-diff
::  importing lib/manx-utils, provides us with easy sollutions for unpacking HTMl node
/-  manx-utils
::  converting node(html) to counter-diff
:-  [%node %$ %counter-diff]
::  takes node 
|=  =node
::  returns counter-diff
^-  counter-diff
::  providing incoming node to library door 
=/  mu     ~(. manx-utils node)
::  got:mu is using got arm of manx-utils library, and finds a coresponding to name of attribute data in recieved from POST request HTML
::  and we double check if that data is %inc 
=/  head   (?(%inc) (got:mu %head))
::  and finally return [%inc ~] counter-diff poke
[head ~]
