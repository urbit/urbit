/-  docs
=/  act-sur
  '$%  [%title title=@t]\0a [%add =ship]\0a [%del =ship]\0a =='
=/  msg-docs=docs:docs
  :+  `meta:docs`['message' 'A message type for chat applications']
    :+  *poke:docs
      *deps:docs
    *scry:docs
  *kids:docs
^-  docs:docs
:+  `meta:docs`['chat' 'A container for chat messages']
  :+  [act-sur '%title: set title on container\0a%add: add ship to permissions set\0a%del: remove ship from permissions set']
    *deps:docs
  *scry:docs
%-  ~(gas by *kids:docs)
[~[|/%da] msg-docs]^~
