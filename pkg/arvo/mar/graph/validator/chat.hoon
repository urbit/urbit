/-  store=graph-store
|_  [=atom =post:store]
++  grow
  |%
  ++  noun  [atom post]
  --
++  grab
  |%
  ++  noun
    |=  [=^atom =post:store]
    ^-  [^^atom post:store]
    ?>  ?=([@ ~] index.post)
    [atom post]
  --
::
++  grad  %noun
--
