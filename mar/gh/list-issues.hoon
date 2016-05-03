/-  gh
|_  issues/(list issue:gh)
++  grab
  |%
  ++  noun  (list issue:gh)
  --
++  grow
  |%
  ++  json  [%a (turn issues |=(issue:gh raw))]
  ++  mime  [/txt/plain (taco (crip <issues>))]
  --
--
