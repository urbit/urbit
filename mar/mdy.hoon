::
::::  /hoon/core/mdy/pro
  ::
/?  314
/+  frontmatter
!:
|_  [atr=(map cord cord) mud=@t]
++  grow
  |%
  ++  front  atr
  ++  mime  [/text/x-markdown (taco md)]
  ++  md  (role txt)
  ++  txt  (print:frontmatter atr (lore mud))
  --
++  grab
  |%
  ++  noun  ,[(map cord cord) @t]
  ++  md  (cork lore txt)
  ++  txt  parse:frontmatter
  --
++  grad  %txt
--
