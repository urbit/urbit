::
::::  /hoon/core/md/pro
  ::
/?  314
::
|_  mud=@t
++  grow
  |%
  ++  mime  [/text/x-markdown (taco mud)]
  ++  txt
    (lore (cat 3 mud '\0a'))
  --
++  grab
  |%
  ++  mime  |=([p=mite q=octs] q.q)
  ++  noun  ,@t
  ++  txt
    |=  wan=wain
    =+  (role wan)
    (end 3 (dec (met 3 -)) -)
  --
++  grad  %txt
++  garb  %down
--
