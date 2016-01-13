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
    (lore mud)
  --
++  grab
  |%
  ++  mime  |=([p=mite q=octs] q.q)
  ++  noun  ,@t
  ++  txt   role
  --
++  grad  %txt
--
