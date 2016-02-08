::
::::  /hoon/markdown/mar
  ::
/?  314
::
|_  mud=@t
++  grow
  |%
  ++  mime  [/text/x-markdown (taco mud)]
  ++  md  mud
  ++  txt
    (lore mud)
  --
++  grab
  |%
  ++  mime  |=([p=mite q=octs] q.q)
  ++  noun  ,@t
  ++  md  |=(@t +<)
  ++  txt  role
  --
++  grad  %txt
--
