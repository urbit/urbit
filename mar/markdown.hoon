::
::::  /hoon/markdown/mar
  ::
/?    310
::
=,  bytes:eyre
=,  format
|_  mud/@t
++  grow
  |%
  ++  mime  [/text/x-markdown (taco mud)]
  ++  md  mud
  ++  txt
    (to-wain mud)
  --
++  grab
  |%
  ++  mime  |=({p/mite q/octs} q.q)
  ++  noun  @t
  ++  md  |=(@t +<)
  ++  txt  of-wain
  --
++  grad  %txt
--
