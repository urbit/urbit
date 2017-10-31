::
::::  /hoon/umd/mar
  ::
/?    310
/+    cram
::
|_  mud/@t
++  grow
  |%
  ++  mime  [/text/x-markdown (as-octs:mimes:html mud)]
  ++  txt
    (to-wain:format mud)
  ++  elem
    ^-  manx
    elm:(static:cram (ream mud))
  --
++  grab
  |%
  ++  mime  |=({p/mite:eyre q/octs:eyre} q.q)
  ++  noun  @t
  ++  txt   of-wain:format
  --  
++  grad  %txt
++  garb  /down
--
