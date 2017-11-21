::
::::  /hoon/umd/mar
  ::
/?    310
::
|_  mud/@t
++  grow
  |%
  ++  mime  [/text/x-unmark (as-octs:mimes:html mud)]
  ++  txt
    (to-wain:format mud)
  ++  elem
    ^-  manx
    [/div ~(shut ap %xml (rash mud fenced:cram:vast))]
  --
++  grab
  |%
  ++  mime  |=({p/mite q/octs} q.q)
  ++  noun  @t
  ++  txt   of-wain:format
  --
++  grad  %txt
++  garb  /down
--
