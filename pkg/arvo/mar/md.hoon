::
::::  /hoon/md/mar
  ::
/?    310
::
=,  format
=,  mimes:html
|_  txt/wain
::
++  grab                                                ::  convert from
  |%
  ++  mime  |=({p/mite:eyre q/octs:eyre} (to-wain q.q))
  ++  noun  wain                                        ::  clam from %noun
  --
++  grow
  |%
  ++  mime  [/text/plain (as-octs (of-wain txt))]
  --
++  grad  %mime
--
