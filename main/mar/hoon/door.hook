::
::::  /hook/door/hook/mar
  ::
/?  314
!:
|_  own=@t
::
++  grow                                                ::  convert to
  |%
  ++  mime  `^mime`[/text/hoon (taco own)]              ::  convert to %mime
  ++  psal  ;div:(pre:"{(trip own)}")                   ::  convert to %html
  ++  hymn  ;html:(head:title:"Source" "+{psal}")
  ++  txt
    (lore (cat 3 own '\0a'))
  --
++  grab
  |%                                            ::  convert from
  ++  mime  |=([p=mite q=octs] q.q)
  ++  noun  ,@t                                 ::  clam from %noun
  ++  txt
    |=  wan=wain
    ^-  @t
    =+  (role wan)
    (end 3 (dec (met 3 -)) -)
  --
++  grad
  |%
  ++  sted  %txt
  --
--
