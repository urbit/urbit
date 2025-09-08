::
::::  /hoon/snip/mar
  ::
/?    310
=,  html
::
::
=,  mimes:html
|_  [hed=marl tal=marl]
++  grad  %noun
::
++  grow                                                ::  convert to
  |%
  ++  mime
    =<  mime
    |%
    ++  hymn                                            ::  convert to %hymn
      |^  html
      ++  div    ;div:(h1:"*{hed}" div:"*{tal}")                                
      ++  html   ;html:(head:title:"snip" body:"+{div}")
      --
    ++  html  (crip (en-xml hymn))                      ::  convert to %html
    ++  mime  [/text/html (as-octs html)]               ::  convert to %mime
    --
  ++  noun  [hed tal]
  --
++  grab                                                ::  convert from
  |%
  ++  noun  ,[marl marl]                        ::  clam from %noun
  --
--
