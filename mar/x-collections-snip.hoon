/?  310
|_  [hed=marl tal=marl]
++  grow
  |%
  ++  mime
    =<  mime
    |%
    ++  elem  ;div:(h1:"*{hed}" div:"*{tal}")           ::  convert to %elem
    ++  hymn  ;html:(head:title:"snip" body:"+{elem}")  ::  convert to %hymn
    ++  html  (crip (en-xml:^html hymn))                      ::  convert to %html
    ++  mime  [/text/html (as-octs:mimes:^html html)]               ::  convert to %mime
    --
  ++  collections-snip  [hed tal]
  --
::
++  grab
  |%
  +$  noun  [marl marl]
  --
--

