::
::::  /hoon/core/elem/mar
  ::
/?  314
!:
|%
++  getall
  |=  tag=mane
  |=  ele=manx  ^-  marl
  ?:  =(tag n.g.ele)  ~[ele]
  (zing (turn c.ele ..$))
--
::
!:
|_  hed=marl
::
++  grow                                                         ::  convert to
  |%
  ++  mime
    =<  mime
    |%
    ++  elem  ;div:"*{hed}"
    ++  hymn  ;html:(head:title:"headers" body:"+{elem}")        ::  convert to %hymn
    ++  html  (crip (poxo hymn))                                 ::  convert to %html
    ++  mime  [/text/html (taco html)]                           ::  convert to %mime
    --
  --
++  grab  |%                                                     ::  convert from
          ++  noun  marl                                         ::  clam from %noun
          ++  elem  (getall %h1)
--        --
