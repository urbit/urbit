::  Untyped best-guess printer
::
::::  /hoon/pretty-file/lib
  ::
/?    310
::
=<  pretty-file
|%
++  pretty-noun
  |=  pri/*  ^-  tank
  ?~  pri
    leaf+"~"
  ?@  pri
    leaf+?:(((sane %tas) pri) <`@tas`pri> <pri>)
  =<  rose+[" " ?:(- "~[" "[") "]"]^+
  |-  ^-  {? (list tank)}
  ?~  +.pri
    [& ^$(pri -.pri) ~]
  ?@  +.pri
    [| ^$(pri -.pri) ^$(pri +.pri) ~]
  [+< - +>]:[^$(pri -.pri) $(pri +.pri)]
::
++  vale-cord  |=(a/cord `?`(levy (trip a) |=(b/@ |((gte b 32) =(10 b)))))
::
++  wain-to-tang  |=(a/wain (turn a |=(b/cord leaf+(trip b))))
++  pretty-file
  |=  fyl/*  ^-  tang
  =+  `(unit wain)`?@(fyl `(to-wain:format fyl) ((soft wain) fyl))
  ?^  -  (wain-to-tang u)
  [(pretty-noun fyl)]~
--
