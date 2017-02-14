::  Pretty-printing util, should be in lib/
::
::::  /hoon/pretty/cat/gen
  ::
/?    310
::
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
++  pretty-file
  =+  wain-to-tang=|=(a/wain (turn a |=(b/cord leaf+(trip b))))
  |=  fyl/*  ^-  tang
  =+  `(unit wain)`?@(fyl `(to-wain fyl) ((soft wain) fyl))
  ?^  -  (wain-to-tang u)
  [(pretty-noun fyl)]~
--
