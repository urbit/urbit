!:
::  /=try=/bin/input/hoon
::
=>  .(+ =>(+ ^/=main=/pony))
|=  *
|=  ~
=<  main
|%
++  looc  ;~(pose alp (shim 128 255))
++  loon  
  %+  cook
    |=  all=(list ,@t)
    |-  ^-  @t
    ?~  all  %$
    ?~  t.all  i.all
    (cat 3 i.all (cat 3 ' ' $(all t.all)))
  (most ace (cook |=(a=(list ,@) (rap 3 a)) (plus looc)))
::
++  main
  %+  (polo ~ %text "Middle name (or blank): " ~ ~)
    ;~(pose (stag ~ loon) (easy ~))
  |=  [* mid=(unit ,@t)]
  :_  ~
  :~  [%la >mid<]
  ==
--
