::
::::  /hoon/down-jet/lib
  ::
/-    *markdown
::
::::
  ::
//    /%/parse                       ::  inli donp parse
//    /%/rend                        ::  sing sung sang flat into-inner
|%
++  down-jet
  ~%  %down  ..is  ~
  |%
  ++  mark
    ~/  %mark
    |=  p=@t
    (normalize (rash p parse))
  ::
  ++  print  sing
  --
--
