::  Kiln: Fuse local desk from (optionally-)foreign sources
::
::::  /hoon/fuse/hood/gen
  ::
/+  *hood-kiln
/*  help-text  %txt  /gen/hood/fuse/help/txt
=,  clay
::
::::
  ::
=>
|%
+$  fuse-arg
  $:  des=desk
      ::  specified as [germ path] instead of [path germ] so
      ::  users can write mate//=base= instead of [/=base= %mate]
      ::
      res=[?([%cancel ~] [bas=path con=(list [germ path])])]
  ==
::
++  parse-fuse-source
  |=  bec=beak
  ^-  fuse-source
  ::  This is a slight overload of the label, but
  ::  it provides a nicer interface for the user so
  ::  we'll go with it.
  ::
  ?:  ?=([%tas *] r.bec)
    ?:  =(p.r.bec %track)
      [p.bec q.bec %trak]
    bec
  bec
::
++  de-beak
  |=  pax=path
  ^-  beak
  =/  bem=beam  (need (de-beam pax))
  ?>  =(s.bem /)
  -.bem
::
++  path-to-fuse-source
  |=  pax=path
  ^-  fuse-source
  (parse-fuse-source (de-beak pax))
--
:-  %say
|=  [* [arg=[?(~ fuse-arg)]] [overwrite=$~(| flag) ~]]
:-  %kiln-fuse
?~  arg
  ((slog (turn `wain`help-text |=(=@t leaf+(trip t)))) ~)
:-  des.arg
?:  ?=([%cancel ~] res.arg)
  ~
:+  overwrite
  (path-to-fuse-source bas.res.arg)
%+  turn
  con.res.arg
|=  [g=germ pax=path]
^-  [fuse-source germ]
[(path-to-fuse-source pax) g]
