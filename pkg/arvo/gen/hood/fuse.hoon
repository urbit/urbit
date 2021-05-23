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
      res=[?([%cancel ~] [bas=fuse-source con=(list [fuse-source germ]) ~])]
  ==
--
:-  %say
|=  [* [arg=[?(~ fuse-arg)]] [overwrite=$~(| flag) ~]]
:-  %kiln-fuse
?~  arg
  ((slog (turn `wain`help-text |=(=@t leaf+(trip t)))) ~)
:-  des.arg
?:  ?=([%cancel ~] res.arg)
  ~
[overwrite bas.res.arg con.res.arg]
