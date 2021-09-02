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
+$  fuse-list-arg  $@(~ [des=desk ~])
--
:-  %say
|=  [* [arg=fuse-list-arg] ~]
:-  %kiln-fuse-list
?~  arg
  ~
`des.arg
