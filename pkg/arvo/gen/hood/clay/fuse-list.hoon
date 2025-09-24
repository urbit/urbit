::  List active Fuse octopus merge syncs
::
::::  /hoon/fuse/hood/gen
  ::
/+  *hood-kiln
/*  help-text  %txt  /gen/hood/clay/fuse/help/txt
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
