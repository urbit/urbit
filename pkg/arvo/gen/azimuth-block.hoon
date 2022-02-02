::  Print most recently seen ethereum block
::
::  Note we require 30 confirmation blocks, so we should expect to have
::  processed only those blocks which are this number minus 30.
::
:-  %say
|=  [[now=@da @ our=@p ^] *]
:-  %tang
=;  block=@ud
  [leaf+(scow %ud block)]~
.^  @ud
  %gx
  /(scot %p our)/eth-watcher/(scot %da now)/block/azimuth/noun
==
