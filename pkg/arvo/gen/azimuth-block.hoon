::  Print most recently seen ethereum block
::
::  Note we require 30 confirmation blocks, so we should expect to have
::  processed only those blocks which are this number minus 30.
::
:-  %say
|=  [[now=@da tick=@ud @ our=@p ^] *]
:-  %tang
=;  block=@ud
  [leaf+(scow %ud block)]~
.^(@ud %gx (en-bema [our %eth-watcher [da+now ud+tick]] /block/azimuth/noun))
