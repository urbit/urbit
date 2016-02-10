::
::::  /hoon/comment/write/mar
  ::
!:
|_  {pax/path txt/@t}
++  grab
  |%
  ++  noun  {path @t}
  ++  json  
    (corl need =>(jo (ot pax+(su fel:stab) txt+so ~)))
  --
--
