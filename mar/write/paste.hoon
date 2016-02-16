::
::::  /hoon/paste/write/mar
  ::
/?    314
!:
|_  {typ/?($hoon $md $txt) txt/@t}
++  grab
  |%
  ++  noun  {?($hoon $md $txt) @t}
  ++  json  
    (corl need =>(jo (ot typ+(ci (soft ?($hoon $md $txt)) so) txt+so ~)))
  --
--
