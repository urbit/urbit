::
::::  /hoon/paste/write/mar
  ::
/?    310
|_  [typ=?($hoon $md $txt) txt=@t]
++  grab
  |%
  ++  noun  [?($hoon $md $txt) @t]
  ++  json
    (corl need =>(dejs-soft:format (ot typ+(ci (soft ?($hoon $md $txt)) so) txt+so ~)))
  --
--
