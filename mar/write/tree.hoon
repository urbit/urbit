::
::::  /hoon/tree/write/mar
  ::
/?    310
::
=,  mimes:html
=,  format
|_  [sup=spur mim=mime]
++  grab
  |%
  ++  noun  [spur mime]
  ++  json
    =,(dejs (ot sup+(su fel:stab) mime+(cu |=(a=@t [/ (as-octs a)]) so) ~))
  --
--
