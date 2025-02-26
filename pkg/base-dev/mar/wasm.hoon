::
::::  /hoon/wasm/mar
  ::
/?    310
::
::::  wasm - just an octstream
  ::
=,  mimes:html
|_  wasm=octs
++  grab  |%
          ++  noun  octs
          ++  mime  |=([* p=octs] p)
          --
++  grow  |%
          ++  mime  [/application/x-urb-unknown wasm]
          --
++  grad  %mime
--
