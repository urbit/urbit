/-  *resource
|%
+$  atom        @
+$  index       (list atom)
+$  uid         [=resource =index]
::
::  must be sha256 hash
+$  hash  @ux
::
+$  signature   [p=@ux q=ship r=life]
+$  signatures  (set signature)
+$  post
  $:  author=ship
      =hash
      =index
      contents=(list content)
      signatures=[p=signatures q=hash]
      time-sent=time
  ==
::
+$  content
  $%  [%text =cord]
      [%url =cord]
      [%code expression=cord output=(list tank)]
      [%reference =uid]
      ::  [%cage =cage]
  ==
--
