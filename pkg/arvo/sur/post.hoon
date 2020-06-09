/-  *resource
|%
+$  atom        @
+$  index       (list atom)
+$  uid         [=resource =index]
::
::  mug hash of +validated-portion
+$  hash  @ux
::
+$  signature   [p=@ux q=ship r=life]
+$  signatures  (set signature)
+$  post
  $:  author=ship
      =index
      time-sent=time
      contents=(list content)
      hash=(unit hash)
      =signatures
  ==
::
+$  validated-portion
  $:  parent-hash=(unit hash)
      author=ship
      =index
      time-sent=time
      contents=(list content)
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
