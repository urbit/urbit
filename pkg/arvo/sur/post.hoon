/-  *resource
|%
+$  atom        @
+$  index       (list atom)
+$  uid         [=resource =index]
::
+$  hash        @ux
+$  signature   @ux
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
