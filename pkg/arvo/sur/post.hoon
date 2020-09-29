/-  *resource
|%
+$  index       (list atom)
+$  uid         [=resource =index]
::
::  +sham (half sha-256) hash of +validated-portion
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
+$  indexed-post  [a=atom p=post]
::
+$  validated-portion
  $:  parent-hash=(unit hash)
      author=ship
      time-sent=time
      contents=(list content)
  ==
::
+$  content
  $%  [%text text=cord]
      [%url url=cord]
      [%code expression=cord output=(list tank)]
      [%reference =uid]
      ::  TODO: maybe use a cask?
      ::[%cage =cage]
  ==
::  +compare-indexes: total ordering function for $index
::
++  compare-indexes
  |=  [a=index b=index]
  ^-  ?
  ?-    [a b]
      [~ ~]  &
      [~ *]  |
      [* ~]  &
      *
    ?>  ?=(^ a)
    ?>  ?=(^ b)
    ?:  =(i.a i.b)
      $(a t.a, b t.b)
    (gth i.a i.b)
  ==
--
