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
      [%mention =ship]
      [%url url=cord]
      [%code expression=cord output=(list tank)]
      [%reference =uid]
      ::  TODO: maybe use a cask?
      ::[%cage =cage]
  ==
--
