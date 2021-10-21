=<  post
|%
+$  resource  [entity=ship name=term]
+$  index     (list @)
+$  uid       [resource index]
+$  id  time
::
+$  gid  [=ship =time]
+$  contents
  $~  [%0 ~]
  $%  [%0 p=contents-0]
      [%xeno p=*]
  ==
::
++  contents-0
  =<  contents
  |%
  +$  contents  (list content)
  ::
  +$  content
    $%  [%text text=cord]
        [%mention =ship]
        [%url url=cord]
        [%code expression=cord output=(list tank)]
        [%reference =reference]
    ==
  ::
  +$  reference
    $%  [%graph group=resource =uid]
        [%group group=resource]
        [%app =ship =desk =path]
        [%feed =gid]
    ==
  --
+$  envelope
  $:  =id
      children=(set id)
  ==
::
+$  letter
  $:  parent=(unit id)
      author=ship
      =contents
      time-sent=time
  ==
::
+$  missive  [envelope letter]
::
+$  post  (pair missive stamps)
::
++  stamps
  =<  self
  |%
  +$  react  @tE
  +$  self
    $:  likes=(set ship)
        reacts=(map ship react)
    ==
  +$  update
    $%  [%ini p=self]
        [%like ~]
        [%unlike ~]
        ::
        [%react =react]
        [%unreact ~]
    ==
  --
::
+$  update
  $%  [%add-post =post]
      [%del-post ~]
      [%stamps =update:stamps]
  ==
::
+$  response
  $%  [%posted old=id new=id]
  ==
--

