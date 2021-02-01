/+  *contact-store
::
|_  upd=update
++  grad  %noun
++  grow
  |%
  ++  noun  upd
  ++  json  (update:enjs upd)
  ++  resource
    |^
    ?-  -.upd
      %initial     [nobody %contacts]
      %add         [nobody %contacts]
      %remove      [nobody %contacts]
      %edit        [nobody %contacts]
      %allow       !!
      %disallow    !!
      %set-public  !!
    ==
    ::
    ++  nobody
      ^-  @p
      (bex 128)
    --
  --
::
++  grab
  |%
  ++  noun  update
  ++  json  update:dejs
  --
--
