::
::::  /hoon/pill/mar
  ::
/-  aquarium
=,  aquarium
=,  mimes:html
|_  pil=pill
++  grow
  |%
  ++  mime  [/application/octet-stream (as-octs (jam pil))]
  --
++  grab
  |%
  ++  noun  pill
  ++  mime
    |=  [p=mite:eyre q=octs:eyre]
    =+  o=(pair ,* ,*) :: ,*)
    =+  (,[boot-ova=* kernel-ova=(list o) userspace-ova=(list o)] (cue q.q))
    =/  convert
      |=  ova=(list o)
      ^-  (list unix-event)
      %+  turn  ova
      |=  ovo=o
      =/  sof  ((soft unix-event) ovo)
      ?~  sof
        ~&  [%unknown-event p.ovo]
        !!
      ~&  [%known-event (wire p.ovo) (@tas -.q.ovo)]
      u.sof
    ::  =/  boot-ova  (convert boot-ova)
    =/  kernel-ova  (convert kernel-ova)
    =/  userspace-ova  (convert userspace-ova)
    [boot-ova kernel-ova userspace-ova]
  --
++  grad  %mime
--
