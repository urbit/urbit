/+  up-qor, up-qat, up-qah
=*  qor  (up-qor @ @)
=*  qat  (up-qat @)
=*  qah  (up-qah @ @)
=|  j=@
=/  ty
  $?
    :: monotonic put
    ::
      %map-put
      %qor-put
      %qat-put
      %qat-raw
      %qat-vip
      %qah-put
      %qah-raw
      %qah-vip
    :: monotonic get
    ::
      %map-get
      %qor-get
      %qat-get
      %qah-get
      %qat-see
      %qah-see
    :: monotonic del
    ::
      %map-del
      %qor-del
      %qat-del
      %qah-del
    :: bot
    ::
      %qor-bot
      %qat-bot
      %qah-bot
  ==
::
:-  %say
|=  [* [n=@ ty=ty ~] [apt=_| ~]]
:-  %noun
::
~&  [ty n=n]
::
?-    ty
    %map-put
  =|  acc=(map @ @)
  =+  ~>  %bout
      |-  ^+  acc
      ?:  (gth j n)
        acc
      $(acc (~(put by acc) j j), j +(j))
  ?:  apt
    ~(apt by -)
  &
::
    %qor-put
  =|  acc=pro:qor
  =+  ~>  %bout
      |-  ^+  acc
      ?:  (gth j n)
        acc
      $(acc (put:qor acc j j j), j +(j))
  ?:  apt
    (apt:qor -)
  &
::
    %qat-put
  =|  acc=pri:qat
  =+  ~>  %bout
      |-  ^+  acc
      ?:  (gth j n)
        acc
      $(acc (put:qat acc j j j), j +(j))
  ?:  apt
    (apt:qat -)
  &
::
    %qat-raw
  =|  acc=pri:qat
  =+  ~>  %bout
      |-  ^+  acc
      ?:  (gth j n)
        acc
      $(acc (raw:qat acc j j j), j +(j))
  ?:  apt
    (apt:qat -)
  &
::
    %qat-vip
  =|  acc=pri:qat
  =+  ~>  %bout
      |-  ^+  acc
      ?:  (gth j n)
        acc
      $(acc (vip:qat acc j j j), j +(j))
  ?:  apt
    (apt:qat -)
  &
::
    %qah-put
  =|  acc=pri:qah
  =+  ~>  %bout
      |-  ^+  acc
      ?:  (gth j n)
        acc
      $(acc (put:qah acc j j j), j +(j))
  ?:  apt
    (apt:qah -)
  &
::
    %qah-raw
  =|  acc=pri:qah
  =+  ~>  %bout
      |-  ^+  acc
      ?:  (gth j n)
        acc
      $(acc (raw:qah acc j j j), j +(j))
  ?:  apt
    (apt:qah -)
  &
::
    %qah-vip
  =|  acc=pri:qah
  =+  ~>  %bout
      |-  ^+  acc
      ?:  (gth j n)
        acc
      $(acc (vip:qah acc j j j), j +(j))
  ?:  apt
    (apt:qah -)
  &
::
    %map-get
  =|  acc=(map @ @)
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  ~(apt by acc)  acc
      acc
    $(acc (~(put by acc) j j), j +(j))
  =|  l=(unit)
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(l (~(get by pos) j), j +(j))
::
    %qor-get
  =|  acc=pro:qor
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qor acc)  acc
      acc
    $(acc (put:qor acc j j j), j +(j))
  =|  l=(unit)
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(l (get:qor pos j), j +(j))
::
    %qat-get
  =|  acc=pri:qat
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qat acc)  acc
      acc
    $(acc (put:qat acc j j j), j +(j))
  =|  l=(unit)
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(l (get:qat pos j), j +(j))
::
    %qah-get
  =|  acc=pri:qah
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qah acc)  acc
      acc
    $(acc (put:qah acc j j j), j +(j))
  =|  l=(unit)
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(l (get:qah pos j), j +(j))
::
    %qat-see
  =|  acc=pri:qat
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qat acc)  acc
      acc
    $(acc (put:qat acc j j j), j +(j))
  =|  l=(pair)
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(l (see:qat pos j n), j +(j))
::
    %qah-see
  =|  acc=pri:qah
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qah acc)  acc
      acc
    $(acc (put:qah acc j j j), j +(j))
  =|  l=(pair)
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(l (see:qah pos j n), j +(j))
::
    %map-del
  =|  acc=(map @ @)
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  ~(apt by acc)  acc
      acc
    $(acc (~(put by acc) j j), j +(j))
  =|  l=_acc
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(l (~(del by pos) j), j +(j))
::
    %qor-del
  =|  acc=pro:qor
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qor acc)  acc
      acc
    $(acc (put:qor acc j j j), j +(j))
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(pos (del:qor pos j), j +(j))
::
    %qat-del
  =|  acc=pri:qat
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qat acc)  acc
      acc
    $(acc (put:qat acc j j j), j +(j))
  ~>  %bout
  |-  ^-  %done
  ?:  (gth j n)
    %done
  $(pos (del:qat pos j), j +(j))
::
    %qah-del
  =|  acc=pri:qah
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qah acc)  acc
      acc
    $(acc (put:qah acc j j j), j +(j))
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(pos (del:qah pos j), j +(j))
::
    %qor-bot
  =|  acc=pro:qor
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qor acc)  acc
      acc
    $(acc (put:qor acc j j j), j +(j))
  ~>  %bout
  |-
  =/  b  (bot:qor pos)
  ?~  b
    %done
  $(pos q.u.b)
::
    %qat-bot
  =|  acc=pri:qat
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qat acc)  acc
      acc
    $(acc (put:qat acc j j j), j +(j))
  ~>  %bout
  |-
  =/  b  (bot:qat pos)
  ?~  b
    %done
  $(pos s.u.b)
::
    %qah-bot
  =|  acc=pri:qah
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
      ?:  apt
        ?>  (apt:qah acc)  acc
      acc
    $(acc (put:qah acc j j j), j +(j))
  =|  l=_acc
  ~>  %bout
  |-
  =/  b  (bot:qah pos)
  ?~  b
    %done
  $(pos s.u.b)
==
