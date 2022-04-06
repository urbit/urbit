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
      ::
      %qor-put
      ::
      %qat-put
      %qat-raw
      %qat-vip
      ::
      %qah-put
      %qah-raw
      %qah-vip
    :: monotonic get
    ::
      %map-get
      %qor-get
      %qat-get
      %qah-get
      :: XX see
    :: XX monotonic del
    :: XX bot
  ==
::
:-  %say
|=  [* [n=@ ty=ty ~] ~]
:-  %noun
::
~&  [ty n=n]
::
?-    ty
    %map-put
  =|  acc=(map @ @)
  ~>  %bout
  |-  ^-  %done
  =>  ?:  (gth j n)
        acc
      $(acc (~(put by acc) j j), j +(j))
  %done
::
    %qor-put
  =|  acc=pro:qor
  ~>  %bout
  |-  ^-  %done
  =>  ?:  (gth j n)
        acc
      $(acc (put:qor acc j j j), j +(j))
  %done
::
    %qat-put
  =|  acc=pri:qat
  ~>  %bout
  |-  ^-  %done
  =>  ?:  (gth j n)
        acc
      $(acc (put:qat acc j j j), j +(j))
  %done
::
    %qat-raw
  =|  acc=pri:qat
  ~>  %bout
  |-  ^-  %done
  =>  ?:  (gth j n)
        acc
      $(acc (raw:qat acc j j j), j +(j))
  %done
::
    %qat-vip
  =|  acc=pri:qat
  ~>  %bout
  |-  ^-  %done
  =>  ?:  (gth j n)
        acc
      $(acc (vip:qat acc j j j), j +(j))
  %done
::
    %qah-put
  =|  acc=pri:qah
  ~>  %bout
  |-  ^-  %done
  =>  ?:  (gth j n)
        acc
      $(acc (put:qah acc j j j), j +(j))
  %done
::
    %qah-raw
  =|  acc=pri:qah
  ~>  %bout
  |-  ^-  %done
  =>  ?:  (gth j n)
        acc
      $(acc (raw:qah acc j j j), j +(j))
  %done
::
    %qah-vip
  =|  acc=pri:qah
  ~>  %bout
  |-  ^-  %done
  =>  ?:  (gth j n)
        acc
      $(acc (vip:qah acc j j j), j +(j))
  %done
::
    %map-get
  =|  acc=(map @ @)
  =/  pos
    |-  ^+  acc
    ?:  (gth j n)
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
      acc
    $(acc (put:qah acc j j j), j +(j))
  =|  l=(unit)
  ~>  %bout
  |-
  ?:  (gth j n)
    %done
  $(l (get:qah pos j), j +(j))
==
