::
::::  /hoon/http/lib
  ::
  ::
  ::
/?    310
!:
|%
++  httpreq
    |=  $:    ost=bone  pour-path=wire
        $=  params
          $:  domain=(list cord)  end-point=path
              req-type=$?(%get [%post json])  headers=math
              queries=quay
          ==
        ==
    :^  ost  %them  pour-path
    `(unit hiss)`[~ (httpreq-to-hiss params)]
::
++  httpreq-to-hiss
    |=  $:  domain=(list cord)  end-point=path
          req-type=$?(%get [%post p=json])  headers=math
        queries=quay
        ==
    ^-  hiss                                ::  cast to hiss
    =-  ~&  hiss=-  -
    :-  ^-  parsed-url=purl
        :+  :+  security=%.y
              port=~
            host=[%.y [path=domain]]
          endpoint=[extensions=~ point=end-point]       ::  ++pork,
        q-strings=queries                               ::  ++quay
    ?@  req-type
      [%get headers ~]
    [%post headers ~ (tact (pojo p.req-type))]
--
