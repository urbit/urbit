!:  ::  /=try=/bin/http/hoon
!?      164
::::
|=  *
|=  [folder=[mih=span dez=span caz=span sup=path] urls=(list tape)]
^-  bowl
::::
=+  ^=  requests
    %-  ~(gas by *(map path ,[p=tape q=purl]))
    |-  ^-  (list ,[p=path q=tape r=purl])
    %+  turn  urls
    |=  [a=tape]
    :+  `path`[(scot %t (rap 3 a)) ~]
      a
    (scan a auri:epur)
=<  apex
|%
++  apex  `bowl`[gifts stuff]
++  gifts
  ^-  (list gift)
  %+  turn  (~(tap by requests) *(list ,[p=path q=tape r=purl]))
  |=  [a=path b=tape c=purl]
  [%tq a [c [%get ~ ~]]]
::
++  stuff
  ?:  =(~ requests)  ~
  :-  ~
  :_  work
  ^-  (list slip)
  =+  unresolved=(~(tap by requests) ~)
  %+  turn  unresolved
  |=  [a=path b=tape c=purl]
  [a [%hp ~]]
::
++  work
  |=  [now=@da pax=path not=note]
  ^-  bowl
  ?>  ?=([%hp *] not)
  =+  request=(need (~(get by requests) pax))
  =+  more=apex(requests (~(del by requests) pax))
  =-  more(p (weld actions p.more))
  ^=  actions  ^-  (list gift)
  :*  [%la %leaf "{p.request}: {(scow %ud p.p.not)}"]
      ?~  r.p.not  ~
      :_  ~
      :+  %ok
        dez.folder
      :+  %&
        ~
      :-  *cart
      =+  tol=:(weld sup.folder pax `path`/http)
      :~  :-  tol
          %+  feel
            (weld `path`[mih.folder dez.folder caz.folder ~] tol)
          q.u.r.p.not
      ==
  ==
--
