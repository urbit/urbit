/-  *language-server
::
|%
++  parse-error
  |=  =tape
  ^-  (unit [=path =range])
  =/  parse-pair
    %+  cook
      |=([row=@ud col=@ud] [(dec row) col])
    (ifix [lac rac] ;~((glue ace) dem dem))
  =/  parse-path
    %+  cook
      |=(p=path (slag 3 p))
    (ifix [net (jest '::')] (more net urs:ab))
  =/  parse-full
    ;~(plug parse-path ;~(sfix ;~((glue dot) parse-pair parse-pair) ban))
  (rust tape parse-full)
::
++  get-errors-from-tang
  |=  [uri=@t =tang]
  ^-  (list range)
  =/  =path
    (uri-to-path uri)
  %+  murn  tang
  |=  =tank
  ^-  (unit range)
  ?.  ?=([%leaf *] tank)
    ~
  =/  error
    (parse-error p.tank)
  ?~  error
    ~
  ?:  =(path path.u.error)
    `range.u.error
  ~
::
++  uri-to-path
  |=  uri=@t
  ^-  path
  =/  pier-root=(set cord)
    %-  sy
    ['app' 'gen' 'lib' 'mar' 'ren' 'sur' 'sys' 'test' ~]
  =/  path=(list cord)
    (parse-uri uri)
  |-
  ?<  ?=(~ path)
  ?:  (~(has in pier-root) i.path)
    `^path`path
  $(path t.path)
::
++  parse-uri
  |=  uri=@t
  =-  (fall - /fail)
  %+  rush  uri
  %+  more
    ;~(pose (plus fas) dot)
  %+  cook
    crip
  (star ;~(pose col hep alf))
::
--
