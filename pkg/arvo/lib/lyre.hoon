/-  *lyre
|%
++  enjs
  |%
  ::
  ++  poke
    |=  a=^poke
    ^-  json
    %-  pairs:enjs:format
    :~  app+s+app.a
        mark+s+mark.a
        dat+dat.a
    ==
  ::
  ::  +dom conversions
  ::
  ++  dom
    |=  a=^dom
    ^-  json
    ?-  -.a
      ^        [%a (dom hed.a) (dom tal.a) ~]
      %text    (text a)
      %button  (button a)
      %form    (form a)
    ==
  ::
  ++  text
    |=  a=^dom
    ^-  json
    ?>  ?=(%text -.a)
    %+  frond:enjs:format
      %text
    [%s bod.a]
  ::
  ++  button
    |=  a=^dom
    ^-  json
    ?>  ?=(%button -.a)
    %+  frond:enjs:format
      %button
    %-  pairs:enjs:format
    :~  body+(dom bod.a)
        action+(poke act.a)
    ==
  ::
  ++  form
    |=  a=^dom
    ^-  json
    ?>  ?=(%form -.a)
    %+  frond:enjs:format
      %form
    %-  pairs:enjs:format
    :~  app+s+app.a
        mark+s+mark.a
        body+(form-dom bod.a)
    ==
  ::
  ::
  ::
  ++  form-dom
    |=  a=^form-dom
    ^-  json
    ?-  -.a
      ^            [%a (form-dom hed.a) (form-dom tal.a) ~]
      %text-input  (text-input a)
      %submit      (submit a)
    ==
  ::
  ++  text-input
    |=  a=^form-dom
    ^-  json
    ?>  ?=(%text-input -.a)
    %+  frond:enjs:format
      %text-input
    %-  pairs:enjs:format
    :~  name+s+name.a
    ==
  ::
  ++  submit
    |=  a=^form-dom
    ^-  json
    ?>  ?=(%submit -.a)
    %+  frond:enjs:format
      %submit
    (dom bod.a)
  ::
  --
--
