/-  *lyre
|%
++  enjs
  |%
  ++  dom
    |=  d=^dom
    ^-  json
    ?-  -.d
      %text    (text d)
      %button  (button d)
    ==
  ::
  ++  poke
    |=  p=^poke
    ^-  json
    %-  pairs:enjs:format
    :~  app+s+app.p
        mark+s+mark.p
        dat+dat.p
    ==
  ::
  ++  text
    |=  b=^dom
    ^-  json
    ?>  ?=(%text -.b)
    %+  frond:enjs:format
      %text
    [%s bod.b]
  ::
  ++  button
    |=  b=^dom
    ^-  json
    ?>  ?=(%button -.b)
    %+  frond:enjs:format
      %button
    %-  pairs:enjs:format
    :~  body+(dom bod.b)
        action+(poke act.b)
    ==
  --
--
