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
  ++  peer
    |=  a=^peer
    ^-  json
    %-  pairs:enjs:format
    :~  app+s+app.a
        path+s+(spat pax.a)
    ==
  ::
  ++  dom
    |=  a=^dom
    ^-  json
    ?-  -.a
      %empty       empty
      %text        (text a)
      %button      (button a)
    ::
      %form        (form a)
      %text-input  (text-input a)
      %submit      (submit a)
    ::
      %size        (size a)
      %padding     (padding a)
      %horizontal  (horizontal a)
      %vertical    (vertical a)
    ::
      %include     (include a)
      %component   (component a)
    ==
  ::
  ++  empty
    ^-  json
    (frond:enjs:format %empty ~)
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
        body+(dom bod.a)
        :+  %data  %o
        %-  ~(run by dat.a)
        |=  val=@t
        ^-  json
        [%s val]
    ==
  ::
  ++  text-input
    |=  a=^dom
    ^-  json
    ?>  ?=(%text-input -.a)
    %+  frond:enjs:format
      %text-input
    %-  pairs:enjs:format
    :~  name+s+name.a
    ==
  ::
  ++  submit
    |=  a=^dom
    ^-  json
    ?>  ?=(%submit -.a)
    %+  frond:enjs:format
      %submit
    (dom bod.a)
  ::
  ::
  ++  size
    |=  a=^dom
    ^-  json
    ?>  ?=(%size -.a)
    %+  frond:enjs:format
      %size
    %-  pairs:enjs:format
    :~  width+(numb:enjs:format w.a)
        height+(numb:enjs:format h.a)
        body+(dom bod.a)
    ==
  ::
  ++  padding
    |=  a=^dom
    ^-  json
    ?>  ?=(%padding -.a)
    %+  frond:enjs:format
      %padding
    %-  pairs:enjs:format
    :~  top+(numb:enjs:format t.a)
        bottom+(numb:enjs:format b.a)
        left+(numb:enjs:format l.a)
        right+(numb:enjs:format r.a)
        body+(dom bod.a)
    ==
  ::
  ++  horizontal
    |=  a=^dom
    ^-  json
    ?>  ?=(%horizontal -.a)
    %+  frond:enjs:format
      %horizontal
    [%a (turn bod.a dom)]
  ::
  ++  vertical 
    |=  a=^dom
    ^-  json
    ?>  ?=(%vertical -.a)
    %+  frond:enjs:format
      %vertical
    [%a (turn bod.a dom)]
  ::
  ++  include
    |=  a=^dom
    ^-  json
    ?>  ?=(%include -.a)
    %+  frond:enjs:format
      %include
    (frond:enjs:format %js s+js.a)
  ::
  ++  component
    |=  a=^dom
    ^-  json
    ?>  ?=(%component -.a)
    %+  frond:enjs:format
      %component
    %-  pairs:enjs:format
    :~  name+s+name.a
        sub+?~(sub.a ~ (peer u.sub.a))
    ==
  --
--
























