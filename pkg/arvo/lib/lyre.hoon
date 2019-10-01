/?  309
/-  *lyre
|%
++  link
  |=  nom=@tas
  ^-  poke
  [%lyre %lyre-action (action:enjs [%switch-view nom])]
::
++  enjs
  |%
  ::
  ++  action
    |=  act=^action
    ^-  json
    %+  frond:enjs:format
      -.act
    ?-  -.act
        %new-view
      %-  pairs:enjs:format
      :~  nom+s+nom.act
          dep+(dependencies dep.act)
      ==
    ::
        %change-deps
      %-  pairs:enjs:format
      :~  nom+s+nom.act
          dep+(dependencies dep.act)
      ==
    ::
        %switch-view
      (frond:enjs:format %nom s+nom.act)
    ::
        %delete-view
      (frond:enjs:format %nom s+nom.act)
    ::
    ==
  ::
  ++  dependencies
    |^
    |=  dep=^dependencies
    ^-  json
    %-  pairs:enjs:format
    :~  clay+(clay clay.dep)
        gall+(gall gall.dep)
        raw+~
        ren+s+ren.dep
    ==
    ::
    ++  clay
      |=  c=(list [beam care:^clay])
      ^-  json
      :-  %a
      %+  turn  c
      |=  [bem=beam car=care:^clay]
      ^-  json
      %-  pairs:enjs:format
      :~  bem+s+(spat (en-beam:format bem))
          car+s+car
      ==
    ::
    ++  gall
      |=  g=(list [@tas path])
      ^-  json
      :-  %a
      %+  turn  g
      |=  [app=@tas pax=path]
      ^-  json
      %-  pairs:enjs:format
      :~  app+s+app
          pax+s+(spat pax)
      ==
    --
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
    |^
    |=  a=^dom
    ^-  json
    ?-  -.a
      %empty       empty
      %text        (text a)
      %button      (button a)
      %image       (image a)
    ::
      %form        (form a)
      %text-input  (text-input a)
      %submit      (submit a)
    ::
::      %size        (size a)
      %padding     (padding a)
      %horizontal  (horizontal a)
      %vertical    (vertical a)
      %list        (list a)
      %box         (box a)
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
      %-  pairs:enjs:format
      :~  :+  %style
            %a
          %+  turn  sty.a
          |=  s=?(^typography ^space)
          ?-  s
            ^typography  (typography s)
            ^space      (space s)
          ==
      ::
          body+s+bod.a
      ==
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
    ++  image
      |=  a=^dom
      ^-  json
      ?>  ?=(%image -.a)
      %+  frond:enjs:format
        %image
      s+dat.a
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
      ::
          :+  %style
            %a
          %+  turn  sty.a
          |=  s=^typography
          (typography s)
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
::    ++  size
::      |=  a=^dom
::      ^-  json
::      ?>  ?=(%size -.a)
::      %+  frond:enjs:format
::        %size
::      %-  pairs:enjs:format
::      :~  width+(numb:enjs:format w.a)
::          height+(numb:enjs:format h.a)
::          body+(dom bod.a)
::      ==
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
    ++  list
      |=  a=^dom
      ^-  json
      ?>  ?=(%list -.a)
      %+  frond:enjs:format
        %list
      %-  pairs:enjs:format
      :~  :+  %style
            %a
          %+  turn  sty.a
          |=  s=?(^flex ^layout ^typography ^bg-color ^border ^space)
          ?-  s
            ^flex        (flex s)
            ^layout      (layout s)
            ^typography  (typography s)
            ^bg-color    (bg-color s)
            ^border      (border s)
            ^space       (space s)
          ==
      ::
          body+a+(turn bod.a dom)
      ==
    ::
    ++  box
      |=  a=^dom
      ^-  json
      ?>  ?=(%box -.a)
      %+  frond:enjs:format
        %box
      %-  pairs:enjs:format
      :~  :+  %style
            %a
          %+  turn  sty.a
          |=  s=?(^flex ^layout ^typography ^bg-color ^border ^space)
          ?-  s
            ^flex        (flex s)
            ^layout      (layout s)
            ^typography  (typography s)
            ^bg-color    (bg-color s)
            ^border      (border s)
            ^space       (space s)
          ==
      ::
          body+(dom bod.a)
      ==
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
    ::
    ::  styling
    ::
    ++  size
      |=  a=^size
      ^-  json
      ?-  -.a
        %per  s+(crip "{<+.a>}%")
        %pix  s+(crip "{<+.a>}px")
      ==
    ::
    ++  layout
      |=  a=^layout
      ^-  json
      ?-  -.a
          %width
        (pairs:enjs:format property+s+-.a value+(size +.a) ~)
          %height
        (pairs:enjs:format property+s+-.a value+(size +.a) ~)
      ==
    ::
    ++  flex
      |=  a=^flex
      ^-  json
      ?-  -.a
          %axis
        (pairs:enjs:format property+s+-.a value+s++.a ~)
          %basis
        (pairs:enjs:format property+s+-.a value+(size +.a) ~)
          %grow
        (pairs:enjs:format property+s+-.a value+(numb:enjs:format +.a) ~)
          %shrink
        (pairs:enjs:format property+s+-.a value+(numb:enjs:format +.a) ~)
      ==
    ::
    ++  typography
      |=  a=^typography
      ^-  json
      ?-  -.a
          %color
        (pairs:enjs:format property+s+-.a value+s++.a ~)
          %font-family
        (pairs:enjs:format property+s+-.a value+s++.a ~)
          %font-size
        (pairs:enjs:format property+s+-.a value+(numb:enjs:format +.a) ~)
          %font-weight
        (pairs:enjs:format property+s+-.a value+(numb:enjs:format +.a) ~)
      ==
    ::
    ++  bg-color
      |=  a=^bg-color
      ^-  json
      (pairs:enjs:format property+s+-.a value+s++.a ~)
    ::
    ++  border
      |=  a=^border
      ^-  json
      (pairs:enjs:format property+s+-.a value+b++.a ~)
    ::
    ++  space
      |=  a=^space
      ^-  json
      (pairs:enjs:format property+s+-.a value+(numb:enjs:format +.a) ~)
    --
  --
--
























