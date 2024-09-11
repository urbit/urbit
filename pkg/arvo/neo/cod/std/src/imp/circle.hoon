/@  circle
/@  ircle-diff
/-  oxy=oxygen
^-  kook:neo
|%
++  state  pro/%circle
++  poke   (sy %circle-diff %eyre-task ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  [[|/%p |] pro/%sig ~]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  =<
  |_  [=bowl:neo =aeon:neo =pail:neo]
  +*  web  ~(. +> [bowl pail])
  ++  init
    |=  pal=(unit pail:neo)
    :-  [(bind:oxy bowl) ~]
    circle/!>(~)
  ++  poke
    |=  [=stud:neo vax=vase]
    ?+    stud  !!
        %eyre-task
      (handle:web !<(task:eyre:neo vax))
        %circle-diff
      =/  diff  !<(circle-diff vax)
      :_  pail
      ?-    -.diff
          %add
        %+  turn  ~(tap by p.diff)
        |=  [=ship =made:neo]
        [(snoc here.bowl p/ship) %make made]
      ::
          %del
        %+  turn  ~(tap in p.diff)
        |=  =ship
        [(snoc here.bowl p/ship) %tomb ~]
      ==
    ==
  --
  |_  [=bowl:neo =pail:neo]
  ++  handle
    |=  [eyre-id=@ta req=inbound-request:eyre]
    ^-  (quip card:neo pail:neo)
    :_  pail
    ?+    method.request.req  
        ~|(%unsupported-http-method !!)
    ::
        %'GET'
      =;  manx
        (respond:oxy [bowl eyre-id req manx])
      %~  render
        ui
      (pave:neo pax:(parse-url:oxy request.req))
    ::
        %'POST'
      =;  poke
        [here.bowl %poke [%circle-diff !>(poke)]]~
      ^-  diary-diff
      =/  body  (parse-body:oxy request.req)
      =/  s  (slav %p (~(vol manx-utils body) "ship"))
      =/  =made:neo  [%sig `sig/!>(~) ~]
      [%add (~(gas by *(map ship made:neo)) [s made] ~)]
    ==
  ::
  ++  render
    |=  here=pith
    ^-  manx
    ;div.p3
      ;div.p2.mw-page.ma
        ;+  title
        ;+  (form-new-ship here)
        ;+  friends-list
      ==
    ==
  ::
  ++  friends-list
    ^-  manx
    ;div.fc.g2
      ;*
      %+  turn
        %+  murn
          ~(tap in ~(key by ~(tar of:neo kids.bowl)))
        |=  =road:neo  
        ?.  ?=([[%p ship=@] *] road)
          ~
        `ship.road
      |=  =ship
      ^-  manx
      ;div.border.p2.mono.fr.jb
        ;div: {<ship>}
        ;button.b1.border.hover.br1:  x
      ==
    ==
  ++  my-address
    (en-tape:pith:neo :(weld /hawk here.bowl))
  ++  form-new-ship
    |=  here=pith
    ^-  manx
    ;form.fr.jc.p3
      =hx-post  (en-tape:pith:neo here)
      =hx-swap  "afterend"
      =hx-target  "this"
      =head  "add"
      ;input.border
        =name  "ship"
        =type  "text"
        =autocomplete  "off"
        =placeholder  "~sampel-palnet"
        =oninput  "this.setAttribute('value', this.value)"
        ;
      ==
      ;button.border.b1
        ; add
      ==
    ==
  ++  title
    ^-  manx
    ;div.fr.jc.ac.prose
      ;h1.tc.border.wfc.fr.jc.ac
        =style
          """
          width:100px;
          height:100px;
          border-radius:99%
          """
        ; circle
      ==
    ==
  --
--
