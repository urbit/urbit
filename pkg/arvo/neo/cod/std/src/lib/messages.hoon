/@  message
/-  feather-icons
|%
::  This arm expects messages to be stored at
::  ~[%pub [%da ~2024.1.1]] or ~[%sub [%da ~2024.1.1]]
::  relative to here.bowl.
++  render-messages
  |=  =bowl:neo
  ^-  manx
  ;div.fc.g2
    =label  "Messages"
    ;*
    %+  turn
      %+  sort
        %+  murn
          ~(tap of:neo kids.bowl)
        |=  [=pith =idea:neo]
        ?~  pith  ~
        ?.  =(%message p.pail.idea)  ~
        `[pith idea]
      |=  [a=[=pith *] b=[=pith *]]
      =/  adate  +:(snag 1 pith.a)
      =/  bdate  +:(snag 1 pith.b)
      (lth adate bdate)
    render-message
  ==
::
++  render-message
  |=  [pax=pith =idea:neo]
  =/  msg  !<(message q.pail.idea)
  ^-  manx
  ;div.fc.g2.border.p3.br1
    ;div.fr.ac.jb
      ;p.s-2.f3: {(scow %p from.msg)}
      ;p.s-2.f3: {(scow %da now.msg)}
    ==
    ;p: {(trip contents.msg)}
  ==
::
++  render-sender
  |=  [=bowl:neo location=pith]
  ^-  manx
  ;form.fc.g2
    =hx-post  "{(en-tape:pith:neo :(welp /neo/hawk here.bowl location))}?stud=txt"
    =hx-swap  "beforebegin"
    =hx-on-submit  "this.reset()"
    =hx-target  "this"
    =head  "msg"
    ;textarea.p2.border.br1
      =name  "text"
      =placeholder  ". . ."
      =oninput  "this.setAttribute('value', this.value)"
      =rows  "4"
      =required  ""
      =autocomplete  "off"
      ;
    ==
    ;button.p2.b1.br1.bd1.wfc.hover.loader
      ;span.loaded.s2:  create
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
  ::
--