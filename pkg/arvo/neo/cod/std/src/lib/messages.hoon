/@  message
/-  feather-icons
|%
::  This arm expects messages to be stored at
::  ~[%pub [%da ~2024.1.1]] or ~[%sub [%da ~2024.1.1]]
::  relative to here.bowl.
++  pith-tape
  |=  =pith 
  ^-  tape
  (en-tape:pith:neo pith)
::
++  style 
  ;style
  ;+  ;/  %-  trip
  '''
   .fe {
    height: 100vh;
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    justify-content: flex-end
    }
  '''
  ==
::::
++  script 
  ;script
  ;+  ;/  %-  trip
  '''
    document.getElementById("render-sender").scrollIntoView({  
      block: "center",
      inline: "start",
      behavior: "instant"});
  '''
  ==
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
  ~&  >  msg
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
  ~&  >  "/neo/hawk{(pith-tape (welp here.bowl location))}?stud=message"
  ^-  manx
  ;form.fc.g2.wf
    =hx-post  "/neo/hawk{(pith-tape (welp here.bowl location))}?stud=message"
    =hx-swap  "none"  ::"outerHTML swap:5s"
    =hx-on-submit  "this.reset()"
    =hx-target  "find button .loading"
    =head  "msg"
    =id  "render-sender"
    ;textarea.p2.border.br1
      =name  "text"
      =placeholder  ". . ."
      =oninput  "this.setAttribute('value', this.value)"
      =rows  "4"
      =required  ""
      =autocomplete  "off"
      ;
    ==
    ;input.hidden
      =name  "ship"
      =value  (scow %p our.bowl)
    ;
    ==
    ;input.hidden
      =name  "date"
      =value  (scow %da now.bowl)
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