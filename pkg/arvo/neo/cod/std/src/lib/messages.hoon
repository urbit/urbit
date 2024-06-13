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
  .msg{
    max-width: 70%;
    min-width: 40%;
  }
  '''
  ==
::::
++  script
  ;script
  ;+  ;/  %-  trip
  '''
  function scrollToBottom(el) {
    el.scrollTop = el.scrollHeight;
  }
  function maybeScrollToBottom(el) {
    let pos = el.scrollTop + el.clientHeight;
    let height = el.scrollHeight;
    if ((height - pos) < 200) {
      console.log('scrolling', height, pos);
      scrollToBottom(el);
    } else {
      console.log('NOT scrolling', height, pos);
    }
  }
  document.querySelectorAll('.messages').forEach(el => scrollToBottom(el));
  '''
  ==
::
++  refresher
  |=  =bowl:neo
  ;div.absolute.hidden
    =style  "top: 1em; left: 1em;"
    ;div.loader.refresher
      =hx-get  "{(en-tape:pith:neo :(weld /neo/hawk here.bowl))}?no-save"
      =hx-on-htmx-after-request  "let msgs = $(this).closest('.messages')[0]; maybeScrollToBottom(msgs);"
      =hx-trigger  "every 7s, refresh"
      =hx-target  "closest .top"
      =hx-select  ".top"
      =hx-swap  "morph"
      ;span.loaded;
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
::
++  render-messages
  |=  =bowl:neo
  ^-  manx
  ;div.fc.g2.top.scroll-y.messages
    =style  "grid-area: messages; padding: 30px 0;"
    =label  "Messages"
    =id  "messages"
    ;+  (refresher bowl)
  ;*
    %+  turn
      %+  sort
        %+  murn
          ~(tap of:neo kids.bowl)
        |=  [=pith =idea:neo]
        ?~  pith  ~
        ?.  =(%message p.q.saga.idea)  ~
        `[pith idea]
      |=  [a=[=pith *] b=[=pith *]]
      =/  adate  +:(snag 1 pith.a)
      =/  bdate  +:(snag 1 pith.b)
      (lth adate bdate)
    |=  [pax=pith =idea:neo]
    (render-message pax idea bowl)
  ==
::
++  render-message
  |=  [pax=pith =idea:neo =bowl:neo]
  =/  msg  !<(message q.q.saga.idea)
  ~&  >  msg
  ^-  manx
  ?:  =(our.bowl from.msg)
    (render-our-message msg)
  ::;div.fc.g2.border.p3.br1.grow.msg
  ;div.fc.g2.grow.msg
    =style  "align-self: flex-start;"
    ;div.fr.ac.jb
      ;p.s-2.f3: {(scow %p from.msg)}
      ;p.s-2.f3: {(scow %da now.msg)}
    ==
    ;div.border.br1.p3
      ;p: {(trip contents.msg)}
    ==
  ==
::
++  render-our-message
|=  msg=message
;div.fc.g2.grow.msg
  =style  "align-self: flex-end;"
  ;div.fr.ac.jb
    ;p.s-2.f3: {(scow %p from.msg)}
    ;p.s-2.f3: {(scow %da now.msg)}
  ==
  ;div.fr.je.border.br1.b1.p3
  ;p:  {(trip contents.msg)}
  ==
==
::
++  render-sender
  |=  [=bowl:neo location=pith]
  ^-  manx
  ;form.fr.g1.wf.af.js
    =style  "grid-area: sender;"
    =hx-post  "/neo/hawk{(pith-tape (welp here.bowl location))}?stud=message"
    =hx-on-htmx-after-request  "$(this).emit('message-sent');"
    =hx-swap  "none"
    =hx-on-submit  "this.reset()"
    =head  "msg"
    =id  "render-sender"
    ;textarea.p2.border.br1.grow
      =name  "text"
      =placeholder  ". . ."
      =oninput  "this.setAttribute('value', this.value)"
      =rows  "2"
      =required  ""
      =autocomplete  "off"
      =maxlength  "2048"
      ;
    ==
    ;input.hidden
      =name  "ship"
      =value  (scow %p our.bowl)
    ;
    ==
    ;date-now(name "date");
    ;button.p2.b1.br1.bd1.wfc.hover.loader
      ;span.loaded.s2:  send
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
  ::
--
