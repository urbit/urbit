/@  ui-event
/@  task
/@  task-diff
^-  kook:neo
=<
|%
++  state  pro/%manx
++  poke   (sy %ui-event %rely %gift ~)
++  kids
  *kids:neo
++  deps
  ^-  deps:neo
  %-  my
  :~  :-  %src
      :-  req=&
      :-  [pro/%task (sy %task-diff %gift ~)]
      :+  ~  %z
      %-  my
      :~  [[|/%ud |] pro/%task (sy %task-diff %gift ~)]
      ==
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    manx/!>((render (task-map bowl) p:(~(got by deps.bowl) %src)))
  ::
  ++  poke
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    ~&  >>  stud
    =/  state  (task-obj bowl)
    ?+    stud  ~|(bad-stud/stud !!)
    ::
        %ui-event
      =/  event  !<(ui-event vase)
      =/  here=pith:neo  p:(~(got by deps.bowl) %src)
      =/  =lore:neo  q:(~(got by deps.bowl) %src)
      ~&  >>  path.event
      ?+    path.event
        ~|(missing-event-handler-for/path.event !!)
      ::
          [%submit %new-task ~]
        ~&  data.event
        =/  text=@t
          (~(got by data.event) 'task-input')
        :_  pail
        :~  :-  here
            :+  %poke
              %task-diff
            !>([%new [text | & ~] %.n])
        ==
      ::
          [%click %checkbox ~]
        ~&  p:(~(got by deps.bowl) %src)
        =/  key  (got-key data.event)
        =/  =idea:neo  (~(got of:neo lore) key)
        =/  t  !<(task q.pail.idea)
        :_  pail
        :~  :-  (welp here key)
            :+  %poke
              %task-diff
            !>([%edit text.t !done.t])
        ==
      ::
          [%click %move-up ~]
        =/  key  (got-key data.event)
        ?~  undex=(find ~[key] order.state)
          ~|(%cant-find-it-bro !!)
        =/  index  u.undex
        ?:  =(0 index)
        [~ pail]
        ::
        =/  without  (oust [index 1] order.state)
        =/  new-order  (into without (dec index) key)
        ~&  new-order/new-order
        :_  pail
        :~
          :-  here
          :+  %poke
            %task-diff
          !>([%reorder new-order])
        ==
      ::
          [%click %move-down ~]
        =/  key  (got-key data.event)
        ?~  undex=(find ~[key] order.state)
          ~|(%cant-find-it-bro !!)
        =/  index  u.undex
        ?:  =((dec (lent order.state)) index)
        [~ pail]
        ::
        =/  without  (oust [index 1] order.state)
        =/  new-order  (into without +(index) key)
        :_  pail
        :~
          :-  here
          :+  %poke
            %task-diff
          !>([%reorder new-order])
        ==
      ::
          [%click %delete ~]
        =/  key  (got-key data.event)
        :_  pail
        :~  :-  here
            :+  %poke
              %task-diff
            !>([%oust key])
        ==
        ::
          [%input %task-text ~]
        =/  text=@t  (~(got by data.event) '/target/value')
        =/  pith  (got-key data.event)
        =/  =idea:neo  (~(got of:neo lore) pith)
        =/  t  !<(task q.pail.idea)
        :_  pail
        :~  :-  (welp here pith)
            :+  %poke
              %task-diff
            !>([%edit text done.t])
        ==
      ==
    ::
        %rely
      :-  ~
      manx/!>((render (task-map bowl) p:(~(got by deps.bowl) %src)))
    ==
  --
--
::
::  XX:  style attribute with color/background-color here just for demonstartion till style tag will be fixed and browser could read var from feather
|%
++  render
  |_  [tasks=(map pith task) here=pith:neo]
  ++  $
    ^-  manx
    ;html
      ;head
        ;meta(charset "utf-8");
      ==
      ;body.fr.jc.wf
        =style  "margin-top: 30px;"
        ;main.fc.ac
        =style  "width: 30rem;"
          :: ;h1.p2: Tasks
          ;div
            ;p.p2.s2.mono: {(trip text:(~(got by tasks) /))}
          ==
          ::;+  task-form
          ;+  subtasks
          ;+  task-form
        ==
      ==
    ==
  ::
  ++  task-form
    ^-  manx
    ;form.p3.fr.g1.wf
      =mast-after-swap  "this.reset()"
      =event  "/submit/new-task"
      ;input.p2.bd1.br2.grow.b0
        =name   "task-input"
        =style  "outline: none;"
        =placeholder  "new task"
        =autocomplete  "off";
      ;button.p2.b1.br1.hover.pointer
        ; create
      ==
    ==
  ::
  ++  subtasks
    ^-  manx
    ;div.fc.g2.wf
    =id  "top"
      ;*
      ~&  [%render-order order:(~(got by tasks) /)]
          %+  turn  order:(~(got by tasks) /)
          |=  =pith
          =/  task  (~(got by tasks) pith)
          =/  key  (en-tape:pith:neo pith)
          ;div.fr.jb.p2.br1.g2
          =onmouseover  "this.childNodes[3].classList.remove('hidden'); this.classList.add('b1'); this.childNodes[1].classList.remove('b0'); this.childNodes[1].classList.add('b1');"
          =onmouseout   "this.childNodes[3].classList.add('hidden'); this.classList.remove('b1'); this.childNodes[1].classList.add('b0'); this.childNodes[1].classList.remove('b1');"
            ;div.fr.ac.g1
            ;+
              =;  m
                ?:  done.task
                  m(a.g [[%checked ""] a.g.m])
                m
              ^-  manx
              ;input.pointer
                =style  "outline: none; accent-color:var(--b3);"
                =id        "task-checkbox"
                =type      "checkbox"
                =data-key  key
                =return    "/target/data-key"
                =name      "done"
                =event     "/click/checkbox"
                ;
              ==
            ==
            ;input.p2.bd0.br1.pointer.grow.hover.b0
              =style  "outline: none;"
              :: =onclick  "this.classList.add('bd1');"
              :: =onblur  "this.classList.remove('bd1');"
              :: =onmouseover  "this.classList.add('b1');"
              :: =onmouseout  "this.classList.remove('b2');"
              =id            (tail key)
              =type          "text"
              =data-key      key
              =autocomplete  "off"
              =value         (trip text.task)
              =return        "/target/data-key /target/value"
              =event         "/input/task-text"
              ;
            ==
            ;div.hidden.fr.g2
              ;button.p2.br1.hover.pointer.b1
                =data-key  key
                =return    "/target/data-key"
                =event     "/click/move-down"
                ; ↓
              ==
              ;button.p2.br1.hover.pointer.b1
                =data-key  key
                =return    "/target/data-key"
                =event     "/click/move-up"
                ; ↑
              ==
          ::  
              ;button.p2.br1.hover.pointer.b1
                =type      "submit"
                =name      "oust"
                =data-key  key
                =return    "/target/data-key"
                =event     "/click/delete"
                ;span:  X
              ==
            ==
            ;+  
              =/  to-kid=tape
                ;:  welp  "/mast/mast-task-ui"
                %-  en-tape:pith:neo 
                here
                key
                ==
              ^-  manx
              ;a.p2.br1.f0.hover.b1
                =style  "text-decoration: none !important;"
                =href  to-kid
                ;span.br1.hfc:  →
              ==
          ==
    ==
  --
::
++  task-map
  |=  =bowl:neo
  ^-  (map pith task)
  %-  malt
  %+  turn
    %~  tap
      of:neo
    q:(~(got by deps.bowl) %src)
  |=  [=pith =idea:neo]
  :-  pith
  !<(task q.pail.idea)
::
++  task-obj
  |=  =bowl:neo
  ^-  task
  !<  task
  q.pail:(~(got of:neo q:(~(got by deps.bowl) %src)) /)
::
++  got-key
  |=  data=(map @t @t)
  ^-  pith:neo
  %-  pave:neo 
  %-  stab 
  (~(got by data) '/target/data-key')
::
--
