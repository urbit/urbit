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
      :+  ~  %y
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
    manx/!>((render (task-map bowl)))
  ::
  ++  poke
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  ~|(bad-stud/stud !!)
    ::
        %ui-event
      =/  event  !<(ui-event vase)
      ?+    path.event
        ~|(missing-event-handler-for/path.event !!)
      ::
          [%submit %new-task ~]
        =/  text=@t
          (~(got by data.event) 'task-input')
        :_  pail
        :~  :-  p:(~(got by deps.bowl) %src)
            :+  %poke 
              %task-diff
            !>([%new [text %.y %.y ~] %.y])
        ==
      :: 
          [%checkbox * ~]
        ~&  >  path.event
        =/  inner  (oust [0 1] (pave:neo path.event))
        ~&  >>  inner
        ::=/  sub=pith
        ::  :~  :-  %ud
        ::      (slav %ud i.t.t.path.event)
        ::  ==
        =/  =lore:neo  q:(~(got by deps.bowl) %src)
        =/  =idea:neo  (~(got of:neo lore) inner)
        =/  t  !<(task q.pail.idea)
        :_  pail
        :~  :-  (welp p:(~(got by deps.bowl) %src) inner)
            :+  %poke 
              %task-diff
            !>([%edit text.t !done.t])
        ==
      ==
    ::
        %rely
      :-  ~
      manx/!>((render (task-map bowl)))
    ==
  --
--
::
|%
++  render
  |_  tasks=(map pith task)
  ++  $
    ^-  manx
    ;html
      ;head
        ;meta(charset "utf-8");
      ==
      ;body
        =style  "margin: 0; width: 100%; display: grid; place-items: center;"
        ;main
          ;h1: Tasks
          ;div
            ;p: {(trip text:(~(got by tasks) /))}
          ==
          ;+  task-form
          ;+  subtasks
        ==
      ==
    ==
  ::
  ++  task-form
    ^-  manx
    ;form
      =event  "/submit/new-task"
      ;textarea(name "task-input", style "height: 10rem; width: 25rem; margin-block: 1rem;");
      ;button: Enter
    ==
  ::
  ++  subtasks
    ^-  manx
    ;div
      ;*  %+  turn
            %~  tap  by
            (~(del by tasks) /)
          |=  [=pith =task]
          =/  key  (en-tape:pith:neo pith)
          ~&  >>  (welp "/checkbox" key)
          ;div
            ;p: {(trip text.task)}
            ;input
              =id     "task-checkbox"
              =type   "checkbox"
              =name   "done"
              =event  (welp "/checkbox" key)
              ;
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
--
