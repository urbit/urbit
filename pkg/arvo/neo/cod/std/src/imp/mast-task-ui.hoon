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
          [%click %new ~]
        =/  text=@t
          (~(got by data.event) '/task-input/value')
        :_  pail
        :~  :-  p:(~(got by deps.bowl) %src)
            :+  %poke 
              %task-diff
            !>([%new [text %.y %.y ~] %.y])
        ==
      ::
          [%click %checkbox ~]
        =/  t=pith  #/placeholder
          ::(~(got by data.event) '/task-input/value') :: XX encode this
        :_  pail
        :~  :-  t
            :+  %poke 
              %task-diff
            !>([%edit 'placeholder' %.y])  :: XX get text from issue
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
    ;div
      ;textarea(id "task-input", style "height: 10rem; width: 25rem; margin-block: 1rem;");
      ;button
        =event  "/click/new"
        =return  "/task-input/value"
        ;+  ;/  "Enter"
      ==
    ==
  ::
  ++  subtasks
    ^-  manx
    ;div
      ;*  %+  turn
            %~  tap  by
            (~(del by tasks) /)
          |=  [=pith =task]
          ;div
            ;p: {(trip text.task)}
            :: ;input
            ::   =type   "checkbox"
            ::   =name   "done"
            ::   =event  "/click/checkbox"
            ::   =return  :: XX what to return here?
            ::   ;
            :: ==
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
