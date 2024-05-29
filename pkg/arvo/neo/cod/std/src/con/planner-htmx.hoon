/@  planner
/@  planner-entry
/-  feather-icons
:-  [%planner %$ %htmx]
|=  p=planner
|=  =bowl:neo
^-  manx
::  XX  change id from da to ux. confusing
|^
  ;div.wf.planner.relative
    ;div.mw-page.p-page.ma.fc.g3
      ;+  controls
      ;+  add-chapter
      ;+  days
    ==
  ==
+$  pent  (pair @da planner-entry)
++  post-url
  ;:  welp
    "/neo/hawk"
    (en-tape:pith:neo here.bowl)
    "?stud=planner-diff"
  ==
++  controls
  ;form.fr.g4.p3.jc.ac.sticky.b0.wf.z1
    =style  "top:0; left:0;"
    =hx-post  post-url
    =hx-target  "find .loading"
    =hx-swap  "outerHTML"
    =head  "move"
    ;label.fr.g2.ac.js
      ;span.f3: from
      ;input.p-1.br1.bd1
        =type  "date"
        =name  "start"
        =oninput  "$(this).attr('value', this.value)"
        =value  (da-to-date start.p)
        =required  ""
        ;
      ==
    ==
    ;label.fr.g2.ac.js
      ;span.f3: to
      ;input.p-1.br1.bd1
        =type  "date"
        =name  "end"
        =oninput  "$(this).attr('value', this.value)"
        =value  (da-to-date end.p)
        =required  ""
        ;
      ==
    ==
    ;button.loader.p-1.br1.bd1.b1.hover
      ;span.loading
        ;+  loading.feather-icons
      ==
      ;span.loaded: view
    ==
  ==
++  add-chapter
  ;div.fc.g1.ae
    ;button.p2.br1.bd1.hover.b1.wfc
      =onclick  "$(this).next().toggleClass('hidden'); $(this).toggleClass('toggled');"
      ; create multi-day event
    ==
    ;form.hidden.p2.fc.g2.br1.bd1.wf
      =style  "margin-bottom: 30px;"
      =hx-post  post-url
      =hx-target  "find .loading"
      =hx-swap  "outerHTML"
      =head  "make"
      ;div.hidden(name "kind", value "chapter");
      ;label.fc.g1.af.js
        ;span.f3: name
        ;input.p-1.br1.bd1
          =type  "text"
          =name  "text"
          =placeholder  "multi-day event name"
          =autocomplete  "off"
          =oninput  "$(this).attr('value', this.value)"
          =required  ""
          ;
        ==
      ==
      ;label.fc.g1.af.js
        ;span.f3: from
        ;input.p-1.br1.bd1
          =type  "date"
          =name  "start"
          =oninput  "$(this).attr('value', this.value)"
          =required  ""
          ;
        ==
      ==
      ;label.fc.g1.af.js
        ;span.f3: to
        ;input.p-1.br1.bd1
          =type  "date"
          =name  "end"
          =oninput  "$(this).attr('value', this.value)"
          =required  ""
          ;
        ==
      ==
      ;button.action.loader.p2.b1.hover.bd1.br1
        ;span.loading
          ;+  loading.feather-icons
        ==
        ;span.loaded: create
      ==
    ==
  ==
++  events
  |=  d=@da
  ^-  (list pent)
  %-  sort
  :_  |=  [a=pent b=pent]
      (lth when.q.a when.q.b)
  %+  murn  ~(tap of:neo kids.bowl)
  |=  [=pith =idea:neo]
  ^-  (unit pent)
  ?~  pith  ~
  =/  entry  !<(planner-entry q.pail.idea)
  ?.  ?&
        (gte when.entry d)
        (lth when.entry (add d ~d1))
        =(kind.entry %event)
        !tombed.entry
      ==
    ~
  `[`@da`->.pith entry]
++  chapter
  |=  d=@da
  ^-  (unit pent)
  %-  mole  |.
  %+  snag  0
  ^-  (list pent)
  %-  sort
  :_  |=  [a=pent b=pent]
      (gth when.q.a when.q.b)
  %+  murn  ~(tap of:neo kids.bowl)
  |=  [=pith =idea:neo]
  ^-  (unit pent)
  =/  entry  !<(planner-entry q.pail.idea)
  ?.  ?&
        (gte d when.entry)
        (lte d (add when.entry length.entry))
        =(kind.entry %chapter)
        !tombed.entry
      ==
    ~
  `[`@da`->.pith entry]
++  days
  ;div.fc
    ;*
    =/  sta  d:(yell start.p)
    =/  end  d:(yell end.p)
    =?  end  (gth sta end)  (add sta 15)
    %+  turn  (gulf sta end)
    |=  day=@
    =/  t  *tarp
    =/  d=@da  (yule t(d day))
    =/  ch  (chapter d)
    ;div
      =class  "fc af"
      ;+  (chapter-btn d ch)
      ;+  (date-btn d ch)
      ;div.fc
        ;*
        %+  turn  (events d)
        |=  =pent
        (event-btn pent ch)
      ==
    ==
  ==
++  date-btn
  |=  [d=@da chapter=(unit pent)]
  =/  chapter-class  ?~(chapter "b0" "b1")
  ;div.fc
    =id  "date{(scow %p d)}"
    ;button
      =class  "action s0 hover p-1 tl {chapter-class}"
      =onclick  "$(this).next().toggleClass('hidden'); $(this).toggleClass('toggled')"
      ;+  (pretty-date d)
    ==
    ;div
      =class  "hidden"
      =style  "padding: 0 10px 20px 20px;"
      ;form
        =class  "p1 fc g1"
        =hx-post  post-url
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        =head  "make-event"
        ;div.fr.ac.g2.js
          ;label.fc.af.hidden
            ;span.s-1.f3: date
            ;input.p-1.bd1.br1.b0.fr
              =autocomplete  "off"
              =type  "date"
              =name  "date"
              =required  ""
              =value  (da-to-date d)
              =oninput  "$(this).attr('value', this.value);"
              ;
            ==
          ==
          ;label.fc
            ;span.s-1.f3: start
            ;input.p-1.bd1.br1.b0.wf
              =autocomplete  "off"
              =type  "time"
              =name  "start"
              =oninput  "$(this).attr('value', this.value);"
              ;
            ==
          ==
          ;label.fc
            ;span.s-1.f3: end
            ;input.p-1.bd1.br1.b0.wf
              =autocomplete  "off"
              =type  "time"
              =name  "end"
              =oninput  "$(this).attr('value', this.value);"
              ;
            ==
          ==
        ==
        ;input.p1.bd1.br1.b0.s1
          =autocomplete  "off"
          =type  "text"
          =name  "text"
          =autocomplete  "off"
          =oninput  "$(this).attr('value', this.value);"
          =placeholder  "event title"
          ;
        ==
        ;button.action.loader.p2.b1.hover.bd1.br1
          ;span.loading
            ;+  loading.feather-icons
          ==
          ;span.loaded: create event
        ==
      ==
    ==
  ==
++  chapter-btn
  |=  [d=@da chapter=(unit pent)]
  ?~  chapter  ;/("")
  ?.  ?&
        (gte d when.q.u.chapter)
        (lth d (add when.q.u.chapter ~d1))
      ==
    ;/("")
  ;div.fc
    =id  "chapter{(scow %p p.u.chapter)}"
    ;button
      =class  "action s0 f3 hover p-1 tc b1"
      =morph-retain  "class"
      =onclick  "$(this).next().toggleClass('hidden'); $(this).toggleClass('toggled')"
      =style  "margin-top: 10px;"
      ; {(trip text.q.u.chapter)}
    ==
    ;div
      =class  "hidden b0 fc g1"
      =morph-retain  "class"
      =style  "padding: 0 10px 20px 20px;"
      ;form
        =class  "fc g1 p1"
        =hx-post  post-url
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        =planner-id  (scow %da p.u.chapter)
        =head  "edit"
        =kind  "chapter"
        ;div.fr.ac.js.g2
          ;input.p-1.br1.bd1.wfc
            =type  "date"
            =name  "start"
            =required  ""
            =oninput  "$(this).attr('value', this.value);"
            =value  (da-to-date when.q.u.chapter)
            ;
          ==
          ;input.p-1.br1.bd1.wfc
            =type  "date"
            =name  "end"
            =required  ""
            =oninput  "$(this).attr('value', this.value);"
            =value  (da-to-date (add when.q.u.chapter length.q.u.chapter))
            ;
          ==
        ==
        ;input.planner-chap-in.p2.br1.bd1
          =type  "text"
          =name  "text"
          =required  ""
          =autocomplete  "off"
          =oninput  "$(this).attr('value', this.value);"
          =placeholder  "title"
          =value  (trip text.q.u.chapter)
          ;
        ==
        ;button.loader.p2.br1.bd1.b1.hover
          ;span.loading
            ;+  loading.feather-icons
          ==
          ;span.loaded: update multi-day event
        ==
      ==
      ;div.p1.fr.je
        ;button.action.p-1.br1.bd1.b1.wfc.hover.loader
          =type  "button"
          =hx-post  post-url
          =hx-target  "find .loading"
          =hx-swap  "outerHTML"
          =head  "tomb"
          =planner-id  (scow %da p.u.chapter)
          ;span.loaded: delete
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
      ==
    ==
  ==
++  event-btn
  |=  [=pent chapter=(unit pent)]
  =/  chapter-class  ?~(chapter "b0" "b1")
  ;div
    =id  "event{(scow %p p.pent)}"
    =class  "fc"
    ;button
      =class  "action bold s1 f0 hover tl {chapter-class}"
      =style  "padding-left:25px;"
      =onclick  "$(this).next().toggleClass('hidden'); $(this).toggleClass('toggled')"
      ; {(trip text.q.pent)}
    ==
    ;div
      =class  "hidden"
      =style  "padding: 0 10px 20px 20px;"
      ;form
        =class  "p1 fc g1"
        =hx-post  post-url
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        =planner-id  (scow %da p.pent)
        =head  "edit-event"
        ;div.fr.ac.js.g2
          ;label.fc.af
            ;span.s-1.f3: date
            ;input.p-1.bd1.br1.b0.fr
              =autocomplete  "off"
              =type  "date"
              =name  "date"
              =required  ""
              =value  (da-to-date when.q.pent)
              =oninput  "$(this).attr('value', this.value);"
              ;
            ==
          ==
          ;label.fc.af
            ;span.s-1.f3: start
            ;input.p-1.br1.bd1.grow
              =type  "time"
              =name  "start"
              =oninput  "$(this).attr('value', this.value);"
              ;
            ==
          ==
          ;label.fc.af
            ;span.s-1.f3: end
            ;input.p-1.br1.bd1.grow
              =type  "time"
              =name  "end"
              =oninput  "$(this).attr('value', this.value);"
              ;
            ==
          ==
        ==
        ;input.p2.br1.bd1
          =type  "text"
          =name  "text"
          =required  ""
          =autocomplete  "off"
          =oninput  "$(this).attr('value', this.value);"
          =placeholder  "event name"
          =value  (trip text.q.pent)
          ;
        ==
        ;button.loader.p2.br1.bd1.b1.hover
          ;span.loading
            ;+  loading.feather-icons
          ==
          ;span.loaded: update event
        ==
      ==
      ;div.fr.p1.je
        ;button.p-1.br1.bd1.b1.hover.loader
          =type  "button"
          =hx-post  post-url
          =hx-target  "find .loading"
          =hx-swap  "outerHTML"
          =head  "tomb"
          =planner-id  (scow %da p.pent)
          ;span.loaded: delete
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
      ==
    ==
  ==
++  da-to-date
  |=  d=@da
  =/  y  (yore d)
  =/  year  (y-co:co y:y)
  =/  month  (y-co:co m:y)
  =/  day  (y-co:co d:t:y)
  "{year}-{month}-{day}"
++  da-to-datetime
  |=  d=@da
  =/  y  (yore d)
  =/  year  (y-co:co y:y)
  =/  month  (y-co:co m:y)
  =/  day  (y-co:co d:t:y)
  =/  hour  (y-co:co h:t:y)
  =/  min  (y-co:co h:t:y)
  "{year}-{month}-{day}T{hour}:{min}"
++  pretty-date
  |=  d=@da
  ^-  manx
  =/  i  d:(yell d)
  =/  w  (~(sit fo 7) (add i 6))
  =/  weekday
    %+  snag  w
    ^-  (list tape)
    ["S" "M" "T" "W" "T" "F" "S" ~]
  =/  yor  (yore d)
  =/  day  (y-co:co d:t:yor)
  =/  month
    %+  snag  (dec m:yor)
    ^-  (list tape)
    :~
      "JAN"
      "FEB"
      "MAR"
      "APR"
      "MAY"
      "JUN"
      "JUL"
      "AUG"
      "SEP"
      "OCT"
      "NOV"
      "DEC"
    ==
  =/  color
    ?:  =(i d:(yell now.bowl))
      "f-3 bold"
    ?:  =(0 w)
      "f0"
    "f3"
  ;div
    =class  "s-1 fr g1 mono {color}"
    ;span.s-1: {weekday}
    ;span.s-1: {day}
    ;span.s-1: {month}
  ==
--
