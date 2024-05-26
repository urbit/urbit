/@  planner
/@  planner-entry
/-  _/feather-icons
:-  [%planner %htmx]
|=  p=planner
|=  =bowl:neo
^-  manx
|^
  ;div.wf.planner
    ;div.mw-page.p-page.ma
      ;+  controls
      ;+  days
    ==
  ==
+$  entry
  $:
    kind=?(%event %chapter %era)
    length=@dr
    text=@t
  ==
++  entries  (list (pair @da entry))
+$  state  [from=@da to=@da =entries]
++  controls
  ;form.fr.g4.p3.jc.ac
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=planner-diff"
    =hx-trigger  "input from:.planner-control"
    =hx-target  "find .loading"
    =hx-swap  "outerHTML"
    =head  "move"
    ;label.fr.g2.ac.js
      ;span.f3: from
      ;input.p-1.br1.bd1.planner-control
        =type  "date"
        =name  "start"
        =oninput  "$(this).attr('value', this.value)"
        =value  (da-to-html-date start.p)
        =required  ""
        ;
      ==
    ==
    ;label.fr.g2.ac.js
      ;span.f3: to
      ;input.p-1.br1.bd1.planner-control
        =type  "date"
        =name  "end"
        =oninput  "$(this).attr('value', this.value)"
        =value  (da-to-html-date end.p)
        =required  ""
        ;
      ==
    ==
    ;div.loader
      ;span.loading
        ;+  loading.feather-icons
      ==
      ;span.loaded(style "opacity: 0;"): ---
    ==
  ==
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
    =/  events
      %+  murn  ~(tap by kids.bowl)
      |=  [=pith =pail:neo]
      ^-  (unit [id=@da planner-entry])
      =/  entry  !<(planner-entry q.pail)
      ?.
        ?&
          (gte when.entry d)
          (lth when.entry (add d ~d1))
          =(kind.entry %event)
          !tombed.entry
        ==
        ~
      `[`@da`->.pith entry]
    =/  chapters
      %+  skim  ~
      |=  [p=@da q=entry]
      ?&
        (gte d p)
        (lth d (add p length.q))
        =(kind.q %chapter)
      ==
    =/  in-chapter  (gth (lent chapters) 0)
    =/  chapter-class  ?:(in-chapter "b1" "b0")
    =/  chapter  ?.  in-chapter  ~  `(snag 0 chapters)
    ;div
      =class  "fc af"
      ;+  (chapter-btn d chapter)
      ;+  (date-btn d chapter)
      ;div.fc
        ;*
        %+  turn  events
        |=  [id=@da entry=planner-entry]
        (event-btn id entry ~)
      ==
    ==
  ==
++  date-btn
  |=  [d=@da chapter=(unit (pair @da entry))]
  =/  chapter-class  ?~(chapter "b0" "b1")
  ;div
    =class  "fc {chapter-class}"
    ;button
      =class  "s0 f3 hover p-1 tl {chapter-class}"
      =onclick  "$(this).next().toggleClass('hidden'); $(this).toggleClass('toggled')"
      ; {<d>}
    ==
    ;div
      =class  "hidden"
      =style  "padding: 0 10px 20px 20px;"
      ;form
        =class  "p3 bd1 br1 fr g2"
        =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=planner-diff"
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        =head  "make"
        =when  (da-to-html-date d)
        ;input.p2.bd1.br1.grow.b0
          =style  "width: 0;"
          =autocomplete  "off"
          =type  "text"
          =name  "text"
          =oninput  "$(this).attr('value', this.value);"
          =placeholder  "ben's birthday"
          ;
        ==
        ;button.loader.p2.b2.hover.bd1.br1
          ;span.loading
            ;+  loading.feather-icons
          ==
          ;span.loaded: create
        ==
      ==
    ==
  ==
++  chapter-btn
  |=  [d=@da chapter=(unit (pair @da entry))]
  ?~  chapter  ;/("")
  ?.
      ?&
        (gte d p.u.chapter)
        (lth d (add p.u.chapter ~d1))
      ==
    ;/("")
  ;div
    =class  "fc"
    ;button
      =class  "s0 f3 hover p-1 tc b1"
      =onclick  "$(this).next().toggleClass('hidden'); $(this).toggleClass('toggled')"
      =style  "margin-top: 10px;"
      ; {(trip text.q.u.chapter)}
    ==
    ;div
      =class  "hidden b1"
      =style  "padding: 0 10px 20px 20px;"
      ;div
        =class  " p3 bd1 br1"
        ;button.b2.br2.hover.wfc: delete
      ==
    ==
  ==
++  event-btn
  |=  [id=@da entry=planner-entry chapter=(unit planner-entry)]
  =/  chapter-class  ?~(chapter "b0" "b1")
  ;div
    =class  "fc {chapter-class}"
    ;button
      =morph-retain  "class"
      =class  "bold s1 f0 hover tl {chapter-class}"
      =style  "padding-left:25px;"
      =onclick  "$(this).next().toggleClass('hidden'); $(this).toggleClass('toggled')"
      ; {(trip text.entry)}
    ==
    ;div
      =class  "hidden"
      =morph-retain  "class"
      =style  "padding: 0 10px 20px 20px;"
      ;form
        =class  "p3 bd1 br1 fc g1"
        =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=planner-diff"
        =hx-trigger  "input changed delay:0.4s from:.planner-entry-in"
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        =planner-id  (scow %da id)
        =head  "edit"
        ;div.fr.ac.jb.g2
          ;input.planner-entry-in.p-1.br1.bd1.wfc
            =type  "date"
            =name  "when"
            =required  ""
            =oninput  "$(this).attr('value', this.value);"
            =value  (da-to-html-date when.entry)
            ;
          ==
          ;button.p-1.br1.bd1.b1.hover.loader
            =type  "button"
            =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=planner-diff"
            =hx-target  "find .loading"
            =hx-swap  "outerHTML"
            =head  "tomb"
            =planner-id  (scow %da id)
            ;span.loaded: delete
            ;span.loading
              ;+  loading.feather-icons
            ==
          ==
        ==
        ;input.planner-entry-in.p2.br1.bd1
          =type  "text"
          =name  "text"
          =required  ""
          =autocomplete  "off"
          =oninput  "$(this).attr('value', this.value);"
          =placeholder  "some text"
          =value  (trip text.entry)
          ;
        ==
        ;div.loader
          ;span.loading: ...
          ;span.loaded(style "opacity: 0"): ...
        ==
      ==
    ==
  ==
++  da-to-html-date
  |=  d=@da
  =/  y  (yore d)
  =/  year  (y-co:co y:y)
  =/  month  (y-co:co m:y)
  =/  day  (y-co:co d:t:y)
  "{year}-{month}-{day}"
--
