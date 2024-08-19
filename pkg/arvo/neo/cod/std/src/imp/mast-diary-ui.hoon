/@  ui-event
/@  txt
/@  diary-diff
^-  kook:neo
=<
|%
++  state  pro/%manx
++  poke   (sy %ui-event %rely %gift ~)
++  kids
  *kids:neo
  :: ^-  kids:neo
  :: :+  ~  %y
  :: %-  my
  :: :~  [[&/%selection |] pro/%sig ~]
  :: ==
++  deps
  ^-  deps:neo
  %-  my
  :~  :^  %src  &  [pro/%diary (sy %diary-diff ~)]
      :+  ~  %y
      %-  my
      :~  [[|/%da |] only/%txt ~]
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
    manx/!>((render (get-render-data bowl)))
  ::
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    ^-  (quip card:neo pail:neo)
    ?+  sud  ~|(bad-stud/sud !!)
      ::
        %ui-event
      =/  eve  !<(ui-event vaz)
      ?+  path.eve  ~|(missing-event-handler-for/path.eve !!)
        ::
          [%submit %diary-form ~]
        =/  dat=@t          (~(got by data.eve) 'diary-input')
        =/  dif=diary-diff  [%put-entry now.bowl dat]
        =/  dst=pith:neo    p:(~(got by deps.bowl) %src)
        :_  pail
        :~  [dst %poke diary-diff/!>(dif)]
        ==
        ::
          [%click %delete @ta ~]
        =/  key=@da         (slav %da i.t.t.path.eve)
        =/  dif=diary-diff  [%del-entry key]
        =/  dst=pith:neo    p:(~(got by deps.bowl) %src)
        :_  pail
        :~  [dst %poke diary-diff/!>(dif)]
        ==
        ::
      ==
      ::
        %rely
      `manx/!>((render (get-render-data bowl)))
      ::
    ==
  ::
  --
--
::
|%
::
+$  render-data
  $:  diary-entries=(list [date=@da =txt])
      selection=(unit @da)
      =bowl:neo
  ==
::
++  render
  |_  render-data
  ::
  ++  $
    ^-  manx
    ;html
      ;head
        ;meta(charset "utf-8");
        ;title: diary
        ;link
          =href  "/blue/blue-mime/{(scow %p our.bowl)}/static/feather"
          =rel  "stylesheet"
          ;
        ==
        ;link
          =href  "https://em-content.zobj.net/source/microsoft-teams/363/memo_1f4dd.png"
          =rel  "icon"
          ;
        ==
        ;script
          ;+  ;/
            %-  trip
            '''
            function setLoading(idStr) {
              let target = document.getElementById(idStr);
              target.classList.add('htmx-request');
            };
            function setLoaded(idStr) {
              let target = document.getElementById(idStr);
              target.classList.remove('htmx-request');
            };
            '''
        ==
      ==
      ;+  body
    ==
  ::
  ++  body
    ^-  manx
    ;body
      ;main.p-page.mw-page.ma.fc.g5
        ;h1.bold.f-3: MAST
        ;+  diary-form
        ;+  diary-items
      ==
    ==
  ::
  ++  diary-form
    ^-  manx
    ;form.fc.g2.as
      =event        "/submit/diary-form"
      =js-on-event  "setLoading('form-button');"
      ;textarea.p2.br1.bd1.wf
        =name  "diary-input"
        =required  ""
        =placeholder  "today, I ..."
        =rows  "5"
        ;*  ~
      ==
      ;button#form-button.loader.b1.p2.br1.bd1.wfc.hover
        ;span.loaded: create
        ;span.loading.s-2.f4
          ; ...
        ==
      ==
    ==
  ::
  ++  diary-items
    ^-  manx
    ;div.fc.g2
      ;*  %+  turn  diary-entries
          |=  [date=@da =txt]
          =/  key=tape  <date>
          ;div.fr.af.g2
            =key  key
            =js-on-add  "setLoaded('form-button');"
            ;div.fc.g1.grow.br1.p-2.b1
              ;p.f3: {(pretty-date date)}
              ;p.bold: {(trip txt)}
            ==
            ;button.loader.p2.br1.b1.hover
              =event        "/click/delete/{key}"
              =js-on-event  "setLoading('{key}');"
              =id           key
              ;span.loaded
                ; X
              ==
              ;span.loading.s-2.f4
                ; loading
              ==
            ==
          ==
    ==
  ::
  --
::  ::  ::  ::  ::  ::  ::  ::  ::  ::
++  get-render-data
  |=  =bowl:neo
  ^-  render-data
  :*  (get-diary-entries deps.bowl)
      (get-selection kids.bowl)
      bowl
  ==
::
++  get-diary-entries
  |=  deps=(map term (pair pith:neo lore:neo))
  ^-  (list [date=@da =txt])
  =/  data=(unit (pair pith:neo lore:neo))
    (~(get by deps) %src)
  ?~  data  ~|(%no-diary !!)
  =/  entries=(list [date=@da =txt])
    %+  turn  ~(tap by kid.q.u.data)
    |=  (pair iota:neo (axal:neo idea:neo))
    ?>  &(?=(^ p) ?=(%da -.p) ?=(^ fil.q))
    [+.p !<(txt q.pail.u.fil.q)]
  %+  sort  entries
  |=  (pair [date=@da =txt] [date=@da =txt])
  (gth date.p date.q)
::
++  get-selection
  |=  kids=(axal:neo idea:neo)
  ^-  (unit @da)
  =/  data=(unit (axal:neo idea:neo))
    (~(get by kid.kids) %selection)
  ?~  data  ~
  ?~  fil.u.data  ~
  [~ !<(@da q.pail.u.fil.u.data)]
::
++  pretty-date  :: from diary-htmx
  |=  date=@da
  ^-  tape
  =/  d  (yore date)
  "{(y-co:co y:d)}-{(y-co:co m:d)}-{(y-co:co d:t:d)}"
::
--