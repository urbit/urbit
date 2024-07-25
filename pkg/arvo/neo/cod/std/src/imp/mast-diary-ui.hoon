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
  :~  :^  %diary  &  [pro/%diary (sy %diary-diff ~)]
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
    =/  diary-entries
      (get-diary-entries deps.bowl)
    :-  ~
    manx/!>((render [diary-entries ~]))
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
          [%click %submit ~]
        =/  dat=(unit @t)
          (~(get by data.eve) '/diary-input/value')
        ?~  dat
          ~|(%diary-input-fail !!)
        =/  dif=diary-diff
          [%put-entry now.bowl u.dat]
        =/  dst=pith:neo
          p:(~(got by deps.bowl) %diary)
        :_  pail
        :~  [dst %poke diary-diff/!>(dif)]
        ==
        ::
          [%click %delete @ta ~]
        =/  key=@da
          (slav %da i.t.t.path.eve)
        =/  dif=diary-diff
          [%del-entry key]
        =/  dst=pith:neo
          p:(~(got by deps.bowl) %diary)
        :_  pail
        :~  [dst %poke diary-diff/!>(dif)]
        ==
        ::
      ==
      ::
        %rely
      =/  diary-entries
        (get-diary-entries deps.bowl)
      `manx/!>((render [diary-entries ~]))
      ::
    ==
  ::
  --
--
::
|%
::
++  render
  |_  $:  diary-entries=(list [date=@da =txt])
          selection=(unit @da)
      ==
  ++  $
    ^-  manx
    ;html
      ;head
        ;meta(charset "utf-8");
      ==
      ;body
        =style  "margin: 0; width: 100%; display: grid; place-items: center;"
        ;main
          ;h1: Diary
          ;+  diary-form
          ;+  diary-items
        ==
      ==
    ==
  ::
  ++  diary-form
    ^-  manx
    ;div
      ;textarea(id "diary-input", style "height: 10rem; width: 25rem; margin-block: 1rem;");
      ;button
        =event  "/click/submit"
        =return  "/diary-input/value"
        ;+  ;/  "Enter"
      ==
    ==
  ::
  ++  diary-items
    ^-  manx
    ;div
      ;*  %+  turn  diary-entries
          |=  [date=@da =txt]
          =/  key=tape  <date>
          ;div
            =key  key
            ;p: {(pretty-date date)}
            ;p: {(trip txt)}
            ;button
              =event  "/click/delete/{key}"
              ;+  ;/  "✖"
            ==
          ==
    ==
  ::
  --
::  ::  ::  ::  ::  ::  ::  ::  ::  ::
++  get-diary-entries
  |=  sam=(map term (pair pith:neo lore:neo))
  ^-  (list [date=@da =txt])
  =/  dat=(unit (pair pith:neo lore:neo))
    (~(get by sam) %diary)
  ?~  dat
    ~|(%no-diary !!)
  =/  pod=(list [date=@da =txt])
    %+  turn  ~(tap by kid.q.u.dat)
    |=  (pair iota:neo (axal:neo idea:neo))
    ?>  &(?=(^ p) ?=(%da -.p))
    ?>  ?=(^ fil.q)
    [+.p !<(txt q.pail.u.fil.q)]
  %+  sort  pod
  |=  (pair [date=@da =txt] [date=@da =txt])
  (gth date.p date.q)
::
++  get-selection
  |=  sam=(axal:neo idea:neo)
  ^-  (unit @da)
  =/  dat=(unit (axal:neo idea:neo))
    (~(get by kid.sam) %selection)
  ?~  dat  ~
  ?~  fil.u.dat  ~
  [~ !<(@da q.pail.u.fil.u.dat)]
::
++  pretty-date  :: from diary-htmx
  |=  date=@da
  ^-  tape
  =/  d  (yore date)
  "{(y-co:co y:d)}-{(y-co:co m:d)}-{(y-co:co d:t:d)}"
::
--
