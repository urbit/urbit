/@  txt
/@  ui-event
^-  kook:neo
=<
|%
++  state  pro/%manx
++  poke   (sy %ui-event %rely %gift ~)
++  kids   *kids:neo
++  deps
  ^-  deps:neo
  %-  my
  :~  :^  %src  &  [pro/%txt (sy %txt ~)]
      ~
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    manx/!>((render bowl))
  ::
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    =/  dst  p:(~(got by deps.bowl) %src)
    ^-  (quip card:neo pail:neo)
    ?+  sud  ~|(bad-stud/sud !!)
        %ui-event
      =/  eve  !<(ui-event vaz)
      ?+  path.eve  ~|(missing-event-handler-for/path.eve !!)
        [* %form ~]
          =/  new  (~(got by data.eve) '/target/value')
          :_  pail
          :~  [dst %poke txt/!>(new)]
          ==
      ==
    ::
        %rely
      `manx/!>((render bowl))
    ==
  --
--
::
|%
++  render
  |_  =bowl:neo
  ++  $
    ^-  manx
    =/  =tape  (trip value)
    =/  =iota  (rear p:(~(got by deps.bowl) %src))
    ;html
      ;head
        ;title: {(trip ?@(iota iota (scot iota)))} [txt]
        ;link
          =rel  "stylesheet"
          =href  "/blue/blue-mime/{(scow %p our.bowl)}/static/feather"
          ;
        ==
        ;link
          =rel  "icon"
          =href  "https://em-content.zobj.net/source/google/298/page-facing-up_1f4c4.png"
          ;
        ==
      ==
      ;body
        ;textarea.wf.hf.b0.p3.mono.s0
          =event  "/input/form"
          =return  "/target/value"
          =debounce  "0.5"
          =placeholder  "txt"
          =autocomplete  "off"
          =spellcheck  "false"
          ;+  ;/  tape
        ==
      ==
    ==
  ++  value
    ^-  txt
    =/  =lore:neo  q:(~(got by deps.bowl) %src)
    =/  =pail:neo  pail:(~(got of:neo lore) /)
    !<(txt q.pail)
  --
--
