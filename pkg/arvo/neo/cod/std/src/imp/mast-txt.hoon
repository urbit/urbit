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
    ;html
      ;head
        ;title: txt
        ;style
          ;+  ;/  %-  trip
          '''
          * {
            box-sizing: border-box;
          }
          html {
            height: 100%;
          }
          body {
            width: 100%;
            height: 100%;
            display: flex;
            margin: 0;
          }
          textarea {
            margin: 0;
            border: none;
            padding: 10px;
            font-family: monospace;
            background: black;
            color: white;
            width: 100%;
            flex-grow: 1;
            user-resize: none;
          }
          '''
        ==
      ==
      ;body
        ;textarea
          =event  "/input/form"
          =return  "/target/value"
          =debounce  "0.5"
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
