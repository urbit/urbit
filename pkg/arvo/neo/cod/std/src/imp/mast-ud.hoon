/@  ud
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
  :~  :^  %src  &  [pro/%ud (sy %ud ~)]
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
          =/  new  (slav %ud (~(got by data.eve) 'ud'))
          :_  pail
          :~  [dst %poke ud/!>(new)]
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
    =/  =tape  (scow %ud value)
    ;html
      ;head
        ;title: ud
        ;link
          =rel  "stylesheet"
          =href  "/blue/blue-mime/{(scow %p our.bowl)}/static/feather"
          ;
        ==
      ==
      ;body.fr.ac.jc
        ;form.fr.jc.ac.g3.p5.mw-page
          =event  "/submit/form"
          ;div.f4.o6: {tape}
          ;input.p2.s1.grow.br1.bd1
            =type  "number"
            =name  "ud"
            =min  "0"
            =max  "999"
            =step  "1"
            =value  tape
            ;*  ~
          ==
          ;button.p2.br1.b1.hover: save
        ==
      ==
    ==
  ++  value
    ^-  ud
    =/  =lore:neo  q:(~(got by deps.bowl) %src)
    =/  =pail:neo  pail:(~(got of:neo lore) /)
    !<(ud q.pail)
  --
--
