/@  txt
::
^-  kook:neo
=<
|%
++  state  pro/%txt
++  poke  ~
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%da |]
      [pro/%txt ~]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :_  (need old)
    :~  :-  [p/our.bowl %wizard ~]
        :+  %poke 
          %wizard-poke
        !>([%add-wizard %nested-text (wizard bowl)])
    ==
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    !!
  --
--
::
|%
++  wizard
  |=  =bowl:neo
  ^-  manx
  ;form.fc.g2
    =style   "margin-bottom: 30px;"
    =method  "post"
    =action  "/wizard/nested-text/{(en-tape:pith:neo here.bowl)}"
    ;textarea.p2.bd1.br1
      =name  "pith"
      =placeholder  "name"
      =rows  "1"
      ;
    ==
    ;textarea.hidden
      =name  "stud"
      =value  "%txt"
      ;
    ==
    ;textarea.hidden
      =name  "head-pail"
      =value  "%txt"
      ;
    ==
    ;textarea.p2.bd1.br1
      =name  "vase"
      =placeholder  "text"
      =rows  "3"
      ;
    ==
    ;button
      =type  "submit"
      ; Submit
    ==
  ==
--