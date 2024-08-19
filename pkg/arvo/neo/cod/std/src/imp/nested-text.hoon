/@  txt
/-  srv=sky-server
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
  ;html
    ;head
      ;meta(charset "utf-8");
      ;*  standard-head-tags:srv
    ==
    ;body
      ;form.fc.g2
        =style   "margin-bottom: 30px;"
        =method  "post"
        ;input.p2.bd1.br1
          =name  "pith"
          =placeholder  "name"
          =rows  "1"
          ;
        ==
        ;input.hidden
          =name  "stud"
          =value  "%nested-txt"
          ;
        ==
        ;input.hidden
          =name  "head-pail"
          =value  "%txt"
          ;
        ==
        ;input.p2.bd1.br1
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
    ==
  ==
--