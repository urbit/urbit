/@  txt
/@  message
::
^-  kook:neo
|%
++  state  pro/%sig
++  poke  (sy %message %txt ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%da |]
      [pro/%message (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    [~ sig/!>(~)]
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(our ship.src):bowl
    :_  state
    ?+    stud  !!
        %message
      =/  msg  !<(message vax)
      :~  :-  (welp here.bowl ~[da/now.msg])
          [%make %message `message/vax ~]
      ==
        %txt
      =/  contents=@t  !<(txt vax)
      :~  :-  (welp here.bowl ~[da/now.bowl])
          [%make %message `message/!>([ship.src.bowl now.bowl contents]) ~]
      ==
    ==
  --
--
