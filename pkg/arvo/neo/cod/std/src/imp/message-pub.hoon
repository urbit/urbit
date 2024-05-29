/@  ships
/@  ships-diff
/@  txt
::
^-  kook:neo
|%
++  state  pro/%ships  :: ships that are allowed to post
++  poke  (sy %txt %ships-diff ~)
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
    [~ ships/!>((sy our.bowl ~))]
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  members  !<(ships q.state)
    ?+    stud  !!  
        %txt
      ?>  (~(has in members) ship.src.bowl)
      =/  contents=@t  !<(txt vax)
      :_  state
      :~  :-  (welp here.bowl ~[da/now.bowl])
          [%make %message `message/!>([ship.src.bowl now.bowl contents]) ~]
      ==
    ::
        %ships-diff
      ?>  =(our ship.src):bowl
      =/  poke  !<(ships-diff vax)
      ?-    -.poke
          %put
        [~ ships/!>((~(put in members) ship.poke))]
          %del
        [~ ships/!>((~(del in members) ship.poke))]
      ==
    ==
  --
--