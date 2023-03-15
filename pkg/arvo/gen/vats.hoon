/-  *hood
:-  %say
=>
:: usage: +vats, =filter [%filter], =short [?]
::
|%
+$  filter  ?(%suspended %running %blocking %exists %default)
::
+$  short  _|
::
++  report-running
  |=  [our=@p now=@da =short]
  ^-  tang
  =/  desks  
  .^((set desk) %cd /(scot %p our)/base/(scot %da now)) 
  ::preps desk variable, set of desks, scries clay for desks 
  ::
  =.  desks  ::filtering starts here
    %-  silt
    %+  skim 
      ~(tap in desks) 
    |=  syd=desk
    =/  deskstatus  
    .^(rock:tire:clay %cx /(scot %p our)//(scot %da now)/tire)
    =(%live -:(~(got by deskstatus) syd))
  (create-report our now short desks)
::
++  report-suspended
  |=  [our=@p now=@da =short]
  ^-  tang
  =/  desks  
  .^((set desk) %cd /(scot %p our)/base/(scot %da now)) 
  ::preps desk variable, set of desks, scries clay for desks 
  ::
  =.  desks
    %-  silt
    %+  skim
      ~(tap in desks) 
    |=  syd=desk
    =/  deskstatus  
    .^(rock:tire:clay %cx /(scot %p our)//(scot %da now)/tire)
    =(%dead -:(~(got by deskstatus) syd))
  (create-report our now short desks)
::
++  report-blocking
  |=  [our=@p now=@da =short]
  ^-  tang
  =/  kel  (weft .^(* cx/(en-beam [our %base da+now] /sys/kelvin)))  
  =/  pikes  
  .^(pikes %gx /(scot %p our)/hood/(scot %da now)/kiln/pikes/kiln-pikes)
  ::preps pikes, a (map desk pike)  
  ::
  =/  desks
  %~  key  by
  %-  molt  
  %+  skim  
    ~(tap by pikes) 
  |=  input=[=desk =pike] 
  ::Must return a bool. Check if pike contains desk kel
  ::
  ?.  =(%base desk.input)
    |
  ?.  =(%live zest.pike.input) 
    |
  !(~(has in wic.pike.input) kel) 
  (create-report our now short desks)
::
++  report-exists
  |=  [our=@p now=@da =short]
  ^-  tang
  =/  desks  
  .^((set desk) %cd /(scot %p our)/base/(scot %da now)) 
  ::preps desk variable, set of desks, scries clay for desks 
  ::
  =.  desks
    %-  silt
    %+  skim
      ~(tap in desks) 
    |=  syd=desk
    =/  deskstatus=cass:clay  
    .^(=cass:clay %cw /(scot %p our)/[syd]/(scot %da now))
    !=(ud.deskstatus 0)
  (create-report our now short desks)
::
++  create-report
  |=  [our=@p now=@da =short desks=(set desk)]
  =/  prep  (report-prep our now)
  ?:  short
    (turn ~(tap in desks) |=(syd=desk (report-vat-short prep our now syd)))
  (turn ~(tap in desks) |=(syd=desk (report-vat prep our now syd)))
--
::
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=~ [=filter =short ~]]
    ==
?+  filter  [%tang (report-vats p.bec now short)]
  %running  [%tang (report-running p.bec now short)]
  %suspended  [%tang (report-suspended p.bec now short)]
  %blocking  [%tang (report-blocking p.bec now short)]
  %exists  [%tang (report-exists p.bec now short)]
==