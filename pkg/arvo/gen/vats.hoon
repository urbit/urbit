/-  *hood
:-  %say
=>
:: usage: +vats, =options [%option], =short [?]
|%
+$  options  ?(%suspended %running %blocking %exists %default)

+$  short  _|

++  report-running
  |=  [our=@p now=@da =short]
  ^-  tang
  =/  desks  .^((set desk) %cd /(scot %p our)/base/(scot %da now)) ::preps desk variable, set of desks, scries clay for desks 
  =.  desks
  %-  silt
  %+  skim
  ~(tap in desks) 
  |=  syd=desk
  =/  deskstatus  .^(rock:tire:clay %cx /(scot %p our)//(scot %da now)/tire)
  =(%live -:(~(got by deskstatus) syd))
  =/  prep  (report-prep our now)
  ?:  short
    %+  turn  ~(tap in desks) :: applies the following gate to list of desks
    |=(syd=desk (report-vat-short prep our now syd))
  %+  turn  ~(tap in desks) 
  |=(syd=desk (report-vat prep our now syd))

++  report-suspended
  |=  [our=@p now=@da =short]
  ^-  tang
  =/  desks  .^((set desk) %cd /(scot %p our)/base/(scot %da now)) ::preps desk variable, set of desks, scries clay for desks 
  =.  desks
  %-  silt
  %+  skim
  ~(tap in desks) 
  |=  syd=desk
  =/  deskstatus  .^(rock:tire:clay %cx /(scot %p our)//(scot %da now)/tire)
  =(%dead -:(~(got by deskstatus) syd))
  =/  prep  (report-prep our now) 
  ?:  short
    %+  turn  ~(tap in desks) :: applies the following gate to list of desks
    |=(syd=desk (report-vat-short prep our now syd))
  %+  turn  ~(tap in desks) 
  |=(syd=desk (report-vat prep our now syd))

++  report-blocking
  |=  [our=@p now=@da =short]
  ^-  tang
  =/  kel  (weft .^(* cx/(en-beam [our %base da+now] /sys/kelvin)))  
  =/  pikes  .^(pikes %gx /(scot %p our)/hood/(scot %da now)/kiln/pikes/kiln-pikes) ::preps pikes, a (map desk pike)  
  =/  desks  
  %+  skim  ::produces a list
  ~(tap by pikes) ::turns a map into a list
  |=  input=[=desk =pike] ::Must a bool. Check if pike contains desk kel
  ?.  =(%base desk.input) ::if base, not blocker
    |
  ?.  =(%live zest.pike.input) ::if desk is not live return negative
    |
  !(~(has in wic.pike.input) kel) 
  =/  prep  (report-prep our now) 
  ?:  short
    %+  turn  ~(tap in ~(key by (molt desks))):: applies the following gate to list of desks
    |=(syd=desk (report-vat-short prep our now syd))    
  %+  turn  ~(tap in ~(key by (molt desks)))
  |=(syd=desk (report-vat prep our now syd))

++  report-exists
  |=  [our=@p now=@da =short]
  ^-  tang
  =/  desks  .^((set desk) %cd /(scot %p our)/base/(scot %da now)) ::preps desk variable, set of desks, scries clay for desks 
  =.  desks
  %-  silt
  %+  skim
  ~(tap in desks) 
  |=  syd=desk
  =/  deskstatus=cass:clay  .^(=cass:clay %cw /(scot %p our)/[syd]/(scot %da now))
  !=(ud.deskstatus 0)
  =/  prep  (report-prep our now) 
  ?:  short
    %+  turn  ~(tap in desks) 
    |=(syd=desk (report-vat-short prep our now syd))
  %+  turn  ~(tap in desks) 
  |=(syd=desk (report-vat prep our now syd))

--
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=~ [=options =short ~]]
    ==
?+  options  [%tang (report-vats p.bec now short)]
%running  [%tang (report-running p.bec now short)]
%suspended  [%tang (report-suspended p.bec now short)]
%blocking  [%tang (report-blocking p.bec now short)]
%exists  [%tang (report-exists p.bec now short)]
==


