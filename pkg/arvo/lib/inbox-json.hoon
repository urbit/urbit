/-  *inbox
|%
++  inbox-to-json
  |=  box=inbox
  =,  enjs:format
  ^-  ^json
  %+  frond  %inbox-initial
  %-  pairs
  %+  turn  ~(tap by box)
  |=  [=path =mailbox]
  ^-  [cord ^json]
  :-  (spat path)
  %-  pairs
  :~  [%envelopes [%a (turn envelopes.mailbox enve)]]
      [%owner (ship owner.mailbox)]
      [%read (numb read.mailbox)]
  ==
::
++  as                                                :::  array as set
  =,  dejs:format
  |*  a/fist
  (cu ~(gas in *(set _$:a)) (ar a))
::
++  slan  |=(mod/@tas |=(txt/@ta (need (slaw mod txt))))
::
++  seri                                              :::  serial
  =,  dejs:format
  ^-  $-(json serial)
  (cu (slan %uv) so)
::
++  pa 
  |=  a/path
  ^-  json
  s+(spat a)
::
++  enve
  |=  =envelope
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  [%uid s+(scot %uv uid.envelope)]
      [%author (ship author.envelope)]
      [%when (time when.envelope)]
      [%message s+message.envelope]
  ==
--

