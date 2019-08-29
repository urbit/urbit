/-  *inbox
/+  hall-json
=,  dejs:format
|%
++  inbox-to-json
  |=  box=inbox-initial
  =,  enjs:format
  ^-  ^json
  %+  frond  %initial
  %-  pairs
  %+  turn  ~(tap by box)
  |=  [=path =mailbox]
  ^-  [@t ^json]
  :-  (spat path)
  %-  pairs
  :~  [%envelopes [%a (turn envelopes.mailbox enve:enjs:hall-json)]]
      [%owner (ship:enjs:format owner.mailbox)]
      [%read (numb read.mailbox)]
  ==
::
++  as                                                :::  array as set
  |*  a/fist
  (cu ~(gas in *(set _$:a)) (ar a))
--

