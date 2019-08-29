/-  *inbox
|%
++  inbox-to-json
  |=  box=inbox-initial
  =,  enjs:format
  ^-  ^json
  %+  frond  %inbox-initial
  %-  pairs
  %+  turn  ~(tap by box)
  |=  [=path =mailbox]
  ^-  [@t ^json]
  :-  (spat path)
  %-  pairs
  :~  [%envelopes [%a (turn envelopes.mailbox enve)]]
      [%owner (ship:enjs:format owner.mailbox)]
      [%read (numb read.mailbox)]
  ==
::
++  as                                                :::  array as set
  =,  dejs:format
  |*  a/fist
  (cu ~(gas in *(set _$:a)) (ar a))
::
++  pa 
  |=  a/path
  ^-  json
  s+(spat a)
::
++  enve
  |=  =envelope
  =,  enjs:format
  ^-  json
  %+  frond  %envelope
  %-  pairs
  :~  [%author s+(scow %p author.envelope)]
      [%when (time when.envelope)]
      [%message s+message.envelope)]
  ==
--

