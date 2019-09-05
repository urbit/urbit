/-  *inbox
|%
++  inbox-to-json
  |=  box=inbox
  =,  enjs:format
  ^-  json
  %+  frond  %inbox-initial
  %-  pairs
  %+  turn  ~(tap by box)
  |=  [=path =mailbox]
  ^-  [cord json]
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
++  lank                                              :::  tank as string arr
  |=  a/tank
  ^-  json
  =,  enjs:format
  a+(turn (wash [0 80] a) tape)
::
++  re                                                ::  recursive reparsers
  |*  {gar/* sef/_|.(fist:dejs-soft:format)}
  |=  jon/json
  ^-  (unit _gar)
  =-  ~!  gar  ~!  (need -)  -
  ((sef) jon)
::
++  dank                                              ::  tank
  ^-  $-(json (unit tank))
  =,  ^?  dejs-soft:format
  %+  re  *tank  |.  ~+
  %-  of  :~
    leaf+sa
    palm+(ot style+(ot mid+sa cap+sa open+sa close+sa ~) lines+(ar dank) ~)
    rose+(ot style+(ot mid+sa open+sa close+sa ~) lines+(ar dank) ~)
  ==
::
++  eval                                              :::  %exp speech
  :::  extract contents of an %exp speech, evaluating
  :::  the {exp} if there is no {res} yet.
  ::
  |=  a/json
  ^-  [cord (list tank)]
  =,  ^?  dejs-soft:format
  =/  exp  ((ot expression+so ~) a)
  %-  need
  ?~  exp
    [~ '' ~]
  :+  ~  u.exp
  =/  res  ((ot output+(ar dank) ~) a)
  ?^  res
    u.res
  p:(mule |.([(sell (slap !>(..^zuse) (ream u.exp)))]~))  ::TODO  oldz
::
++  lett
  |=  =letter
  ^-  json
  =,  enjs:format
  ?-  -.letter
      %text
    (frond %text s+text.letter)
  ::
      %url
    (frond %url s+url.letter)
  ::
      %code
    %+  frond  %code
    %-  pairs
    :~  [%expression s+expression.letter]
        [%output a+(turn output.letter lank)]
    ==
  ::
  ==
::
++  enve
  |=  =envelope
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  [%uid s+(scot %uv uid.envelope)]
      [%author (ship author.envelope)]
      [%when (time when.envelope)]
      [%letter (lett letter.envelope)]
  ==
--

