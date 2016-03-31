::  Twitter credentials
::
::::  /hoon/cred/twit/mar
  ::
/-  plan-acct
/+  httr-to-json
|_  {acc/plan-acct raw/json}
++  grab
  |%
  ++  noun  {plan-acct ^json}
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  ++  json
    |=  jon/^json  ^-  {plan-acct ^json}
    =+  usr=(need ((ot 'screen_name'^so ~):jo jon))
    =+  url=(scan "https://twitter.com/FIXME" aurf:epur)
    =.  q.q.p.url  /[usr]             ::  XX friendlier url format  #717
    [[usr (some url)] jon]
  --
++  grow
  |%
  ++  tank  >[+<]<
  --
--
