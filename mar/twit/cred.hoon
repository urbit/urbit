::  Twitter credentials
::
::::  /hoon/cred/twit/mar
  ::
/-  plan-acct
/+  httr-to-json, twitter
|_  {acc/plan-acct raw/json}
++  grab
  |%
  ++  noun  {plan-acct ^json}
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  ++  json
    |=  jon/^json  ^-  {plan-acct ^json}
    =+  usr=(need ((ot 'screen_name'^so ~):dejs-soft:format jon))
    =+  url=(user-url:render:twitter usr)
    [[usr (some url)] jon]
  --
++  grow
  |%
  ++  tank  >[+<.+]<
  --
--
