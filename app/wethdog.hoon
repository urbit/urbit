::  "wet-dog": ethereum ceremony watchdog
::
/-  json-rpc
::
=,  ethereum
=,  ethe
::
|%
++  state
  $:  latest=@ud
      when=@da
      good=?
      errs=(set @t)
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
      [%rest wire @da]
      [%wait wire @da]
  ==
--
::
|%
++  ceremony   0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964
::
++  parity     'http://104.198.35.227:8545'
::
++  etherscan-key  "RM6CE2MZXGVVUQCVWNM1IWCU57U734MF4N"
::
++  ifttt-all
  ^-  (list tape)
  :~  "your webhook key here"
  ==
--
::
|_  [bol=bowl:gall state]
::
++  prep
  |=  old=(unit state)
  ^-  [(list move) _+>]
  ?~  old  [~ ..prep]
  [~ ..prep(+<+ u.old)]
::
++  poke-noun
  |=  wat=@t
  ^-  [(list move) _+>]
  =-  [-^~ ..poke-noun]
  ?:  =('flow' wat)  ask-flow
  !!
::
++  ask-flow
  ^-  move
  %+  ask-node  /flow
  :+  %eth-get-transaction-count
    ceremony
  label+%latest
::
++  ask-node
  |=  [wir=wire req=request]
  ^-  move
  :-  ost.bol
  :^  %hiss  wir  ~
  :+  %json-rpc-response  %hiss
  ^-  hiss:eyre
  %+  json-request
    (need (de-purl:html parity))
  ~!  req
  (request-to-json `'no crash pls' req)
::
++  sigh-json-rpc-response
  |=  [wir=wire res=response:json-rpc]
  ^-  [(list move) _+>]
  ?>  ?=([%flow ~] wir)
  ~|  [%endpoint-error res]
  ?>  ?=(%result -.res)
  =+  new=(parse-hex-result res.res)
  =+  had=!=(0 latest)
  =?  when  (gth new latest)  now.bol
  =?  latest  (gth new latest)  new
  =^  moz  +>.$
    ?:(had check-flow [~ +>.$])
  [[(wait /flow (add now.bol ~m2)) moz] +>.$]
::
++  check-flow
  ^-  [(list move) _..prep]
  =+  want=(gth now.bol (add when ~m15))
  :_  ..check-flow(good !want)
  ?:  =(good !want)  ~
  %+  ifttt  "flow"
  :-  ?.(good 'Good!' 'Bad!')
  ?.  good  'Transactions are steadily confirming again.'
  %-  crip
  "No new transactions seen since {(scow %ud latest)} at {(scow %da when)} UTC."
::
++  ifttt
  |=  [wat=tape sub=cord bod=cord]
  ^-  (list move)
  %+  turn  ifttt-all
  |=  who=tape
  ^-  move
  :-  ost.bol
  :^  %hiss  /  ~
  :+  %httr  %hiss
  ^-  hiss:eyre
  %+  json-request
    %-  need
    %-  de-purl:html
    %-  crip
    %+  weld
      "https://maker.ifttt.com/trigger/"
    :(weld "wethdog-" wat "/with/key/" who)
  :-  %o
  %-  ~(gas by *(map @t json))
  :~  'value1'^s+sub
      'value2'^s+bod
  ==
::
++  etherscan-success
  ^-  move
  %+  etherscan  /es
  :+  "account"  "txlist"
  %-  ~(gas by *(map tape tape))
  :~  "address"^['0' 'x' ((x-co:co 40) ceremony)]
      "page"^"1"
      "offset"^"200"
      "sort"^"desc"
  ==
::
++  etherscan
  |=  [wir=wire mod=tape act=tape arg=(map tape tape)]
  ^-  move
  :-  ost.bol
  :^  %hiss  wir  ~
  :+  %json  %hiss
  ^-  hiss:eyre
  :_  [%get ~ ~]
  %-  need
  %-  de-purl:html
  %-  crip
  %+  weld
    "http://api.etherscan.io/api?module={mod}&action={act}"
  =.  arg  (~(put by arg) "apikey" etherscan-key)
  ^-  tape
  %-  ~(rep in arg)
  |=  [[nom=tape val=tape] out=tape]
  ^-  tape
  :(weld out '&'^nom '='^val)
::
++  sigh-json
  |=  [wir=wire jon=json]
  ^-  [(list move) _+>]
  ?>  ?=([%es ~] wir)
  ?>  ?=(%o -.jon)
  =+  (~(got by p.jon) 'result')
  ?>  ?=(%a -.-)
  =;  los=(list cord)
    =+  lon=(skip los ~(has in errs))
    ?:  =(~ lon)  [~ +>.$]
    :_  +>.$(errs (~(gas in errs) lon))
    %+  ifttt  "flow"
    :-  'Bad! Transaction errors!'
    %-  crip
    %+  weld  "In transaction(s) "
    %+  roll  (turn lon trip)
    |=  [i=tape o=tape]
    :(weld o " ; " i)
  %+  murn  p
  |=  j=json
  ^-  (unit cord)
  ?>  ?=(%o -.j)
  ?.  (~(has by p.j) 'isError')  ~
  ?:  =(s+'0' (~(got by p.j) 'isError'))  ~
  =+  (~(got by p.j) 'hash')
  ?>(?=(%s -.-) `p)
::
++  sigh
  |=  *
  [~ +>]
::
++  wait
  |=  [wir=wire wen=@da]
  ^-  move
  [ost.bol %wait wir wen]
::
++  wake-flow
  |=  [wir=wire ~]
  ^-  [(list move) _+>]
  [[ask-flow etherscan-success ~] +>]
--
