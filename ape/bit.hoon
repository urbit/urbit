::
::
::::
  :: 
/?  314
/-  talk, bit-api
/+  talk, sole, http
!:
::::  sivtyv-barnel
  ::  be sure to have oauth2-code markk
[talk bit-api .]
|%
++  axle
$:  cred/(unit {app-secret/@t client-id/@t})
    oauth-code/(unit @t)
    auth/(unit {access/@t refresh/@t})
    contacts/(map @t @t)
    outgoing/(map ship @t)  incoming/(map @t ship)
==
++  move  {bone note}
++  hapt  {ship path}
++  note
  $%  {$send wire {ship term} $poke mesa}
      {$them wire (unit hiss)}
      {$hiss wire mark bit-any}
  ==
++  gift
  $%  {$nice $~}
      {$mean ares}
  ==
++  sign
  $%  {$thou p/httr}
      {$unto $coup p/(unit tang)}
  ==
++  mesa
  $%  {$bit-addr-request $~}
      {$bit-addr-made @t}
      {$talk-command command}
  ==
--
::
!:
|_  {hid/bowl vat/axle}
:: ++  prep  ~&(%bit-prep-wipe ,_`.)
++  return  [*(list move) .]
++  redirect-uri  [hostname `/~~/home#pub#bit#fab ~]
++  hostname  ^-  hart
  ?+  (clan our.hid)  !!
     $czar  [| ?+(our.hid `8.443 $~fyr `8.444) `/localhost]
     $duke  [| `8.444 `/org#urbit#(crip +:<our.hid>)]
  ==
++  auth-url
  %+  weld  "https://sandbox.coinbase.com#oauth#authorize"
  %-  tail:earn  :~
    'response_type'^%code
    'client_id'^client-id:(need cred.vat)
    'redirect_uri'^(crip (earn redirect-uri))
    'scope'^'user balance buy sell send transactions addresses'
    'meta[send_limit_amount]'^'1'
    'meta[send_limit_curency]'^'BTC'
    'meta[send_limit_period]'^'day'
  ==
++  old-api
  |=  {pour-path/wire end-point/path req/$@($get {$post p/json})}
  ^-  move
  %^  send:http  ost.hid  pour-path 
  [/com#coinbase#api v1/end-point req ~ ['access_token' access:(need auth.vat)]~]
::
++  api-call
  |=  {res/mark req/bit-api-call}  ^+  return
  ~|  %missing-access-token
  [[ost.hid %hiss / res bit-api-call#[access:(need auth.vat) req]]~ +>.$]
::
++  print  |=(msg/tape [[(publish [%lin & (crip msg)]~)]~ +>.$])
++  callback-uri  [hostname [`%json /~/to#[dap.hid]/json] ~[anon#'' wire#'/']]
++  poke-auth
  |=  arg/{secr/cord id/cord}
  =.  cred.vat  `arg
  =+  prl=`purl`(need (epur (crip auth-url)))
  :_  +>.$  
  :-  %-  publish
      :~  [%lin & 'secret and client id saved successfully']
          [%lin & 'please click on the url below to continue authentication']
          [%url prl ~] 
      ==
  ~
::
++  sigh-tang  |=({* err/tang} (mean err))
++  send-friend                      ::  inter-app interface
  |=  {way/wire his/ship mez/$%({$bit-addr-request $~} {$bit-addr-made @t})}
  ^-  move
  [ost.hid %send way [his dap.hid] %poke mez]
::
++  poke-bit-send  |=(req/bit-send (api-call %bit-transaction send#req))
++  sigh-bit-transaction
  |=({* trid/@t} (print "transaction success {(trip trid)}"))
::
++  poke-bit-ship-send                        ::  send to ship
  |=  {to/ship amount/@t}
  =.  outgoing.vat  (~(put by outgoing.vat) to amount)
  :_  +>.$
  :_  ~
  (send-friend /sent to %bit-addr-request ~)
::
++  poke-bit-addr-request             ::  give your address to receive bit
  |=  $~                              ::  gen new address with callback
  :_  +>.$
  :_  ~
  %+  old-api  /addr-request#(scot %p src.hid) 
  :+  /addresses  %post
  %+  joba  %address
  %-  jobe  :~
    label#(jape "address for {<src.hid>}")
    'callback_url'^(jape (earn callback-uri))
  ==
  ::
++  pour-addr-request             ::  send new address to sender
  |=  {pax/path rez/json}
  =+  adr=(need ((ot address#so ~):jo rez))
  =+  his=(slav %p +<.pax)
  ~!  incoming.vat
  =.  incoming.vat
    (~(put by incoming.vat) adr his) 
  :_  +>.$
  :_  ~
  (send-friend [%message ~] his %bit-addr-made adr)
::
++  poke-bit-addr-made             ::  receive address for sending
  |=  addr/@t
  =+  amount=(~(got by outgoing.vat) src.hid)             ::  get amount to send
  (api-call %bit-transaction txt-send#[addr amount])
::
++  poke-oauth2-code
  |=  code/cord
  =.  oauth-code.vat
    [~ code]
  ?<  ?=($~ cred.vat)  
  =+  req=[code [client-id app-secret]:u.cred.vat redirect-uri]
  [[ost.hid %hiss / %oauth-tokens bit-get-token#req]~ +>.$]
::
++  sigh-oauth-tokens
  |=  {wire toke/{access/@t refresh/@t}}
  =.(auth.vat `toke (print "authenticated."))
::
++  poke-buy
  |=(arg/{amount/@t currency/@t} ?<(=(amount.arg ~) (api-call %json %buy arg)))
::
++  poke-sell
  |=(arg/{amount/@t currency/@t} ?<(=(amount.arg ~) (api-call %json %sell arg)))
++  sigh-json  |=({* a/json} ~&(a return))
++  poke-list  |=($~ (api-call %bit-accounts %list ~))
++  sigh-bit-accounts  |=({* acc/bit-accounts} ~&(accounts=acc return))
++  poke-send-raw
  |=  arg/{to/@t amone/(unit @t) cur/(unit @t) amtwo/(unit @t) nt/(unit @t)}
  =+  ^=  adr
      ?~  (~(get by contacts.vat) to.arg)  to 
      ~&('contact-exists' (~(get by contacts.vat) to.arg))
  =+  ^=  info
      ?~  cur.arg  
        [to#s#to.arg amount#s#(need amone.arg) ~]
      ?~  nt.arg
        :*  to#s#to.arg 
            'amount_currency_iso'^s#(need cur.arg) 
            'amount_string'^s#(need amtwo.arg) 
            ~
        ==
      :*  to#s#to.arg 
          amount#s#(need amtwo.arg) 
          'amount_string'^s#(need amtwo.arg)
          ~
      ==
  =+  ^=  pst
      (jobe transaction#(jobe info) ~)
  [[(old-api /send /transactions#'send_money' %post pst) ~] +>.$]
::
++  poke-json
  |=  arg/json
  ~&  arg
  =+  [adr amo]=(need %.(arg (ot address#so amount#no ~):jo))
  =+  frm=(~(get by incoming.vat) adr)
  (print "just received {(trip amo)} BTC from {<frm>}")
  ::(old-api /get-id /transactions#[transaction-id] %get)
::
++  pour-get-id             ::  display transaction info
  |=  {pax/path rez/json}
  :_  +>.$
  :_  ~
  !! 
::
++  thou
  |=  {way/wire res/httr}
  ?+  -.way  !!
    $message  `+>.$
    ?($addr-request $get-id)
      ~|  'must receive a 200'
      ~|  res
      ?>  =(200 p.res)
      %-  ?-(-.way $addr-request pour-addr-request, $get-id pour-get-id)
      [way (rash q:(need r.res) apex:poja)]
  ==
::
++  publish
  |=  act/(list speech)
  ^-  move
  =+  ^=  thotz
      %+  turn  act
      |=  sp/speech  ^-  thought
      =+  ^=  tail
      :-  ^-  audience
          :+  :-  ^-  partner
                  [%& our.hid ?+((clan our.hid) !! $czar %court, $duke %porch)]
              ^-  (pair envelope delivery)
              [`envelope`[& ~] %pending]
            ~
          ~
      `statement`[now.hid ~ sp]
      :-  `@`(sham eny.hid tail)
      tail
  [ost.hid %send /auth [our.hid %talk] %poke [%talk-command %publish thotz]]
--
