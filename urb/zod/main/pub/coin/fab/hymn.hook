::
::::
  ::
/?    314
/=    gas    /$    fuel
::
::::  ~sivtyv-barnel
  ::
  !:

^-  manx

=+  :-  authcode=<(fall (~(get by qix.gas) %'code') '')>
    auth-url="https://www.coinbase.com/oauth/authorize?".    :: "concat" . "enate"
    "response_type=code".
    "&client_id=2e688dde3f7655e7c261313a286e69e7c61ec5502459408b7818c4c74c77bf45".
    "&redirect_uri=http://localhost:8444/gen/main/pub/fab/coin".
    "&scope=user+balance+buy+sell+send+transactions".
    "&meta[send_limit_amount]=1&meta[send_limit_curency]=BTC&meta[send_limit_period]=day"
;html
  ;head
    ;title: Coinbase Auth
    ;script@"/gop/hart.js";
    ;script@"/gen/main/lib/urb.js";
    ;script@"https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.js";
  ==
  ;body
     ;a/"{auth-url}": Authenticate
     ;script: authcode = {authcode}
     ;div#changes;
     ;script:'''
             if (authcode)
             urb.send({
                     appl: "coin",
                     data: authcode,
                     mark: "oauth2-code"
                     })
             '''
  ==
==
