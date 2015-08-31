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

=+  authcode=(fall (~(get by qix.gas) %'code') '')
;html
  ;head
  ;title: Coinbase Auth
  ;script@"/~/at/home/lib/urb.js";
  ==
  ;body
  ;*
  ?~  authcode
    :_  ~
    ;div: Something went wrong. Please try to auth again.
   ;=
    ;script: authcode = {(pojo %s authcode)}
    ;script:'''
            if (authcode)
            urb.send({
                    appl: "bit",
                    data: authcode,
                    mark: "oauth2-code"
              }, function(){
              })
            '''
    ;div: 'Success. Your auth-token has been sent to your app. You can close this now.'
   ==
  ==
==
