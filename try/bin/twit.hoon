!:  ::  /=try=/bin/http/hoon
!?      164
::::
::
=>  %=    .
        +
      =>  +
      |%
      ::
      ++  sifo
        |=  tig=@
        ^-  tape
        =+  poc=(mod (sub 3 (mod (met 3 tig) 3)) 3)
        =+  pad=(lsh 3 poc (swap 3 tig))
        =+  ^=  ska
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        =+  ^=  sif
            %-  flop
            |-  ^-  tape
            ?~  pad
              ~
            =+  d=(end 0 6 pad)
            [(snag d ska) $(pad (rsh 0 6 pad))]
        (weld (scag (sub (lent sif) poc) sif) (trip (fil 3 poc '=')))
     ::
      ++  cuss                                                ::  upper
        |=  vib=tape
        ^-  @t
        %+  rap  3
        (turn vib |=(a=@ ?.(&((gte a 'a') (lte a 'z')) a (sub a 32))))
      ::
      ++  hmac
        |=  [key=@ mes=@]
        =+  ip=(fil 3 64 0x36)
        =+  op=(fil 3 64 0x5c)
        =+  ^=  kex
            ?:  (gth (met 3 key) 64)
              (lsh 3 44 (shan (swap 3 key)))
            (lsh 3 (sub 64 (met 3 key)) (swap 3 key))
        =+  inn=(shan (swap 3 (cat 3 (swap 3 mes) (mix ip kex))))
        (shan (swap 3 (cat 3 inn (mix op kex))))
      ::
      ++  unt  |=(a=@ (div (sub a ~1970.1.1) (bex 64)))
      ::
      ++  curl
        |=  [met=meth req=[p=path q=tape r=purl] head=math body=(unit octs)] 
        =+  ^=  work
            |=  [now=@da pax=path not=note]
            ^-  bowl
            ?>  ?=([%hp *] not)
            ?~  r.p.not
              !!
            :_  ~
            ^-  (list gift)
            [[%$ !>((trip q.u.r.p.not))] ~]
        =+  gifts=`(list gift)`[[%tq p.req [r.req [met head body]]] ~]
        =+  stuff=[~ `(list slip)`[[p.req [%hp ~]] ~] work]
        ^-  bowl
        [gifts stuff]
      ++  req
        |=  a=tape
        ^-  [p=path q=tape r=purl]
        [`path`[(scot %t (rap 3 a)) ~] a (scan a auri:epur)]
      ::
      ++  app
        |%  ++  cok  'hDDOTPfGHGlsOUbhpy6qc6XbW'
            ++  cos  'olCkea6wm3XG4pnVCHuPIozUF2ggH1sHjnBtuT4Ai6rCOeQGzO'
            ++  aok  '2485712317-R77Lpdu5rAJadRVxTXPpnxvcwS0IfNG7QEzLPty'
            ++  aos  'a41d83XId0P7QQbodkPYv3zxoEL0Cq9EsN2eXZBZAwAWA'
        --
      ::
      ++  oath
        |_  [met=meth lur=tape et=@ en=@]
        ++  non  (turn (rip 2 (shaw et 128 en)) |=(a=@ ~(x ne a)))
        ++  tim  (slag 2 (scow %ui (unt et)))
        ::
        ++  bas  ^-  tape
          ;:  weld  (trip (cuss (trip `@t`met)))  "&"  (urle lur)  "&"
              %-  urle
              ;:  weld 
                "oauth_consumer_key="  (trip cok:app)  "&" 
                "oauth_nonce="  non  "&" 
                "oauth_signature_method=HMAC-SHA1&"
                "oauth_timestamp="  tim  "&" 
                "oauth_token="  (trip aok:app)  "&"
                "oauth_version=1.0"
              ==
          ==
        ++  sky  ^-  @t
          (crip :(weld (urle (trip cos:app)) "&" (urle (trip aos:app))))
        ++  sig  ^-  tape
          (sifo (swap 3 (hmac (swap 3 sky) (crip bas))))
        ++  hed
          %-  crip
          ;:  weld  "OAuth "
              "oauth_consumer_key="  "\""  (trip cok:app)  "\", "
              "oauth_nonce="  "\""  non  "\", "
              "oauth_signature="  "\""  (urle sig)  "\", "
              "oauth_signature_method=\"HMAC-SHA1\", "
              "oauth_timestamp="  "\""  tim  "\", "
              "oauth_token="  "\""  (trip aok:app)  "\", "
              "oauth_version=1.0"
              ==
        --
      --
    ==
|=  [est=time eny=@uw]
|=  ~
^-  bowl
::::
=+  a="https://api.twitter.com/1.1/help/privacy.json"
=+  token=~(hed oath %get a est eny)
=+  ^=  head
    %-  ~(gas by *math)
    [['Authorization' [token ~]] ~]
(curl %get (req a) head ~)
