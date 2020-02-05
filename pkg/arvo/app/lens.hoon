/-  lens, *sole
/+  base64, *server, default-agent
/=  lens-mark  /:  /===/mar/lens/command
                   /!noun/
=,  format
|%
::  +lens-out: json or named octet-stream
::
+$  lens-out
  $%  [%json =json]
      [%mime =mime]
  ==
+$  state
  $%  $:  %0
          job=(unit [eyre-id=@ta com=command:lens])
      ==
  ==
::
--
::
=|  =state
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old=vase
  `this(state !<(^state old))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall _this)
  ?.  ?=(%handle-http-request mark)
    (on-poke:def mark vase)
  =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
  ?>  ?=(~ job.state)
  ::
  =/  request-line  (parse-request-line url.request.inbound-request)
  =/  site  (flop site.request-line)
  ::
  =/  jon=json
    (need (de-json:html q:(need body.request.inbound-request)))
  =/  com=command:lens
    (json:grab:lens-mark jon)
  ::
  ?:  ?=(%export -.source.com)
    ~&  [%export app.source.com]
    :_  this(job.state (some [eyre-id com]))
    [%pass /export %agent [our.bowl app.source.com] %watch /export]~
  ::
  ?:  ?=(%import -.source.com)
    ?~  enc=(de:base64 base64-jam.source.com)
      !!
    ::
    =/  c=*  (cue q.u.enc)
    ::
    :_  this(job.state (some [eyre-id com]))
    [%pass /import %agent [our.bowl app.source.com] %poke %import !>(c)]~
  ::
  :_  this(job.state (some [eyre-id com]))
  [%pass /sole %agent [our.bowl %dojo] %watch /sole/[eyre-id]]~
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?:  ?=([%http-response *] path)
    `this
  (on-watch:def path)
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card:agent:gall _this)
  |^
  ?+    wire  (on-agent:def wire sign)
      [%import ~]
    ?>  ?=(%poke-ack -.sign)
    ?>  ?=(^ job.state)
    :_  this(job.state ~)
    %+  give-simple-payload:app  eyre-id.u.job.state
    [[200 ~] `(as-octt:mimes:html "\"Imported data\"")]
  ::
      [%export ~]
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign
        `this
      ?>  ?=(^ job.state)
      :_  this(job.state ~)
      (give-simple-payload:app eyre-id.u.job.state not-found:gen)
    ::
        %fact
      =^  cards  this  (take-export !<(* q.cage.sign))
      :_  this(job.state ~)
      :_  cards
      ?>  ?=(^ job.state)
      ?>  ?=(%export -.source.com.u.job.state)
      [%pass /export %agent [our.bowl app.source.com.u.job.state] %leave ~]
    ==
  ::
      [%sole ~]
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?>  ?=(^ job.state)
      ?^  p.sign
        :_  this(job.state ~)
        (give-simple-payload:app eyre-id.u.job.state not-found:gen)
      :_  this  :_  ~
      :*  %pass  /sole
          %agent  [our.bowl %dojo]
          %poke  %lens-command  !>
          [eyre-id.u.job.state com.u.job.state]
      ==
    ::
        %fact
      ?>  ?=(%sole-effect p.cage.sign)
      =^  cards  this  (take-sole-effect !<(sole-effect q.cage.sign))
      [[[%pass /sole %agent [our.bowl %dojo] %leave ~] cards] this]
    ==
  ==
  ::
  ++  take-export
    |=  data=*
    ^-  (quip card:agent:gall _this)
    ?>  ?=(^ job.state)
    ?>  ?=(%export -.source.com.u.job.state)
    =/  app-name=tape  (trip app.source.com.u.job.state)
    =/  output=@t  (crip "/{app-name}/jam")
    ::
    =/  jon=json
      =/  =atom  (jam data)
      =/  =octs  [(met 3 atom) atom]
      =/  enc  (en:base64 octs)
      (pairs:enjs:format file+s+output data+s+enc ~)
    ::
    :_  this
    %+  give-simple-payload:app  eyre-id.u.job.state
    (json-response:gen (json-to-octs jon))
  ::
  ++  take-sole-effect
    |=  fec=sole-effect
    ^-  (quip card:agent:gall _this)
    =/  out
      |-  ^-  (unit lens-out)
      =*  loop  $
      ?+  -.fec
        ~
      ::
          %tan
        %-  some
        :-  %json
        %-  wall:enjs:format
        (turn (flop p.fec) |=(=tank ~(ram re tank)))
      ::
          %txt
        (some %json s+(crip p.fec))
      ::
          %sag
        %-  some
        [%mime p.fec (as-octs:mimes:html (jam q.fec))]
      ::
          %sav
        ::  XX use +en:base64 or produce %mime a la %sag
        ::
        %-  some
        :-  %json
        %-  pairs:enjs:format
        :~  file+s+(crip <`path`p.fec>)
            data+s+(crip (en-base64:mimes:html q.fec))
        ==
      ::
          %mor
        =/  all  `(list lens-out)`(murn p.fec |=(a=sole-effect loop(fec a)))
        ?~  all  ~
        ~|  [%multiple-effects all]
        ?>  ?=(~ t.all)
        (some i.all)
      ==
    ::
    ?~  out
      [~ this]
    ::
    ?>  ?=(^ job.state)
    :_  this(job.state ~)
    %+  give-simple-payload:app  eyre-id.u.job.state
    ?-  -.u.out
        %json
      (json-response:gen (json-to-octs json.u.out))
    ::
        %mime
      =/  headers
        :~  ['content-type' 'application/octet-stream']
            ?>  ?=([@ @ ~] p.mime.u.out)
            :-  'content-disposition'
            ^-  @t
            %^  cat  3
              'attachment; filename='
            (rap 3 '"' i.p.mime.u.out '.' i.t.p.mime.u.out '"' ~)
        ==
      [[200 headers] (some q.mime.u.out)]
    ==
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:gall _this)
  ?.  ?=(%bound +<.sign-arvo)
    (on-arvo:def wire sign-arvo)
  [~ this]
::
++  on-fail   on-fail:def
--
