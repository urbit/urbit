/-  lens, *sole
/+  *server, default-agent, dbug
/=  lens-mark  /mar/lens/command  ::  TODO: ask clay to build a $tube
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
++  export-app
  |=  [app=@tas our=@p now=@da]
  .^(* %gx /(scot %p our)/[app]/(scot %da now)/export/noun)
++  export-all
  |=  [our=@p now=@da]
  ^-  (list [@tas *])
  %+  turn
    ^-  (list @tas)
    :~  %group-store
        %metadata-store
        %invite-store
        %graph-store
    ==
  |=  app=@tas
  [app (export-app app our now)]
--
::
=|  =state
%-  agent:dbug
^-  agent:gall
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
  ::
  ?:  &(?=(%noun mark) ?=(%cancel q.vase))
    ?>  (team:title [our src]:bowl)
    ~&  %lens-cancel
    [~ this(job.state ~)]
  ::
  ?.  ?=(%handle-http-request mark)
    (on-poke:def mark vase)
  =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
  =/  request-line  (parse-request-line url.request.inbound-request)
  =/  site  (flop site.request-line)
  ::
  =/  body=@t  q:(need body.request.inbound-request)
  ?:  =('#import_' (end [3 8] body))
    ~&  %import-all
    =/  by-app  ;;((list [@tas *]) (cue (rsh [3 8] body)))
    :_  this
    %+  weld  (give-simple-payload:app eyre-id not-found:gen)
    %+  turn  by-app
    |=  [app=@tas data=*]
    ^-  card:agent:gall
    [%pass /import-all %agent [our.bowl app] %poke %import !>(data)]
  =/  jon=json
    (need (de:json:html body))
  =/  com=command:lens
    (json:grab:lens-mark jon)
  ::
  ?:  ?=(%cancel -.source.com)
    ~&  %lens-cancel
    :_  this(job.state ~)
    (give-simple-payload:app eyre-id (json-response:gen [%s 'cancelled']))
  ::
  ?>  ?=(~ job.state)
  ::
  ?+  -.source.com
    :_  this(job.state (some [eyre-id com]))
    =/  =path  /sole/(scot %p our.bowl)/[eyre-id]
    [%pass /sole %agent [our.bowl %dojo] %watch path]~
  ::
      %export
    :_  this(job.state (some [eyre-id com]))
    [%pass /export %agent [our.bowl app.source.com] %watch /export]~
  ::
      %import
    ?~  enc=(de:base64:mimes:html base64-jam.source.com)
      !!
    ::
    =/  c=*  (cue q.u.enc)
    ::
    :_  this(job.state (some [eyre-id com]))
    [%pass /import %agent [our.bowl app.source.com] %poke %import !>(c)]~
  ::
      %export-all
    =/  output  (crip "{<our.bowl>}-export/atom")
    =/  jon
      =/  =atom  (jam (export-all our.bowl now.bowl))
      =/  =octs  [(met 3 atom) atom]
      =/  enc    (en:base64:mimes:html octs)
      (pairs:enjs:format file+s+output data+s+enc ~)
    :_  this
    %+  give-simple-payload:app  eyre-id
    (json-response:gen jon)
  ::
      %import-all
    ~&  %import-all
    =/  enc  (de:base64:mimes:html base64-jam.source.com)
    ?~  enc  !!
    =/  by-app  ;;((list [@tas *]) (cue q.u.enc))
    :_  this
    %+  weld  (give-simple-payload:app eyre-id not-found:gen)
    %+  turn  by-app
    |=  [app=@tas data=*]
    ^-  card:agent:gall
    [%pass /import-all %agent [our.bowl app] %poke %import !>(data)]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?:  ?=([%http-response *] path)
    `this
  (on-watch:def path)
::
++  on-leave  on-leave:def
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    [%x %export-all ~]
    ``noun+!>((jam (export-all our.bowl now.bowl)))
  ==
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
      =/  enc  (en:base64:mimes:html octs)
      (pairs:enjs:format file+s+output data+s+enc ~)
    ::
    :_  this
    %+  give-simple-payload:app  eyre-id.u.job.state
    (json-response:gen jon)
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
        %-  some
        :-  %json
        %-  pairs:enjs:format
        :~  file+s+(crip <`path`p.fec>)
            data+s+(en:base64:mimes:html (met 3 q.fec) q.fec)
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
      (json-response:gen json.u.out)
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
