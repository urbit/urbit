/-  *docket, hood, treaty
/+  *server, agentio, default-agent, multipart, dbug, verb
|%
+$  card  card:agent:gall
+$  app-state
  $:  %2
      ::  local
      charges=(map desk charge)
  ==
::  $cache: impermanent state
+$  cache
  by-base=(map term desk)
::
+$  inflated-state
  [app-state cache]
::  +lac: toggle verbosity
++  lac  &
::
++  ver
  |%
  ++  poke  1
  ++  scry  1
  ++  peer  1
  --
::
--
^-  agent:gall
%-  agent:dbug
%+  verb  |
=|  inflated-state
=*  state  -
=<
|_  =bowl:gall
+*  this  .
    io    ~(. agentio bowl)
    pass  pass:io
    def   ~(. (default-agent this %|) bowl)
    cc    ~(. +> bowl)
    ch    ch:cc
    cg    cg:cc
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  (~(watch-our pass /kiln) %hood /kiln/vats)
      (~(connect pass /eyre) [~ /] %docket)
      (~(wait pass /init) (add 1 now.bowl))
      (~(connect pass /eyre) [~ /apps] %docket)
  ==
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  |^
  =+  !<(old=app-states vase)
  =?  old    ?=(?(~ ^) -.old)  [%1 old]
  =^  cards  old
    ?.  ?=(%1 -.old)  `old
    =/  rein=cage  kiln-rein+!>([%base %.y ~ ~])
    =/  nuke=cage  kiln-uninstall+!>(%hodl)
    :_  old(- %2)
    :~  [%pass /rein %agent [our.bowl %hood] %poke rein]
        [%pass /nuke %agent [our.bowl %hood] %poke nuke]
    ==
  ?>  ?=(%2 -.old)
  =.  -.state  old
  ::  inflate-cache needs to be called after the state is set
  ::
  =.  +.state  inflate-cache
  [cards this]
  ::
  ++  inflate-cache
    ^-  cache
    %-  ~(gas by *(map term desk))
    %+  murn  ~(tap by charges)
    |=  [=desk =charge]
    ?.  ?=(%glob -.href.docket.charge)  ~
    `:_(desk base.href.docket.charge)
  ::
  +$  app-states
    $^  state-0-ket
    $%  state-0-sig
        state-1
        app-state
    ==
  ::
  +$  state-1  [%1 (map desk charge)]
  +$  state-0-sig
    $:  ~
    ==
  ::
  +$  state-0-ket
    $:  (map desk charge)
    ==
  --
::
++  on-save  !>(-.state)
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  =^  cards  state
    ?+  mark  (on-poke:def:cc mark vase)
      %docket-install    (install !<([ship desk] vase))
      %docket-uninstall  (uninstall !<(desk vase))
    ::
        %noun
      =+  ;;([%kick =desk] q.vase)
      :_(state ~(fetch-glob ch desk))
    ::
        %handle-http-request
      =+  !<([id=@ta req=inbound-request:eyre] vase)
      (handle-http-request:cc id req)
    ==
  [cards this]
  ::
  ++  install
    |=  [=ship =desk]
    ^-  (quip card _state)
    =+  .^(=treaty:treaty %gx (scry:io %treaty /treaty/(scot %p ship)/[desk]/noun))
    ?:  (~(has by charges) desk)
      ~|  bad-install-desk/desk
      !!
    =.  charges
      (~(put by charges) desk docket.treaty %install ~)
    =*  cha   ~(. ch desk)
    :_  state
    ~[add-fact:cha (install:cha ship desk)]
  ::
  ++  uninstall
    |=  =desk
    ^-  (quip card _state)
    =/  =charge  ~|(no-charge-installed+desk (~(got by charges) desk))
    =.  charges  (~(del by charges) desk)
    =?  by-base  ?=(%glob -.href.docket.charge)
      (~(del by by-base) base.href.docket)
    =*  cha  ~(. ch desk)
    :_  state
    ~[del-fact:cha uninstall:cha]
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  =^  cards  state
    ?+  path  (on-watch:def path)
        [%http-response *]
      ?>  (team:title [our src]:bowl)
      `state
    ::
        [%charges ~]
      ?>  (team:title [our src]:bowl)
      `state
    ::
        [%glob @ @ ~]
      =*  base  i.t.path
      =/  hash  (slav %uv i.t.t.path)
      =/  desk     ~|(path/path (~(got by by-base) i.t.path))
      =/  =charge  ~|(desk/desk (~(got by charges) desk))
      ?>  ?=(%glob -.chad.charge)
      =/  have  (hash-glob:cc glob.chad.charge)
      ~|  [%glob-unavailable requested=hash have=have]
      ?>  =(hash have)
      :_  state
      (fact-init-kick:io (glob:cg glob.chad.charge))
    ==
  [cards this]
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  [~ ~]
    [%x %ver %poke ~]  ``noun+!>(poke:ver)
    [%x %ver %peer ~]  ``noun+!>(peer:ver)
    [%x %ver %scry ~]  ``noun+!>(scry:ver)
    [%x %our ~]  ``json+!>(s+(scot %p our.bowl))
    ::
      [%x %dbug %state ~]
    =-  ``noun+!>(-)
    %_  state
        charges
      %-  ~(run by charges)
      |=  =charge
      =?  chad.charge  ?=(%glob -.chad.charge)
        :-  %glob
        %-  ~(run by glob.chad.charge)
        |=(=mime mime(q.q 1.337))
      charge
    ==
    ::
      [%x %charges ~]
    :-  ~  :-  ~
    %-  charge-update:cg
    :-  %initial
    %-  ~(gas by *(map desk charge))
    %+  turn  ~(tap by charges)
    |=  [=desk =charge]
    [desk (get-light-charge charge)]
    ::
      [%x %charges @ %version ~]
    ?~  charge=(~(get by charges) i.t.t.path)
      [~ ~]
    ``noun+!>(version.docket.u.charge)
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  =^  cards  state
    ?+  wire  ~&(bad-docket-take+wire `state)
      ~  `state
      [%rein ~]      ~&(%reined `state)
      [%nuke ~]      ~&(%nuked `state)
      [%kiln ~]      take-kiln
      [%charge @ *]  (take-charge i.t.wire t.t.wire)
    ==
  [cards this]
  ::
  ++  take-kiln
    ^-  (quip card _state)
    ?+    -.sign  (on-agent:def:cc wire sign)
        %kick  [(~(watch-our pass /kiln) %hood /kiln/vats)^~ state]
        %fact
      |^  ^-  (quip card _state)
      ?+  p.cage.sign  ~|(take-kiln-mark/p.cage.sign !!)
        %kiln-vats-snap-0  (on-snap !<(snap:hood q.cage.sign))
        %kiln-vats-diff-0  (on-diff !<(diff:hood q.cage.sign))
      ==
      ::
      ++  on-snap
        |=  =snap:hood
        ^-  (quip card _state)
        =|  fex=(list card)
        =/  ark  ~(tap by snap)
        |-  ^-  (quip card _state)
        ?~  ark  [(flop fex) state]
        =^  caz  state  (on-commit i.ark)
        $(ark t.ark, fex (weld (flop caz) fex))
      ::
      ++  on-diff
        |=  =diff:hood
        =+  !<(=diff:hood q.cage.sign)
        ?-    -.diff
            %commit   (on-commit [desk arak]:diff)
            %suspend  (on-suspend [desk arak]:diff)
            %revive   (on-revive [desk arak]:diff)
            ?(%block %reset %merge-sunk %merge-fail)
          `state
        ==
      ::
      ++  on-commit
        |=  [=desk =arak:hood]
        ^-  (quip card _state)
        =*  cha  ~(. ch desk)
        ?.  docket-exists:cha
          ~?  ?&  !=(%base desk)
                  !=(%kids desk)
              ==
            [dap.bowl %no-docket-file-for desk]
          `state
        ::  always update the docket in state to match clay's
        ::
        =/  =docket            docket:cha
        =/  pre=(unit charge)  (~(get by charges) desk)
        =.  charges            (new-docket:cha docket)
        ::  if the new chad is a site, we're instantly done
        ::
        ?:  ?=(%site -.href.docket)
          =.  charges  (new-chad:cha %site ~)
          :-  ~[add-fact:cha]
          state
        ::
        =.  by-base  (~(put by by-base) base.href.docket desk)
        ::  if the glob specification is unchanged, keep it
        ::
        ?:  &(?=(^ pre) =(href.docket.u.pre href.docket) ?=(%glob -.chad.u.pre))
          [~[add-fact:cha] state]
        ::  if the glob spec changed, but we already host it, keep it
        ::  (this is the "just locally uploaded" case)
        ::
        ?:  ?&  ?=(^ pre)
                ?=(%glob -.chad.u.pre)
              ::
                .=  [(sham glob.chad.u.pre) %ames our.bowl]
                glob-reference.href.docket
            ==
          [~[add-fact:cha] state]
        ::  if the glob changed, forget the old and fetch the new
        ::
        =.  charges  (new-chad:cha %install ~)
        [[add-fact:cha fetch-glob:cha] state]
      ::
      ++  on-suspend
        |=  [=desk =arak:hood]
        ^-  (quip card _state)
        =*  cha  ~(. ch desk)
        ?.  (~(has by charges) desk)  `state
        =/  glob=(unit glob)
          =/  =chad
            chad:(~(got by charges) desk)
          ?:(?=(%glob -.chad) `glob.chad ~)
        =.  charges  (new-chad:cha %suspend glob)
        :_(state ~[add-fact:cha])
      ::
      ++  on-revive
        |=  [=desk =arak:hood]
        ^-  (quip card _state)
        =*  cha  ~(. ch desk)
        ?.  (~(has by charges) desk)  `state
        =/  =charge  (~(got by charges) desk)
        ?.  ?=(%glob -.href.docket.charge)
          =.  charges  (new-chad:cha %site ~)
          :_(state ~[add-fact:cha])
        =.  charges
          %-  new-chad:cha
          ?.  ?=([%suspend ~ *] chad.charge)
            [%install ~]
          [%glob u.glob.chad.charge]
        :_(state [add-fact fetch-glob]:cha)
      --
    ==
  ++  take-charge
    |=  [=desk =^wire]
    ^-  (quip card _state)
    ~|  [%took-for-nonexistent-charge desk]
    ?>  |((~(has by charges) desk) ?=([%uninstall ~] wire))
    =*  cha  ~(. ch desk)
    ?+  wire  ~|(%bad-charge-wire !!)
    ::
        [%install ~]
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign
        `state
      =.  charges   (new-chad:cha hung+'Failed install')
      ((slog leaf+"Failed installing %{(trip desk)}" u.p.sign) `state)
    ::
        [%uninstall ~]
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign  `state
      ((slog leaf+"Failed to uninstall %{(trip desk)}" u.p.sign) `state)
    ::
        [%glob @ ?(%http %ames) @ ~]
      ?-  -.sign
        %kick   `state
      ::
          ?(%poke-ack %watch-ack)
        ?~  p.sign  `state
        =/  act=tape  ?:(?=(%poke-ack -.sign) "start" "listen")
        =.  charges
          %-  new-chad:cha
          ?:  ?=(%http i.t.t.wire)
            hung+'failed to fetch glob via http'
          hung+'failed to fetch glob via ames'
        ((slog leaf+"docket: couldn't {act} thread; will retry" u.p.sign) `state)
      ::
          %fact
        ?+    p.cage.sign  `state
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          ?.  |(=(term %cancelled) =(term %http-request-cancelled))
            =.  charges  (new-chad:cha hung+'glob-failed')
            :-  ~[add-fact:cha]
            ((slog leaf+"docket: thread failed;" leaf+<term> tang) state)
          %-  (slog leaf+"docket: thread cancelled; retrying" leaf+<term> tang)
          =.  charges  (new-chad:cha %install ~)
          :_   state
          [add-fact:cha fetch-glob:cha]
        ::
            %thread-done
          =+  !<(=glob q.cage.sign)
          =/  =charge   (~(got by charges) desk)
          ?.  ?=(%glob -.href.docket.charge)
            `state
          =*  want=@uv  hash.glob-reference.href.docket.charge
          =/  plea=@uv  (slav %uv i.t.wire)
          ?.  =(want plea)
            ::  we requested this at some point but no longer want it
            ::
            `state
          =/  have=@uv  (hash-glob glob)
          ?.  =(want have)
            =.  charges   (new-chad:cha hung+'glob hash mismatch')
            %.  `state
            =/  url=@t  (fall (slaw %t i.t.t.t.wire) '???')
            %-  slog
            :~  leaf+"docket: glob hash mismatch on {<desk>} from {(trip url)}"
                leaf+"expected: {<want>}"
                leaf+"received: {<have>}"
            ==
          =.  charges   (new-chad:cha glob+glob)
          =.  by-base   (~(put by by-base) base.href.docket.charge desk)
          :_(state ~[add-fact:cha])
        ==
      ==
    ==
  --
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  =^  cards  state
    ?+  wire  (on-arvo:def wire sign)
        [%init ~]
      =*  cha  ~(. ch q.byk.bowl)
      =.  charges  (~(put by charges) q.byk.bowl [docket:cha %install ~])
      [fetch-glob:cha state]
    ::
        [%eyre ~]
      ?>  ?=([%eyre %bound *] sign)
      ?:  accepted.sign   `state
      ~&  [dap.bowl %failed-to-bind path.binding.sign]
      `state
    ==
  [cards this]
::
++  on-fail  on-fail:def
++  on-leave  on-leave:def
--
::
|_  =bowl:gall
++  io    ~(. agentio bowl)
++  pass  pass:io
++  def   ~(. (default-agent state %|) bowl)
::
++  hash-glob  sham
++  cg
  |%
  ++  glob            |=(g=^glob glob-0+!>(g))
  ++  docket          |=(d=^docket docket-0+!>(d))
  ++  charge-update   |=(u=^charge-update charge-update+!>(u))
  ++  kiln-uninstall  |=(=desk kiln-uninstall+!>(desk))
  ++  kiln-install
    |=([here=desk her=ship there=desk] kiln-install+!>([here her there]))
  --
::
++  handle-http-request
  |=  [eyre-id=@ta inbound-request:eyre]
  ^-  (quip card _state)
  ::
  =;  [payload=simple-payload:http caz=(list card) =_state]
    :_  state
    %+  weld  caz
    (give-simple-payload:app eyre-id payload)
  ::
  ::NOTE  we don't use +require-authorization-simple here because we want
  ::      to short-circuit all the below logic for the unauthenticated case.
  ?.  authenticated
    :_  [~ state]
    =-  [[307 ['location' -]~] ~]
    (cat 3 '/~/login?redirect=' url.request)
  ::
  =*  headers   header-list.request
  =/  req-line  (parse-request-line url.request)
  ::
  |^  ?+  method.request  [[405^~ ~] ~ state]
        %'GET'   [handle-get-request ~ state]
        %'POST'  handle-upload
      ==
  ::
  ++  handle-get-request
    ^-  simple-payload:http
    ?+  [site ext]:req-line  (redirect:gen '/apps/grid/')
        [[%session ~] [~ %js]]
      %-  inline-js-response
      (rap 3 'window.ship = "' (rsh 3 (scot %p our.bowl)) '";' ~)
    ::
        [[%docket %upload ~] ?(~ [~ %html])]
      [[200 ~] `(upload-page ~)]
    ::
        [[%apps @ *] *]
      %+  payload-from-glob
        (snag 1 site.req-line)
      req-line(site (slag 2 site.req-line))
    ==
  ::
  ++  upload-page
    |=  msg=(list @t)
    ^-  octs
    %-  as-octt:mimes:html
    %-  en-xml:html
    ^-  manx
    ::  desks: with local globs, eligible for upload
    ::
    =/  desks=(list desk)
      %+  murn  ~(tap by charges)
      |=  [d=desk [docket *]]
      ^-  (unit desk)
      ?:(?=(%glob -.href) `d ~)
    ::
    ;html
      ;head
        ;title:"%docket globulator"
        ;meta(charset "utf-8");
        ;style:'''
               * { font-family: monospace; margin-top: 1em; }
               li { margin-top: 0.5em; }
               '''
      ==
      ;body
        ;h2:"%docket globulator"
        ;+  ?.  =(~ msg)
              :-  [%p ~]
              (join `manx`;br; (turn msg |=(m=@t `manx`:/"{(trip m)}")))
            ;ol(start "0")
              ;li:"""
                  make sure the desk you want to upload a glob for has a
                  desk.docket with %base and %glob- entries.
                  """
              ;li:"select the desk you want to upload the glob for."
              ;li:"""
                  select a directory containing the glob contents.
                  usually contains at least an /index.html.
                  """
              ;li:"glob!"
            ==
            (safari and internet explorer do not support uploading directory
            trees properly. please glob from other browsers.)
        ;+  ?:  =(~ desks)
              ;p:"no desks eligible for glob upload"
            ;form(method "post", enctype "multipart/form-data")
              ;label
                ;+  :/"desk: "
                ;select(name "desk")
                  ;*  %+  turn  desks
                      |=(d=desk =+((trip d) ;option(value -):"{-}"))
                ==
              ==
              ;br;
              ;label
                ;+  :/"data: "
                ;input
                  =type             "file"
                  =name             "glob"
                  =directory        ""
                  =webkitdirectory  ""
                  =mozdirectory     "";
              ==
              ;br;
              ;button(type "submit"):"glob!"
            ==
      ==
    ==
  ::
  ++  handle-upload
    ^-  [simple-payload:http (list card) _state]
    ?.  ?=([[%docket %upload ~] ?(~ [~ %html])] [site ext]:req-line)
      [[404^~ ~] [~ state]]
    ::
    =;  [desk=@ta =glob err=(list @t)]
      =*  error-result
        :_  [~ state]
        [[400 ~] `(upload-page err)]
      ::
      ?.  =(~ err)  error-result
      ::
      =*  cha      ~(. ch desk)
      =/  =charge  (~(got by charges) desk)
      ::
      =?  err  =(~ glob)
        ['no files in glob' err]
      =?  err  !?=(%glob -.href.docket.charge)
        ['desk does not use glob' err]
      ::
      ?.  =(~ err)  error-result
      :-  [[200 ~] `(upload-page 'successfully globbed' ~)]
      ?>  ?=(%glob -.href.docket.charge)
      ::
      =.  charges  (new-chad:cha glob+glob)
      =.  by-base
        =-  (~(put by by-base) - desk)
        base.href.docket.charge
      ::
      :_  state
      ::
      =/  ours=?
        =/  loc  location.glob-reference.href.docket.charge
        ?&  ?=(%ames -.loc)
            =(our.bowl ship.loc)
        ==
      ::
      :*  add-fact:cha
        ::
          ?.  ours  ~
          ^-  (list card)
          =-  [%pass /write/[desk] %arvo %c %info -]~
          %+  foal:space:userlib
            /(scot %p our.bowl)/[desk]/(scot %da now.bowl)/desk/docket-0
          %-  docket:cg
          docket.charge(glob-reference.href [(hash-glob glob) %ames our.bowl])
      ==
    ::
    ?~  parts=(de-request:multipart [header-list body]:request)
      ~&  headers=header-list.request
      [*@ta *glob 'failed to parse submitted data' ~]
    ::
    %+  roll  u.parts
    |=  [[name=@t part:multipart] desk=@ta =glob err=(list @t)]
    ^+  [desk glob err]
    ?:  =('desk' name)
      ::  must be a desk with existing charge
      ::
      ?.  ((sane %ta) body)
        [desk glob (cat 3 'invalid desk: ' body) err]
      ?.  (~(has by charges) body)
        [desk glob (cat 3 'unknown desk: ' body) err]
      [body glob err]
    :-  desk
    ::  all submitted files must be complete
    ::
    ?.  =('glob' name)  [glob (cat 3 'weird part: ' name) err]
    ?~  file            [glob 'file without filename' err]
    ?~  type            [glob (cat 3 'file without type: ' u.file) err]
    ?^  code            [glob (cat 3 'strange encoding: ' u.code) err]
    =/  filp            (rush u.file fip)
    ?~  filp            [glob (cat 3 'strange filename: ' u.file) err]
    ::  ignore metadata files and other "junk"
    ::TODO  consider expanding coverage
    ::
    ?:  =('.DS_Store' (rear `path`u.filp))
      [glob err]
    ::  make sure to exclude the top-level dir from the path
    ::
    :_  err
    %+  ~(put by glob)  (slag 1 `path`u.filp)
    [u.type (as-octs:mimes:html body)]
  ::
  ++  fip
    =,  de-purl:html
    %+  cook
      |=(pork (weld q (drop p)))
    (cook deft (more fas smeg))
  ::
  ++  inline-js-response
    |=  js=cord
    ^-  simple-payload:http
    %.  (as-octs:mimes:html js)
    %*  .  js-response:gen
      cache  %.n
    ==
  ::
  ++  payload-from-glob
    |=  [from=@ta what=request-line]
    ^-  simple-payload:http
    =/  des=(unit desk)
      (~(get by by-base) from)
    ?~  des  not-found:gen
    =/  cha=(unit charge)
      (~(get by charges) u.des)
    ?~  cha  not-found:gen
    ?.  ?=(%glob -.chad.u.cha)  not-found:gen
    =*  glob  glob.chad.u.cha
    =/  suffix=^path
      (weld site.what (drop ext.what))
    ?:  =(suffix /desk/js)
      %-  inline-js-response
      (rap 3 'window.desk = "' u.des '";' ~)
    =/  requested
      ?:  (~(has by glob) suffix)  suffix
      /index/html
    =/  data=mime
      (~(got by glob) requested)
    =/  mime-type=@t  (rsh 3 (crip <p.data>))
    =;  headers
      [[200 headers] `q.data]
    :-  content-type+mime-type
    ?:  =(/index/html requested)  ~
    ~[max-1-wk:gen]
  --
::
++  get-light-charge
  |=  =charge
  ?.  ?=(%glob -.chad.charge)  charge
  charge(glob.chad *glob)
::  +ch: Charge engine
++  ch
  |_  =desk
  ++  pass  |=(=wire ~(. ^pass [%charge desk wire]))
  ++  glob-wire
    |=  glob-reference
    ^-  wire
    :+  %glob
      (scot %uv hash)
    ?-  -.location
      %http  /http/(scot %t url.location)
      %ames  /ames/(scot %p ship.location)
    ==
  ++  add-fact
    =/  =charge  (~(got by charges) desk)
    =-  (fact:io - /charges ~)
    (charge-update:cg %add-charge desk (get-light-charge charge))
  ++  del-fact  (fact:io (charge-update:cg %del-charge desk) /charges ~)
  ++  install
    |=  [=ship remote=^desk]
    (poke-our:(pass /install) %hood (kiln-install:cg desk ship remote))
  ++  uninstall
    (poke-our:(pass /uninstall) %hood (kiln-uninstall:cg desk))
  ++  new-docket
    |=  d=^docket
    %+  ~(put by charges)  desk
    [d chad:(~(gut by charges) desk *charge)]
  ++  new-chad  |=(c=chad (~(jab by charges) desk |=(charge +<(chad c))))
  ++  fetch-glob
    ^-  (list card)
    =/  =charge
      ~|  desk/desk
      (~(got by charges) desk)
    =/  tid=@t  (cat 3 'docket-' (scot %uv (sham (mix eny.bowl desk))))
    ?>  ?=(%glob -.href.docket.charge)
    =/  ref  glob-reference.href.docket.charge
    ?:  ?&  ?=(%ames -.location.ref)
            =(our.bowl ship.location.ref)
        ==
      ~>  %slog.0^leaf/"docket: awaiting manual glob for {<desk>} desk"
      ~
    ~>  %slog.0^leaf/"docket: fetching {<-.location.ref>} glob for {<desk>} desk"
    =/  =cage
      :-  %spider-start
      !>([~ `tid byk.bowl(r da+now.bowl) %glob !>(`[ref desk])])
    :~  (leave-our:(pass (glob-wire ref)) %spider)
        (watch-our:(pass (glob-wire ref)) %spider /thread-result/[tid])
        (poke-our:(pass (glob-wire ref)) %spider cage)
    ==
  ++  docket-loc  `path`/desk/docket-0
  ++  docket-exists
    ?:  =(0 ud:.^(cass:clay %cw (scry:io desk ~)))  %.n
    .^(? %cu (scry:io desk docket-loc))
  ::
  ++  docket  .^(^docket %cx (scry:io desk docket-loc))
  --
--

