/-  *docket, hood, treaty
/+  *server, multipart, agentio, default-agent, dbug, verb
|%
+$  card  card:agent:gall
+$  state-0
  $:  ::  local
      charges=(map desk charge)
  ==
::  $cache: impermanent state
+$  cache
  by-base=(map term desk)
::
+$  inflated-state
  [state-0 cache]
::  +lac: toggle verbosity
++  lac  &
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
%+  verb  &
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
  ~&  %ayy
  =+  !<(old=state-0 vase)
  =*  cha  ~(. ch q.byk.bowl)
  |^
  =.  -.state  old
  =.  +.state  inflate-cache
  `this
  ::
  ++  inflate-cache
    ^-  cache
    %-  ~(gas by *(map term desk))
    %+  murn  ~(tap by charges)
    |=  [=desk =charge]
    ?.  ?=(%glob -.href.docket.charge)  ~
    `:_(desk base.href.docket.charge)
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
    ?<  ~|(%bad-install-desk (~(has by charges) desk))
    =.  charges
      (~(put by charges) desk docket.treaty %install ~)
    =*  cha   ~(. ch desk)
    :_  state
    ~[add-fact:cha (install:cha ship desk)]
  ::
  ++  uninstall
    |=  =desk
    ^-  (quip card _state)
    ~|  %no-charge-install
    =/  =charge  (~(got by charges) desk)
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
        [%glob *glob]
      charge
    ==
    ::
      [%x %charges ~]
    :-  ~  :-  ~
    :-  %charge-update
    !>  ^-  charge-update
    :-  %initial
    %-  ~(gas by *(map desk charge))
    %+  turn  ~(tap by charges)
    |=  [=desk =charge]
    [desk (get-light-charge charge)]
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  =^  cards  state
    ?+  wire  ~&(bad-docket-take+wire `state)
      ~  `state
      [%kiln ~]  take-kiln
      [%charge @ *]  (take-charge i.t.wire t.t.wire)
    ==
  [cards this]
  ::
  ++  take-kiln
    ^-  (quip card _state)
    ?+  -.sign   (on-agent:def:cc wire sign)
      %kick  [(~(watch-our pass /kiln) %hood /kiln/vats)^~ state]
    ::
        %fact
      ?.  ?=(%kiln-vats-diff p.cage.sign)  `state
      =+  !<(=diff:hood q.cage.sign)
      ?.  (~(has by charges) desk.diff)  `state
      =*  cha  ~(. ch desk.diff)
      ?+  -.diff  `state
      ::
          %suspend
        =.  charges  (new-chad:cha %suspend ~)
        :_(state ~[add-fact:cha])
      ::
          %revive
        =/  =charge  (~(got by charges) desk.diff)
        ?.  ?=(%glob -.href.docket.charge)
          =.  charges  (new-chad:cha %site ~)
          :_(state ~[add-fact:cha])
        =.  charges  (new-chad:cha %install ~)
        :_(state [add-fact fetch-glob]:cha)
      ==
    ==
  ++  take-charge
    |=  [=desk =^wire]
    ^-  (quip card _state)
    ~|  %took-for-nonexistent-charge
    ?>  (~(has by charges) desk)
    =*  cha  ~(. ch desk)
    ?+  wire  ~|(%bad-charge-wire !!)
    ::
        [%install ~]
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign
        :_(state ~[warp-next:cha]) :: request warp
      =.  charges   (new-chad:cha hung+'Failed install')
      ((slog leaf+"Failed installing %{(trip desk)}" u.p.sign) `state)
    ::
        [%uninstall ~]
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign  `state
      ((slog leaf+"Failed to uninstall %{(trip desk)}" u.p.sign) `state)
    ::
        [%glob ~]
      ?-  -.sign
        %kick   `state
      ::
          ?(%poke-ack %watch-ack)
        ?~  p.sign  `state
        =/  act=tape  ?:(?=(%poke-ack -.sign) "start" "listen")
        =.  charges  (new-chad:cha hung+'glob-failed')
        :-  ~[add-fact:cha]
        ((slog leaf+"docket: couldn't {act} thread; will retry" u.p.sign) state)
      ::
          %fact
        ?+    p.cage.sign  `state
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          =.  charges  (new-chad:cha hung+'glob-failed')
          :-  ~[add-fact:cha]
          ((slog leaf+"docket: thread failed; will retry" leaf+<term> tang) state)
        ::
            %thread-done
          =+  !<(=glob q.cage.sign)
          =/  =charge  (~(got by charges) desk)
          ?>  ?=(%glob -.href.docket.charge)
          =.  charges  (new-chad:cha glob+glob)
          =.  by-base  (~(put by by-base) base.href.docket.charge desk)
          :_(state ~[add-fact:cha])
        ==
      ==
    ==
  --
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  |^  ^-  (quip card _this)
  =^  cards  state
    ?+  wire  (on-arvo:def wire sign)
        [%init ~]
      =*  cha  ~(. ch q.byk.bowl)
      =.  charges  (~(put by charges) q.byk.bowl [docket:cha %install ~])
      [fetch-glob:cha state]
    ::
      [%charge @ *]  (take-charge t.wire)
    ::
        [%eyre ~]
      ?>  ?=([%eyre %bound *] sign)
      ?:  accepted.sign   `state
      ~&  [dap.bowl %failed-to-bind path.binding.sign]
      `state
    ==
  [cards this]
  ::
  ++  take-charge
    |=  [=desk =^wire]
    =*  cha  ~(. ch desk)
    ?+  wire  ~|(%lc-arvo-bad-wire !!)
    ::
        [%warp ~]
      ?>  ?=([?(%clay %behn) %writ *] sign)
      ?.  (~(has by charges) desk)  `state
      ?~  p.sign  ::
        `state
      =*  cage  r.u.p.sign
      ?>  =(%docket p.cage)
      =+  !<(=docket q.cage)
      ?:  ?=(%site -.href.docket)
        :_  state(charges (~(put by charges) desk [docket [%site ~]]))
        ~[add-fact:cha warp-next:cha]
      =.  charges  (~(put by charges) desk [docket %install ~])
      =.  by-base  (~(put by by-base) base.href.docket desk)
      :_  state
      [warp-next:cha add-fact:cha fetch-glob:cha]
    ==
  --
::
++  on-fail  on-fail:def
++  on-leave  on-leave:def
--
|_  =bowl:gall
++  io    ~(. agentio bowl)
++  pass  pass:io
++  def  ~(. (default-agent state %|) bowl)
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
    ;html
      ;head
        ;title:"%docket globulator"
        ;meta(charset "utf-8");
        ;style:'* { font-family: monospace; margin-top: 1em; }'
      ==
      ;body
        ;h2:"%docket globulator"
        ;+  ?.  =(~ msg)
              :-  [%p ~]
              (join `manx`;br; (turn msg |=(m=@t `manx`:/"{(trip m)}")))
            ;p:"ur on ur own kid, glhf"  ::TODO  instructions
        ;form(method "post", enctype "multipart/form-data")
          ::TODO  could be dropdown
          ;input(type "text", name "desk", placeholder "desk");
          ;br;
          ;input
            =type             "file"
            =name             "glob"
            =directory        ""
            =webkitdirectory  ""
            =mozdirectory     "";
          ;br;
          ;button(type "submit"):"Glob!"
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
      =*  cha      ~(. ch desk)
      =/  =charge  (~(got by charges) desk)
      ::
      ::REVIEW  these are important, right?
      =?  err  =(~ glob)
        ['no files for glob' err]
      =?  err  ?=(%glob -.href.docket.charge)
        ['desk does not use glob' err]
      ::
      ?.  =(~ err)
        :_  [~ state]
        [[400 ~] `(upload-page err)]
      :-  [[200 ~] `(upload-page 'successfully globbed' ~)]
      ::
      =.  charges  (new-chad:cha glob+glob)
      =.  by-base
        =-  (~(put by by-base) - desk)
        ?>  ?=(%glob -.href.docket.charge)
        base.href.docket.charge
      [~[add-fact:cha] state]
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
    ::TODO  do we want to ignore .DS_Store and other such files?
    ?.  =('glob' name)  [glob (cat 3 'weird part: ' name) err]
    ?~  file            [glob 'file without filename' err]
    ?~  type            [glob (cat 3 'file without type: ' u.file) err]
    ?^  code            [glob (cat 3 'strange encoding: ' u.code) err]
    =/  filp            (rush u.file fip)
    ?~  filp            [glob (cat 3 'strange filename: ' u.file) err]
    :_  err
    ::  make sure to exclude the top-level dir from the path
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
    =/  data=mime
      (~(gut by glob) suffix (~(got by glob) /index/html))
    =/  mime-type=@t  (rsh 3 (crip <p.data>))
    =;  headers
      [[200 headers] `q.data]
    :~  content-type+mime-type
        max-1-wk:gen
        'service-worker-allowed'^'/'
    ==
  --
::
++  get-light-charge
  |=  =charge
  ?.  ?=(%glob -.chad.charge)  charge
  charge(glob.chad *glob)
::  +ch: Charge engine
++  ch
  |_  =desk
  ++  pass  |=(slug=term ~(. ^pass /charge/[desk]/[slug]))
  ++  add-fact
    =/  =charge  (~(got by charges) desk)
    (fact:io charge-update+!>([%add-charge desk (get-light-charge charge)]) /charges ~)
  ++  del-fact  (fact:io charge-update+!>([%del-charge desk]) /charges ~)
  ++  install
    |=  [=ship remote=^desk]
    (poke-our:(pass %install) %hood kiln-install+!>([desk ship remote]))
  ++  uninstall
    (poke-our:(pass %uninstall) %hood kiln-uninstall+!>(desk))
  ++  warp-next  (warp-our:(pass %warp) desk `[%next %x da+now.bowl /desk/docket])
  ++  warp-sing  (warp-our:(pass %warp) desk `[%sing %x da+now.bowl /desk/docket])
  ++  new-chad  |=(c=chad (~(jab by charges) desk |=(charge +<(chad c))))
  ++  fetch-glob
    =/  =charge  (~(got by charges) desk)
    =/  tid=@t  (cat 3 'docket-' (scot %uv (sham (mix eny.bowl desk))))
    ?>  ?=(%glob -.href.docket.charge)
    ?>  ?=(%http -.glob-location.href.docket.charge)
    =*  url  url.glob-location.href.docket.charge
    =/  =cage  spider-start+!>([~ `tid byk.bowl(r da+now.bowl) %glob !>(`url)])
    :~  (watch-our:(pass %glob) %spider /thread-result/[tid])
        (poke-our:(pass %glob) %spider cage)
    ==
  ++  docket  .^(^docket %cx (scry:io q.byk.bowl /desk/docket))
  --
--

