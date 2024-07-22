/@  renderer
/@  diary-diff
/-  serv=sky-server
/-  feather-icons
/-  manx-utils
/-  b=blue
^-  kook:neo
=<
|%
++  state  pro/%renderer
++  poke   (sy %http-request %rely %gift ~)
++  kids
  :+  ~  %y
  %-  malt
  :~  :-  [|/%uv |]
      [pro/%renderer (sy %http-request ~)]
  ==
++  deps
  %-  ~(gas by *band:neo)
  :~  :-  %src
      :-  req=&
      :-  [pro/%diary (sy %diary-diff ~)]
      :+  ~  %y
      %-  ~(gas by *lads:neo)
      :~  :-  [|/%da |]
          [[%only %txt] ~]
      ==
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  !!
        %rely
      =/  dep  p:(~(got by deps.bowl) %src) 
      (reset:b [bowl %diary-ui pail dep])
    ::
        %gift
      (render-child:b [!<(gift:neo vax) bowl q.pail])
    ::
    ::  A poke comes in as a POST request. 
    ::  Forward it and wait for %rely.
        %http-request
      =;  poke
        :_  pail
        :~  :+  p:(~(got by deps.bowl) %src) 
              %poke
            [%diary-diff !>(poke)]
        ==
      ^-  diary-diff
      =/  body  (parse-body:serv !<(request:http vax))
      =/  mu  ~(. manx-utils body)
      =/  head  (@tas (got:mu %head))
      ?+    head  !!
          %put-entry
        =/  id  (slav %da (vol:mu "now"))
        =/  text  (vol:mu "text")
        [%put-entry id text]
      ::
          %del-entry
        [%del-entry (slav %da (got:mu %diary-id))]
      ==
    ==
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    (render [bowl vase])
  --
--
::
|%
++  render
  |=  [=bowl:neo =vase]
  ^-  (quip card:neo pail:neo)
  =/  sesh  session:!<(renderer vase)
  =/  mac=[manx (list card:neo)]
    ~(render html [bowl sesh])
  :-  +.mac
  renderer/!>([sesh `-.mac])
::
++  html
  |_  [=bowl:neo sesh=road:neo]
  ++  render
    ^-  [manx (list card:neo)]
    =;  entries=(list [manx card:neo])
      :_  (turn entries |=([* =card:neo] card))
      ;div.p-page
        ;div.ma.fc.g2.mw-page
          ;+  form-put-entry
          ;*  (turn entries |=([=manx *] manx))
        == 
      ==
    %-  turn
    :_  (recur:b [%txt-ui bowl sesh])
    %+  sort
      %~  tap
        of:neo
      %.  /
      %~  del 
        of:neo
      q:(~(got by deps.bowl) %src)
    |=  [a=[=pith *] b=[=pith *]]
    (gth ->.pith.a ->.pith.b)
  ::
  ++  form-put-entry
    ;form.fc.g2
      =style         "margin-bottom: 30px;"
      =hx-post       "/neo/blue{(en-tape:pith:neo sesh)}"
      =hx-on-submit  "this.reset()"
      =hx-target     "find .loading"
      =hx-swap       "outerHTML"
      =head          "put-entry"
      ;date-now(name "now");
      ;textarea.p2.bd1.br1
        =name  "text"
        =placeholder  "today, i ..."
        =oninput  "this.setAttribute('value', this.value)"
        =rows  "5"
        =required  ""
        =autocomplete  "off"
        ;
      ==
      ;button.p2.b1.br1.bd1.wfc.hover.loader
        ;span.loaded.s2: create
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  --
--