/@  order
/-  serv=sky-server
/*  s-k-y
/*  wi-nd
/*  hawk-icon
^-  kook:neo
=<
|%
++  state  pro/%eyre-task
++  poke   (sy %rely ~)
++  kids  *kids:neo
++  deps
  %-  ~(gas by *band:neo)
  :~  :-  %src
      :-  req=&
      :-  [pro/%sig ~]
      :+  ~  %y
      schema.serv
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  ~|(bad-stud/stud !!)
        %rely
      :_  pail
      =/  task  !<(task:eyre:neo q.pail)
      (eyre-cards [bowl task])
    ==
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<(=task:eyre:neo vase)
    =/  [eyre-id=@ta req=inbound-request:eyre]  task
    ?+    method.request.req  ~|(%unsupported-http-method !!)
        %'GET'
      :_  [stud vase]
      (eyre-cards [bowl task])
    ::
    ==
  --
--
::
|%
++  eyre-cards
  |=  [=bowl:neo [eyre-id=@ta req=inbound-request:eyre]]
  =+  #/[p/our.bowl]/$/eyre
  :~  (head-card - eyre-id)
  ::
      :*  -
          %poke
          %eyre-sign
          !>
          :+  eyre-id
            %data
          :-
            ~
          %-  manx-to-octs
          %~  render
            web
          :-  bowl
          (pave:neo pax:(parse-url-frfr:serv request.req))
      ==
  ::
      (done-card - eyre-id)
      [here.bowl %cull ~]
      [here.bowl %tomb ~]
  ==
::
++  head-card
  |=  [=pith eyre-id=@ta]
  :*  pith
      %poke
      %eyre-sign
      !>
      :^    eyre-id
          %head
        200
      :~
        ['HX-Replace-Url' '/sky']
        ['content-type' 'text/html']
      ==
  ==
::
++  done-card
  |=  [=pith eyre-id=@ta]
  [pith %poke eyre-sign/!>([eyre-id %done ~])]
::
++  manx-to-octs
  |=  man=manx
  (as-octt:mimes:html (en-xml:html man))
::
++  web
  |_  [=bowl:neo name=pith]
  ++  render
    ^-  manx
    %-  lift
    ;s-k-y
      =our  (scow %p our.bowl)
      =default-strategies  default-strategies-json
      ;
    ==
  ++  lor
    ^-  lore:neo
    q:(~(got by deps.bowl) %src)
  ++  fot
    ~(. of:neo lor)
  ++  dap
    |=  =pith:neo
    ~(tap of:neo (dip:fot pith))
  ++  strategy-map
    ^-  (list (pair pith order))
    %+  turn  (dap /strategy)
    |=  [=pith =idea:neo]
    ^-  (pair pith:neo order)
    [pith !<(order q.pail.idea)]
  ++  default-strategies-json
    ::  the default strategies
    ::  as a json tape
    ^-  tape
    %-  trip
    %-  en:json:html
    ^-  json
    :-  %o
    %-  malt
    %+  turn  strategy-map
    |=  [key=pith val=order]
    ^-  [@t json]
    :-  (crip (en-tape:pith:neo key))
    :-  %a
    ^-  (list json)
    %+  turn  val
    |=  v=pith
    ^-  json
    [%s (crip (en-tape:pith:neo v))]
    ::
  ++  theme-pairs
    ^-  (list (pair @t @t))
    %+  turn  (dap /theme)
    |=  [=pith =idea:neo]
    ^-  (pair @t @t)
    =/  =iota:neo  (snag 0 pith)
    [?@(iota iota (scot iota)) !<(@t q.pail.idea)]
  ++  theme-tape
    ^-  tape
    %-  zing
    %+  turn  theme-pairs
    |=  [key=@t val=@t]
    """
    --{(trip key)}: {(trip val)};
    """
  ++  icon-url
    ^~
    %-  trip
    %^    cat
        3
      'data:image/png;base64,'
    %-  ~(en base64:mimes:html & |)
    (as-octs:mimes:html hawk-icon)
  ++  favicon
    ^~
    =;  m  m(a.g [[%href icon-url] a.g.m])
    ^-  manx
    ;link
      =rel  "icon"
      =type  "image/png"
      ;
    ==
  ++  manifest-url
    ^~
    %-  trip
    %^    cat
        3
      'data:application/json;utf-8,'
    %-  en:json:html
    %-  pairs:enjs:format
    :~
      ['name' s+'sky']
      ['description' s+'an urbit namespace viewer']
      ['start_url' s+'http://localhost/neo/sky']  ::  XX
      ['display' s+'standalone']
      ['background_color' s+'black']
      ['theme_color' s+'black']
      :+  'icons'  %a
      :~
        %-  pairs:enjs:format
        :~
          ['src' s+(crip icon-url)]
          ['sizes' s+'196x196']
          ['type' s+'image/png']
        ==
      ==
    ==
  ++  manifest
    ^~
    =;  m  m(a.g [[%href manifest-url] a.g.m])
    ^-  manx
    ;link
      =rel  "manifest"
      ;
    ==
  ::
  ++  lift
    |=  in=manx
    ^-  manx
    ;html
      =style  theme-tape
      ;head
        ;meta(charset "UTF-8");
        ;title: sky
        ;*  standard-head-tags.serv
        ;meta
          =name  "htmx-config"
          =content  (trip '{"ignoreTitle":"true"}')
          ;
        ==
        ;script
          ;+  ;/  %-  trip
          '''
          document.addEventListener('feather-changed', (e) => {
            e.detail.forEach(r => {
              document.documentElement.style
                .setProperty('--'+r.variable, `${r.value}${r.unit||''}`);
            })
            let windows = document.querySelectorAll('wi-nd');
            windows.forEach(w => {
              $(w).poke('set-feather-values', e.detail)
            })
            let rules = document.querySelector('s-k-y').currentFeatherRules;
          });
          document.addEventListener('feather-reset', (e) => {
            let windows = document.querySelectorAll('wi-nd');
            windows.forEach(w => {
              $(w).poke('reset-feather-values')
            })
            let rules = document.querySelector('s-k-y').currentFeatherRules;
          });
          window.addEventListener('resize', () => {
            $('s-k-y').attr('open', null);
          });
          window.addEventListener('message', function(event) {
            if (event.data?.messagetype == 'sky-poll-response') {
              let wid = event.data.wid;
              let here = event.data.here;
              let prefix = event.data.prefix;
              let wind = document.querySelector(`[wid='${wid}']`);
              if (wind) {
                $(wind).poke('iframe-moved', {here, prefix})
              }
            }
            else if (event.data?.messagetype == 'iframe-wants-feather') {
              let rules = document.querySelector('s-k-y').currentFeatherRules;
              let wid = event.data.wid;
              let wind = document.querySelector(`[wid='${wid}']`);
              if (wind) {
                $(wind).poke('set-feather-values', rules)
              }
            }
          });
          '''
        ==
        ;script: {(trip s-k-y)}
        ;script: {(trip wi-nd)}
        ;+  favicon
        ;+  manifest
      ==
      ;body
        =hx-ext  "response-targets"
        =hx-swap  "outerHTML"
        =hx-boost  "true"
        =hx-history  "false"
        =hx-replace-url  "/neo/sky"
        ;+  in
      ==
    ==
  --
--
