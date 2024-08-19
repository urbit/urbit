/@  ui-event
/@  txt
/@  order
/-  serv=sky-server
^-  kook:neo
=<
|%
++  state  pro/%manx
++  poke   (sy %ui-event %rely %gift ~)
++  kids  *kids:neo
++  deps
  ^-  deps:neo
  %-  my
  :~  :^  %src  &  [pro/%sig ~]
      :+  ~  %z
      schema.serv
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    manx/!>((render bowl))
  ::
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    ^-  (quip card:neo pail:neo)
    ?+  sud  ~|(bad-stud/sud !!)
      ::
        %ui-event
      =/  eve  !<(ui-event vaz)
      =/  rod  `(pole @tas)`path.eve
      =/  dst=pith:neo    p:(~(got by deps.bowl) %src)
      ?+  rod  ~|(missing-event-handler-for/path.eve !!)
        ::
          [%click %default-theme ~]
        :_  pail
        ~(default-theme-cards render bowl)
        ::
          [%change %theme ~]
        =/  dat  ~(. by data.eve)
        =/  name  `@tas`(got:dat '/target/data-name')
        :_  pail
        ^-  (list card:neo)
        :~
          [(welp dst #/theme/[name]/unit) %make %txt `txt/!>((got:dat '/target/data-unit')) ~]
          [(welp dst #/theme/[name]/value) %make %txt `txt/!>((got:dat '/target/value')) ~]
        ==
      ::
          [%strategy-change %sky ~]
        =/  dat  ~(. by data.eve)
        =/  detail  (got:dat '/event/detail')
        :_  pail
        (~(strategy-change-cards render bowl) (need (de:json:html detail)))
      ==
      ::
        %rely
      `manx/!>((render bowl))
      ::
    ==
  ::
  --
--
::
|%
::
++  render
  |_  [=bowl:neo]
  ++  $
    ^-  manx
    %-  lift
    ;s-k-y
      =our  (scow %p our.bowl)
      =event  "/strategy-change/sky"
      =return  "/event/detail"
      =default-strategies  default-strategies-json
      ;+  form-theme
      ;+  notifications
    ==
  ++  dst
    ^-  pith:neo
    p:(~(got by deps.bowl) %src)
  ++  lor
    ^-  lore:neo
    q:(~(got by deps.bowl) %src)
  ++  fot
    ~(. of:neo lor)
  ++  dap
    |=  =pith:neo
    ~(tap of:neo (dip:fot pith))
  ++  dar
    |=  =pith:neo
    ~(tar of:neo (dip:fot pith))
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
  ++  strategy-change-cards
    |=  jon=json
    ^-  (list card:neo)
    =/  here
      %-  pave:neo
      %-  (ot ~[here+pa]):dejs:format
      jon
    ::
    =/  strats=order
      %+  turn
        %-  (ot ~[strategies+(ar pa)]):dejs:format
        jon
      pave:neo
    :~
      [:(welp dst #/strategy here) %make %order `order/!>(strats) ~]
    ==
  ::
  ++  default-theme-cards
    ^-  (list card:neo)
    ::
    %-  zing
    ^-  (list (list card:neo))
    %+  turn  (dap /theme)
    |=  [rel=pith:neo =idea:neo]
    =/  p  :(welp dst #/theme rel)
    ~&  del-theme/p
    :~
      [p %cull ~]
      [p %tomb ~]
    ==
  ::
  ++  theme-rules
    ^-  (list [@t @t @t])
    =/  data-map  (dar /theme)
    =/  vars=(set iota)
      %-  ~(run in ~(key by data-map))
      |=  =pith
      (snag 0 pith)
    =/  rules=(set [@t @t @t])
      %-  ~(run in vars)
      |=  =iota
      ^-  [@t @t @t]
      =/  name=@t  ?@(iota iota (scot iota))
      =/  value=idea:neo  (~(gut by data-map) #/[iota]/value %*(. *idea:neo pail txt/!>('')))
      =/  unit=idea:neo  (~(gut by data-map) #/[iota]/unit %*(. *idea:neo pail txt/!>('')))
      [name !<(@t q.pail.value) !<(@t q.pail.unit)]
    ~(tap in rules)
  ++  theme-tape
    ^-  tape
    %-  zing
    %+  turn  theme-rules
    |=  [name=@t val=@t suffix=@t]
    """
    --{(trip name)}: {(trip val)}{(trip suffix)};
    """
  ++  css-input
    |=  [label=tape name=@t nit=@t =mart fallback=@t]
    ^-  manx
    =/  theme  (malt theme-rules)
    =/  tem  `(unit [value=@t @t])`(~(get by theme) name)
    ;label.fc.g2
      ;span.mono.f3.s-1: {label}
      ;+
        =;  m
          m(a.g (welp a.g.m mart))
        ^-  manx
        ;input
          =event  "/change/theme"
          =data-name  (trip name)
          =data-unit  (trip nit)
          =autocomplete  "off"
          =value  (trip ?~(tem fallback value.u.tem))
          =return  "/target/value /target/data-name /target/data-unit"
          =oninput  "$(this).emit('feather-changed', [\{variable:'{(trip name)}', unit: '{(trip nit)}', value: this.value}])"
          =class  "wf";
    ==
  ++  css-color
    |=  [label=tape name=@t =mart fallback=@t]
    ^-  manx
    =/  theme  (malt theme-rules)
    =/  tem  `(unit [value=@t @t])`(~(get by theme) name)
    ;label.fr.ac.jc.g1.wf
      ;span.mono.f3.s-1: {label}
      ;+
        =;  m
          m(a.g (welp a.g.m mart))
        ^-  manx
        ;input
          =event  "/change/theme"
          =data-name  (trip name)
          =data-unit  ""
          =autocomplete  "off"
          =value  (trip ?~(tem fallback value.u.tem))
          =return  "/target/value /target/data-name /target/data-unit"
          =oninput  "$(this).emit('feather-changed', [\{variable:'{(trip name)}', unit: '', value: this.value}])"
          =class  "grow"
          =type  "color";
    ==
  ++  css-select
    |=  [label=tape name=@t options=(list (pair @t @t)) fallback=@t]
    ^-  manx
    =/  theme  (malt theme-rules)
    =/  tem  `(unit [value=@t @t])`(~(get by theme) name)
    ;label.fc.ac.jc.g1.wf
      ;span.mono.f3: {label}
      ;select.br1.bd1.p-1.wf
        =event  "/change/theme"
        =data-name  (trip name)
        =data-unit  ""
        =autocomplete  "off"
        =return  "/target/value /target/data-name /target/data-unit"
        =oninput  "$(this).emit('feather-changed', [\{variable:'{(trip name)}', unit: '', value: this.value}])"
        =class  "wf"
        ;*
        %+  turn  options
        |=  [opt=@t label=@t]
        =;  m
          m
        ^-  manx
        ;option
          =value  (trip opt)
          ; {(trip label)}
        ==
      ==
    ==
++  main-font-options
  ^-  (list (pair @t @t))
  :~
    ['"Arial", sans-serif' 'Arial']
    ['"Gill Sans", sans-serif' 'Gill Sans']
    ['"Helvetica Neue", Helvetica, sans-serif' 'Helvetica']
    ['"Charter", serif' 'Charter']
  ==
++  mono-font-options
  ^-  (list (pair @t @t))
  :~
    ['"Courier New", Courier, monospace' 'Courier']
    ['"Monaco", monospace' 'Monaco']
    ['"Andale Mono", monospace' 'Andale Mono']
  ==
  ++  form-theme
    ::
    ::  MAINTENTANCE NOTE: THE DEFAULT VALUES OF THESE FORMS MUST LINE
    ::    UP WITH THE DEFAULT VARIABLE VALUES IN FEATHER.CSS
    ::
    ;div.fc.g5(slot "theme")
      ;div.fc.g3
        ;+  (css-input "Opacity" 'sky-opacity' '' ~[[%type "range"] [%min "0.1"] [%max "1"] [%step "0.01"]] '1')
        ;+  (css-input "Outer gap" 'sky-outer-gap' 'px' ~[[%type "range"] [%min "0"] [%max "45"] [%step "1"]] '8')
        ;+  (css-input "Inner gap" 'sky-inner-gap' 'px' ~[[%type "range"] [%min "0"] [%max "45"] [%step "1"]] '8')
        ;+  (css-input "Window border" 'sky-window-border' 'px' ~[[%type "range"] [%min "0"] [%max "4"] [%step "0.01"]] '2')
      ==
      ;div.fc.g5
        ;+  (css-input "Base font size" '1in' 'px' ~[[%type "range"] [%min "2.5"] [%max "5.5"] [%step "0.01"]] '4')
        ;+  (css-input "Monospaced font scale" 'mono-scale' '' ~[[%type "range"] [%min "0.5"] [%max "1.5"] [%step "0.01"]] '0.8')
        ;+  (css-input "Letter spacing" 'letter-spacing' 'em' ~[[%type "range"] [%min "-0.1"] [%max "0.4"] [%step "0.001"]] '0.024')
        ;+  (css-input "Line height" 'line-height' '' ~[[%type "range"] [%min "0.8"] [%max "2"] [%step "0.01"]] '1.4')
      ==
      ;div.fc.g5
        ;+  (css-select "Main Font" 'font' main-font-options 'Arial')
        ;+  (css-select "Mono Font" 'font-mono' mono-font-options 'Courier')
      ==
      ;h2: light
      ;div.fr.g2.wf
        ;div.fc.g1.wf
          ;+  (css-color "b4" 'light-b4' ~ '#9999a9')
          ;+  (css-color "b3" 'light-b3' ~ '#aaaaba')
          ;+  (css-color "b2" 'light-b2' ~ '#bbbbcb')
          ;+  (css-color "b1" 'light-b1' ~ '#ccccdc')
          ;+  (css-color "b0" 'light-b0' ~ '#dddded')
          ;+  (css-color "b-1" 'light-b-1' ~ '#55dd33')
          ;+  (css-color "b-2" 'light-b-2' ~ '#ddaa33')
          ;+  (css-color "b-3" 'light-b-3' ~ '#dd5522')
        ==
        ;div.fc.g1.wf
          ;+  (css-color "f4" 'light-f4' ~ '#777797')
          ;+  (css-color "f3" 'light-f3' ~ '#555575')
          ;+  (css-color "f2" 'light-f2' ~ '#444464')
          ;+  (css-color "f1" 'light-f1' ~ '#333353')
          ;+  (css-color "f0" 'light-f0' ~ '#111131')
          ;+  (css-color "f-1" 'light-f-1' ~ '#339911')
          ;+  (css-color "f-2" 'light-f-2' ~ '#aaaa22')
          ;+  (css-color "f-3" 'light-f-3' ~ '#993311')
        ==
      ==
      ;h2: dark
      ;div.fr.g2.wf
        ;div.fc.g1.wf
          ;+  (css-color "b4" 'dark-b4' ~ '#666676')
          ;+  (css-color "b3" 'dark-b3' ~ '#555565')
          ;+  (css-color "b2" 'dark-b2' ~ '#444454')
          ;+  (css-color "b1" 'dark-b1' ~ '#333343')
          ;+  (css-color "b0" 'dark-b0' ~ '#222232')
          ;+  (css-color "b-1" 'dark-b-1' ~ '#225511')
          ;+  (css-color "b-2" 'dark-b-2' ~ '#555511')
          ;+  (css-color "b-3" 'dark-b-3' ~ '#551111')
        ==
        ;div.fc.g1.wf
          ;+  (css-color "f4" 'dark-f4' ~ '#8888a8')
          ;+  (css-color "f3" 'dark-f3' ~ '#aaaada')
          ;+  (css-color "f2" 'dark-f2' ~ '#bbbbcb')
          ;+  (css-color "f1" 'dark-f1' ~ '#ccccdc')
          ;+  (css-color "f0" 'dark-f0' ~ '#eeeefe')
          ;+  (css-color "f-1" 'dark-f-1' ~ '#55cc33')
          ;+  (css-color "f-2" 'dark-f-2' ~ '#ccbb33')
          ;+  (css-color "f-3" 'dark-f-3' ~ '#ee7755')
        ==
      ==
      ;button.p2.br1.bd1.b1
        =event  "/click/default-theme"
        =js-on-event  "document.documentElement.style = ''"
        ; Reset to default
      ==
    ==
  ++  notifications
    ;div.fc.g2(slot "notifications")
      ;div.p3.br1.bd1: first
      ;div.p3.br1.bd1: second
      ;div.p3.br1.bd1: third
    ==
  ++  lift
    |=  in=manx
    ^-  manx
    ;html
      =style  theme-tape
      ;head
        ;meta(charset "UTF-8");
        ;title: sky
        ;*  standard-head-tags.serv
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
            else if (event.data?.messagetype == 'sky-poll-response-favicon') {
              let wid = event.data.wid;
              let favicon = event.data.favicon;
              let wind = document.querySelector(`[wid='${wid}']`);
              if (wind) {
                $(wind).poke('favicon-changed', favicon)
              }
            }
            else if (event.data?.messagetype == 'sky-poll-response-title') {
              let wid = event.data.wid;
              let title = event.data.tabTitle;
              let wind = document.querySelector(`[wid='${wid}']`);
              if (wind) {
                $(wind).poke('title-changed', title)
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
        ;script(src "/blue/blue-mime/{(scow %p our.bowl)}/static/s-k-y");
        ;script(src "/blue/blue-mime/{(scow %p our.bowl)}/static/wi-nd");
        ;link
          =rel  "icon"
          =type  "image/png"
          =href  "/blue/blue-mime/{(scow %p our.bowl)}/static/hawk-icon";
        ;link
          =rel  "manifest"
          =crossorigin  "use-credentials"
          =href  "/blue/blue-mime/{(scow %p our.bowl)}/static/sky-manifest";
      ==
      ;body
        ;+  in
      ==
    ==
  --
--
