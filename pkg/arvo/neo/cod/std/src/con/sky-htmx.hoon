/@  sky
/@  sky-settings
/-  sky-server
/*  s-k-y
/*  wi-nd
/*  hawk-icon
:-  [%sky %$ %htmx]
|=  =sky
|=  =bowl:neo
^-  manx
|^
  =;  m
    %-  lift
    ?:  menu.sky  m
    m(a.g [[%closed ""] a.g.m])
  ^-  manx
  ;s-k-y.wf.hf.relative
    =our  (scow %p our.bowl)
    =style  "opacity: var(--sky-opacity); padding: var(--sky-outer-gap);"
    =id  "air"
    =hawks  "{<open.sky>}"
    ;*
    =<  p
    %^    spin
        hawks.sky
      0
    |=  [[id=@da =pith:neo] a=@]
    :_  +(a)
    ;wi-nd
      =slot  "s{<a>}"
      =here  (en-tape:pith:neo pith)
      ;
    ==
  ==
::
++  map-to-css-tape
  |=  m=(map @t @t)
  ^-  tape
  %-  zing
  %+  turn  ~(tap by m)
  |=  [key=@t val=@t]
  """
  --{(trip key)}: {(trip val)};
  """
++  theme-style
  =/  body-style
    """
    body \{
      background-color: var(--b1);
      background-image: var(--sky-bg-url);
      background-size: var(--sky-bg-size);
      background-repeat: var(--sky-bg-repeat);
    }
    """
  =/  settings
    ^-  (unit sky-settings)
    =/  s  (~(get of:neo kids.bowl) /settings)
    ?~  s  ~
    :-  ~
    !<  sky-settings
    q.q.saga.u.s
  ;style
    ;+  ;/
    ?~  settings
      body-style
    """
    {body-style}
    html \{
      {(map-to-css-tape u.settings)}
    }
    """
  ==
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
    ;head
      ;meta(charset "UTF-8");
      ;title: s k y
      ;*  standard-head-tags.sky-server
      ;meta
        =name  "htmx-config"
        =content  (trip '{"ignoreTitle":"true"}')
        ;
      ==
      ;script
        ;+  ;/  %-  trip
        '''
        function urbitTimestamp() {
          let now = new Date();
          let year = now.getFullYear();
          let month = now.getMonth() + 1;
          let date = now.getDate();
          let hour = String(now.getHours()).padStart(2, '0');
          let min = String(now.getMinutes()).padStart(2, '0');
          let sec = String(now.getSeconds()).padStart(2, '0');
          return `~${year}.${month}.${date}..${hour}.${min}.${sec}`;
        }
        window.addEventListener('message', function(event) {
          let wid = event.data.wid;
          let here = event.data.here;
          let prefix = event.data.prefix;
          let wind = document.querySelector(`[wid='${wid}']`);
          $(wind).poke('iframe-moved', {here, prefix})
        });
        '''
      ==
      ;script: {(trip s-k-y)}
      ;script: {(trip wi-nd)}
      ;+  favicon
      ;+  manifest
    ==
    ;body
      =hx-ext  "dom-enc,response-targets,morph"
      =hx-swap  "outerHTML"
      =hx-boost  "true"
      =hx-history  "false"
      =hx-replace-url  "/neo/sky"
      =our  (scow %p our.bowl)
      ;+  in
      ;+  theme-style
    ==
  ==
--
