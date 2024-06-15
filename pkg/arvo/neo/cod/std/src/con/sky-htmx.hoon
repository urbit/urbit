/@  sky
/@  sky-settings
/-  feather-icons
/*  date-now
/*  a-i-r
/*  feather
/*  reset
/*  hawk-icon
/*  jquery
/*  htmx-js
/*  htmx-response-targets
/*  htmx-idiomorph
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
  ;a-i-r.wf.hf.relative
    =style  "opacity: var(--sky-opacity); padding: var(--sky-outer-gap);"
    =id  "air"
    =hawks  "{<open.sky>}"
    =morph-retain  "closed"
    =hx-on-hawks-moved  hawks-moved-js
    =hx-on-htmx-after-request
      """
      let verb = event.detail.requestConfig.verb;
      let url = new URL(event.detail.xhr.responseURL);
      let pams = new URLSearchParams(url.search);
      if (verb === 'get' &&
          url.pathname.startsWith('/neo/hawk/~') &&
          !pams.has('no-save')
         ) \{
        $('.nav-refresher').emit('refresh');
      }
      """
    ;*  p:(spin (scag open.sky hawks.sky) 0 ha-wk)
    ;+  menu-btn
    ;+  nav
  ==
::
++  hawks-moved-js
  ::  js to run whenever the order or number of hawks changed.
  ::  it will:
  ::   - trigger a refresh of the nav
  ::   - loop through any slotted hawks and reslot them starting from 0
  %-  trip
  '''
  let air = $(this);
  let num = parseInt(air.attr('hawks'));
  let hawks = air.children('[slot]').filter('.hawk').get();
  hawks.sort((a, b) => {
    return (a.getAttribute('slot') < b.getAttribute('slot')) ? -1 : 1;
  }).forEach((h, i) => {
    h.setAttribute('slot', `s${i}`);
  });
  air.find('.nav-refresher').emit('refresh');
  '''
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
      ""
    """
    html \{
      {(map-to-css-tape u.settings)}
    }
    """
  ==
++  menu-btn
  =;  m
    ?:  menu.sky  m
    m(a.g [[%closed ""] a.g.m])
  ^-  manx
  ;button.hover.f2.b2.fc.ac.jc.air-btn.wf
    =slot  "button"
    =onclick  "this.parentNode.toggleAttribute('closed'); this.toggleAttribute('closed');"
    =hx-post  "/neo/hawk/{<our.bowl>}/sky?stud=sky-diff"
    =head  "menu"
    =hx-target  "this"
    =hx-swap  "none"
    ;div.fc.ac.jc.bold.s3.f3(style "height: 2rem;"): ~
  ==
++  menu-btn-style
  ;style
    ;+  ;/  %-  trip
    '''
    .air-btn {
      position: relative;
      padding: 4px;
      border-radius: 3px;
      border: 2px solid var(--b2);
    }
    @media(max-width: 900px) {
      .air-btn {
        position: absolute;
        bottom: 45px;
        left: 25px;
        padding: 30px;
        width: 70px;
        height: 70px;
        z-index: 10;
        border-radius: 50px;
        border: 1px solid var(--b3);
      }
      a-i-r {
        padding: 0 !important;
      }
    }
    '''
  ==
++  ha-wk
  |=  [[id=@da =pith] a=@]
  :_  +(a)
  =/  ext
    ?:  =(pith /)  ""
    (en-tape:pith:neo pith)
  =/  idt  `tape`(zing (scan +:(scow %da id) (most dot (star ;~(less dot prn)))))
  ;div.wf.hf.fc.jc.ac.f2.s3.spinner
    =slot  "s{<a>}"
    =id  "hawk-{idt}"
    =hx-get  "/neo/hawk{ext}?slot={<a>}&hawk-id={<id>}&no-save"
    =morph-retain  "slot"
    =hx-trigger  "load"
    =hx-target  "this"
    =hx-swap  "morph"
    ;+  loading.feather-icons
  ==
++  nav
  ;nav.wf.hf.p2.fc.g2.sky-nav
    =slot  "nav"
    ;div.nav-refresher.loader.p3
      =hx-get  "/neo/sky"
      =hx-swap  "outerHTML"
      =hx-target  "closest nav"
      =hx-select  "nav.sky-nav"
      =hx-trigger  "refresh"
      ;span.loaded;
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
    ;+  new-tab
    ;*
    =<  p
    %^    spin
        hawks.sky
      0
    |=  [[id=@da =pith] a=@]
    :_  +(a)
    =/  idt  `tape`(zing (scan +:(scow %da id) (most dot (star ;~(less dot prn)))))
    =/  color  (trip ?:((lth a open.sky) 'b2' 'b1'))
    =/  close-hawk-js
      %-  trip
      '''
      let toClose = parseInt(this.getAttribute('close'));
      let air = $('a-i-r');
      let num = parseInt(air.attr('hawks'));
      let hawks = air.children('[slot]').filter('.hawk');
      if (toClose < num) {
        air.attr('hawks', hawks.length-1);
      }
      hawks.each(function() {
        let slot = parseInt($(this).attr('slot').slice(1));
        if (slot == toClose) {
          $(this).remove();
        }
      });
      $(this).emit('hawks-moved');
      '''
    :: =/  maximize-hawk-js
    ::   %-  trip
    ::   '''
    ::   let air = $('a-i-r');
    ::   let num = parseInt(air.attr('hawks'));
    ::   air.attr('hawks', num+1);
    ::   let toMax = '#hawk-' + this.getAttribute('hawk');
    ::   $(toMax).attr('slot', 's-1');
    ::   $(this).emit('hawks-moved');
    ::   '''
    ;div
      =id  "hawk-tab-{idt}"
      =hx-ext  "ignore:html-enc"
      =class  "fr ac br1 {color}"
      ;button
        =class  "loader p2 tl br1 hover grow {color}"
        =hx-post  "/neo/hawk/{<our.bowl>}/sky?stud=sky-diff&head=maximize&slot={<a>}"
        ::  XX optimistically render
        ::=hx-on-htmx-after-request  maximize-hawk-js
        =hawk  idt
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        ;span.loaded: {(en-tape:pith:neo pith)}
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
      ;button
        =class  "loader p2 tl br1 hover {color}"
        =close  "{<a>}"
        =hx-post  "/neo/hawk/{<our.bowl>}/sky?stud=sky-diff&head=close&slot={<a>}"
        =hx-swap  "none"
        =hx-on-htmx-after-request  close-hawk-js
        ;span.loaded.f3
          ;+  close.feather-icons
        ==
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ==
++  new-tab
  ::  XX optimistically render
  ;button.loader.b2.p2.tc.br1.hover.wfc.s-1
    =hx-ext  "ignore:html-enc"
    =hx-post  "/neo/hawk/{<our.bowl>}/sky?stud=sky-diff&head=new-hawk"
    =hx-target  "find .loading"
    =hx-swap  "outerHTML"
    =hx-on-htmx-after-request  "$(this).emit('hawks-moved')"
    =type  "button"
    =hx-vals  (trip 'js:{now: urbitTimestamp()}')
    ;span.loaded.fr.ac.js.g2
      ;+  add.feather-icons
      ;span.f3: new window
    ==
    ;span.loading
      ;+  loading:feather-icons
    ==
  ==
++  eye
  ;div#eye.fixed.hidden
    =style  "bottom: 30px; left: 30px;"
    =morph-no-swap  ""
    ;script
      ;+  ;/  %-  trip
      '''
      function handleKey(e) {
        let focused = document.activeElement;
        let textarea = ['TEXTAREA'].includes(focused.nodeName)
        let textinput = ['text', 'number', 'email', 'password'].includes(focused.getAttribute('type'));
        if (textarea || textinput) {
          if (e.key === 'Escape') {
            closeEye();
            document.activeElement.blur();
          }
          return;
        }
        if (e.key === ' ') {
          e.preventDefault();
          if (window.eye.open) {
            closeEye();
          } else {
            openEye();
          }
        } else if (e.key === 'Escape') {
          e.preventDefault();
          document.activeElement.blur();
        } else if (!e.ctrlKey && !e.metaKey && !e.altKey) {
          if (window.eye?.open) {
            e.preventDefault();
            let area = window.eye?.spots?.filter(s => s[0][0] === e.key);
            if (area.length === 1) {
              let btn = area[0][1];
              btn.click();
              btn.focus();
              closeEye();
            }
            else if (!!area.length) {
              let news = area.map(c => (c[0].length < 2) ? c : [c[0].slice(1), c[1]]);
              window.eye.spots = news;
              closeEye(true);
              openEye();
            } else {
              closeEye();
            }
          }
        }
      }
      function openEye() {
        window.eye.open = true;
        document.getElementById('eye').classList.remove('hidden');
        buildGazeSpots();
      }
      function closeEye(keep) {
        window.eye.open = false;
        if (!keep) {
          window.eye.spots = null;
        }
        document.getElementById('eye').classList.add('hidden');
        document.querySelectorAll('.gaze').forEach(g => g.remove());
      }
      function buildGazeSpots() {
        let buttons = window.eye?.spots?.map(s => s[1]) ||
          document.querySelectorAll(
            'a, button, summary, [role="button"], input, textarea, .clickable'
          );
        let chars = ['a', 's', 'd', 'f', 'k', 'm', 'n', 'r', 't', 'y', 'u', 'i', 'c', 'v', 'b'];
        buttons.forEach((b, i) => {
          let d = b.getBoundingClientRect();
          if (d.right > 0 && d.right > 0) {
            let t = document.createElement('div');
            var lent = Math.floor((i / chars.length) + 1);
            var word = ''
            while (lent > 0) {
              let ch = chars[i % chars.length];
              word = `${word}${ch}`;
              lent = lent - 1;
            }
            t.textContent = word.slice(-1);
            t.className = 'b-1 br2 p1 s0 bold fixed gaze z2'
            t.style = `top: ${Math.max(0, d.top - 10)}px; left: ${Math.max(0, d.left - 10)}px;`
            document.getElementById('eye')?.parentNode.appendChild(t);
            window.eye.spots = [[word, b], ...(window.eye?.spots || [])]
          }
        })
      }
      window.addEventListener('keydown', handleKey);
      '''
    ==
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
++  htmx-extensions
  ::  htmx extension which encodes the request
  ::  as the serialized HTML of the calling element
  ::
  ::  XX usage of this should be optional.
  ::  requests should default to form-encoded.
  %-  trip
  '''
  htmx.defineExtension('html-enc', {
    onEvent: function (name, evt) {
      if (name === "htmx:configRequest") {
        evt.detail.headers['Content-Type'] = "text/html";
      }
    },
    encodeParameters : function(xhr, parameters, elt) {
      xhr.overrideMimeType('text/html');
      let xmls = new XMLSerializer();
      return (xmls.serializeToString(elt));
    }
  });
  Idiomorph.defaults.ignoreActive = true;
  Idiomorph.defaults.callbacks.beforeAttributeUpdated = (name, node, type) => {
    if (node.hasAttribute('morph-retain')) {
      let ribs = node.getAttribute('morph-retain').split(',').map(t => t.trim());
      if (ribs.includes(name)) {
        return false;
      }
    }
  }
  Idiomorph.defaults.callbacks.beforeNodeMorphed = (oldNode, newNode) => {
    if (oldNode?.nodeName !== "#text") {
      if (oldNode.hasAttribute('morph-no-swap') && oldNode.id === newNode.id) {
        return false;
      }
      else if (
        newNode.hasAttribute('morph-if-class') &&
        !oldNode.classList.contains(newNode.getAttribute('morph-if-class'))
      ) {
        return false;
      }
    }
  }
  '''
::
++  lift
  |=  in=manx
  ^-  manx
  ;html
    ;head
      ;meta(charset "UTF-8");
      ;title: s k y
      ;script: {(trip jquery)}
      ;script: {(trip htmx-js)}
      ;script: {(trip htmx-response-targets)}
      ;script: {(trip htmx-idiomorph)}
      ;script: {htmx-extensions}
      ;meta
        =name  "viewport"
        =content
          """
          width=device-width,
          initial-scale=1.0,
          maximum-scale=1.0
          """
        ;
      ==
      ;meta
        =name  "htmx-config"
        =content  (trip '{"ignoreTitle":"true"}')
        ;
      ==
      ::;style
      ::  ;+  ;/  %-  trip
      ::  '''
      ::  @font-face {
      ::    font-family: 'Urbit Sans';
      ::    src: url("https://media.urbit.org/fonts/UrbitSans/UrbitSansVFWeb-Regular.woff2") format("woff2");
      ::    font-style: normal;
      ::    font-weight: 100 700;
      ::  }
      ::  '''
      ::==
      ;style: {(trip reset)}
      ;style: {(trip feather)}
      ;script
        ;+  ;/  %-  trip
        '''
        window.log = function() {
          if (this.console) {
            console.log(Array.prototype.slice.call(arguments));
          }
        };
        jQuery.fn.log = function (msg) {
          console.log(msg, this);
          return this;
        };
        jQuery.fn.emit = function (name) {
          (this[0]).dispatchEvent(
            new Event(
              name,
              { bubbles: true, cancelable: true, composed: true }
            )
          );
          return this;
        };
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
        '''
      ==
      ;script: {(trip a-i-r)}
      ;script: {(trip date-now)}
      ;+  favicon
      ;+  manifest
    ==
    ;body
      =hx-ext  "html-enc,response-targets,morph"
      =hx-swap  "outerHTML"
      =hx-boost  "true"
      =hx-history  "false"
      =hx-replace-url  "/neo/sky"
      =style
        """
        background-color: var(--b1);
        background-image: var(--sky-bg-url);
        background-size: var(--sky-bg-size);
        background-repeat: var(--sky-bg-repeat);
        """
      ;+  in
      ;+  eye
      ;+  menu-btn-style
      ;+  theme-style
    ==
  ==
--
