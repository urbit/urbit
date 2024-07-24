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
|_  =bowl:neo
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
        let textinput = ['text', 'number', 'email', 'password'].includes(focused.type);
        if (textarea || textinput) {
          if (e.key === 'Escape') {
            document.activeElement.blur();
          }
          return;
        }
        else if (e.key === 'Escape') {
          closeEye();
          document.activeElement.blur();
        }
        else if (e.key === ' ') {
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
      ;title: hawk
      ;script: {(trip jquery)}
      ;script: {(trip htmx-js)}
      ;script: {(trip htmx-response-targets)}
      ;script: {(trip htmx-idiomorph)}
      ;script: {htmx-extensions}
      ::  ;link
      ::    =rel  "stylesheet"
      ::    =href  "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.1/cdn/themes/light.css"
      ::    ;
      ::  ==
      ::  ;script
      ::    =type  "module"
      ::    =src  "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.1/cdn/shoelace.js"
      ::    ;
      ::  ==
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
          console.log(this);
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
        function urbitOur() {
          return document.body.getAttribute('our');
        }
        function hawkAtSlot(slot) {
          return document.querySelector(`.hawk[slot='${slot}']`);
        }
        function hawkDrag(event) {
          let fromHawk = $(event.target).closest('.hawk').get(0);
          event.dataTransfer.setData("text", fromHawk.slot);
        }
        function hawkDragEnter(event) {
          event.preventDefault();
          $('a-i-r').find('.drag-overlay').addClass('hidden');
          $(event.target).closest('.hawk').children('.drag-overlay').removeClass('hidden');
        }
        function hawkDragAllow(event) {
          event.preventDefault();
        }
        function hawkDragLeave(event) {
          event.preventDefault();
        }
        function hawkDrop(event) {
          event.preventDefault();
          $('a-i-r').find('.drag-overlay').addClass('hidden');
          let fromSlot = event.dataTransfer.getData("text");
          let fromHawk = $('a-i-r').children(`.hawk[slot='${fromSlot}']`).get(0);
          let toHawk = $(event.target).closest('.hawk').get(0);
          let toSlot = toHawk.slot;
          if (toSlot != fromSlot) {
            toHawk.setAttribute('slot', fromSlot);
            fromHawk.setAttribute('slot', toSlot);
            const formData = new URLSearchParams();
            formData.append('from', fromSlot.slice(1));
            formData.append('to', toSlot.slice(1));
            fetch(`/neo/hawk/${urbitOur()}/sky?stud=sky-diff&head=swap-hawks`, {
              method: 'POST',
              headers: {
                'Content-Type': 'application/x-www-form-urlencoded'
              },
              body: formData.toString(),
            })
            $('a-i-r').emit('hawks-moved');
          }
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
      =our  (scow %p our.bowl)
      =style
        """
        background-color: var(--b1);
        background-image: var(--sky-bg-url);
        background-size: var(--sky-bg-size);
        background-repeat: var(--sky-bg-repeat);
        """
      ;+  in
      ;+  eye
      ;+  stub-styling
    ==
  ==
++  stub-styling
;style
  ;+  ;/  %-  trip
  '''
  html {
    --line-height: 1.4;
    --sky-outer-gap: 8px;
    --dark-b-1: #225511;
    --dark-b3: #555555;
    --light-b-1: #55dd33;
    --letter-spacing: 0.024em;
    --dark-f-2: #ccbb33;
    --light-b1: #cccccc;
    --light-b0: #dddddd;
    --light-b3: #aaaaaa;
    --light-b-3: #dd5522;
    --dark-b4: #666666;
    --light-f2: #444444;
    --dark-b-3: #551111;
    --dark-f4: #888888;
    --dark-f-3: #ee7755;
    --light-b-2: #ddaa33;
    --sky-opacity: 0.88;
    --dark-b0: #222222;
    --light-b2: #bbbbbb;
    --light-f-3: #993311;
    --light-f1: #333333;
    --dark-f-1: #55cc33;
    --dark-b1: #333333;
    --dark-f1: #cccccc;
    --light-f3: #555555;
    --light-b4: #999999;
    --sky-bg-url: ;
    --dark-f3: #aaaaaa;
    --light-f0: #111111;
    --sky-bg-size: contain;
    --light-f4: #777777;
    --1in: 4px;
    --font-mono: monospace;
    --dark-f0: #eeeeee;
    --sky-inner-gap: 8px;
    --dark-b2: #444444;
    --dark-b-2: #555511;
    --font: Urbit Sans, sans-serif;
    --mono-scale: 0.8;
    --light-f-2: #aaaa22;
    --dark-f2: #bbbbbb;
    --light-f-1: #339911;
  }
  '''
==
--
