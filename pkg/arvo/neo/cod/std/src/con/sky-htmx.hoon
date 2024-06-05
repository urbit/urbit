/@  sky
/@  sky-settings
/-  feather-icons
:-  [%sky %$ %htmx]
|=  =sky
|=  =bowl:neo
^-  manx
|^
  ;div.wf.hf.relative
    ;a-i-r.wf.hf.relative
      =style  "opacity: var(--sky-opacity); padding: var(--sky-outer-gap);"
      =id  "air"
      =hawks  "{<open.sky>}"
      =morph-retain  "closed"
      ;+  menu-btn
      ;+  menu-btn-style
      ;+  theme-style
      ;+  nav
      ;*  p:(spin (scag open.sky hawks.sky) 0 ha-wk)
    ==
    ;+  eye
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
  =/  settings
    ^-  (unit sky-settings)
    =/  s  (~(get of:neo kids.bowl) /settings)
    ?~  s  ~
    :-  ~
    !<  sky-settings
    q.pail.u.s
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
  ;button.hover.f2.b2.fc.ac.jc.air-btn.wf
    =slot  "button"
    =onclick  "$(this).closest('a-i-r').attr('closed', !$(this).parent().attr('closed'))"
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
        bottom: 25px;
        right: 25px;
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
  ;div.wf.hf.br1
    =slot  "s{<a>}"
    =id  "hawk-windshield-{idt}"
    ;div.wf.hf.fc.jc.ac.f2.s3.spinner
      =id  "hawk-{idt}"
      =morph-if-class  "spinner"
      ;+  loading.feather-icons
    ==
    ;div.hidden
      =hx-get  "/neo/hawk{ext}?slot={<a>}&id={<id>}&no-save"
      =hx-trigger  "load"
      =hx-target  "#hawk-{idt}"
      =hx-swap  "morph"
      ;
    ==
  ==
++  nav
  ;nav.wf.hf.p2.fc.g2
    =slot  "nav"
    ;div.mt2.o0;
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
    ;div
      =id  "hawk-tab-{idt}"
      =class  "fr ac br1 {color}"
      ;button
        =class  "loader p2 tl br1 hover grow {color}"
        =hx-post  "/neo/hawk/{<our.bowl>}/sky?stud=sky-diff"
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        =head  "maximize"
        =hawk-slot  "{<a>}"
        ;span.loaded: {(en-tape:pith:neo pith)}
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
      ;button
        =class  "loader p2 tl br1 hover {color}"
        =hx-post  "/neo/hawk/{<our.bowl>}/sky?stud=sky-diff"
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        =head  "close"
        =hawk-slot  "{<a>}"
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
  ;button.loader.b2.p2.tc.br1.hover.wfc.s-1
    =hx-post  "/neo/hawk/{<our.bowl>}/sky?stud=sky-diff"
    =hx-target  "find .loading"
    =hx-swap  "outerHTML"
    =type  "button"
    =head  "new-tab"
    ;span.loaded.fr.ac.js.g2
      ;+  add.feather-icons
      ;span.f3: new tab
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
--
