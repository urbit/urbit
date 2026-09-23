|_  own=@t
::
++  grow                                                ::  convert to
  |%
  ++  mime  `^mime`[/text/x-hoon (as-octs:mimes:html own)] ::  convert to %mime
  ++  hymn
    ^-  manx
    =/  to-wall
      |=  =tape
      ^-  wall
      %+  roll  (flop tape)
      |=  [char=@tD =wall]
      ?~  wall
        [[char ~] ~]
      ?:  =('\0a' char)
        [~ wall]
      [[char i.wall] t.wall]
    ::
    =/  src=tape  (trip own)
    =/  count=@ud  (lent (to-wall src))
    =;  [style=tape script=tape]
      ;html
        ;head
          ;title: source
          ;meta(charset "utf-8");
          ;meta(name "viewport", content "width=device-width, initial-scale=1");
          ;style: {style}
        ==
        ;body
          ;div.viewer
            :: ;div.viewer-bar
            ::   ;span.viewer-name: {(scow %ud count)} lines
            ::   ;button.copy-link(id "copy-link", hidden ""): copy link
            :: ==
            ;div.code
              ;pre.gutter(id "gutter");
              ;div.code-body
                ;div(id "hl");
                ;pre.code-pre(id "code"): {src}
              ==
            ==
          ==
          ;script: {script}
        ==
      ==
    ::
    :-
      ^~  ^=  style
      ^-  tape
      %-  trip
      '''
      :root {
        --bg:        #fbfbf9;
        --panel:     #ffffff;
        --gutter-bg: #f3f3ef;
        --line:      #e4e4dd;
        --fg:        #2b2b2b;
        --fg-dim:    #9a9a90;
        --sel:       #fff7d6;
        --sel-edge:  #f1e4a8;
        --accent:    #4a6da7;
        --lh:        20px;
        --mono: ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas,
                "Liberation Mono", "Courier New", monospace;
      }
      * { box-sizing: border-box; }
      html, body {
        margin: 0;
        height: 100%;
        background: var(--bg);
        color: var(--fg);
        font-family: var(--mono);
        -webkit-font-smoothing: antialiased;
      }
      .viewer {
        height: 100vh;
        display: flex;
        flex-direction: column;
        padding: 16px;
        gap: 12px;
      }
      .viewer-bar {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        gap: 12px;
        font-size: 13px;
        color: var(--fg-dim);
      }
      .viewer-name {
        flex: 1 1 auto;
        min-width: 0;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        color: var(--fg);
      }
      .copy-link {
        flex: 0 0 auto;
        padding: 4px 12px;
        border: 1px solid var(--line);
        border-radius: 6px;
        background: var(--panel);
        color: var(--fg-dim);
        font: inherit;
        font-size: 12px;
        cursor: pointer;
        transition: color .12s, border-color .12s;
      }
      .copy-link:hover { color: var(--accent); border-color: var(--accent); }
      .copy-link[hidden] { display: none; }
      .code {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        align-items: flex-start;
        overflow: auto;
        background: var(--panel);
        border: 1px solid var(--line);
        border-radius: 8px;
      }
      .code-body {
        position: relative;
        flex: 1 1 auto;
        min-width: 0;
        width: max-content;
      }
      #gutter, #code {
        margin: 0;
        padding: 14px 0;
        font-family: var(--mono);
        font-size: 13px;
        line-height: var(--lh);
        white-space: pre;
        tab-size: 2;
      }
      #gutter {
        position: sticky;
        left: 0;
        z-index: 2;
        flex: 0 0 auto;
        padding-left: 16px;
        padding-right: 12px;
        text-align: right;
        color: var(--fg-dim);
        background: var(--gutter-bg);
        border-right: 1px solid var(--line);
        user-select: none;
        -webkit-user-select: none;
        cursor: pointer;
      }
      #code {
        display: block;
        padding-left: 14px;
        padding-right: 18px;
        color: var(--fg);
      }
      #hl {
        position: absolute;
        left: 0;
        right: 0;
        z-index: 0;                     /* under the text */
        display: none;
        background: var(--sel);
        box-shadow: inset 2px 0 0 var(--sel-edge);
        pointer-events: none;
      }
      #code { position: relative; z-index: 1; }   /* text above the bar */
      :focus-visible { outline: 2px solid var(--accent); outline-offset: -2px; }
      '''
    ^~  ^=  code
    ^-  tape
    %-  trip
    '''
    (function () {
      "use strict";
      const panel = document.querySelector(".code"); // the scroll container
      const code = document.getElementById("code");
      const gutter = document.getElementById("gutter");
      const hl = document.getElementById("hl");
      if (!panel || !code || !gutter || !hl) return;
      const text = code.textContent;
      let lineCount = text.length ? text.split("\n").length : 1;
      if (text.endsWith("\n")) lineCount -= 1;
      if (lineCount < 1) lineCount = 1;
      (function fillGutter() {
        let s = "";
        for (let i = 1; i <= lineCount; i++) s += i + "\n";
        gutter.textContent = s;
      })();
      let lineH = 0;
      let codeTop = 0; // code's content-box top padding, the y-origin of line 1
      function measure() {
        const cs = getComputedStyle(code);
        const lh = parseFloat(cs.lineHeight);
        lineH = isNaN(lh) ? parseFloat(cs.fontSize) * 1.55 : lh;
        codeTop = parseFloat(cs.paddingTop) || 0;
        positionHighlight(); // keep the bar correct after a re-measure
      }
      let anchor = null;
      let selLo = 0;
      let selHi = 0;
      function positionHighlight() {
        if (!selLo) { hl.style.display = "none"; return; }
        hl.style.display = "block";
        hl.style.top = (codeTop + (selLo - 1) * lineH) + "px";
        hl.style.height = ((selHi - selLo + 1) * lineH) + "px";
      }
      function applySel(lo, hi) {
        selLo = lo;
        selHi = hi;
        positionHighlight();
      }
      function deselect() {
        selLo = selHi = 0;
        anchor = null;
        positionHighlight();
        history.replaceState(null, "", location.pathname + location.search);
      }
      function parseHash(hash) {
        const m = /^#L(\d+)(?:-L(\d+))?$/.exec(hash || "");
        if (!m) return null;
        let lo = parseInt(m[1], 10);
        let hi = m[2] ? parseInt(m[2], 10) : lo;
        if (hi < lo) { const t = lo; lo = hi; hi = t; }
        lo = Math.max(1, Math.min(lo, lineCount));
        hi = Math.max(1, Math.min(hi, lineCount));
        return [lo, hi];
      }
      function setHash(lo, hi) {
        const frag = lo === hi ? "L" + lo : "L" + lo + "-L" + hi;
        history.replaceState(null, "", "#" + frag);
      }
      function scrollToLine(n) {
        const y = codeTop + (n - 1) * lineH;
        if (y < panel.scrollTop || y > panel.scrollTop + panel.clientHeight) {
          panel.scrollTop = Math.max(0, y - panel.clientHeight / 2);
        }
      }
      function selectFromHash(scroll) {
        const r = parseHash(location.hash);
        if (!r) return false;
        anchor = r[0];
        applySel(r[0], r[1]);
        if (scroll) scrollToLine(r[0]);
        return true;
      }
      function lineFromEvent(e) {
        // y within the code content: pointer relative to the code pre's rendered
        // top, which already accounts for scroll since getBoundingClientRect is
        // viewport-relative and the pre moves as the panel scrolls.
        const rect = code.getBoundingClientRect();
        const y = e.clientY - rect.top - codeTop;
        let n = Math.floor(y / lineH) + 1;
        if (n < 1) n = 1;
        if (n > lineCount) n = lineCount;
        return n;
      }
      function onClick(e) {
        const inGutter = e.target === gutter;
        if (!inGutter && !e.shiftKey) return; // normal text interaction in code
        const n = lineFromEvent(e);
        if (!e.shiftKey && selLo === n && selHi === n) {
          deselect();
          return;
        }
        if (e.shiftKey && anchor !== null) {
          applySel(Math.min(anchor, n), Math.max(anchor, n));
          setHash(selLo, selHi);
        } else {
          anchor = n;
          applySel(n, n);
          setHash(n, n);
        }
      }
      gutter.addEventListener("click", onClick);
      code.addEventListener("click", onClick);
      document.addEventListener("keydown", function (e) {
        if (e.key === "Escape") deselect();
      });
      window.addEventListener("hashchange", function () {
        if (!selectFromHash(true)) deselect();
      });
      let rz = null;
      window.addEventListener("resize", function () {
        clearTimeout(rz);
        rz = setTimeout(measure, 100);
      });
      measure();
      selectFromHash(true);
    })();
    '''
  ++  txt
    (to-wain:format own)
  --
++  grab
  |%                                            ::  convert from
  ++  mime  |=([p=mite q=octs] q.q)
  ++  noun  @t                                  ::  clam from %noun
  ++  txt   of-wain:format
  --
++  grad  %txt
--
