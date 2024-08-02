customElements.define('wi-nd',
class extends HTMLElement {
  static get observedAttributes() {
    //
    return [
      "wid",
      "here",
      "searching",  // boolean. true is user is using the search bar in the header
      "tabs",       // currently unused. soon be space-separated list of iframe prefixes for each renderer
      "current",    // currently boolean. soon interpret to match prefix(tab)
      "dragging",
    ];
  }
  constructor() {
    //
    super();
    const shadow = this.attachShadow({ mode: 'open' });
    shadow.adoptedStyleSheets = [sharedStyles];
    this.shadowRoot.innerHTML = `
      <style>
       @import url('https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200');
       .mso,
       .material-symbols-outlined {
         font-family: 'Material Symbols Outlined';
         font-weight: normal;
         font-style: normal;
         font-size: 1em;
         line-height: 1;
         letter-spacing: normal;
         text-transform: none;
         display: inline-block;
         white-space: nowrap;
         word-wrap: normal;
         display: flex;
         flex-direction: column;
         align-items: center;
         justify-content: center;
         direction: ltr;
         -webkit-font-feature-settings: 'liga';
         -webkit-font-smoothing: antialiased;
         font-variation-settings:
           'FILL' 0,
           'wght' 400,
           'GRAD' 0,
           'opsz' 24;
       }
       :host {
         position: relative;
         display: flex;
         flex-direction: column;
         width: 100%;
         height: 100%;
         overflow: hidden;
         border-radius: 3px;
       }
       #drag-overlay {
         background: blue;
         opacity: 0%;
         width: 100%;
         height: 100%;
         position: absolute;
         top: 0;
         left: 0;
         z-index: 50;
       }
       :host(.dragging) #drag-overlay {
         opacity: 14%;
       }
       @media (max-width: 900px) {
         #axns {
           display: none;
         }
       }
      </style>
      <div id="drag-overlay" class="hidden"></div>
      <header class="b2 fr af js g1">
        <button class="p2 s-1 b2 br1 hover" id="tree-toggle"><span class="mso">sort</span></button>
        <div id="breadcrumbs" class="grow fr g1 af js"></div>
        <form id="searchbar" class="grow fr hidden">
          <input
            id="input-here"
            class="f0 grow b2 br1 p-1 s-1"
            style="outline: none;"
            autocomplete="off"
            spellcheck="false"
          />
        </form>
        <div id="axns" class="fr">
          <button
            class="p1 s-1 b2 hover br1 fc jc ac"
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('minimize'))"
            >
            <span class="mso">minimize</span>
          </button>
          <button
            class="p1 s-1 b2 hover br1 fc jc ac"
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('close'))"
            >
            <span class="mso">close</span>
          </button>
          <div
            class="p1 s-1 b2 grabber f4 fc jc ac"
            draggable="true"
            id="dragger"
            >
            <span class="mso">drag_indicator</span>
          </div>
        </div>
      </header>
      <div id="tabs" class="fc grow">
      </div>
    `
    this.intervalId = null;
  }
  connectedCallback() {
    $(this.gid('searchbar')).off();
    $(this.gid('searchbar')).on('submit', (e) => {
      e.preventDefault();
      this.setAttribute('here', $(this.gid('input-here')).val());
    });
    $(this.gid('input-here')).off();
    $(this.gid('input-here')).on('focusout', (e) => {
      $(this).removeAttr('searching');
    });
    $(this.gid('input-here')).on('blur', (e) => {
      $(this).removeAttr('searching');
    });
    $(this.gid('tree-toggle')).off();
    $(this.gid('tree-toggle')).on('click', (e) => {
      if ($(this).attr('current') === '/neo/tree') {
        $(this).attr('current', '/neo/hawk');
      } else {
        $(this).attr('current', '/neo/tree');
      }
    });

    $(this.gid('dragger')).off();
    $(this.gid('dragger')).on('dragstart', (e) => {
      e.originalEvent.dataTransfer.setData('text/plain', this.getAttribute('wid'));
    })
    $(this.gid('dragger')).on('dragenter', (e) => {
      $(this).emit('drag-start');
    })
    $(this.gid('dragger')).on('dragend', (e) => {
      $(this).emit('drag-end');
    })

    $(this).off();
    $(this).on('close', () => {
      $(this).emit('close-window');
    })
    $(this).on('minimize', () => {
      $(this).emit('minimize-window');
    })
    $(this).on('dragenter', (e) => {
      $(this).addClass('dragging');
    })
    $(this).on('dragover', (e) => {
      e.preventDefault();
    })
    $(this).on('dragleave', (e) => {
      $(this).removeClass('dragging');
    })
    $(this).on('drop', (e) => {
      e.preventDefault();
      $(this).emit('drag-end');
      let wid = e.originalEvent.dataTransfer.getData('text/plain');
      let wind = $(`[wid='${wid}']`);
      let newSlot = parseInt(this.getAttribute('slot').slice(1));
      let oldSlot = parseInt(wind.attr('slot')?.slice(1));
      if (!isNaN(oldSlot) && oldSlot < newSlot) {
        newSlot = newSlot + 0.5;
      } else {
        newSlot = newSlot - 0.5;
      }
      wind.attr('slot', `s${newSlot}`);
      $(this).emit('fix-slots');
    })
    this.setAttribute('wid', `${Date.now()}`);

    // poll iframes for changes every 350ms
    this.intervalId = setInterval(() => {
      let here = this.getAttribute('here');
      $(this.gid('tabs')).children().each(function() {
        this.contentWindow.postMessage({ messagetype: "sky-poll", here});
      });
    }, 350);

    $(this).on('iframe-moved', (e) => {
      this.prefixWhichChanged = e.detail.prefix;
      $(this).attr('here', e.detail.here);
    });
    $(this).on('set-feather-values', (e) => {
      $(this.gid('tabs')).children().each(function() {
        this.contentWindow.postMessage({
          messagetype: "feather-change",
          rules: e.detail
        });
      });
    });
  }
  disconnectedCallback() {
    if (this.intervalId !== null) {
      clearInterval(this.intervalId);
      this.intervalId = null;
    }
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
    if (name === "here") {
      let prefixes = ["/neo/hawk", "/neo/tree"];
      let keepPrefix = this.prefixWhichChanged;
      this.prefixWhichChanged = undefined;
      let rebuildPrefixes = prefixes.filter(p => p != keepPrefix);

      // remove non-changed iframes
      $(this.gid('tabs')).children().filter(function() {
        return rebuildPrefixes.includes($(this).attr('prefix'));
      }).remove();

      // rebuild non-changed iframes
      rebuildPrefixes.forEach(p => {
        let frame = this.createIframe(p, newValue, ($(this).attr('current') || '/neo/hawk') == p);
        $(this.gid('tabs')).append(frame);
      });
      this.buildBreadcrumbs();
      $(this.gid('input-here')).val(newValue);
      $(this).emit('here-moved');
    } else if (name === "searching") {
      if (newValue === null) {
        $(this.gid('breadcrumbs')).removeClass('hidden');
        $(this.gid('searchbar')).addClass('hidden');
      } else {
        $(this.gid('breadcrumbs')).addClass('hidden');
        $(this.gid('searchbar')).removeClass('hidden');
        this.gid('input-here').focus();
        this.gid('input-here').setSelectionRange(999,999);
      }
    } else if (name === "current") {
      if (newValue === null) {
        newValue = "/neo/hawk"
      }
      $(this.gid('tabs')).children().hide().filter(`[prefix='${newValue}']`).show();
    } else if (name === "dragging") {
      if (newValue === null) {
        $(this).removeClass('dragging');
        $(this.gid('drag-overlay')).addClass('hidden');
      } else {
        $(this.gid('drag-overlay')).removeClass('hidden');
      }
    }
  }
  qs(sel) {
    return this.shadowRoot.querySelector(sel);
  }
  gid(id) {
    return this.shadowRoot.getElementById(id);
  }
  get path() {
    let here = this.getAttribute("here") || "/";
    return here.slice(1).split("/").filter(s => !!s.trim().length);
  }
  createIframe(prefix, here, open) {
    let el = document.createElement('iframe');
    el.setAttribute('prefix', prefix);
    el.setAttribute('lazy', '');
    el.setAttribute('src', prefix+here);
    el.setAttribute('style', 'width: 100%; flex-grow: 1; border: none; background: var(--b0);');
    if (!open) {
      el.hidden = true;
    }
    el.addEventListener('load', () => {
      this.registerServiceWorker(el, prefix);
    });
    return el;
  }
  registerServiceWorker(iframe, prefix) {
    //  for convenience, this part is inject by wi-nd.
    //  in future, due to the need to sandbox the iframes,
    //  this must be provided by the iframe's contents.
    const iframeDoc = iframe.contentWindow.document;
    let wid = this.getAttribute('wid');
    const inlineScript = iframeDoc.createElement('script');
    inlineScript.textContent = `
      window.parent.postMessage(
        {
          messagetype: 'iframe-wants-feather',
          wid: '${wid}',
        }, '*'
      );
      window.addEventListener('message', (event) => {
        if (event.data?.messagetype === 'sky-poll') {
          let windowHere = event.data.here;
          if (!windowHere) {
            console.error('bad here', event.data);
            return;
          }
          let here = window.location.pathname.slice(${prefix.length});
          if (here != windowHere) {
            window.parent.postMessage({
              messagetype: 'sky-poll-response',
              wid: '${wid}',
              here: here,
              prefix: '${prefix}'
            }, '*');
          }
        }
        else if (event.data?.messagetype === 'feather-change') {
          event.data.rules.forEach(r => {
            document.documentElement.style
              .setProperty(
                '--'+r.variable,
                r.value+r.unit,
              );
          })
        }
      });
    `;
    iframeDoc.body.appendChild(inlineScript);
  }
  buildBreadcrumbs() {
    let breadcrumbs = $(this.gid('breadcrumbs'));
    breadcrumbs.children().remove();
    //
    this.path.forEach((p, i) => {
      let chevron = $(document.createElement('span'));
      chevron.addClass('s-2 f4 o6 fc ac jc no-select');
      chevron.text('›');
      breadcrumbs.append(chevron);
      //
      let crumb = $(document.createElement('button'));
      crumb.addClass((i === 0 ? 'p-1' : 'p1') + ' b2 hover br1 s-1 f2');
      crumb.text(i === 0 ? "/" : this.path[i]);
      crumb.on('click', () => {
        $(this).attr('here', "/"+this.path.slice(0, i+1).join("/"));
      });
      breadcrumbs.append(crumb);
    })
    let spacer = $(document.createElement('button'));
    spacer.addClass('grow b2 br1 hover')
    spacer.on('click', () => {
      $(this).attr('searching', '');
    });
    breadcrumbs.append(spacer);
  }
});
