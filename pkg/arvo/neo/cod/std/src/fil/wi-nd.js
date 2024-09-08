customElements.define('wi-nd',
class extends HTMLElement {
  static get observedAttributes() {
    //
    return [
      "wid",
      "here",
      "searching",  // boolean. true is user is using the search bar in the header
      "strategies", // space-separated list of iframe prefixes
      "renderer",    // current iframe strategy
      "menu",
      "dragging",
      "tab-title",
      "favicon",
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
         border: var(--sky-window-border, 1px) solid var(--b2);
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
        <button class="p2 s-1 b2 hover mono toggled f0" id="menu-toggle"></button>
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
      <div id="menu" class="b2 p3 hidden fc g3">
        <h1>menu</h1>
      </div>
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
      this.setAttribute('renderer', this.strategies[0]);
      this.rebuildIframe();
    });
    $(this.gid('input-here')).off();
    $(this.gid('input-here')).on('focusout', (e) => {
      $(this).removeAttr('searching');
    });
    $(this.gid('input-here')).on('blur', (e) => {
      $(this).removeAttr('searching');
    });
    $(this.gid('menu-toggle')).off();
    $(this.gid('menu-toggle')).on('click', (e) => {
      this.toggleAttribute('menu');
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
    this.buildMenu()

    // poll iframes for changes every 350ms
    this.intervalId = setInterval(() => {
      let here = this.getAttribute('here');
      let favicon = this.getAttribute('favicon');
      let tabTitle = this.getAttribute('tab-title');
      $(this.gid('tabs')).children().each(function() {
        this.contentWindow.postMessage({
          messagetype: "sky-poll",
          here,
          favicon,
          tabTitle,
        });
      });
    }, 350);

    $(this).on('title-changed', (e) => {
      if (!!e.detail) {
        $(this).attr('tab-title', e.detail);
      } else {
        $(this).attr('tab-title', null);
      }
      $(this).emit('here-moved');
    });
    $(this).on('favicon-changed', (e) => {
      if (!!e.detail) {
        $(this).attr('favicon', e.detail);
      } else {
        $(this).attr('favicon', null);
      }
      $(this).emit('here-moved');
    });
    $(this).on('iframe-moved', (e) => {
      $(this).attr('renderer', e.detail.prefix);
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
    $(this).on('reset-feather-values', (e) => {
      $(this.gid('tabs')).children().each(function() {
        this.contentWindow.postMessage({
          messagetype: "feather-reset",
        });
      });
    });
    $(this).on('bookmark-renderer', (e) => {
      this.setAttribute('strategies', (this.getAttribute('strategies') || '') + ' ' + e.detail);
      $(this).emit('strategy-change', this.strategyPoke);
    });
    $(this).on('unbookmark-renderer', (e) => {
      let newstrats = this.strategies.slice(0, -1).filter(s => s != e.detail);
      this.setAttribute('strategies', newstrats);
      $(this).emit('strategy-change', this.strategyPoke);
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
      this.setAttribute('strategies', (this.defaultStrategies[newValue] || []).join(' '))
      this.buildBreadcrumbs();
      this.buildMenu()
      $(this.gid('input-here')).val(newValue);
      $(this).emit('here-moved');
    }
    else if (name === "searching") {
      if (newValue === null) {
        $(this.gid('breadcrumbs')).removeClass('hidden');
        $(this.gid('searchbar')).addClass('hidden');
      } else {
        $(this.gid('breadcrumbs')).addClass('hidden');
        $(this.gid('searchbar')).removeClass('hidden');
        this.gid('input-here').focus();
        this.gid('input-here').setSelectionRange(999,999);
      }
    }
    else if (name === "renderer") {
      $(this.gid('menu-toggle')).text(this.prettyCurrent)
      if (oldValue !== newValue) {
        this.rebuildIframe();
      }
      this.buildMenu()
    }
    else if (name === "menu") {
      if (newValue === null) {
        $(this.gid('menu')).addClass('hidden');
        $(this.gid('menu-toggle')).removeClass('o7');
      } else {
        $(this.gid('menu')).removeClass('hidden');
        $(this.gid('menu-toggle')).addClass('o7');
      }
    }
    else if (name === "strategies") {
      this.buildMenu()
    }
    else if (name === "dragging") {
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
  get here() {
    return this.getAttribute("here") || "/";
  }
  get path() {
    return this.here.slice(1).split("/").filter(s => !!s.trim().length);
  }
  get defaultStrategies() {
    let strats = document.querySelector('s-k-y')?.getAttribute('default-strategies');
    return JSON.parse(strats || '{}');
  }
  get strategies() {
    const userStrategies = (this.getAttribute('strategies') || '')
      .split(' ')
      .map(m => m.trim())
      .filter(f => !!f);
  
    const uniqueStrategies = new Set([...userStrategies, '/hawk', '/tree', '/self']);
  
    return [...uniqueStrategies];
  }
  get strategyPoke() {
    let poke = {
      here: this.here,
      strategies: this.strategies.slice(0, -1)
    }
    return JSON.stringify(poke);
  }
  get renderer() {
    let c = this.getAttribute('renderer');
    return (c || this.strategies[0]);
  }
  get rendererLabels() {
    //
    //  this is assuming a naming structure that should
    //  not need to be assumed. fix this thix this fix this
    //  ... eventually
    //
    return {
      "/hawk": () => "hawk",
      "/tree": () => "tree",
      "/self": () => "self",
      //
      "/mast": (x) => {
        let words = x.split("/").map(s => s.trim()).filter(s => !!s);
        if (words.length != 2) {
          words = ["mast", "mast-error"];
        }
        return words[1].split('-').slice(1).join(' ');
      },
      //
      "/blue": (x) => {
        let words = x.split("/").map(s => s.trim()).filter(s => !!s);
        if (words.length != 2) {
          words = ["b", "b-error"];
        }
        return words[1].split('-').slice(1).join(' ');
      },
    }
  }
  labelLookup(renderer) {
    let entries = Object.entries(this.rendererLabels);
    let entry = entries.filter(([k, v]) => renderer.startsWith(k))[0];
    if (!entry) return;
    return entry[1](renderer);
  }
  get prettyCurrent() {
    let r = this.renderer;
    let m = this.labelLookup(r)
    if (m) {
      return m;
    }
    return r
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
  rebuildIframe() {
    $(this.gid('tabs')).children().remove();
    let frame = this.createIframe(this.renderer, this.here, true);
    $(this.gid('tabs')).append(frame);
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
          let here = window.location.pathname.slice(${prefix.length});
          if (here != windowHere) {
            window.parent.postMessage({
              messagetype: 'sky-poll-response',
              wid: '${wid}',
              here: here,
              prefix: '${prefix}'
            }, '*');
          }

          let windowFavicon = event.data.favicon || "";
          let faviconEl = document.querySelector('link[rel="icon"]');
          let favicon;
          if (faviconEl) {
            favicon = new URL(faviconEl.href, document.baseURI).href;
          } else {
            favicon = "";
          }
          if (favicon != windowFavicon) {
            window.parent.postMessage({
              messagetype: 'sky-poll-response-favicon',
              wid: '${wid}',
              favicon: favicon,
            }, '*');
          }

          let windowTitle = event.data.tabTitle || "";
          let title = document.title || "";
          if (title != windowTitle) {
            window.parent.postMessage({
              messagetype: 'sky-poll-response-title',
              wid: '${wid}',
              tabTitle: title,
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
        else if (event.data?.messagetype === 'feather-reset') {
          document.documentElement.style = '';
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
      if (i > 0) {
        chevron.text('›');
      }
      breadcrumbs.append(chevron);
      //
      let crumb = $(document.createElement('button'));
      crumb.addClass((i === 0 ? 'p-1' : 'p1') + ' b2 hover br1 s-1 f2');
      crumb.text((i === 0 && this.path[0].startsWith('~')) ? "/" : this.path[i]);
      crumb.on('click', () => {
        $(this).attr('here', "/"+this.path.slice(0, i+1).join("/"));
        $(this).attr('renderer', this.strategies[0]);
        this.rebuildIframe();
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
  buildMenu() {
    let menu = this.gid('menu');
    $(menu).children().remove();
    //
    /*let top = $(`
      <div class="fc g1">
        <span class="s-2 f3">renderer</span>
        <div class="fr g3 ac js">
          <h4></h4>
          <button
            id="bm-save-btn"
            class="p1 s-1 f2 br1 bd1 b1 wfc"
            >
            save
          </button>
          <button
            id="bm-del-btn"
            class="p1 s-1 f-3 br1 bd1 b1 wfc hidden"
            >
            unsave
          </button>
          <div class="grow"></div>
          <a
            href="${this.renderer}${this.here}"
            class="p-1 s-1 f0 br1 bd2 b2 wfc fr ac js g1"
            target="_blank"
            >
            <span>pop-out</span>
            <span class="mso">arrow_outward</span>
          </a>
        </div>
      </div>
    `);*/
    let top = $(`
      <div class="fc g1">
        <div class="fr g3 ac js">
          <div class="grow"></div>
          <a
            href="${this.renderer}${this.here}"
            class="p-1 s-1 f0 br1 bd2 b2 wfc fr ac js g1"
            target="_blank"
            >
            <span>pop-out</span>
            <span class="mso">arrow_outward</span>
          </a>
        </div>
      </div>
    `);
    /*$(top).find('h4').text(this.renderer);
    if (this.strategies.includes(this.renderer)) {
      $(top).find('#bm-save-btn').addClass('hidden')
    }
    $(top).find('#bm-save-btn').on('click', (e) => {
      $(this).emit('bookmark-renderer', this.renderer)
    });
    if (this.strategies.slice(0, -1).includes(this.renderer)) {
      $(top).find('#bm-del-btn').removeClass('hidden')
    }
    $(top).find('#bm-del-btn').on('click', (e) => {
      $(this).emit('unbookmark-renderer', this.renderer)
    });*/
    //menu.appendChild(top.get(0));
    //
    let bookmarks = $(`
      <div class="fc g1">
        <span class="s-2 f3">renderers</span>
        <div class="frw g2 ac js">
        </div>
      </div>
    `);
    //
    this.strategies.forEach(s => {
      let bookmark = $(`<button class="b1 br1 bd1 p-1 wfc"></button>`);
      bookmark.text(this.labelLookup(s) || s);
      $(bookmark).on('click', (e) => {
        $(this).attr('renderer', s)
      })
      if (s === this.renderer) {
        $(bookmark).addClass('toggled');
      }
      bookmarks.find('.frw').append(bookmark);
    })
    menu.appendChild(bookmarks.get(0));
    //
    let any = $(`
      <form class="fr g1 af js wf" onsubmit="event.preventDefault()">
        <input type="text" class="grow br1 bd1 p-1 b0 wf" autocomplete="off" required placeholder="/any/renderer" />
        <button class="p-1 br1 bd1 b1 hover">submit</button>
      </form>
    `);
    any.on('submit', (e) => {
      e.preventDefault();
      $(this).attr('renderer', any.find('input').val());
    })
    menu.appendChild(any.get(0));
  }
});
