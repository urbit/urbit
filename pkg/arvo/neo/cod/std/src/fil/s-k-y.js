customElements.define('s-k-y',
class extends HTMLElement {
  static get observedAttributes() {
    //
    return [
      "our",
      "open",
      "windows-open",
      "default-strategies",
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
       * {
         box-sizing: border-box;
       }
       :host {
         width: 100%;
         height: 100%;
         max-height: 100%;
         overflow: hidden;
         margin: 0;
         position: relative;
         opacity: var(--sky-opacity, 1);
         padding: var(--sky-outer-gap, 8px);
         background-color: var(--b1);

         display: grid;
         grid-template-columns: 50px auto;
         grid-template-rows: auto 1fr;
         grid-template-areas:
         "tray main"
         "tray main";
       }
       :host(.open) {
         grid-template-columns: 320px auto;
         grid-template-rows: auto 1fr;
         grid-template-areas:
         "tray main"
         "nav main";
       }
       :host(.open) #nav {
         display: flex;
       }
       #nav {
         grid-area: nav;
         display: none;
         flex-direction: column;
         justify-content: flex-start;
         align-items: stretch;
         gap: 12px;
         overflow: auto;
       }
       :host(.open) #tray {
         display: none;
       }
       #tray {
         grid-area: tray;
         display: flex;
         padding-bottom: 15px;
       }
       /*
        *  grid display
        *
        */
       main {
         display: grid;
         grid-area: main;
         overflow: hidden;
         padding-left: var(--sky-inner-gap, 8px);
       }
       #s0, #s1, #s2, #s3 {
         overflow: auto;
       }
       #button {
         grid-area: btn;
         height: fit-content;
       }
       #s-none {
         grid-area: void;
       }
       #s0 {
         grid-area: s0;
       }
       #s1 {
         grid-area: s1;
       }
       #s2 {
         grid-area: s2;
       }
       #s3 {
         grid-area: s3;
       }
       main.open-0 {
         grid-template-columns: 1fr;
         grid-template-rows: 1fr;
         grid-template-areas:
         "void";
       }
       main #s-none {
         display: none;
       }
       main.open-0 #s-none {
         display: flex;
       }
       main.open-0 #s0,
       main.open-0 #s1,
       main.open-0 #s2,
       main.open-0 #s3 {
         display: none;
       }
       main.open-1 {
         grid-template-columns: 1fr;
         grid-template-rows: 1fr;
         grid-template-areas:
         "s0";
       }
       main.open-1 #s0 {
         display: block;
       }
       main.open-1 #s1,
       main.open-1 #s2,
       main.open-1 #s3 {
         display: none;
       }
       main.open-2 {
         grid-template-columns: 1fr 1fr;
         grid-template-rows: 1fr;
         grid-template-areas:
         "s0 s1";
       }
       main.open-2 #s0,
       main.open-2 #s1 {
         display: block;
       }
       main.open-2 #s2,
       main.open-2 #s3 {
         display: none;
       }
       main.open-3 {
         grid-template-columns: 1fr 1fr;
         grid-template-rows: 1fr 1fr;
         grid-template-areas:
         "s0 s1"
         "s0 s2";
       }
       main.open-3 #s0,
       main.open-3 #s1,
       main.open-3 #s2 {
         display: block;
       }
       main.open-3 #s3 {
         display: none;
       }
       main.open-4 {
         grid-template-columns: 2fr 1fr 1fr;
         grid-template-rows: 1fr 1fr;
         grid-template-areas:
         "s0 s1 s1"
         "s0 s2 s3";
       }
       main.open-4 #s0,
       main.open-4 #s1,
       main.open-4 #s2,
       main.open-4 #s3 {
         display: block;
       }
       /*
        *  gaps
        *
        */
       main.open-1 #s0 {
         padding-right: 0;
       }
       main.open-2 #s0,
       main.open-3 #s0,
       main.open-4 #s0 {
         padding-right: var(--sky-inner-gap, 8px);
       }
       main.open-1 #s1,
       main.open-2 #s1 {
         padding-bottom: 0;
       }
       main.open-3 #s1,
       main.open-4 #s1 {
         padding-bottom: var(--sky-inner-gap, 8px);
       }
       main.open-1 #s2,
       main.open-2 #s2,
       main.open-3 #s2 {
         padding-right: 0;
       }
       main.open-4 #s2 {
         padding-right: var(--sky-inner-gap, 8px);
       }
       /*
        *  mobile
        *
        */
       @media (max-width: 900px) {
         :host {
           grid-template-columns: auto;
           grid-template-rows: 1fr auto;
           grid-template-areas:
           "main"
           "tray";
           padding: 0 !important;
         }
         :host(.open) {
           grid-template-columns: auto;
           grid-template-rows: 1fr auto;
           grid-template-areas:
           "nav"
           "tray";
         }
         :host(.open) main {
           display: none;
         }
         :host(:not(.open)) main {
           display: grid;
           grid-template-columns: auto;
           grid-template-rows: auto;
           grid-template-areas:
           "s0";
         }
         :host(.open) #tray,
         :host(:not(.open)) #tray {
           display: flex;
           flex-direction: row;
           padding: 6px 6px 10px 6px;
         }
         #tray .hideable {
           display: none;
         }
         #nav {
           padding: 8px;
         }
         #nav .hideable {
           display: none;
         }
         main {
           padding: 0;
         }
         main #s0 {
           display: block;
           padding: 0;
           padding-right: 0 !important;
         }
         main #s1 {
           display: none !important;
         }
         main #s2 {
           display: none !important;
         }
         main #s3 {
           display: none !important;
         }
       }
      </style>
      <div id="tray" class="fc g2 js af">
        <button
          class="br1 p1 b2 hover fc js ac grow"
          onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('sky-open', {bubbles:true, composed: true}))"
          >
          <span class="p1 s-1 bold">~</span>
        </button>
        <button
          class="p2 br1 bd1 b3 hover f3"
          onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('toggle-notifications'))"
          >
          <span class="mso">notifications</span>
        </button>
        <button
          class="p2 br1 bd1 b3 hover f3 hidden"
          onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('toggle-settings'))"
          >
          <span class="mso">settings</span>
        </button>
      </div>
      <nav id="nav" class="fc" style="padding-bottom: 15px;">
        <div id="tab-controller" class="fc g3 grow">
          <button
            class="br1 p1 b2 hover fc jc ac hideable"
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('sky-open', {bubbles:true, composed: true}))"
            >
            <span class="p1 s-1 bold">~</span>
          </button>
          <div class="fc g3 grow scroll-y">
            <button
              onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('new-window'))"
              class="wfc p2 br1 bd1 b2 hover fr g3 ac"
              >
              <span class="mso">add</span>
              <span class="f3">new window</span>
            </button>
            <div id="tabs" class="fc g2"></div>
            <div class="grow"></div>
            <div class="bd1 br1 p2 fc g1 o6">
              <p class="bold f-3">This is a developer alpha.</p>
              <p class="s-1">
                Your data in this app is NOT private from the rest of the network
                and will NOT persist across upgrades.
              </p>
            </div>
          </div>
          <footer class="fc g2">
            <button
              onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('toggle-notifications'))"
              class="p2 br1 bd1 b3 hover fr g3 ac f3 hideable"
              >
              <span class="mso">notifications</span>
              notifications
            </button>
            <div class="fr g2">
              <button
                onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('toggle-settings'))"
                class="p2 br1 bd1 b3 hover fr g3 ac f3 hideable grow"
                >
                <span class="mso">settings</span>
                settings
              </button>
              <button
                onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('toggle-help'))"
                class="p2 br1 bd1 b3 hover fr g3 ac f3 hideable"
                >
                <span class="mso">question_mark</span>
              </button>
            </div>
          </footer>
        </div>
        <div id="notifications" class="p2 fc g3 grow scroll-y hidden">
          <button
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('toggle-notifications'))"
            class="p2 br1 bd1 b3 hover fr g3 ac f3"
            >
            <span class="mso">close</span>
            close
          </button>
          <h1>Notifications</h1>
          <slot name="notifications">
            <div class="f3 wf hf fc jc ac">Nothing to see right now</div>
          </slot>
        </div>
        <div id="settings" class="p2 fc g3 grow scroll-y hidden">
          <button
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('toggle-settings'))"
            class="p2 br1 bd1 b3 hover fr g3 ac f3"
            >
            <span class="mso">close</span>
            close
          </button>
          <slot name="theme"></slot>
        </div>
        <div id="help" class="p2 fc g3 grow scroll-y hidden">
          <button
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('toggle-help'))"
            class="p2 br1 bd1 b3 hover fr g3 ac f3"
            >
            <span class="mso">close</span>
            close
          </button>
          <div class="prose">
            <h1>Help</h1>
            <details class="br1 bd1">
              <summary class="br1 b2 hover p2">Install Sky as a PWA</summary>
              <div class="p2">
                <h3>Desktop</h3>
                <ol>
                  <li>Open this page in Chrome</li>
                  <li>Click the "Install" button that appears in the searchbar.</li>
                </ol>
              </div>
            </details>
          </div>
        </div>
      </nav>
      <main>
        <slot name="s-none" id="s-none">
          <div class="wf hf b0 br1 fc ac jc f4">no windows open</div>
        </slot>
        <slot name="s0" id="s0"></slot>
        <slot name="s1" id="s1"></slot>
        <slot name="s2" id="s2"></slot>
        <slot name="s3" id="s3"></slot>
      </main>
      <slot id="default" style="display: none;"></slot>
    `
  }
  get windowsOpen() {
    return parseInt(this.getAttribute("windows-open") || "0");
  }
  get our() {
    return this.getAttribute('our');
  }
  get currentFeatherRules() {
    return this.qs('feather-settings')?.currentFeatherRules || [];
  }
  get windows() {
    let slots = $(this).children('wi-nd[slot]').get().toSorted((a, b) => {
      let aSlot = parseFloat(a.getAttribute('slot').slice(1));
      let bSlot = parseFloat(b.getAttribute('slot').slice(1));
      if (aSlot > bSlot) return 1;
      if (aSlot < bSlot) return -1;
      return 0;
    });
    let noslots = $(this).children('wi-nd:not([slot])').get();
    return [...slots, ...noslots];
  }
  qs(sel) {
    return this.shadowRoot.querySelector(sel);
  }
  qsa(sel) {
    return this.shadowRoot.querySelectorAll(sel);
  }
  gid(id) {
    return this.shadowRoot.getElementById(id);
  }
  connectedCallback() {
    $(this).off();
    $(this).on("sky-open", (e) => {
      this.toggleAttribute("open");
      $(this).poke('save-layout');
    })
    $(this).on('fix-slots', () => {
      this.fixSlots();
    })
    $(this).on('new-window', (e) => {
      let wind = document.createElement('wi-nd');
      let here = `/${this.our}/home`;
      $(wind).attr('here', here);
      $(wind).attr('renderer', this.chooseStrategy(here));
      $(wind).attr('slot', `s-1`);
      $(this).append(wind);
      this.growFlock();
      this.fixSlots();
    })
    $(this).on('close-window', (e) => {
      let wind = $(e.target);
      if (wind.attr('slot') != undefined) {
        this.shrinkFlock();
      }
      wind.remove();
      this.fixSlots();
      this.renderTabs();
    })
    $(this).on('minimize-window', (e) => {
      let wind = $(e.target);
      if (wind.attr('slot') != undefined) {
        wind.removeAttr('slot');
        this.shrinkFlock();
      }
      this.fixSlots();
      this.renderTabs();
    })
    $(this).on('maximize-window', (e) => {
      let wind = $(e.target);
      if (!wind.attr('slot')) {
        this.growFlock();
      }
      wind.attr('slot', 's-1');
      this.fixSlots();
      this.renderTabs();
    })
    $(this).on('drag-start', (e) => {
      $(this.windows).attr('dragging', '');
    })
    $(this).on('drag-end', (e) => {
      $(this.windows).removeAttr('dragging');
    })
    $(this).on('here-moved', () => {
      this.renderTabs();
    })
    $(this.gid('s0')).off();
    $(this.gid('s0')).on('slotchange', (e) => {
      this.renderTabs();
    });
    $(this.gid('s1')).off();
    $(this.gid('s1')).on('slotchange', () => {
      this.renderTabs();
    });
    $(this.gid('s2')).off();
    $(this.gid('s2')).on('slotchange', () => {
      this.renderTabs();
    });
    $(this.gid('s3')).off();
    $(this.gid('s3')).on('slotchange', () => {
      this.renderTabs();
    });
    $(this.gid('default')).off();
    $(this.gid('default')).on('slotchange', () => {
      this.renderTabs();
    });

    $(this.gid('nav')).off();
    $(this.gid('nav')).on('dragover', (e) => {
      e.preventDefault();
    })
    $(this.gid('nav')).on('drop', (e) => {
      e.preventDefault();
      let wid = e.originalEvent.dataTransfer.getData('text/plain');
      let wind = $(`[wid='${wid}']`);
      wind.poke('minimize');
    });
    //
    $(this).on("toggle-notifications", () => {
      if (this.getAttribute('open') === 'notifications') {
        this.setAttribute('open', '');
      } else {
        this.setAttribute('open', 'notifications');
      }
    })
    $(this).on("toggle-settings", () => {
      if (this.getAttribute('open') === 'settings') {
        this.setAttribute('open', '');
      } else {
        this.setAttribute('open', 'settings');
      }
    })
    $(this).on("toggle-help", () => {
      if (this.getAttribute('open') === 'help') {
        this.setAttribute('open', '');
      } else {
        this.setAttribute('open', 'help');
      }
    })
    $(this).on("save-layout", () => {
      this.saveLayout();
    });
    this.qs("main").className = `open-${this.windowsOpen}`;
    this.restoreLayout();
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
    if (name === "open") {
      $(this.gid('notifications')).addClass('hidden');
      $(this.gid('settings')).addClass('hidden');
      $(this.gid('help')).addClass('hidden');
      $(this.gid('tab-controller')).addClass('hidden');
      if (newValue ===  null) {
        $(this).removeClass("open");
      } else if (newValue === 'notifications') {
        $(this).addClass("open");
        $(this.gid('notifications')).removeClass('hidden');
      } else if (newValue === 'settings') {
        $(this).addClass("open");
        $(this.gid('settings')).removeClass('hidden');
      } else if (newValue === 'help') {
        $(this).addClass("open");
        $(this.gid('help')).removeClass('hidden');
      } else {
        $(this).addClass("open");
        $(this.gid('tab-controller')).removeClass('hidden');
      }
    } else if (name === "windows-open") {
      this.qs("main").className = `open-${this.windowsOpen}`;
    }
  }
  get defaultStrategies() {
    let strats = this.getAttribute('default-strategies')
    return JSON.parse(strats || '{}');
  }
  chooseStrategy(here) {
    let strats = this.defaultStrategies;
    let strat = strats[here] || ['/self'];
    return strat[0];
  }
  renderIcon(name) {
    let s = document.createElement('span');
    s.className = 'mso';
    s.textContent = name;
    return s;
  }
  renderTabs() {
    let tabs = $(this.gid('tabs'));
    tabs.children().remove();
    let windowsOpen = this.windowsOpen;
    let that = this;
    $(this.windows).each(function(i) {
      let wind = this;
      let tab = document.createElement('div');
      $(tab).addClass('b2 br1 fr af js bd1');
      if (i < windowsOpen) {
        $(tab).addClass('toggled');
      }

      let mux = document.createElement('button');
      mux.className = "b2 hover br1 bd0 p2 grow tl fr g2 ac js"
      mux.style = "overflow: hidden; white-space: nowrap; text-overflow: ellipsis; text-align: left;"
      let im = wind.getAttribute('favicon') ? `
        <img src="${wind.getAttribute('favicon')}" style="width: 20px; height: 20px;" />
        ` : ``;
      mux.innerHTML = `
        ${im}
        <span style="overflow: hidden; white-space: nowrap; text-overflow: ellipsis; text-align: left;">
          ${$(wind).attr('tab-title') || $(wind).attr('here')}
        </span>
      `;
      let max = $(mux);
      $(max).on('click', () => {
        $(wind).emit('maximize-window');
      });

      let min = document.createElement('button');
      $(min).append(that.renderIcon('minimize'));
      $(min).addClass('b2 hover br1 bd0 p1 f3');
      $(min).on('click', () => {
        $(wind).emit('minimize-window');
      });
      if (i >= windowsOpen) {
        $(min).hide();
      }

      let close = document.createElement('button');
      $(close).append(that.renderIcon('close'));
      $(close).addClass('b2 hover br1 bd0 p1 f3');
      $(close).on('click', () => {
        $(wind).emit('close-window');
      });

      $(tab).append(max);
      $(tab).append(min);
      $(tab).append(close);
      tabs.append(tab);
    })
    $(this).poke('save-layout');
  }
  fixSlots() {
    let slotted = $(this.windows).filter('[slot]').get().slice(0, 3);
    $(this.windows).removeAttr('slot');
    slotted.forEach((s, i) => {
      s.setAttribute('slot', `s${i}`);
    })
  }
  growFlock() {
    $(this).attr('windows-open', Math.min(3, this.windowsOpen + 1));
  }
  shrinkFlock() {
    $(this).attr('windows-open', Math.max(0, this.windowsOpen - 1));
  }
  saveLayout() {
    let layout = {
      open: this.hasAttribute('open'),
      windowsOpen: parseInt(this.getAttribute('windows-open')),
      windows: $(this).children('wi-nd').get().map(w => {
        return {
          here: w.getAttribute('here'),
          slot: w.getAttribute('slot'),
          strategies: w.getAttribute('strategies'),
          renderer: w.getAttribute('renderer'),
        }
      })
    }
    localStorage.setItem('sky-layout', JSON.stringify(layout))
  }
  restoreLayout() {
    let layoutString = localStorage.getItem('sky-layout');
    if (!!layoutString) {
      let layout = JSON.parse(layoutString);
      $(this).attr('open', layout.open ? '' : null);
      $(this).attr('windows-open', `${layout.windowsOpen}`);
      $(this).children('wi-nd').remove();
      layout.windows.forEach(w => {
        let wind = document.createElement('wi-nd');
        $(wind).attr('here', w.here);
        $(wind).attr('renderer', w.renderer);
        $(wind).attr('strategies', w.strategies);
        $(wind).attr('slot', !!w.slot ? w.slot : null);
        $(this).append(wind);
      })
    } else {
      // create initial layout
      let layout = {
        open: false,
        windowsOpen: 1,
        windows: [
          {
            here: `/${this.our}/home`,
            renderer: `/hawk`,
            strategies: ``,
            slot: 's0'
          }
        ]
      }
      localStorage.setItem('sky-layout', JSON.stringify(layout))
      this.restoreLayout();
    }
  }
});
