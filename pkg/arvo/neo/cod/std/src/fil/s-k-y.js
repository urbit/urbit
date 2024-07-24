customElements.define('s-k-y',
class extends HTMLElement {
  static get observedAttributes() {
    //
    return ["our", "closed", "hawks"];
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
       ::slotted(*) {
         /* overflow: auto; */
       }
       :host {
         display: grid;
         width: 100%;
         height: 100%;
         max-height: 100%;
         overflow: hidden;
         margin: 0;
         grid-template-columns: 230px auto;
         grid-template-rows: auto 1fr;
         grid-template-areas:
         "tray main"
         "nav main";
       }
       :host(.closed) {
         grid-template-columns: 50px auto;
         grid-template-rows: auto 1fr;
         grid-template-areas:
         "tray main"
         "tray main";
       }
       :host(.closed) #nav {
         display: none;
       }
       #nav {
         grid-area: nav;
         display: flex;
         flex-direction: column;
         justify-content: flex-start;
         align-items: stretch;
         gap: 12px;
         overflow: auto;
       }
       :host(.closed) #tray {
         display: flex;
       }
       #tray {
         grid-area: tray;
         display: none;
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
         padding-left: var(--sky-inner-gap);
       }
       #s0, #s1, #s2, #s3 {
         overflow: auto;
       }
       #button {
         grid-area: btn;
         height: fit-content;
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
         ".";
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
         padding-right: var(--sky-inner-gap);
       }
       main.open-1 #s1,
       main.open-2 #s1 {
         padding-bottom: 0;
       }
       main.open-3 #s1,
       main.open-4 #s1 {
         padding-bottom: var(--sky-inner-gap);
       }
       main.open-1 #s2,
       main.open-2 #s2,
       main.open-3 #s2 {
         padding-right: 0;
       }
       main.open-4 #s2 {
         padding-right: var(--sky-inner-gap);
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
         :host(.closed) {
           grid-template-columns: auto;
           grid-template-rows: 1fr auto;
           grid-template-areas:
           "nav"
           "tray";
         }
         :host(.closed) main {
           display: none;
         }
         :host(:not(.closed)) main {
           display: grid;
           grid-template-columns: auto;
           grid-template-rows: auto;
           grid-template-areas:
           "s0";
         }
         :host(.closed) #tray,
         :host(:not(.closed)) #tray {
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
         :host(.closed) #nav {
           display: flex;
         }
         :host(:not(.closed)) #nav {
           display: none;
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
        <button class="p2 br1 bd1 b2 hover o7 hideable toggled">1</button>
        <button class="p2 br1 bd1 b2 hover o7 hideable">2</button>
        <button class="p2 br1 bd1 b2 hover o7 hideable">3</button>
        <button class="p2 br1 bd1 b3 hover f3"><span class="mso">settings</span></button>
        <button class="p2 br1 bd1 b3 hover f3"><span class="mso">notifications</span></button>
      </div>
      <nav id="nav" class="fc g8" style="padding-bottom: 15px;">
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
        </div>
        <footer class="fc g2">
          <select class="fr p2 br1 bd1 b0">
            <option>workspace 1</option>
            <option>workspace 2</option>
            <option>workspace 3</option>
          </select>
          <button
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('open-settings'))"
            class="p2 br1 bd1 b3 hover fr g3 ac f3 hideable"
            >
            <span class="mso">settings</span>
            settings
          </button>
          <button
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('open-settings'))"
            class="p2 br1 bd1 b3 hover fr g3 ac f3 hideable"
            >
            <span class="mso">notifications</span>
            notifications
          </button>
        </footer>
      </nav>
      <main>
        <slot name="s0" id="s0"></slot>
        <slot name="s1" id="s1"></slot>
        <slot name="s2" id="s2"></slot>
        <slot name="s3" id="s3"></slot>
      </main>
      <slot id="default" style="display: none;"></slot>
    `
  }
  get hawks() {
    return parseInt(this.getAttribute("hawks") || "0");
  }
  get our() {
    return this.getAttribute('our');
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
  gid(id) {
    return this.shadowRoot.getElementById(id);
  }
  connectedCallback() {
    $(this).off();
    $(this).on("sky-open", (e) => {
      this.toggleAttribute("closed");
    })
    $(this).on('fix-slots', () => {
      this.fixSlots();
    })
    $(this).on('new-window', () => {
      let wind = document.createElement('wi-nd');
      $(wind).attr('here', `/${this.our}/home`);
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
      $(this.gid('nav')).addClass('active');
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
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
    if (name === "closed") {
      this.classList.toggle("closed");
    } else if (name === "hawks") {
      this.qs("main").className = `open-${this.hawks}`;
    }
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
    let hawks = this.hawks;
    let that = this;
    $(this.windows).each(function(i) {
      let wind = this;
      let tab = document.createElement('div');
      $(tab).addClass('b2 br1 fr ac js g1 bd1');
      if (i < hawks) {
        $(tab).addClass('toggled');
      }

      let max = document.createElement('button');
      $(max).text($(wind).attr('here'));
      $(max).addClass('b2 hover br1 bd0 p2 grow tl');
      max.style = 'overflow: hidden; white-space: nowrap; text-overflow: ellipsis; text-align: left;';
      $(max).on('click', () => {
        $(wind).emit('maximize-window');
      });

      let min = document.createElement('button');
      $(min).append(that.renderIcon('minimize'));
      $(min).addClass('b2 hover br1 bd0 p1 f3');
      $(min).on('click', () => {
        $(wind).emit('minimize-window');
      });
      if (i >= hawks) {
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
  }
  fixSlots() {
    let slotted = $(this.windows).filter('[slot]').get().slice(0, 3);
    $(this.windows).removeAttr('slot');
    slotted.forEach((s, i) => {
      s.setAttribute('slot', `s${i}`);
    })
  }
  growFlock() {
    $(this).attr('hawks', Math.min(3, this.hawks + 1));
  }
  shrinkFlock() {
    $(this).attr('hawks', Math.max(0, this.hawks - 1));
  }
});
