customElements.define('s-k-y',
class extends HTMLElement {
  static get observedAttributes() {
    //
    return ["hawks"];
  }
  constructor() {
    //
    super();
    this.attachShadow({ mode: 'open' });
    this.shadowRoot.adoptedStyleSheets = [sharedStyles];
    this.shadowRoot.innerHTML = `
      <style>
       :host {
         width: 100%;
         height: 100%;
       }
      </style>
      <a-i-r class="wf hf">
        <nav slot="nav" class="wf hf scroll-y fc g2 p2">
          <button
            class="p2 tc hover br1 b1"
            onclick="this.getRootNode().host.newTab()"
            >
            +
          </button>
          <div class="fr g2">
            <button
              class="p1 br1 b1 grow hover"
              onclick="this.getRootNode().host.cull()"
              >
              -
            </button>
            <button
              class="p1 br1 b1 grow hover"
              onclick="this.getRootNode().host.grow()"
              >
              +
            </button>
          </div>
          <div id="tabs" class="fc g1 wf">
            <template id="tab-template">
              <div class="mono fr b1 br1" style="cursor: pointer;">
                <button
                  class="action p2 b1 hover br1 hidden"
                >+</button>
                <button
                  class="action p2 b1 hover grow tl br1"
                ></button>
                <button
                  class="action p2 hover br1"
                >x</button>
              </div>
            </template>
          </div>
        </nav>
        <slot slot="s1" name="s1"></slot>
        <slot slot="s2" name="s2"></slot>
        <slot slot="s3" name="s3"></slot>
        <slot slot="s4" name="s4"></slot>
        <slot style="display: none;"></slot>
      </a-i-r>
    `;
  }
  connectedCallback() {
    //
    if (this.isMobile) {
      this.className = "fc grow wf af"
    } else {
      this.className = "fr wf hf"
    }
    this.gid("s0")?.addEventListener("slotchange", () => {this.reactSlot("s0")});
    this.gid("s1")?.addEventListener("slotchange", () => {this.reactSlot("s1")});
    this.gid("s2")?.addEventListener("slotchange", () => {this.reactSlot("s2")});
    this.gid("s3")?.addEventListener("slotchange", () => {this.reactSlot("s3")});
    this.gid("default")?.addEventListener("slotchange", (e) => this.reactDefault(e));
    this.addEventListener("here-change", (e) => {
      this.updateTabs();
      this.syncTabs();
    })
    this.addEventListener("cull", (e) => {
      this.cull();
      this.updateTabs();
    });
    this.addEventListener("true", (e) => {
      this.trueSlots();
      this.updateTabs();
      this.syncTabs();
    });
    this.addEventListener("grow", (e) => {
      this.grow();
      this.updateTabs();
    });
    this.addEventListener("clone-hawk", (e) => {
      let here = e.detail.here;
      let slot = e.detail.slot;
      this.newTab(`/neo/hawk${here}`, slot);
      this.updateTabs();
    })
    this.addEventListener("inspect-hawk", (e) => {
      let stud = e.detail.stud
      if (stud) {
        this.newTab(`/neo/hawk/src/std/imp/${stud}`);
      }
      this.updateTabs();
    })
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
    /* if (name === "open") { */
    /* this.gid("aside").classList.toggle("hidden"); */
    /* if (this.isMobile) { */
    /* if (newValue === null) { */
    /* this.gid("cs0").classList.remove("hidden"); */
    /* } else { */
    /* this.gid("cs0").classList.add("hidden"); */
    /* } */
    /* } else { */
    /* this.gid("cs0").classList.remove("hidden"); */
    /* } */
    /* } else if (name === "hawks") { */
    if (name === "hawks") {
      this.qs("a-i-r").setAttribute("hawks", newValue);
      this.trueSlots();
      this.updateTabs();
    }
  }
  get isMobile() {
    return (window.innerWidth < 900)
  }
  get hawks() {
    return parseInt(this.getAttribute("hawks") || "0");
  }
  get slottedHawks() {
    return [...this.childNodes].filter(c => c.nodeName === 'HA-WK');
  }
  qs(sel) {
    return this.shadowRoot.querySelector(sel);
  }
  gid(id) {
    //
    return this.shadowRoot.getElementById(id);
  }
  slotted(id) {
    //
    return (this.gid(id)?.assignedElements() || [null])[0];
  }
  trueSlots() {
    //
    let kids = [...this.childNodes].filter(c => c.nodeName === 'HA-WK');
    let hawks = parseInt(this.getAttribute("hawks") || "1");
    kids.forEach((k, i) => {
      if (i < hawks) {
        k.setAttribute("slot", `s${i+1}`)
      } else  {
        k.removeAttribute("slot");
      }
    })
    this.updateTabs();
  }
  reactSlot(id) {
    //
    let s = this.slotted(id);
    let c = this.gid("c"+id);
    let slot = parseInt(id.slice(1));
    if (s && slot <= this.hawks) {
      // display hawk
      s.classList.add("b0");
      s.classList.add("fc");
      s.classList.add("js");
      s.classList.add("scroll-y");
      //
      s.classList.add("grow");
      c.classList.add("grow");
      //
      s.classList.add("basis-half");
      c.classList.add("basis-half");
      //
      s.classList.remove("basis-none");
      c.classList.remove("basis-none");
      //
      s.classList.remove('hidden');
      c.classList.remove('hidden');
    } else {
      // hide hawk
      if (s) {
        s.classList.remove("grow");
        s.classList.add("basis-none");
        s.classList.remove("basis-half");
        s.classList.add('hidden');
      }
      c.classList.remove("grow");
      c.classList.add("basis-none");
      c.classList.remove("basis-half");
      c.classList.add('hidden');
    }
    this.updateTabs();
  }
  reactDefault(e) {
    //
    this.updateTabs();
  }
  updateTabs() {
    //
    let frames = [...this.childNodes].filter(c => c.nodeName === 'HA-WK');
    let tabs = this.gid("tabs");
    tabs.querySelectorAll('div').forEach(x => x.remove());
    frames.forEach((f, i) => {
      let here = f.getAttribute('here');
      let label = f.getAttribute('label');
      let open = f.hasAttribute('slot');
      let tab = this.gid('tab-template').content.cloneNode(true);
      tab = tab.querySelector('div');
      if (open) {
        tab.classList.add('active');
      }
      let add = tab?.firstElementChild;
      let sel = add.nextElementSibling;
      let del = sel.nextElementSibling;
      sel.textContent = label || here || "/";
      sel.addEventListener('click', (e) => {
        e.preventDefault();
        this.insertAdjacentElement("afterbegin", f);
        this.trueSlots();
      })
      del.addEventListener('click', (e) => {
        e.preventDefault();
        f.suicide();
      })
      tabs.appendChild(tab);
    })
  }
  newTab(here, slot) {
    //
    let hawk = document.createElement("ha-wk");
    let stub = document.createElement("div");
    stub.setAttribute("hx-get", here || "/neo/hawk/home");
    stub.setAttribute("hx-target", "this");
    stub.setAttribute("hx-swap", "outerHTML");
    stub.setAttribute("hx-trigger", "load");
    hawk.appendChild(stub);
    hawk.setAttribute("here", "");
    hawk.setAttribute("stud", "txt");
    hawk.setAttribute("label", "Home");
    let s = this.slotted(slot)
    if (s) {
      s.insertAdjacentElement("beforebegin", hawk);
    } else {
      this.insertAdjacentElement("afterbegin", hawk);
    }
    this.grow();
    htmx.process(document.body);
    this.updateTabs();
    this.syncTabs();
  }
  cull() {
    this.setAttribute("hawks", Math.max(0, this.hawks - 1));
    this.trueSlots();
    this.syncTabs();
  }
  grow() {
    this.setAttribute("hawks", Math.min(4, this.slottedHawks.length, this.hawks + 1));
    this.trueSlots();
    this.syncTabs();
  }
  async syncTabs() {
    let frames = [...this.childNodes].filter(c => c.nodeName === 'HA-WK');
    let forms = frames.map(f => {
      return `
      <p here="${f.getAttribute('here')}"></p>
      `
    })
    await fetch(`/neo/hawk/sky?stud=sky`, {
      method: 'POST',
      headers: {'content-type': 'text/html'},
      body: `<div slots="${this.hawks}">${forms.join("")}</div>`
    })
  }
});
