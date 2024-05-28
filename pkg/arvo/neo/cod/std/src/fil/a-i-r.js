customElements.define('a-i-r',
class extends HTMLElement {
  static get observedAttributes() {
    //
    return ["closed", "hawks"];
  }
  constructor() {
    //
    super();
    this.attachShadow({ mode: 'open' });
    this.shadowRoot.innerHTML = `
      <style>
       * {
         box-sizing: border-box;
       }
       ::slotted(*) {
         overflow: auto;
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
         "btn main"
         "nav main";
       }
       :host(.closed) {
         grid-template-columns: 50px auto;
         grid-template-rows: auto 1fr;
         grid-template-areas:
         "btn main"
         ". main";
       }
       :host(.closed) #nav {
         display: none;
       }
       #nav {
         grid-area: nav;
         display: flex;
         flex-direction: column;
         justify-content: flex-start;
         align-items: center;
         gap: 12px;
         overflow: auto;
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
           "btn";
         }
         :host(.closed) {
           grid-template-columns: auto;
           grid-template-rows: 1fr auto;
           grid-template-areas:
           "nav"
           "btn";
         }
         button {
           height: min-content;
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
         #nav {
           --rem: 24px;
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
      <slot name="button" id="button">
        <button
          style="border: 2px solid var(--b3);"
          onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('sky-open', {bubbles:true, composed: true}))"
          >
          ~
        </button>
      </slot>
      <slot id="nav" name="nav"></slot>
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
  qs(sel) {
    return this.shadowRoot.querySelector(sel);
  }
  gid(id) {
    return this.shadowRoot.getElementById(id);
  }
  connectedCallback() {
    this.addEventListener("sky-open", (e) => {
      this.toggleAttribute("closed");
    })
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
    if (name === "closed") {
      this.classList.toggle("closed");
    } else if (name === "hawks") {
      this.qs("main").className = `open-${this.hawks}`;
    }
  }
});
