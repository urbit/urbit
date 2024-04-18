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
       button {
         border: none;
       }
       :host {
         display: grid;
         width: 100%;
         height: 100%;
         max-height: 100%;
         overflow: hidden;
         margin: 0;
         grid-template-columns: 230px auto;
         grid-template-rows: 50px auto;
         grid-template-areas:
         "btn main"
         "nav main";
       }
       :host(.closed) {
         grid-template-columns: 50px auto;
         grid-template-rows: 50px auto;
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
       main.s0 {
         grid-template-columns: 1fr;
         grid-template-rows: 1fr;
         grid-template-areas:
         ".";
       }
       main.s0 #s1,
       main.s0 #s2,
       main.s0 #s3,
       main.s0 #s4 {
         display: none;
       }
       main.s1 {
         grid-template-columns: 1fr;
         grid-template-rows: 1fr;
         grid-template-areas:
         "s1";
       }
       main.s1 #s1 {
         display: block;
       }
       main.s1 #s2,
       main.s1 #s3,
       main.s1 #s4 {
         display: none;
       }
       main.s2 {
         grid-template-columns: 1fr 1fr;
         grid-template-rows: 1fr;
         grid-template-areas:
         "s1 s2";
       }
       main.s2 #s1,
       main.s2 #s2 {
         display: block;
       }
       main.s2 #s3,
       main.s2 #s4 {
         display: none;
       }
       main.s3 {
         grid-template-columns: 1fr 1fr;
         grid-template-rows: 1fr 1fr;
         grid-template-areas:
         "s1 s2"
         "s1 s3";
       }
       main.s3 #s1,
       main.s3 #s2,
       main.s3 #s3 {
         display: block;
       }
       main.s3 #s4 {
         display: none;
       }
       main.s4 {
         grid-template-columns: 2fr 1fr 1fr;
         grid-template-rows: 1fr 1fr;
         grid-template-areas:
         "s1 s2 s2"
         "s1 s3 s4";
       }
       main.s4 #s1,
       main.s4 #s2,
       main.s4 #s3,
       main.s4 #s4 {
         display: block;
       }
       button {
         grid-area: btn;
       }
       main {
         display: grid;
         grid-area: main;
         overflow: hidden;
       }
       #s1, #s2, #s3, #s4 {
         overflow: auto;
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
       #s4 {
         grid-area: s4;
       }
       button {
         background-color: var(--b1);
         color: var(--f1);
         margin: 8px;
         border: 1px solid var(--f4);
         border-radius: 4px;
       }
       @media (max-width: 900px) {
         :host {
           grid-template-columns: auto;
           grid-template-rows: auto 50px;
           grid-template-areas:
           "main"
           "btn";
         }
         :host(.closed) {
           grid-template-columns: auto;
           grid-template-rows: auto 50px;
           grid-template-areas:
           "nav"
           "btn";
         }
         :host(.closed) main {
           display: none;
         }
         :host(:not(.closed)) main {
           display: grid;
           grid-template-columns: auto;
           grid-template-rows: auto;
           grid-template-areas:
           "s1";
         }
         #s1 {
           display: block;
         }
         main #s2 {
           display: none !important;
         }
         main #s3 {
           display: none !important;
         }
         main #s4 {
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
      <slot name="button">
        <button
          onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('sky-open', {bubbles:true, composed: true}))"
          >
          ~
        </button>
      </slot>
      <slot id="nav" name="nav"></slot>
      <main>
        <slot name="s1" id="s1"></slot>
        <slot name="s2" id="s2"></slot>
        <slot name="s3" id="s3"></slot>
        <slot name="s4" id="s4"></slot>
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
      this.qs("main").className = `s${this.hawks}`;
    }
  }
});
