customElements.define('error-tray',
class extends HTMLElement {
  static get observedAttributes() {
    return ["open"];
  }
  constructor() {
    //
    super();
    this.attachShadow({ mode: 'open' });
    this.shadowRoot.adoptedStyleSheets = [sharedStyles];
    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: none;
          flex-flow: column nowrap;
          width: 100%;
          height: 100%;
          background-color: var(--b1);
          align-items: center;
          justify-content: center;
          top: 0;
          left: 0;
          opacity: 0.95;
        }
      </style>
      <div
        class="p3 wf hf fc g2 scroll-y"
        style="max-width: 400px; max-height: 400px;"
        >
        <slot id="slot"></slot>
        <button
          class="b2 br1 p2 hover"
          onclick="this.getRootNode().host.removeAttribute('open')"
          >
          close
        </button>
      </div>
    `;
  }
  connectedCallback() {
    this.gid("slot").addEventListener("slotchange", (e) => {
      if (this.slotted("slot")) {
        this.setAttribute("open", "");
      } else {
        this.removeAttribute("open");
      }
    })
  }
  attributeChangedCallback(name, oldValue, newValue) {
    if (name === "open") {
      this.style.display = newValue === null ? 'none' : 'flex';
    }
  }
  slotted(id) {
    //
    return (this.gid(id)?.assignedElements() || [null])[0];
  }
  gid(id) {
    //
    return this.shadowRoot.getElementById(id);
  }
});
