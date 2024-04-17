customElements.define('atom-input',
class extends HTMLInputElement {
  static get observedAttributes() {
    return ['value'];
  }
  constructor() {
    super();
  }
  connectedCallback() {
    this.addEventListener("input", (e) => {
      this.mirrorValue();
    });
    this.mirrorValue();
  }
  mirrorValue() {
    if (this.type === 'date') {
      let d = this.value || "2000-01-01";
      let year = d.slice(0,4);
      let month = parseInt(d.slice(5,7));
      let date = parseInt(d.slice(8,10));
      this.setAttribute("value", `~${year}.${month}.${date}`);
    } else {
      this.setAttribute("value", this.value.trim());
    }
  }
}, { extends: "input" })
