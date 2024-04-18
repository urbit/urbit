customElements.define('multiline-input',
class extends HTMLTextAreaElement {
  static get observedAttributes() {
    return ['value'];
  }
  constructor() {
    super();
  }
  connectedCallback() {
    this.autocomplete = "off";
    this.style.resize = 'none';
    this.style.display = 'flex';

    this.mirrorValue();
    this.adjustSize();
    this.addEventListener("input", (e) => {
      this.mirrorValue();
      this.adjustSize();
    });
    setTimeout(() => {this.adjustSize()}, 100);
  }
  mirrorValue() {
    let v = this.value.trimEnd();
    this.setAttribute("value", v);
  }
  adjustSize() {
    this.style.height = 'min-content';
    this.style.height = (this.scrollHeight+3)+'px';
  }
}, { extends: "textarea" })
