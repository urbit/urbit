customElements.define('feather-settings',
class extends HTMLElement {
  static get observedAttributes() {
    //
    return [
    ];
  }
  constructor() {
    //
    super();
    const shadow = this.attachShadow({ mode: 'open' });
    shadow.adoptedStyleSheets = [sharedStyles];
    this.shadowRoot.innerHTML = `
      <style>
       :host {
         display: flex;
         flex-direction: column;
       }
      </style>
      <h1>Settings</h1>
      <div class="fc g2 p1">
        ${this.partSlider('Opacity', 'sky-opacity', null, 0.1, 1, 0.01)}
        ${this.partSlider('Outer gap size', 'sky-outer-gap', 'px', 0, 30, 1)}
        ${this.partSlider('Inner gap size', 'sky-inner-gap', 'px', 0, 30, 1)}
        ${this.partSlider('Base font size', '1in', 'px', 1, 8, 0.01)}
        ${this.partSlider('Monospaced font scale', 'mono-scale', 'px', 0.3, 1.7, 0.01)}
        ${this.partSlider('Letter spacing', 'letter-spacing', 'px', -1, 3, 0.001)}
        ${this.partSlider('Line height', 'line-height', null, 1, 2, 0.01)}
      </div>
    `
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
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
  }
  partSlider(label, variable, unit, min, max, step) {
    return `
<label class="fc g1">
  <span class="s-1 f2">${label}</span>
  <input
    class="wf"
    type="range"
    max="${max || 100}"
    min="${min || 0}"
    step="${step || 5}"
    oninput="this.getRootNode().host.setCss('${variable}', '${unit||''}', this.value)"
  />
</label>
    `
  }
  setCss(variable, unit, value) {
    console.log('setting css', variable, unit, value)
    document.documentElement.style
           .setProperty('--'+variable, `${value}${unit||''}`, 'important');
  }
});
