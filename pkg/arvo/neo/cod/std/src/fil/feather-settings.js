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
         padding: 8px;
         gap: 40px;
       }
      </style>
      <h1>Settings</h1>
      <div class="fc g3">
        ${this.partSlider('Opacity', 'sky-opacity', null, 0.1, 1, 0.01)}
        ${this.partSlider('Outer gap size', 'sky-outer-gap', 'px', 0, 30, 1)}
        ${this.partSlider('Inner gap size', 'sky-inner-gap', 'px', 0, 30, 1)}
      </div>
      <div class="fc g4">
        ${this.partSlider('Base font size', '1in', 'px', 1, 8, 0.01)}
        ${this.partSlider('Monospaced font scale', 'mono-scale', 'px', 0.3, 1.7, 0.01)}
        ${this.partSlider('Letter spacing', 'letter-spacing', 'px', -1, 3, 0.001)}
        ${this.partSlider('Line height', 'line-height', null, 0.7, 2, 0.01)}
        ${this.partDropdown('Main font', 'font', this.mainFonts)}
        ${this.partDropdown('Monospaced font', 'font-mono', this.monoFonts)}
      </div>
      <div class="fc g1">
        <div class="fr g2">${this.partColor('b4', 'b4')}${this.partColor('f4', 'f4')}</div>
        <div class="fr g2">${this.partColor('b3', 'b3')}${this.partColor('f3', 'f3')}</div>
        <div class="fr g2">${this.partColor('b2', 'b2')}${this.partColor('f2', 'f2')}</div>
        <div class="fr g2">${this.partColor('b1', 'b1')}${this.partColor('f1', 'f1')}</div>
        <div class="fr g2">${this.partColor('b0', 'b0')}${this.partColor('f0', 'f0')}</div>
        <div class="fr g2">${this.partColor('b-1', 'b-1')}${this.partColor('f-1', 'f-1')}</div>
        <div class="fr g2">${this.partColor('b-2', 'b-2')}${this.partColor('f-2', 'f-2')}</div>
        <div class="fr g2">${this.partColor('b-3', 'b-3')}${this.partColor('f-3', 'f-3')}</div>
      </div>
      <div>
        <button
          class="p-1 br1 b1 bd1"
          onclick="$(this).host().poke('reset')"
          >
          Reset to defaults
        </button>
      </div>
    `
  }
  connectedCallback() {
    $(this).off();
    $(this).on('reset', () => {
      //
      localStorage.removeItem('feather-settings');
      document.documentElement.style = '';
      this.populateFromCurrent();
    });
    this.populateFromCurrent();
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
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
  get mainFonts() {
    return [
       "DM Sans"
      ,"Lato"
      ,"Georgia"
      ,"Optima"
      ,"DejaVu Sans"
      ,"Palatino"
      ,"Montserrat"
      ,"Verdana"
      ,"Gill Sans"
      ,"Helvetica Neue"
      ,"Open Sans"
      ,"Roboto"
      ,"Arial"
      ,"Futura"
      ,"PT Sans"
    ]
  }
  get monoFonts() {
    return [
       "Andale Mono"
      ,"Courier New"
      ,"Courier"
      ,"Monaco"
      ,"Spot Mono"
    ]
  }
  get currentFeatherRules() {
    //  returns array of {variable: '1in', unit: 'px', value: '4'}
    let rules = [
      { variable: 'b4', unit: ''},
      { variable: 'b3', unit: ''},
      { variable: 'b2', unit: ''},
      { variable: 'b1', unit: ''},
      { variable: 'b0', unit: ''},
      { variable: 'b-1', unit: ''},
      { variable: 'b-2', unit: ''},
      { variable: 'b-3', unit: ''},
      { variable: 'f4', unit: ''},
      { variable: 'f3', unit: ''},
      { variable: 'f2', unit: ''},
      { variable: 'f1', unit: ''},
      { variable: 'f0', unit: ''},
      { variable: 'f-1', unit: ''},
      { variable: 'f-2', unit: ''},
      { variable: 'f-3', unit: ''},
      { variable: 'font', unit: ''},
      { variable: 'font-mono', unit: ''},
      { variable: 'mono-scale', unit: ''},
      { variable: 'line-height', unit: ''},
      { variable: 'sky-opacity', unit: ''},
      { variable: 'letter-spacing', unit: 'px'},
      { variable: 'sky-outer-gap', unit: 'px'},
      { variable: 'sky-inner-gap', unit: 'px'},
      { variable: '1in', unit: 'px'},
    ];
    let docStyles = getComputedStyle(document.querySelector('s-k-y'));
    return rules.map(r => {
      let already = docStyles.getPropertyValue(`--${r.variable}`);
      let value = !!r.unit?.length ? already.slice(0, 0 - r.unit.length) :  already;
      return {value, ...r}
    })
  }
  partSlider(label, variable, unit, min, max, step) {
    return `
      <label class="fc g1">
        <span class="s-1 f2">${label}</span>
        <input
          id="in-${variable}"
          class="wf"
          type="range"
          max="${max || 100}"
          min="${min || 0}"
          step="${step || 5}"
          oninput="$(this).host().emit('feather-changed', [{
            variable:'${variable}',
            unit: '${unit||''}',
            value: this.value
          }])"
        />
      </label>
    `
  }
  partDropdown(label, variable, options) {
    return `
      <label class="fc g1">
        <span class="s-1 f2">${label}</span>
        <select
          id="in-${variable}"
          class="wf p1 br1 bd1"
          type="range"
          oninput="$(this).host().emit('feather-changed', [{
            variable:'${variable}',
            unit: '',
            value: this.value
          }])"
        >
          ${options.map(o => '<option value="' + o + '">' + o + '</option>')}
        </select>
      </label>
    `
  }
  partColor(label, variable) {
    return `
      <label class="fr g1 ac jc grow">
        <span class="s-1 f2 mono">${label}</span>
        <input
          id="in-${variable}"
          class="wf fc ac jc br1"
          type="color"
          oninput="$(this).host().emit('feather-changed', [{
            variable:'${variable}',
            unit: '',
            value: this.value
          }])"
        />
      </label>
    `
  }
  populateFromCurrent() {
    this.currentFeatherRules.forEach(r => {
      let el = this.gid(`in-${r.variable}`);
      if (el) {
        el.value = r.value;
      }
    });
  }
});
