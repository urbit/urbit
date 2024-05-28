customElements.define('date-now',
class extends HTMLElement {
  static get observedAttributes() {
    return ['value'];
  }
  constructor() {
    super();
  }
  connectedCallback() {
    setInterval(() => {
      let now = new Date();
      let year = now.getFullYear();
      let month = now.getMonth() + 1;
      let date = now.getDate();
      let hour = String(now.getHours()).padStart(2, '0');
      let min = String(now.getMinutes()).padStart(2, '0');
      let sec = String(now.getSeconds()).padStart(2, '0');
      this.setAttribute(
        "value",
        `~${year}.${month}.${date}..${hour}.${min}.${sec}`
      )
    }, 1000);
  }
});
