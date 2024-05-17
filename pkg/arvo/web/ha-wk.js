customElements.define('ha-wk',
class extends HTMLElement {
  static get observedAttributes() {
    return ["stud", "here", "label", "tree-open"];
  }
  constructor() {
    //
    super();
    this.attachShadow({ mode: 'open' });
    this.shadowRoot.adoptedStyleSheets = [sharedStyles];
    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: flex;
          height: 100%;
          overflow: hidden;
          flex-direction: column;
          position: relative;
          font-size: 1rem !important;
          background-color: var(--b0);
        }
        section {
          position: sticky;
          z-index: 10;
          top: 0;
          left: 0;
          border: 2px solid var(--b3);
          padding: 8px;
          background-color: var(--b2);
        }
        #breadcrumbs {
          display: flex;
          flex-wrap: wrap;
          gap: 4px;
          flex-grow: 1;
          background-color: var(--b2);
        }
        @media (max-width: 900px) {
          #actions {
            display: none;
          }
        }
      </style>
      <template id="button-template">
        <button
          class="b2 hover p1 br1"
          hx-target="closest ha-wk"
          hx-swap="innerHTML"
          slot="crumbs"
          >
        </button>
      </template>
      <section id="section" class="fc g2">
        <nav class="frw g1 ac">
          <button
            id="tree-btn"
            class="b2 hover p1 br1 mono f2"
            onclick="this.getRootNode().host.toggleAttribute('tree-open')"
            slot="crumbs"
            >
            ${this.iconFiles()}
          </button>
          <button
            class="b2 hover p1 br1 mono f2"
            onclick="this.getRootNode().host.toggleMore(event)"
            slot="crumbs"
            >
            ${this.iconMore()}
          </button>
          <div id="breadcrumbs">
            <slot id="btns" name="crumbs"></slot>
          </div>
          <div id="actions" class="f2">
            <button
              class="b2 hover p1 br1 mono hidden"
              onclick="this.getRootNode().host.clone(event)"
              slot="crumbs"
              >
              c
            </button>
            <button
              class="b2 hover p1 br1 mono"
              onclick="this.getRootNode().host.raise(event)"
              slot="crumbs"
              >
              ${this.chevronLeft()}
            </button>
            <button
              class="b2 hover p1 br1 mono"
              onclick="this.getRootNode().host.drop(event)"
              slot="crumbs"
              >
              ${this.chevronRight()}
            </button>
            <button
              class="b2 hover p1 br1 mono"
              onclick="this.getRootNode().host.burry(event)"
              slot="crumbs"
              >
              ${this.iconClose()}
            </button>
            <button
              class="b2 hover p1 br1 mono hidden"
              onclick="this.getRootNode().host.suicide()"
              slot="crumbs"
              >
              X
            </button>
          </div>
        </nav>
        <aside id="aside" class="hidden p3 b3 fc g3 br1">
          <div class="frw g2">
            <button
                   onclick="this.getRootNode().host.inspect()"
                   class="b3 hover p1 br1 mono"
                   id="stud"
              ></button>
          </div>
        </aside>
          </div>
      </section>
      <div id="tree" class="hidden grow scroll-y scroll-x p2 b2">
        <slot name="tree" id="tree-slot">Tree view</slot>
      </div>
      <slot id="slot">Nothing here</slot>
    `;
  }
  connectedCallback() {
    //
    let treed = false;
    this.gid("slot").addEventListener("slotchange", (e) => {
      let nodes = e.target.assignedNodes();
      let btns = this.gid("breadcrumbs");
      this.gid("btns").assignedNodes().forEach(n => n.remove())
      if (nodes.length) {
        let wrapped = nodes[0];
        if (wrapped.hasAttribute("empty")) {
          this.setAttribute("tree-open", "")
        } else {
          this.removeAttribute("tree-open");
        }
        this.setAttribute("stud", wrapped.getAttribute("stud"));
        if (wrapped.hasAttribute("here")) {
          let here = wrapped.getAttribute("here");
          here = here === "/" ? "" : here;
          let label = wrapped.getAttribute("label");
          this.setAttribute("here", here);
          if (label) {
            this.setAttribute("label", label)
          } else {
            this.removeAttribute("label");
          }
          //
          let segments = here.split("/");
          segments.forEach((s, i) => {
            let btn = this.gid('button-template').content.cloneNode(true);
            btn = btn.querySelector('button');
            let dest = segments.slice(0, i+1).join("/")
            btn.setAttribute("hx-get", `/neo/hawk${dest}`)
            btn.textContent = s + "/";
            this.appendChild(btn);
          })
          let tree = document.createElement("div");
          let treeStub = document.createElement("div");
          treeStub.setAttribute("hx-get", `/neo/hawk${here}?tree`);
          treeStub.setAttribute("hx-target", `this`);
          treeStub.setAttribute("hx-trigger", `load`);
          treeStub.setAttribute("hx-swap", `outerHTML`);
          tree.setAttribute("slot", "tree");
          tree.appendChild(treeStub);
          this.tree?.remove();
          this.appendChild(tree);

          htmx.process(document.body);
        } else {
          this.removeAttribute("here");
          this.removeAttribute("label");
        }
      } else {
        this.setAttribute("tree-open", "");
      }
      const event = new CustomEvent('here-change', {composed: true});
      this.dispatchEvent(event);
    })
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
    if (name === "stud") {
      this.gid("stud").textContent = `%${newValue}`
    } else if (name === "here") {
      const event = new CustomEvent('true', {
        bubbles: true,
        composed: true,
      });
      this.dispatchEvent(event);
    } else if (name === "label") {
      const event = new CustomEvent('true', {
        bubbles: true,
        composed: true,
      });
      this.dispatchEvent(event);
    } else if (name === "tree-open") {
      let open = newValue !== null;
      if (open) {
        this.slotted.classList.add("hidden");
        /* this.gid("section").classList.add("grow"); */
        this.gid("tree").classList.remove("hidden");
        this.gid("tree-btn").classList.add("b3");
      } else {
        this.slotted.classList.remove("hidden");
        /* this.gid("section").classList.remove("grow"); */
        this.gid("tree").classList.add("hidden");
        this.gid("tree-btn").classList.remove("b3");
      }
    }
  }
  gid(id) {
    //
    return this.shadowRoot.getElementById(id);
  }
  get slotted() {
    return (this.gid("slot").assignedNodes() || [null])[0]
  }
  get tree() {
    return (this.gid("tree-slot").assignedNodes() || [null])[0]
  }
  inspect() {
    const event = new CustomEvent('inspect-hawk', {
      bubbles: true,
      composed: true,
      detail: {
        stud: this.getAttribute("stud")
      }
    });
    this.dispatchEvent(event);
  }
  toggleMore(e) {
    this.gid("aside").classList.toggle("hidden");
    e.target.classList.toggle("b3");
  }
  clone() {
    const event = new CustomEvent('clone-hawk', {
      bubbles: true,
      composed: true,
      detail: {
        here: this.getAttribute("here"),
        slot: this.getAttribute("slot"),
      }
    });
    this.dispatchEvent(event);
  }
  burry() {
    this.parentNode.insertAdjacentElement("beforeend", this);
    const event = new CustomEvent('cull', {
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }
  drop() {
    this.nextElementSibling?.insertAdjacentElement("afterend", this);
    const event = new CustomEvent('true', {
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }
  raise() {
    this.previousElementSibling?.insertAdjacentElement("beforebegin", this);
    const event = new CustomEvent('true', {
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }
  shoot() {
    this.parentNode.insertAdjacentElement("afterbegin", this);
    const event = new CustomEvent('true', {
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }
  suicide() {
    let par = this.parentNode;
    this.remove();
    const event = new CustomEvent('true', {
      bubbles: true,
      composed: true,
    });
    par.dispatchEvent(event);
  }
  chevronLeft() {
    return `
      <svg
          xmlns="http://www.w3.org/2000/svg"
          viewBox="0 -960 960 960"
          width="1rem"
          fill="currentColor"><path d="M560-240 320-480l240-240 56 56-184 184 184 184-56 56Z"/></svg>
    `
  }
  chevronRight() {
    return `
      <svg
          xmlns="http://www.w3.org/2000/svg"
          viewBox="0 -960 960 960"
          width="1rem"
          fill="currentColor"><path d="M504-480 320-664l56-56 240 240-240 240-56-56 184-184Z"/></svg>
    `
  }
  iconClose() {
    return `
      <svg
       xmlns="http://www.w3.org/2000/svg"
       viewBox="0 -960 960 960"
       width="1rem"
       fill="currentColor">
       <path d="m256-200-56-56 224-224-224-224 56-56 224 224 224-224 56 56-224 224 224 224-56 56-224-224-224 224Z"/></svg>
    `
  }
  iconMore() {
    return `
      <svg
       xmlns="http://www.w3.org/2000/svg"
       viewBox="0 -960 960 960"
       width="1rem"
       fill="currentColor">
       <path d="M480-160q-33 0-56.5-23.5T400-240q0-33 23.5-56.5T480-320q33 0 56.5
        23.5T560-240q0 33-23.5 56.5T480-160Zm0-240q-33 0-56.5-23.5T400-480q0-33 23.5-56.5T480-560q33
        0 56.5 23.5T560-480q0 33-23.5 56.5T480-400Zm0-240q-33 0-56.5-23.5T400-720q0-33 23.5-56.5T480-800q33
        0 56.5 23.5T560-720q0 33-23.5 56.5T480-640Z"/></svg>
    `
  }
  iconFiles() {
    return `
      <svg
       xmlns="http://www.w3.org/2000/svg"
       viewBox="0 -960 960 960"
       width="1rem"
       fill="currentColor">
       <path d="M120-240v-80h240v80H120Zm0-200v-80h480v80H120Zm0-200v-80h720v80H120Z"/></svg>
    `
  }
})
