customElements.define('wi-nd',
class extends HTMLElement {
  static get observedAttributes() {
    //
    return [
      "wid",
      "here",
      "searching",  // boolean. true is user is using the search bar in the header
      "tabs",    // currently unused. soon be space-separated list of iframe prefixes for each renderer
      "current", // currently boolean. soon interpret to match prefix(tab)
      "dragging",
    ];
  }
  constructor() {
    //
    super();
    const shadow = this.attachShadow({ mode: 'open' });
    shadow.adoptedStyleSheets = [sharedStyles];
    this.shadowRoot.innerHTML = `
      <style>
       @import url('https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200');
       .mso,
       .material-symbols-outlined {
         font-family: 'Material Symbols Outlined';
         font-weight: normal;
         font-style: normal;
         font-size: 1em;
         line-height: 1;
         letter-spacing: normal;
         text-transform: none;
         display: inline-block;
         white-space: nowrap;
         word-wrap: normal;
         display: flex;
         flex-direction: column;
         align-items: center;
         justify-content: center;
         direction: ltr;
         -webkit-font-feature-settings: 'liga';
         -webkit-font-smoothing: antialiased;
         font-variation-settings:
           'FILL' 0,
           'wght' 400,
           'GRAD' 0,
           'opsz' 24;
       }
       :host {
         position: relative;
         display: flex;
         flex-direction: column;
         width: 100%;
         height: 100%;
         overflow: hidden;
         border-radius: 3px;
       }
       #drag-overlay {
         background: blue;
         opacity: 0%;
         width: 100%;
         height: 100%;
         position: absolute;
         top: 0;
         left: 0;
         z-index: 50;
       }
       :host(.dragging) #drag-overlay {
         opacity: 14%;
       }
       @media (max-width: 900px) {
         #axns {
           display: none;
         }
       }
      </style>
      <div hidden id="drag-overlay"></div>
      <header class="b2 p1 fr ac js g2">
        <button class="p-1 s-1 b3 br1 hover" id="tree-toggle"><span class="mso">sort</span></button>
        <div id="breadcrumbs" class="grow fr g1 af js"></div>
        <form id="searchbar" class="grow fr hidden">
          <input id="input-here" class="f2 grow b0 br1 p-1 s-1"/>
        </form>
        <div id="axns" class="fr g2">
          <button
            class="p1 s-1 b2 hover br1"
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('minimize'))"
            >
            <span class="mso">minimize</span>
          </button>
          <button
            class="p1 s-1 b2 hover br1"
            onclick="this.getRootNode().host.dispatchEvent(new CustomEvent('close'))"
            >
            <span class="mso">close</span>
          </button>
          <div
            class="p1 s-1 b2 grabber f3"
            draggable="true"
            id="dragger"
            >
            <span class="mso">drag_indicator</span>
          </div>
        </div>
      </header>
      <div id="tabs" class="fc grow">
      </div>
    `
  }
  connectedCallback() {
    $(this.gid('searchbar')).off();
    $(this.gid('searchbar')).on('submit', (e) => {
      e.preventDefault();
      this.setAttribute('here', $(this.gid('input-here')).val());
    });
    $(this.gid('input-here')).off();
    $(this.gid('input-here')).on('focusout', (e) => {
      $(this).removeAttr('searching');
    });
    $(this.gid('input-here')).on('blur', (e) => {
      $(this).removeAttr('searching');
    });
    $(this.gid('tree-toggle')).off();
    $(this.gid('tree-toggle')).on('click', (e) => {
      this.toggleAttribute('current');
    });

    $(this.gid('dragger')).off();
    $(this.gid('dragger')).on('dragstart', (e) => {
      $(this).emit('drag-start');
      e.originalEvent.dataTransfer.setData('text/plain', this.getAttribute('wid'));
    })
    $(this.gid('dragger')).on('dragend', () => {
      $(this).emit('drag-end');
    })

    $(this).off();
    $(this).on('close', () => {
      $(this).emit('close-window');
    })
    $(this).on('minimize', () => {
      $(this).emit('minimize-window');
    })
    $(this).on('dragenter', (e) => {
      $(this).addClass('dragging');
    })
    $(this).on('dragover', (e) => {
      e.preventDefault();
    })
    $(this).on('dragleave', (e) => {
      $(this).removeClass('dragging');
    })
    $(this).on('drop', (e) => {
      e.preventDefault();
      $(this).removeClass('dragging');
      $(this).emit('drag-end');
      let wid = e.originalEvent.dataTransfer.getData('text/plain');
      let wind = $(`[wid='${wid}']`);
      let newSlot = parseInt(this.getAttribute('slot').slice(1));
      let oldSlot = parseInt(wind.attr('slot')?.slice(1));
      if (!isNaN(oldSlot) && oldSlot < newSlot) {
        newSlot = newSlot + 0.5;
      } else {
        newSlot = newSlot - 0.5;
      }
      wind.attr('slot', `s${newSlot}`);
      $(this).emit('fix-slots');
    })
    this.setAttribute('wid', `${Date.now()}`);
  }
  attributeChangedCallback(name, oldValue, newValue) {
    //
    if (name === "here") {
      if (oldValue !== newValue) {
        let tabs = $(this.gid('tabs'));
        tabs.children().remove();
        let hawk = this.createIframe('/neo/hawk', newValue, $(this).attr('current') != undefined);
        let tree = this.createIframe('/neo/tree', newValue, $(this).attr('current') == undefined);
        tabs.append(hawk);
        tabs.append(tree);
        this.buildBreadcrumbs();
        $(this.gid('input-here')).val(newValue);
        $(this).emit('here-moved');
      }
    } else if (name === "searching") {
      if (newValue === null) {
       $(this.gid('breadcrumbs')).removeClass('hidden');
       $(this.gid('searchbar')).addClass('hidden');
      } else {
       $(this.gid('breadcrumbs')).addClass('hidden');
       $(this.gid('searchbar')).removeClass('hidden');
       this.gid('input-here').focus();
      }
    } else if (name === "current") {
      if (newValue === null) {
        $(this.gid('tabs')).children(`[prefix='/neo/hawk']`).show()
        $(this.gid('tabs')).children(`[prefix='/neo/tree']`).hide();
        $(this.gid('tree-toggle')).removeClass('toggled');
      } else {
        $(this.gid('tabs')).children(`[prefix='/neo/hawk']`).hide()
        $(this.gid('tabs')).children(`[prefix='/neo/tree']`).show();
        $(this.gid('tree-toggle')).addClass('toggled');
      }
    } else if (name === "dragging") {
      if (newValue === null) {
        $(this).removeClass('dragging');
        $(this.gid('drag-overlay')).hide();
      } else {
        $(this.gid('drag-overlay')).show();
      }
    }
  }
  qs(sel) {
    return this.shadowRoot.querySelector(sel);
  }
  gid(id) {
    return this.shadowRoot.getElementById(id);
  }
  get path() {
    let here = this.getAttribute("here") || "/";
    return here.slice(1).split("/").filter(s => !!s.trim().length);
  }
  createIframe(prefix, here, hidden) {
    let el = document.createElement('iframe');
    el.setAttribute('prefix', prefix);
    el.setAttribute('lazy', '');
    el.setAttribute('src', prefix+here);
    el.setAttribute('style', 'width: 100%; flex-grow: 1; border: none; background: var(--b0);');
    if (hidden) {
      el.hidden = true;
    }
    el.addEventListener('load', () => {
      this.registerServiceWorker(el, prefix);
    });
    return el;
  }
  registerServiceWorker(iframe, prefix) {
    const iframeWindow = iframe.contentWindow;
    const iframeDoc = iframeWindow.document;
    let wid = this.getAttribute('wid');
    let pre = prefix.length;
    const inlineScript = iframeDoc.createElement('script');
    inlineScript.textContent = `
      function notifySky() {
        window.parent.postMessage({wid: '${wid}', path: window.location.pathname.slice(${pre})}, '*');
      }
      window.addEventListener('beforeunload', function (e) {
        notifySky();
      });
      window.addEventListener('htmx:beforeHistorySave', function (e) {
        notifySky();
      });
      window.addEventListener('htmx:beforeRequest', function (e) {
        notifySky();
      });
      window.addEventListener('htmx:afterSwap', function (e) {
        notifySky();
      });
    `;
    iframeDoc.body.appendChild(inlineScript);
  }
  buildBreadcrumbs() {
    let breadcrumbs = $(this.gid('breadcrumbs'));
    breadcrumbs.children().remove();
    //
    this.path.forEach((p, i) => {
      let chevron = $(document.createElement('span'));
      chevron.addClass('s-2 f4 o6 fc ac jc');
      chevron.text('›');
      breadcrumbs.append(chevron);
      //
      let crumb = $(document.createElement('button'));
      crumb.addClass((i === 0 ? 'p-1' : 'p1') + ' b2 hover br1 s-1 f2');
      crumb.text(i === 0 ? "/" : this.path[i]);
      crumb.on('click', () => {
        $(this).attr('here', "/"+this.path.slice(0, i+1).join("/"));
      });
      breadcrumbs.append(crumb);
    })
    let spacer = $(document.createElement('button'));
    spacer.addClass('grow b2 br1 hover')
    spacer.on('click', () => {
      $(this).attr('searching', '');
    });
    breadcrumbs.append(spacer);
  }
});
