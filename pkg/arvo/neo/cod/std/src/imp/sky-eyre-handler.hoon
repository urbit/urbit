/@  htmx
/-  serv=server
/*  date-now   
/*  atom-input  
/*  multiline-input 
/*  a-i-r
/*  feather
/*  reset

=>
  |%
  ++  main
    ^-  curb:neo
    [%or rol/[%ui-main pro/%htmx] pro/%htmx ~]
    :: rol/[%ui-main pro/%htmx]
  ++  kids-curb
    ^-  curb:neo
    any/~
  :: rol/[%ui-list pro/%htmx]
  ++  manx-to-octs
    |=  man=manx
    (as-octt:mimes:html (en-xml:html man))
  ::
  ++  render
    |=  [main=manx kids=marl]
    ;div
      ;+  main
      ;div
        ;*  kids
      ==
    ==
  ++  html-enc-js
    ::
    ::  htmx extension which encodes the request
    ::  as the serialized HTML of the calling element
    ::
    %-  trip
    '''
    htmx.defineExtension('html-enc', {
      onEvent: function (name, evt) {
        if (name === "htmx:configRequest") {
          evt.detail.headers['Content-Type'] = "text/html";
        }
      },
      encodeParameters : function(xhr, parameters, elt) {
        xhr.overrideMimeType('text/html');
        let xmls = new XMLSerializer();
        return (xmls.serializeToString(elt));
      }
    });
    Idiomorph.defaults.ignoreActive = true;
    Idiomorph.defaults.callbacks.beforeAttributeUpdated = (name, node, type) => {
      if (node.hasAttribute('morph-retain')) {
        let ribs = node.getAttribute('morph-retain').split(',').map(t => t.trim());
        if (ribs.includes(name)) {
          return false;
        }
      }
    }
    Idiomorph.defaults.callbacks.beforeNodeMorphed = (oldNode, newNode) => {
      if (oldNode?.nodeName !== "#text") {
        if (oldNode.hasAttribute('morph-no-swap') && oldNode.id === newNode.id) {
          return false;
        }
        else if (
          newNode.hasAttribute('morph-if-class') &&
          !oldNode.classList.contains(newNode.getAttribute('morph-if-class'))
        ) {
          return false;
        }
      }
    }
    '''
  ::
  ++  lift
    |=  in=manx
    ^-  manx
    ;html
      ;head
        ;meta(charset "UTF-8");
        ;title: s k y
        ;script(src "https://code.jquery.com/jquery-3.7.1.js");
        ;script(src "https://unpkg.com/htmx.org@1.9.11");
        ;script(src "https://unpkg.com/idiomorph/dist/idiomorph-ext.min.js");
        ;script(src "https://unpkg.com/htmx.org@1.9.11/dist/ext/response-targets.js");
        ;script: {html-enc-js}
        ;meta
          =name  "viewport"
          =content
            """
            width=device-width,
            initial-scale=1.0,
            maximum-scale=1.0
            """
          ;
        ==
        ;meta
          =name  "htmx-config"
          =content  (trip '{"ignoreTitle":"true"}')
          ;
        ==
        ;style
          ;+  ;/  %-  trip
          '''
          @font-face {
            font-family: 'Urbit Sans';
            src: url("https://media.urbit.org/fonts/UrbitSans/UrbitSansVFWeb-Regular.woff2") format("woff2");
            font-style: normal;
            font-weight: 100 700;
          }
          /*
          @font-face {
            font-family: 'Urbit';
            src: url('https://nyc3.digitaloceanspaces.com/drain/hawk/2024.4.10..21.47.28-urbit.ttf') format('truetype');
          }
          */
          '''
        ==
        ;style: {(trip feather)}
        ;style: {(trip reset)}
        ;script
          ;+  ;/
          """
          window.log=function()\{if(this.console)\{console.log(Array.prototype.slice.call(arguments));}};
          jQuery.fn.log=function (msg)\{console.log(msg, this); return this;};
          jQuery.fn.emit=function (name)\{(this[0]).dispatchEvent(new Event(name, \{ bubbles: true, cancelable: true, composed: true })); return this;};
          """
        ==
        ;script: {(trip a-i-r)}
        :: ;+  favicon
        :: ;+  manifest
      ==
      ;body
        =hx-ext  "html-enc,response-targets,morph"
        =hx-swap  "innerHTML"
        =hx-boost  "true"
        =hx-history  "false"
        =hx-replace-url  "/neo/sky"
        =hx-target  "closest .hawk"
        =style
          """
          background-color: var(--b1);
          background-image: var(--sky-bg-url);
          background-size: var(--sky-bg-size);
          background-repeat: var(--sky-bg-repeat);
          """
        ;+  in
      ==
    ==
  --
^-  kook:neo
|%
++  state  pro/%eyre-task
++  poke   *(set stud:neo)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  ~
++  deps  
  %-  ~(gas by *band:neo)
  :~  :-  %src
      ^-  fief:neo
      :-  req=|
      ^-  quay:neo
      :-  [main ~]
      ^-  (unit port:neo)
      :+  ~  %y
      %-  ~(gas by *lads:neo)
      :~  :-  &
          `lash:neo`[kids-curb ~]
      ==
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    `pail
  ++  init
    |=  pal=(unit pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    :_  [stud vase]
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =;  =manx
      =/  head=sign:eyre:neo  [eyre-id %head [200 [['content-type' 'text/html'] ~]]]
      =/  data=sign:eyre:neo  [eyre-id %data `(manx-to-octs manx)]
      =/  done=sign:eyre:neo  [eyre-id %done ~]
      :~  [pith %poke eyre-sign/!>(head)]
          [pith %poke eyre-sign/!>(data)]
          [pith %poke eyre-sign/!>(done)]
          [here.bowl %cull ~]
      ==
    ?~  src=(~(get by deps.bowl) %src)
      ;div: 404
    =/  root=idea:neo  (~(got of:neo q.u.src) /)
    ?>  =(%htmx p.pail.root)
    =/  bol  *bowl:neo
    =.  here.bol  p.u.src
    (lift (!<(htmx q.pail.root) bol))
  --
--

