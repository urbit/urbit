/@  htmx
/-  serv=server
/-  feather-icons
/*  date-now
/*  atom-input
/*  multiline-input
/*  a-i-r
/*  feather
/*  reset
/*  hawk-icon
/*  jquery
/*  htmx-js
/*  htmx-response-targets
/*  htmx-idiomorph
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
    %-  as-octt:mimes:html
    %+  welp  "<!DOCTYPE html>"
    (en-xml:html man)
  ::
  ++  render
    |=  [main=manx kids=marl]
    ;div
      ;+  main
      ;div
        ;*  kids
      ==
    ==
  ++  icon-url
    ^~
    %-  trip
    %^    cat
        3
      'data:image/png;base64,'
    %-  ~(en base64:mimes:html & |)
    (as-octs:mimes:html hawk-icon)
  ++  favicon
    ^~
    =;  m  m(a.g [[%href icon-url] a.g.m])
    ^-  manx
    ;link
      =rel  "icon"
      =type  "image/png"
      ;
    ==
  ++  manifest-url
    ^~
    %-  trip
    %^    cat
        3
      'data:application/json;utf-8,'
    %-  en:json:html
    %-  pairs:enjs:format
    :~
      ['name' s+'sky']
      ['description' s+'an urbit namespace viewer']
      ['start_url' s+'http://localhost/neo/sky']  ::  XX
      ['display' s+'standalone']
      ['background_color' s+'black']
      :+  'icons'  %a
      :~
        %-  pairs:enjs:format
        :~
          ['src' s+(crip icon-url)]
          ['sizes' s+'196x196']
          ['type' s+'image/png']
        ==
      ==
    ==
  ++  manifest
    ^~
    =;  m  m(a.g [[%href manifest-url] a.g.m])
    ^-  manx
    ;link
      =rel  "manifest"
      ;
    ==
  ++  htmx-extensions
    ::  htmx extension which encodes the request
    ::  as the serialized HTML of the calling element
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
        ;script: {(trip jquery)}
        ;script: {(trip htmx-js)}
        ;script: {(trip htmx-response-targets)}
        ;script: {(trip htmx-idiomorph)}
        ;script: {htmx-extensions}
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
        ::;style
        ::  ;+  ;/  %-  trip
        ::  '''
        ::  @font-face {
        ::    font-family: 'Urbit Sans';
        ::    src: url("https://media.urbit.org/fonts/UrbitSans/UrbitSansVFWeb-Regular.woff2") format("woff2");
        ::    font-style: normal;
        ::    font-weight: 100 700;
        ::  }
        ::  '''
        ::==
        ;style: {(trip reset)}
        ;style: {(trip feather)}
        ;script
          ;+  ;/
          """
          window.log=function()\{if(this.console)\{console.log(Array.prototype.slice.call(arguments));}};
          jQuery.fn.log=function (msg)\{console.log(msg, this); return this;};
          jQuery.fn.emit=function (name)\{(this[0]).dispatchEvent(new Event(name, \{ bubbles: true, cancelable: true, composed: true })); return this;};
          """
        ==
        ;script: {(trip a-i-r)}
        ;+  favicon
        ;+  manifest
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
    =.  our.bol  our.bowl
    =.  now.bol  now.bowl
    =.  eny.bol  eny.bowl
    =.  kids.bol  q.u.src
    ::  XX src.bowl
    (lift (!<(htmx q.pail.root) bol))
  --
--
