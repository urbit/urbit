/@  order
/*  feather
/*  htmx-dom-enc
/*  htmx-response-targets
|%
::
::  types
::
++  schema
  ^-  lads:neo
  %-  ~(gas by *lads:neo)
  :~
    ::  /theme/css-variable-name=@ta/unit-suffix = variable-value=@t
    :-  [&/%theme |/%tas &/%value |]  [pro/%txt (sy %txt ~)]
    :-  [&/%theme |/%tas &/%unit |]  [pro/%txt (sy %txt ~)]
    ::
    ::  /window/session-name=@ta/open = @ud
    :-  [&/%window &/%sessions |]  [pro/%order (sy %order ~)]
    ::
    ::  /window/session-name=@ta/open = @ud
    :-  [&/%window |/%ta &/%open |]  [pro/%ud (sy %ud ~)]
    ::
    ::  /window/session-name=@ta/menu = ?
    :-  [&/%window |/%ta &/%menu |]  [pro/%flag (sy %flag ~)]
    ::
    ::  /window/session-name=@ta/order = (list pith)
    :-  [&/%window |/%ta &/%slots |]  [pro/%order (sy %order ~)]
    ::
    ::  /window/session-name=@ta/wid=@uv/here = pith
    :-  [&/%window |/%ta |/%uv &/%here |]  [pro/%pith (sy %pith ~)]
    ::
    ::  /window/session-name=@ta/wid=@uv/strategies = (list pith)
    :-  [&/%window |/%ta |/%uv &/%strategies |]  [pro/%order (sy %order ~)]
    ::
    ::  /strategy/~zod/... = (list pith)
    :-  [&/%strategy &]  [pro/%order (sy %order ~)]
    ::
  ==
::
::  backend
::
++  parse-url
  |=  =request:http
  ^-  [pax=path pam=(map @t @t)]
  =/  parsed
    %+  rash  url.request
    ;~  plug
        ;~(pfix fas (more fas smeg:de-purl:html))
        yque:de-purl:html
    ==
  :-  -.parsed
  (~(uni by (malt +.parsed)) (malt header-list.request))
++  parse-url-frfr
  |=  =request:http
  ^-  [pax=path pam=(map @t @t)]
  =/  parsed
    %+  rash  url.request
    ;~  plug
        ;~(pfix fas (more fas smeg:de-purl:html))
        yque:de-purl:html
    ==
  :-  -.parsed
  (~(uni by (malt +.parsed)) (malt header-list.request))
++  parse-body
  |=  =request:http
  ^-  manx
  %+  fall
    (de-xml:html q:(fall body.request [p=0 q='']))
  *manx
++  parse-form-body
  |=  =request:http
  ^-  (map @t @t)
  =/  body  q:(fall body.request [p=0 q=''])
  =/  form  (cat 3 '?' body)
  %-  malt
  ^-  (list [@t @t])
  %+  fall
    (rush form yque:de-purl:html)
  ~
::
::  frontend
::
++  old-standard-head-tags
  ^-  marl
  ;=
    ;script(src "https://unpkg.com/htmx.org@2.0.2");
    ;script: {(trip htmx-response-targets)}
    ;script: {(trip htmx-dom-enc)}
  ==
++  standard-head-tags
  ^-  marl
  ;=
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
    ;script(src "https://code.jquery.com/jquery-3.7.1.min.js");
    ;link
      =href  "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200"
      =rel  "stylesheet"
      ;
    ==
    ;script
      ;+  ;/
      """
      const sharedStyles = new CSSStyleSheet();
      sharedStyles.replaceSync(`{(trip feather)}`);
      document.adoptedStyleSheets = [sharedStyles];
      """
    ==
    ;script
      ;+  ;/  %-  trip
      '''
      window.log = function() {
        if (this.console) {
          console.log(Array.prototype.slice.call(arguments));
        }
      };
      jQuery.fn.log = function (msg) {
        if (msg) {
          console.log(msg, this);
        } else {
          console.log(this);
        }
        return this;
      };
      jQuery.fn.host = function () {
        let first = this[0];
        let h = first.getRootNode().host;
        return $(h);
      }
      jQuery.fn.emit = function (name, detail) {
        (this[0]).dispatchEvent(
          new CustomEvent(
            name,
            { detail, bubbles: true, cancelable: true, composed: true }
          )
        );
        return this;
      };
      jQuery.fn.poke = function (name, detail) {
        (this[0]).dispatchEvent(
          new CustomEvent(
            name,
            { detail, bubbles: false, cancelable: true, composed: true }
          )
        );
        return this;
      };
      function urbitTimestamp() {
        let now = new Date();
        let year = now.getFullYear();
        let month = now.getMonth() + 1;
        let date = now.getDate();
        let hour = String(now.getHours()).padStart(2, '0');
        let min = String(now.getMinutes()).padStart(2, '0');
        let sec = String(now.getSeconds()).padStart(2, '0');
        return `~${year}.${month}.${date}..${hour}.${min}.${sec}`;
      }
      '''
    ==
  ==
--
