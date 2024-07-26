/*  feather
/*  reset
/*  jquery
/*  htmx-js
/*  htmx-dom-enc
/*  htmx-response-targets
/*  htmx-idiomorph
/*  htmx-morph-config
/*  date-now
|%
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
      :: strip first 2 segments (/neo/hawk)
  :-  (slag 2 -.parsed)
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
    ;script: {(trip jquery)}
    ;script: {(trip htmx-js)}
    ;script: {(trip htmx-response-targets)}
    ;script: {(trip htmx-dom-enc)}
    ;script: {(trip htmx-idiomorph)}
    ;script: {(trip htmx-morph-config)}
    ;script: {(trip date-now)}
    ;link
      =href  "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200"
      =rel  "stylesheet"
      ;
    ==
    ;style: {(trip reset)}
    ;style: {(trip feather)}
    ;script
      ;+  ;/
      """
      const sharedStyles = new CSSStyleSheet();
      sharedStyles.replaceSync(`{(trip reset)}\0a{(trip feather)}`);
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
