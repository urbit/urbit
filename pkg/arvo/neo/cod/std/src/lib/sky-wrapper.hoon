/@  sky
/@  sky-settings
/*  date-now
/*  s-k-y
/*  wi-nd
/*  feather
/*  reset
/*  hawk-icon
/*  jquery
/*  htmx-js
/*  htmx-response-targets
/*  htmx-idiomorph
/*  eye
|_  =bowl:neo
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
::
++  lift
  |=  [in=manx]
  ^-  manx
  ;html
    ;head
      ;meta(charset "UTF-8");
      ;title: s k y
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
      ;script: {(trip htmx-idiomorph)}
      ;link
        =href  "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200"
        =rel  "stylesheet"
        ;
      ==
      ;meta
        =name  "htmx-config"
        =content  (trip '{"ignoreTitle":"true"}')
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
        jQuery.fn.emit = function (name) {
          (this[0]).dispatchEvent(
            new Event(
              name,
              { bubbles: true, cancelable: true, composed: true }
            )
          );
          return this;
        };
        jQuery.fn.poke = function (name) {
          (this[0]).dispatchEvent(
            new Event(
              name,
              { bubbles: false, cancelable: true, composed: true }
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
        function urbitOur() {
          return document.body.getAttribute('our');
        }
        window.addEventListener('message', function(event) {
          let wid = event.data.wid;
          let path = event.data.path;
          document.querySelector(`[wid='${wid}']`)?.setAttribute('here', path);
        });
        '''
      ==
      ;script: {(trip s-k-y)}
      ;script: {(trip wi-nd)}
      ;script: {(trip date-now)}
      ;+  favicon
      ;+  manifest
    ==
    ;body
      =hx-ext  "html-enc,response-targets,morph"
      =hx-swap  "outerHTML"
      =hx-boost  "true"
      =hx-history  "false"
      =hx-replace-url  "/neo/sky"
      ;+  in
    ==
  ==
--
