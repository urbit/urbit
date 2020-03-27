|=  [startup=json scripts=marl]
;html
  ;head
    ;title: Home
    ;meta(charset "utf-8");
    ;meta
      =name     "viewport"
      =content  "width=device-width, initial-scale=1, shrink-to-fit=no";
      ;link(rel "stylesheet", href "/~launch/css/index.css");
      ;link(rel "icon", type "image/png", href "/~launch/img/Favicon.png");
  ==
  ;body
    ;div#root;
    ;script@"/~channel/channel.js";
    ;script@"/~modulo/session.js";
    ;*  scripts
    ;script@"/~launch/js/index.js";
    ;script: window.startupMessage = {(en-json:html startup)}
  ==
==
