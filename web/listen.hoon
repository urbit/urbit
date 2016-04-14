::
::::  /hoon/listen/web
  ::
/?    310
;div.mini-module
  ;script@"/~/at/lib/js/urb.js";
  ;script@"https://cdn.rawgit.com/seatgeek/react-infinite/0.8.0/dist/react-infinite.js";
  ;script@"https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.11.2/moment-with-locales.js";
  ;script@"https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.1/moment-timezone.js";
  ;script@"/talk/main.js";
  ;link/"/talk/main.css"(rel "stylesheet");
  ;talk(readonly "", chrono "reverse", station "comments");
==
