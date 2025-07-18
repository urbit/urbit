::  Initial help message
::
::::  /hoon/start/gen
  ::
:-  %say
|=  [[now=@da * our=@p *] *]
:-  %tang
:~
:+  %rose
["" "" ""]
=/  =hart:eyre
  .^  hart:eyre
      %e
      /(scot %p our)/host/(scot %da now)
  ==
=/  our-url
  ;:  welp
      "http"
      ?:(p.hart "s" "")
      "://"
      (trip -.p.r.hart)
      ?~(q.hart "" (welp ":" (a-co:co u.q.hart)))
  ==
=/  fake-bars
  :~  "  |mount %base"
      "  |hi {<our>} 'hello {<our>}'"
      :(welp "  |eyre/cors/approve '" our-url "'")
  ==
=/  real-bars
  :~  "  |public %base"
      "  |hi ~zod 'hello ~zod'"
      "  |install ~paldev %pals"
  ==
%-  turn
:_  |=(=tape leaf+tape)
%-  zing
:~  :~  "Welcome to Urbit. This command-line interface is the Dojo. You can use it"
        "to control and operate your Urbit ship, or you can use the web interface"
        "if you prefer (as long as this process is running)."
        (welp "Log in at " our-url)
        ""
        "Try some commands out now:"
        "  ."
        "  our"
        "  now"
        "You can run simple scripts in /base/gen with '+':"
        "  +code"
        "  +vats"
        "  +cat /=base=/gen/start/hoon"
        "Or run a system-changing script in /base/gen/hood with '|':"
    ==
    ?:  .^(? %j /(scot %p our)/fake/(scot %da now))
      fake-bars
    real-bars
    :~  ""
        "For more help on generators and threads, type:"
        "  +help"
        ""
        "To close this process and shut down your ship gracefully, type:"
        "  |exit"
        ""
        "Learn more at https://docs.urbit.org"
    ==
==
==
