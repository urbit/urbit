::  Initial help message
::
::::  /hoon/start/gen
  ::
:-  %say
|=  *
:-  %tang
:~
:+  %rose
["" "" ""]
%+  turn
  :~  "Welcome to Urbit.  This command-line interface is the Dojo.  You can use it to"
      "control and operate your Urbit ship, or you can use the web interface if you"
      "prefer (as long as this process is running).  That's probably at"
      "http://localhost:8080 or http://localhost"
      "Try some commands out now:"
      "  our"
      "  now"
      "You can run simple 'generators' (calculations) with a '+' like this:"
      "  +vats"
      "Or run a system-changing generator with a '|' like this:"
      "  |hi ~zod"
      "  |install ~paldev %pals"
      "(Some commands will only work on a live ship, not a development fake ship.)"
      "For more help on generators and threads:"
      "  +help"
      "Finally, to close this process, type:"
      "  |exit"
      "which will shut down your Urbit ship gracefully."
      ""
  ==
|=(=tape leaf+tape)
==
