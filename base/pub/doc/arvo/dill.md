<div class="short">

`%dill`
=======

Our terminal driver.

Unix sends keyboard events to `%dill` from either the console or telnet,
and `%dill` produces terminal output. The only app that should directly
talk to `%dill` is the terminal app. Command-line apps are run by,
receive input from, and produce output to, the `%shell` app, which is
controlled by `%terminal`, which talks to `%dill`, which talks to unix.
Clay also uses `%dill` directly to print out the filesystem change
events, but this is questionable behavior.

`%dill` has two main components. First, it controls the terminal on a
very basic, event-by-event level. This includes keeping track of things
like the dimensions of the terminal, the prompt type (plain text or
password), which duct to produce effects on, and so forth. Second, it
handles terminal events, keystroke by keystroke. Most characters are
simply pushed onto the buffer and blitted to the screen, but some
characters, including control-modified keys, arrow keys, etc. require
special handling. Most of the readline functionality is in `%dill`.

</div>

------------------------------------------------------------------------
