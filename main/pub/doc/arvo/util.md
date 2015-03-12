<div class="short">

CLI Apps
========

These are our simple command-line applications.

You can find them in `/main/app`.

</div>

------------------------------------------------------------------------

### [`:begin`](#begin)

`~zod/try=> :begin [~ship-name [~valid-ticket-for-ship]]`

Start a ship. `:begin` collects all of the necesarry information to
start an Urbit ship. Takes an option `[~ship-name]` or
`[~ship-name [~valid-ticket-for-ship]]` pair.

------------------------------------------------------------------------

### [`:cat`](#cat)

`~zod/try=> :cat path-to-file [...]`

"cat" a file. `:cat` either prints a file, or concatenates and then
prints multiple files to the terminal.

------------------------------------------------------------------------

### [`:cp`](#cp)

`~zod/try=> :cp /path/to/source /path/to/destination`

Copy a file to a given location.

------------------------------------------------------------------------

### [`:grep`](#grep)

`~zod/try=> :grep 'literal'`

"grep" a file or standard input. Currently only supports a literal cord, 
but will eventuall support regular expressions.

------------------------------------------------------------------------

### [`:hi`](#hi)

`~zod/try=> :hi ~ship ["message"]`

Send a ship a message which is empty by default, becoming their neighbor
in the process. Often used to ping ships to check connectivity.

------------------------------------------------------------------------

### [`:into`](#into)

`~zod/try=> :into /path/to/file 'contents'`

Write text to a file. If the specified file does not exist, create a
file by that name. If it does exist, replace its contents.

------------------------------------------------------------------------

### [`:label`](#label)

`~zod/try=> :label %path %label`

"label". Add a label to a change number.

    ~zod/try=> :label %try %zebra
    = new /~zod/try/3
    ~zod/try=> :ls /=try/zebra
    readme

Note that adding a label is part of the delta stream and creates a new
change number, `3`.

------------------------------------------------------------------------

### [`:ls`](#ls)

`~zod/try=> :ls path/to/directory`

"ls". List files at a path. Unlike "ls" in Unix, the current path `%`
must be explicitly given (you cannot call `:ls` with no arguments to
display the files at the current path).

------------------------------------------------------------------------

### [`:mv`](#mv)

`~zod/try=> :mv /path/to/source /path/to/destination`

Move a file to a given location, creating a new revision of the source
that omits the moved file.

------------------------------------------------------------------------

### [`:reload`](#reload)

`~zod/try=> :reload %vane-name [...]`

Reload the standard library (zuse) and/or arvo vanes. If zuse is
reloaded, vanes depending on the changes must be reloaded as well. For
example `:reload %zuse %ford` is necessary to make use of changes in
application code or the REPL.

Possible values for %vane-name see [Overview](overview.md "overview"):

------------------------------------------------------------------------

### [`:reset`](#reset)

`~zod/try=> :reset`

Reloads all vanes. See [`:reset`] for reloading only or a specific vane.

------------------------------------------------------------------------

### [`:rm`](#rm)

`~zod/try=> :rm /path/to/source`

Remove a file.

------------------------------------------------------------------------

### [`:solid`](#solid)

`~zod/try=> :solid`

compiles a kernel into a new full urbit.pill

------------------------------------------------------------------------

### [`:sync`](#sync)

`:sync %source-desk ~hidduc-posmeg %target-desk`

Sets up a subscription to the source desk on the target ship name to the
target desk on your ship.

------------------------------------------------------------------------

### [`:ticket`](#ticket)

`~zod/try=> :ticket ~ship-name`

Creates a will for a ship. `:ticket` outputs the ticket for a Urbit
ship. Takes an option `[~ship-name]`. On destroyes this command creates
a yacht and takes the option \`[\~yacht-name-destroyer-name]

------------------------------------------------------------------------

### [`:thumb`](#thumb)

`~zod/try=> :thumb ~ship-name`

Show the ships information. Only works if you issued a [`:hi`]
[\`\~ship-name] beforehand.

This command is not avaible since the switch from batz to `%gall`!

Use this for the time beeing: - will:
`~zod/try=> ((hard (unit gcos)) .^(%a /=gcos=/~ship-name))` - raw will:
`~zod/try=> ((hard will) .^(%a /=will=/~ship-name))`

------------------------------------------------------------------------

### [`:unsync`](#unsync)

`:unsync %source-desk ~hidduc-posmeg %target-desk`

Cancels the subscription to the source desk on the target ship name to
the target desk on your ship.

------------------------------------------------------------------------

### [`:verb`](#verb)

`~zod/try=> :verb`

Turn verbose arvo mode on/off.

You'll see events, internal cards, and effects.

    [%unix p=%wake //temp]
    [ %give
      %t
      %wake
      ~[
        /c/tyme
        /g/a/~zod/._~~.58_~~.shell_~~.terminal__/w/drug/~zod/main
        /g/a/~harnyr-darlux-bitrux-litnum--falbec-tacsev-magdus-tobsyn/began/u
        /g/a/~harnyr-darlux-bitrux-litnum--falbec-tacsev-magdus-tobsyn/._~~.2_~~.shell_~~.terminal__/u/to-gan
        /g/a/~harnyr-darlux-bitrux-litnum--falbec-tacsev-magdus-tobsyn/._~~.shell_~~.terminal__/u/child/2/main
        /g/a/~harnyr-darlux-bitrux-litnum--falbec-tacsev-magdus-tobsyn/terminal/u/txt
        /d/term-mess
        //term/1
      ]
    ]
    [ %give
      %c
      %writ
      ~[
        /g/a/~zod/._~~.58_~~.shell_~~.terminal__/w/drug/~zod
        /g/a/~harnyr-darlux-bitrux-litnum--falbec-tacsev-magdus-tobsyn/began/u
        /g/a/~harnyr-darlux-bitrux-litnum--falbec-tacsev-magdus-tobsyn/._~~.2_~~.shell_~~.terminal__/u/to-gan
        /g/a/~harnyr-darlux-bitrux-litnum--falbec-tacsev-magdus-tobsyn/._~~.shell_~~.terminal__/u/child/2/main
        /g/a/~harnyr-darlux-bitrux-litnum--falbec-tacsev-magdus-tobsyn/terminal/u/txt
        /d/term-mess
        //term/1
      ]
    ]
    ...

------------------------------------------------------------------------

### [`:ye`](#ye)

`~zod/try=> :ye ["message"]`

Send a message to all ships. Often used to announce a continuity breach.

------------------------------------------------------------------------
