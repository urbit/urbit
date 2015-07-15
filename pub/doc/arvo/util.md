<div class="short">

CLI Apps
========

Our simple command-line applications.

You can find them in `/main/app`.

</div>

------------------------------------------------------------------------

### [`:?begin`](#begin)

`~zod:dojo> :?begin [~ship-name [~valid-ticket-for-ship]]`

Start a ship. `:?begin` collects all of the necessary information to
start an Urbit ship. Takes an option `[~ship-name]` or
`[~ship-name [~valid-ticket-for-ship]]` pair.

------------------------------------------------------------------------

### [`+cat`](#cat)

`~zod:dojo> +cat /path/to/file [...]`

"cat" a file. `+cat` either prints a file, or concatenates and then
prints multiple files to the terminal.

    ~zod:dojo> +cat %/spec/nock/5/txt
    > +cat %/spec/nock/5/txt
    /~zod/home/~2015.6.29..22.33.04..fc76/spec/nock/5/txt
    A noun is an atom or a cell.
    …

------------------------------------------------------------------------

### [`|cp`](#cp)

`~zod:dojo> |cp /path/to/source /path/to/destination`

Copy a file to a given location.

    ~zod:dojo> |cp %/spec/nock/5/txt %/try/6/txt
    > |cp %/spec/nock/5/txt %/try/6/txt
    + /~zod/home/2/try/6/txt
    >=

------------------------------------------------------------------------

### [`grep`](#grep)

<mark>GONE</mark>

<s>`~zod:dojo> :grep 'literal'`

"grep" a file or standard input. Currently only supports a literal cord, 
but will eventuall support regular expressions.</s>

------------------------------------------------------------------------

### [`|hi`](#hi)

`~zod:dojo> |hi ~ship ["message"]`

Send a ship a message which is empty by default, becoming their neighbor
in the process. Often used to ping ships to check connectivity.


    ~zod:dojo> |hi ~doznec
    > |hi ~doznec
    ames: czar zod.urbit.org: ip .192.241.195.84
    >=
    hi ~doznec succesful
    ; ~doznec is your neighbor
    ; ~doznec is your neighbor

and on ~doznec

    ~doznec:dojo> 
    < ~zod:
    ; ~zod is your neighbor


send a message

    ~zod:dojo> |hi ~doznec "say something"
    >=
    hi ~doznec succesful

and on ~doznec

    < ~zod: say something


------------------------------------------------------------------------

### [`:into`](#into)

<mark>GONE</mark>

<s>
`~zod:dojo> :into /path/to/file 'contents'`

Write text to a file. If the specified file does not exist, create a
file by that name. If it does exist, replace its contents.
</s>

------------------------------------------------------------------------

### [`|label`](#label)

<mark>GONE? returns file not found</mark>

<s>
`~zod:dojo> |label %path %label`

"label". Add a label to a change number.

    ~zod:dojo> |label %try %zebra
    = new /~zod/try/3
    ~zod:dojo> :ls /=try/zebra
    readme

Note that adding a label is part of the delta stream and creates a new
change number, `3`.
</s>

------------------------------------------------------------------------

### [`+ls`](#ls)

`~zod:dojo> :+ls path/to/directory`

"ls". List files at a path. Unlike "ls" in Unix, the current path `%`
must be explicitly given (you cannot call `+ls` with no arguments to
display the files at the current path).

    ~zod:dojo> +ls %try
    > +ls %/try/
    readme/md

------------------------------------------------------------------------

### [`|mount`](#mount)

`~zod:dojo> |mount /path/to/directory/version %mount-point`

Your files are not synced to unix by default.
To sync a subtree to unix, run `|mount /path/to/directory %mount-point`.
This will sync it into <pier>/<mount-point>.
If you want to sync your whole home desk into f0/home, for example,
run `|mount % %home`  You can also [`|unmount`](#unmount).

    ~zod:dojo> |mount /~zod/base/0 %base
    > |mount /~zod/base %base
    >=

------------------------------------------------------------------------

### [`|mv`](#mv)

`~zod:dojo> |mv /path/to/source /path/to/destination`

Move a file to a given location, creating a new revision of the source
that omits the moved file.

    ~zod:dojo> |mv %/try/6/txt %/try/7/txt
    > |mv %/try/6/txt %/try/7/txt
    + /~zod/home/3/try/7/txt
    >=

------------------------------------------------------------------------

### [`|reload`](#reload)

`~zod:dojo> |reload %vane-name [...]`

Reload the standard library (zuse) and/or arvo vanes. If zuse is
reloaded, vanes depending on the changes must be reloaded as well. For
example `|reload %zuse %ford` is necessary to make use of changes in
application code or the REPL.

Possible values for %vane-name see [Overview](overview "overview"):

    ~zod:dojo> |reload %zuse
    [%tang /~zod/home/~2015.6.29..23.50.29..134d/arvo/zuse ~hillyx-salhet]
    > |reload %zuse
    >=

------------------------------------------------------------------------

### [`|reset`](#reset)

`~zod:dojo> |reset`

Reloads all vanes. See [`|reload`](#reload) for reloading only or a specific vane.

    ~zod:dojo> |reset
    [%vega-start /~zod/home/~2015.6.29..23.51.42..f335/arvo/hoon]
    %vega-parsed
    [%vega-compiled %163 163]
    %hoon-load
    [%tang /~zod/home/~2015.6.29..23.51.42..f335/arvo/zuse ~hillyx-salhet]
    [%vane %a /~zod/home/~2015.6.29..23.51.42..f335/arvo/ames ~tilwyl-talren]
    %ames-reload
    [%vane %c /~zod/home/~2015.6.29..23.51.42..f335/arvo/clay ~molmur-panlus]
    [%vane %d /~zod/home/~2015.6.29..23.51.42..f335/arvo/dill ~sicbet-miphes]
    [%vane %e /~zod/home/~2015.6.29..23.51.42..f335/arvo/eyre ~solrux-sibnep]
    [gub=30 hov=19 ged=18 ded=1 pox=1 ask=1 kes=1 ney=35 dop=1 liz=1 wup=1 sop=1 wix=1]
    [%vane %f /~zod/home/~2015.6.29..23.51.42..f335/arvo/ford ~librem-sopseg]
    [%vane %g /~zod/home/~2015.6.29..23.51.42..f335/arvo/gall ~sidsub-fasrev]
    [%vane %t /~zod/home/~2015.6.29..23.51.42..f335/arvo/time ~ritwyn-lanrev]
    > |reset
    <<<reset>>>
    >=

------------------------------------------------------------------------

### [`|rm`](#rm)

`~zod:dojo> |rm /path/to/source`

Remove a file.

    ~zod:dojo> |rm %/try/7/txt
    >=

------------------------------------------------------------------------

### [`+solid`](#solid)

`~zod:dojo> +solid`

compiles a kernel into a new full urbit.pill

------------------------------------------------------------------------

### [`|sync`](#sync)

`~zod:dojo> |sync %source-desk ~hidduc-posmeg %target-desk`

Sets up a subscription to the source desk on the target ship name to the
target desk on your ship.

------------------------------------------------------------------------

### [`+ticket`](#ticket)

`~zod:dojo> +ticket ~ship-name`

Creates a will for a ship. `+ticket` outputs the ticket for a Urbit
ship. Takes an option `[~ship-name]`. On destroyes this command creates
a yacht and takes the option \`[\~yacht-name-destroyer-name]

------------------------------------------------------------------------

### [`:thumb`](#thumb)

<mark>GONE</mark>

<s>
`~zod:dojo> :thumb ~ship-name`

Show the ships information. Only works if you issued a [`:hi`]
[\`\~ship-name] beforehand.

This command is not avaible since the switch from batz to `%gall`!

Use this for the time beeing: - will:
`~zod/try=> ((hard (unit gcos)) .^(%a /=gcos=/~ship-name))` - raw will:
`~zod/try=> ((hard will) .^(%a /=will=/~ship-name))`
</s>

------------------------------------------------------------------------

### [`|unmount`](#unmount)

`~zod:dojo> |unmount /path/to/directory`

Your files are not synced to unix by default.
To sync a subtree to unix, run [`|mount`](#mount).
You can unmount with either `|unmount /path/to/directory`
or `|unmount %mount-point`.

    ~zod:dojo> |unmount %base
    > |unmount %base
    >=

------------------------------------------------------------------------

### [`|unsync`](#unsync)

`~zod:dojo> |unsync %source-desk ~hidduc-posmeg %target-desk`

Cancels the subscription to the source desk on the target ship name to
the target desk on your ship.

------------------------------------------------------------------------

### [`|verb`](#verb)

`~zod:dojo> |verb`

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

### [`|ye`](#ye)

`~zod:dojo> |ye ["message"]`

Send a message to all ships. Often used to announce a continuity breach.

------------------------------------------------------------------------
