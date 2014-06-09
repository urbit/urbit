Urbit
=====

[![Build Status](https://travis-ci.org/urbit/urbit.png?branch=master)](https://travis-ci.org/urbit/urbit)

> Tlön is surely a labyrinth, but it is a labyrinth devised  by men, a labyrinth destined to be deciphered by men.  
> - Tlön, Uqbar, Orbis Tertius

Urbit is a new programming and execution environment designed from scratch. Any
resemblance to existing languages or operating systems is coincidental,
cosmetic, or inevitable.

Install
-------------------

###1. Dependencies

Urbit depends on:

    gcc
    gmp
    libsigsegv
    openssl
    libssl-dev (Linux only)
    ncurses (Linux only)

####Ubuntu or Debian

    sudo apt-get install libgmp3-dev libsigsegv-dev openssl libssl-dev libncurses5-dev git make exuberant-ctags

####Fedora

    sudo yum install gcc gcc-c++ git gmp-devel openssl-devel openssl ncurses-devel libsigsegv-devel ctags

####AWS

    sudo yum --enablerepo epel install gcc git gmp-devel openssl-devel ncurses-devel libsigsegv-devel ctags

####OS X

Do you have XCode? Type `gcc` at your terminal prompt.

If it says `no input files`, you have XCode.

Otherwise, install XCode: `https://developer.apple.com/xcode/`, with the command line tools.

To install dependencies pick either one of Homebrew or Macports, but not both:  
  Homebrew -  
  `brew install git gmp libsigsegv openssl`  

  Macports -  
  `sudo port install git gmp libsigsegv openssl`


###2. Build

Clone this repo:

    git clone git://github.com/urbit/urbit.git

`cd` to the unpacked Urbit directory you just created:

    cd urbit

If this works, `ls urb/` should show:

    urbit.pill  zod/

Then just run `make` in the urbit directory.

Sometimes things are just easy.


###3. Run

Run `bin/vere -c mypier` in the urbit directory, where `mypier` is a directory that doesn't yet exist. All your state (an append-only log and a memory checkpoint) will live in this directory.  The name of your pier doesn't matter and is not visible internally.

A _pier_ is an Urbit virtual machine that hosts one or more Urbit identities,
or _ships_.  When you run `bin/vere -c`, it automatically creates a 128-bit ship, or `submarine`.  Your name (a hash of a randomly-generated public key) will look something like:

    ~machec-binnev-dordeb-sogduc--dosmul-sarrum-faplec-nidted

First you'll see a string of messages like:

    vere: urbit home is /Users/cyarvin/Documents/src/u3/urb
    loom: mapped 1024MB
    time: ~2013.9.1..03.57.11..4935
    ames: on localhost, UDP 63908.
    generating 2048-bit RSA pair...

and then it'll pause a little, 'cause this is slow, and then

    saving passcode in /Users/cyarvin/.urbit/~magsut-hopful.txt
    (for real security, write it down and delete the file...)

and, then, if the network gods are happy, your submarine will start pulling
down Arvo files:

     + /~machec-binnev-dordeb-sogduc--dosmul-sarrum-faplec-nidted/main/1/bin/ticket/hoon
     + /~machec-binnev-dordeb-sogduc--dosmul-sarrum-faplec-nidted/main/1/bin/reset/hoon
     + /~machec-binnev-dordeb-sogduc--dosmul-sarrum-faplec-nidted/main/1/bin/ye/hoon
     + /~machec-binnev-dordeb-sogduc--dosmul-sarrum-faplec-nidted/main/1/bin/ls/hoon

and the like.  You'll see a couple pages of this stuff.  Don't worry too much
about the details right now.  Finally, you'll get the Arvo shell prompt (which
is also a Hoon REPL):

    ~machec-binnev-dordeb-sogduc--dosmul-sarrum-faplec-nidted/try=>

If you would like to safely bring this ship back into port (End the Unix process),
just enter Control-D.  


###4. Registration

Arvo instances in the Urbit network, called "ships", are addresses in a finite namespace much like IP numbers.  You should be able to remember your personal IP number. However, numbers are cumbersome for humans to memorize.  Urbit solves this problem by mapping each address to a phonetic name, whose length is proportional to how many of that type of ship there are.  

In this section, we'll get you registered with some Urbit ships. One of these ships, a destroyer, will be both your personal cloud computer and identity in the social network of Urbit.

The long name in your prompt now is that of a submarine. Submarines are  cheap, temporary ships that are tiring to remember but useful for trying Urbit out or browsing anonymously. But this moniker is mouthful.  You can stick with it for now, but you're going to need a wider xterm.

Instead, registering for a destroyer will get you a nice short name like

    ~waclux-tomwyc

Destroyers are rarer ships meant to be associated with a user's digital identity. They are far fewer destroyers in the Urbit namespace than submarines. 

Tlon would be glad to give you a destroyer from the batch that they own. Head over to [tlon.io](http://tlon.io) to complete the automated registration process. 

Your destroyers will arrive in the form of `[ship ticket]` pairs.
Let's say one of your ships is `~waclux-tomwyc` and its ticket is

    ~ribdyr-famtem-larrun-figtyd

(Where do we get these phonetic strings from, anyway?  Just random unsigned integers,
rendered in Hoon's syllabic base, `@p`.)

A new life awaits you on the off-world colonies!  To begin, just
type at the submarine prompt:

    :begin ~waclux-tomwyc

and follow the directions.  When the script completes, hit return
and you'll be the `~waclux-tomwyc` you wanted to be.  Now, when other user's see you in chat or look at your 
Urbit social profile (a fasplan), they can learn whatever information you gave the :begin process.

###5. Play with Arvo

If all went well, you now have a nice short prompt:

    ~waclux-tomwyc/try=>

If all did not go well (send us another email), or you're just
too impatient to wait for your destroyer, you have a big long
prompt.  Which is fine, really, just ugly - and all these
exercises will still work.

####Example commands

Let's try a few quick things to stretch your fingers.  Type these
command lines and you should see the matching results:

    ~waclux-tomwyc/try=> "hello, world"
    "hello, world"

    ~waclux-tomwyc/try=> (add 2 2)
    4

    ~waclux-tomwyc/try=> :hello %world
    "hello, world."

    ~waclux-tomwyc/try=> :cat /=main=/bin/hello/hoon
    ::
    ::  /=main=/bin/hello/hoon
    ::
    |=  *
    |=  [planet=@ta ~]
    ^-  bowl
    :_  ~  :_  ~
    :-  %%
    !>("hello, {(trip planet)}.")

What did you just do?

One, you used Arvo as a Hoon REPL to print the constant `"hello,
world"`, which is a fancy way to write the Nock noun

    [104 101 108 108 111 44 32 119 111 114 108 100 0]

Two, you called the Hoon `add` function to see that two plus two
is four.  Math seems to work the same on the off-world colonies.

Three, you ran the Arvo application `:hello` with the argument
`%world`, which is just a fancy way to write the atom
`431.316.168.567` (or, for non-Germans, `431,316,168,567`).  You
might recognize it better as `0x64.6c72.6f77` - the ASCII
characters in LSB first order.

(Is Urbit German?  Sadly, no.  But all our noun print formats are
URL-safe, which dot is and comma isn't.)

And you (4) used the Arvo application :cat to print the Hoon file

    /=main=/bin/hello/hoon

which, supposing your current date is

    ~2013.9.1..04.38.31..f259

(ie, September 1, 2013 at 4:38:31 GMT/LS25 plus 0xf259/65536
seconds), is equivalent to the global path

    /~waclux-tomwyc/main/~2013.8.23..04.38.31..f259/bin/hello/hoon

which anyone in Urbit can, see and even use - but we're getting
ahead of ourselves.

####Control characters

In any case, what we've seen is that Arvo is a dangerous and
powerful operating system which if handled improperly can cause
serious injury or loss of life.  We exaggerate.  Slightly.

The first thing you need to know is how to control this tool.
Try your arrow keys - you'll see that Arvo has traditional Unix
history editing.  Up and down, left and right work, as do the
simple emacs controls:

    ^A  go to beginning of line
    ^B  left arrow
    ^D  delete next character
    ^E  go to end of line
    ^F  right arrow
    ^K  kill to end of line
    ^L  clear the screen
    ^R  search through history
    ^U  kill the whole line
    ^Y  yank (restore from kill ring)

Don't expect any other emacs (or even readline - this is not readline, it's
internal to Arvo) commands to work.

There are also some special control keys specific to Arvo.  It's
a good idea to learn these first so that you feel in, um,
control.

First, we'll quit out of an infinite loop with `^C`:

    ~waclux-tomwyc/try=> :infinite

When you hit return at the end of this line, Arvo will appear to
hang.  Do not be alarmed!  This is not a bug - it means that
we've started running our infinite loop before printing the next
console prompt.  Simply hit `^C`, and you'll see

    ! intr
    ~waclux-tomwyc/try=> :infinite

(There may be some stacktrace stuff before the `! intr`, depending
on whether your kernel was compiled with debugging.)

Hit `^U` to delete the line and escape from infinity.  Arvo is a
deterministic OS; you interrupted it while processing an event
that would never terminate.  It returns to the state it was in
before you hit return - as if nothing had ever happened.

You're probably used to using nondeterministic, preemptive OSes,
in which the difference between a waiting task and an
executing event isn't apparent to the user.  Since Arvo is not
preemptive, it has two very different states: waiting and
working.

When Arvo is working, `^C` cancels the event it's working on.
This event never happened.  Don't worry, nothing bad will happen
to your computer.

When Arvo is waiting, use `^D` to end the current task, which is
the task that's currently prompting you.  If there is a live
prompt and the cursor is not at the end, `^D` will delete the
current character - as in Unix.

Try this by running

    ~waclux-tomwyc/try=> :begin

    Do you have a ship and a ticket? yes

Then hit `^D` and you'll be back to the command prompt (which,
unlike in Unix, is not a task itself, but part of the OS).

We don't always want to kill the prompting task.  We often want
to switch between tasks, or between tasks and the command line.
Sort of like switching between windows, except in a command line.
We do this with `^X`.  Try

    ~waclux-tomwyc/try=> :begin

    Do you have a ship and a ticket? yes

But hit `^X` instead of `^D`.  You'll get a prompt again.  Use
it:

    ~waclux-tomwyc/try=> :begin

    ~waclux-tomwyc/try=> :hello %world
    "hello, world."
    ~waclux-tomwyc/try=>

Hit `^X` again:

    ~waclux-tomwyc/try=> :begin

    ~waclux-tomwyc/try=> :hello %world
    "hello, world."
    Do you have a ship and a ticket? yes

And finally, hit `^C` to kill the task.

Lastly, Arvo is a single-level store.  Since it's not the '70s
anymore and disk is cheap, everything you do is saved for ever.
(In fact, it's saved in two ways - as a memory image and an event
log - so you, or the government if they haz your filez, can
repeat every computation you've ever performed.)

If the current prompt is just the shell prompt, `^D` on an empty
line will log out - as in Unix:

    ~waclux-tomwyc/try=>
    oxford:~/urbit; pwd
    /Users/cyarvin/urbit
    oxford:~/urbit; echo "hello, world"
    hello, world
    oxford:~/urbit;

Then you can restart and be right back where you were - just
run `bin/vere` without `-c`:

    oxford:~/urbit; bin/vere mypier
    vere: urbit home is /Users/cyarvin/urb
    loom: loaded 9MB
    time: ~2013.9.1..17.23.05..0cc1
    ames: on localhost, UDP 60342.
    http: live on 8080
    rest: checkpoint to event 383
    rest: old 0v1c.gkr1o, new 0v10.m4gdu
    ---------------- playback complete----------------
    waclux-tomwyc/try=>

Use your arrow keys and you'll see your history is still there.
Arvo is indestructible and can be shut down however you like
without losing data.  Also, starting a new task while an old
one is still running will kill the old one safely.

But don't try to operate the same ship on two Unix hosts at the
same time.  This will confuse everyone, including yourself.

####System administration

Sometimes we make changes to Hoon or Arvo (we never make changes
to Nock) and you need to update your ship.

There are two steps to updating.  You need to get the new files,
and you need to install them.  To get them:

    ~waclux-tomwyc/try=> :update
    : /~waclux-tomwyc/arvo/2/hoon/hoon
    : /~waclux-tomwyc/arvo/2/dill/hoon
    : /~waclux-tomwyc/arvo/2/batz/hoon

To install them (the simplest, slowest, most general way):

    ~waclux-tomwyc/try=> :reset

    %reset-start
    %reset-parsed
    %reset-compiled
    %hoon-load
    [%tang /~waclux-tomwyc/arvo/~2013.11.26..20.29.15..090f/zuse ~tirnux-latwex]
    [%vane %a /~waclux-tomwyc/arvo/~2013.11.26..20.29.15..090f/ames ~tolryn-watret]
    [%vane %b /~waclux-tomwyc/arvo/~2013.11.26..20.29.15..090f/batz ~donfex-ladsem]
    [%vane %c /~waclux-tomwyc/arvo/~2013.11.26..20.29.15..090f/clay ~picsug-mitref]
    [%vane %d /~waclux-tomwyc/arvo/~2013.11.26..20.29.15..090f/dill ~dilpex-laptug]
    [%vane %e /~waclux-tomwyc/arvo/~2013.11.26..20.29.15..090f/eyre ~forbur-disben]

All of your state, including running tasks, will be unchanged.

Sometimes the interpreter, called `vere` gets updated. In your urbit directory, back in Unixland, run:

    git pull origin master

every so often to get the latest Urbit source code. You'll need to run:

    make clean; make

before executing `bin/vere pier` again.


###6. Chat

Okay, fine.  You're a long way from being an Arvo ninja.  But -
you're ready for the two most important uses of Urbit right now.
One, coding.  Two, chatting.

To start chatting, simply type

    ~waclux-tomwyc/try=> :chat


and type `?` for the list of commands once `:chat` is running. 

Most of us are hanging out on `:chat` regularly. We can answer any questions you might have and help you get oriented in this new environment.

