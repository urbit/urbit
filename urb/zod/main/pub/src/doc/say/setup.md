#Setup

> Tlön is surely a labyrinth, but it is a labyrinth devised  
> by men, a labyrinth destined to be deciphered by men.  
> - Tlön, Uqbar, Orbis Tertius

Urbit runs on Unix machines only.  It depends on:

+ gmp
+ libsigsegv
+ openssl
+ libssl-dev (Linux only)
+ ncurses (Linux only)

Currently we support OSX, Linux (not all distributions have been
tested) and \*BSD.  There are no instructions for BSD, because
only people with a serious clue run BSD.  Intrepid ninjas may
attempt ports to other OSes.  If you're not an intrepid ninja,
try a VM (eg, VirtualBox) or use one of our AMIs.

###Amazon AMIs

There are public AMIs at the following locations:

        us-west (oregon) ami-6cf88d5c
        us-west (n. california) ami-78d4ec3d
        us-east (n. virginia) ami-cd819ba4

These use Debian Wheezy, so you should be able to 
        
        ssh -i /path/to/your/key/file.pem admin@123.123.123.123

to get in.

Since the codebase is changing frequently, run the following once you have ssh-ed in.

        cd /urbit/
        git pull origin master
        make clean; make;

Then jump to "Run" below to get rolling.


###Docker

[Docker](http://docker.io) is a very convenient way to get in to an Urbit ship quickly. `~simmev-rabryd` maintains the docker approach on GitHub [here](https://github.com/yebyen/urbinit).

Follow the instructions on the GitHub page, then proceed to "Run" below.


###OS X###

1. Do you have XCode?  Type `gcc`.  If it says `no input files`, you have XCode.

   Otherwise, install XCode: `https://developer.apple.com/xcode/`, with the
   command line tools.

2. Install dependencies. Pick either one of Homebrew or Macports, but not both.  

   - Homebrew? Type `brew`.  If it does something, you have Homebrew.

     Otherwise, `ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"`
     will install it.

     And follow up with `brew install git gmp libsigsegv openssl`

     This will ask you for the root password, which ideally you know.

   - Macports? Type `port`.  If it does something, you have Macports.

     Otherwise go [here](http://www.macports.org/install.php "here").

     Then `sudo port install git gmp libsigsegv openssl`

     Enter your root password at the prompt.


###Linux (Ubuntu or Debian)###

1. `sudo apt-get install libgmp3-dev libsigsegv-dev openssl libssl-dev libncurses5-dev git make exuberant-ctags`


###Linux (AWS)###

1. `sudo yum --enablerepo epel install gcc git gmp-devel openssl-devel ncurses-devel libsigsegv-devel ctags`

###Get the source###

Urbit uses git for its revision control. Eventually, Urbit will use itself, but for now,
you need git. If you've followed the above instructions correctly then typing `git` in your terminal should do something. 

If that works, run:

    git clone https://github.com/urbit/urbit.git

to download Urbit from its repository. 

If for some reason you have moral qualms about using Git, you can also just download and unzip `https://github.com/urbit/urbit/archive/master.zip`. This won't provide any version control

###Set up your enviroment###

`cd` to the unpacked Urbit directory you just created:

    cd urbit

If this works, `ls urb` should show:

    urbit.pill  zod/

Great!  Now, let's do some dirty Unix stuff to set up your environment.
If you know what this is doing, feel free to do it right.  Otherwise:

    echo "export URBIT_HOME=`pwd`/urb" >>~/.bash_profile
    source ~/.bash_profile

To make sure this worked,

    echo $URBIT_HOME

should show `/urb` within the current directory.

If this didn't work, you'll have to do this the hard way. run `vi ~/.bash_profile` and fix it.

###Build###

`make`.  Sometimes things are just easy.

<h3 id="run">Run</h3>

Run `bin/vere -c mypier`, where `mypier` is a directory that doesn't yet exist.
All your state (an append-only log and a memory checkpoint) will live in this
directory.  Its name doesn't matter and is not visible internally.

A _pier_ is an Urbit virtual machine that hosts one or more Urbit identities,
or _ships_.  When you run `vere -c`, it automatically creates a 128-bit ship,
or `submarine`.  Your name (a hash of a randomly-generated public key) will
look like:

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

###Register###

Next, you need to decide whether a mere submarine is enough for
you right now.  This monicker is a mouthful.  You can stick with
it (for now), but you're going to need a wider xterm.

Which might be fine!  However, please note that just by sending a
simple email, you can get a much better ship - a `destroyer`,
with a nice short name like

    ~waclux-tomwyc

Just email `urbit@urbit.org`, with your submarine in the subject.
We'll send you destroyers - not one, but _two_.  Yes, two!  Tell
us something cool in the body, and we'll send you even more.

If you have a destroyer, you need to configure it.  Otherwise,
just stretch that xterm wide and skip to section 1.2.

Your destroyers will arrive in the form of `[ship ticket]` pairs.
Let's say one of your ships is `~waclux-tomwyc` and its ticket is

    ~ribdyr-famtem-larrun-figtyd

(What are these strings, anyway?  Just random unsigned integers,
rendered in Hoon's syllabic base, `@p`.)

A new life awaits you on the off-world colonies!  To begin, just
type at the prompt:

    :begin ~waclux-tomwyc

and follow the directions.  When the script completes, hit return
and you'll be the `~waclux-tomwyc` you wanted to be.

##Play with Arvo##

If all went well, you now have a nice short prompt:

    ~waclux-tomwyc/try=>

If all did not go well (send us another email), or you're just
too impatient to wait for your destroyer, you have a big long
prompt.  Which is fine, really, just ugly - and all these
exercises will still work.

###Example commands###

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

###Control characters###

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
run `vere` without `-c`:

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

###System administration###

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

every so often to get the latest Urbit source code.

###Chat###

Okay, fine.  You're a long way from being an Arvo ninja.  But -
you're ready for the two most important uses of Urbit right now.
One, coding.  Two, chatting.

To start coding, read the next chapter.  To start chatting,
simply type

    ~waclux-tomwyc/try=> :chat
    &

and type `?` for help.

[On to the documentation.](/doc/)
