Urbit
=====

> Tlön is surely a labyrinth, but it is 
> a labyrinth devised  by men, a labyrinth 
> destined to be deciphered by men.  
> -- Tlön, Uqbar, Orbis Tertius

Urbit is a new programming and execution environment designed from scratch. Any
resemblance to existing languages or operating systems is coincidental,
cosmetic, or inevitable.

All of the source code is distributed under the MIT license, but is for all
intents and purposes in the public domain.

Install
-------------------

###1. Dependencies

Urbit depends on:

    gcc
    gmp
    libsigsegv
    openssl
    automake
    autoconf
    ragel
    cmake
    re2c
    libtool
    libssl-dev (Linux only)
    ncurses (Linux only)

####Ubuntu or Debian

    sudo apt-get install libgmp3-dev libsigsegv-dev openssl libssl-dev libncurses5-dev git make exuberant-ctags automake autoconf libtool g++ ragel cmake re2c

####Fedora

    sudo yum install gcc gcc-c++ git gmp-devel openssl-devel openssl ncurses-devel libsigsegv-devel ctags automake autoconf libtool cmake re2c

####AWS

    sudo yum --enablerepo epel install gcc git gmp-devel openssl-devel ncurses-devel libsigsegv-devel ctags automake autoconf libtool cmake re2c

####OS X

Do you have XCode? Type `gcc` at your terminal prompt.

If it says `no input files`, you have XCode.

Otherwise, install XCode: `https://developer.apple.com/xcode/`, with the command line tools.

To install dependencies pick either one of Homebrew or Macports, but not both:  
  Homebrew -  
  `brew install git gmp libsigsegv openssl libtool autoconf automake cmake`  

  Macports -  
  `sudo port install git gmp libsigsegv openssl autoconf automake cmake`

Although automake/autoconf/libtool are generally installed by default, some
have reported needing to uninstall and reinstall those three packages, at least
with Homebrew.  YMMV.


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

Run `bin/urbit -c mypier` in the urbit directory, where `mypier` is a directory that doesn't yet exist. All your state (an append-only log and a memory checkpoint) will live in this directory.  The name of your pier doesn't matter and is not visible internally.

A _pier_ is an Urbit virtual machine that hosts one or more Urbit identities,
or _ships_.  When you run `bin/urbit -c`, it automatically creates a 128-bit ship, or `submarine`.  Your name (a hash of a randomly-generated public key) will look something like:

    ~hinfet-rovler-labheb-laddev--ladner-pillur-divmun-tamfur

First you'll see a string of messages like:

    vere: urbit home is mypier
    vere: hostname is cyarvin
    loom: mapped 2048MB
    boot: installed 190 jets
    boot: loading /home/cyarvin/urbit/urb/urbit.pill
    cv_make: loaded pill /home/cyarvin/urbit/urb/urbit.pill, as 8ddffb8
    cv_make: kernel 12e1ffdf, core 23b73cd
    cv_jack: activating kernel 12e1ffdf
    %post-start
    cv_jack: activated
    cv_start: time: ~2015.5.9..18.03.32..ba77

Next vere will generate a 2048 RSA key that will be used as your identity:

    generating 2048-bit RSA pair...

and then it'll pause a little, 'cause this is slow, and then

    saving passcode in mypier/.urb/code.~malmel-ridnep
    (for real security, write it down and delete the file...)

and, then, if the network gods are happy, your submarine will start pulling
down Arvo files:

    [%bos ~zod ~hinfet-rovler-labheb-laddev--ladner-pillur-divmun-tamfur]
    [%behn-init ~hinfet-rovler-labheb-laddev--ladner-pillur-divmun-tamfur]
    ames: czar zod.urbit.org: ip .192.241.195.84
    kick: init: ~hinfet-rovler-labheb-laddev--ladner-pillur-divmun-tamfur
    ames: on localhost, UDP 55659.
    http: live (insecure) on 8081
    http: live ("secure") on 8444
    term: live on 10024
    ; ~zod |Tianming| is your neighbor
    ; ~zod |Tianming| is your neighbor
    <sole>

You will probably be stuck here for a while as your local submarine copies all of its files from the master carrier, `~zod`. This is normal.

Once that is finished, you will be dropped into the main application, the `:dojo` shell (which is coincidentally also a Hoon REPL):

    ~hinfet-rovler-labheb-laddev--ladner-pillur-divmun-tamfur:dojo>

If you would like to safely bring this ship back into port (End the Unix process),
just enter Control-V to switch to the task manager, then Control-D.  

To re-launch your pier after creation run `bin/urbit mypier` (exclude the `-c`)

###4. Registration

Arvo instances in the Urbit network, called "ships", are addresses in a finite namespace much like IP numbers.  You should be able to remember your personal IP number. However, numbers are cumbersome for humans to memorize.  Urbit solves this problem by mapping each address to a phonetic name, whose length is proportional to how many of that type of ship there are.  

In this section, we'll get you registered with some Urbit ships. One of these ships, a destroyer, will be both your personal cloud computer and identity in the social network of Urbit.

The long name in your prompt now is that of a submarine. Submarines are  cheap, temporary ships that are tiring to remember but useful for trying Urbit out or browsing anonymously. But this moniker is mouthful.  You can stick with it for now, but you're going to need a wider xterm.

Instead, registering for a destroyer will get you a nice short name like

    ~waclux-tomwyc

Destroyers are rarer ships meant to be associated with a user's digital identity. They are far fewer destroyers in the Urbit namespace than submarines. 

[During this period of development we are no longer giving away destroyers. If you would like to know when we are offering ships again, please head to urbit.org and enter your email address or email ship [at] urbit.org]

Your destroyers will arrive in the form of `[ship ticket]` pairs.
Let's say one of your ships is `~waclux-tomwyc` and its ticket is

    ~ribdyr-famtem-larrun-figtyd

(Where do we get these phonetic strings from, anyway?  Just random unsigned integers,
rendered in Hoon's syllabic base, `@p`.)

A new life awaits you on the off-world colonies!  To begin, just
type at the `:dojo` prompt:

    :?begin

and enter your ship name and ticket when prompted.  When the script completes, hit return
and you'll begin re-cloning the files from `~zod` needed to boot the destroyer, after which you will become the `~waclux-tomwyc` you wanted to be.

###5. Play with Arvo

If all went well, you now have a nice short prompt:

    ~waclux-tomwyc:dojo>

If all did not go well (send us another email), or you're just
too impatient to wait for your destroyer, you have a big long
prompt.  Which is fine, really, just ugly - and all these
exercises will still work.

####Example commands

Let's try a few quick things to stretch your fingers.  Type these
command lines and you should see the matching results:

    ~waclux-tomwyc:dojo> "hello, world"
    "hello, world"
    
    ~waclux-tomwyc:dojo> (add 2 2)
    4

    ~waclux-tomwyc:dojo> +hello %world
    'hello, dlrow'

    ~waclux-tomwyc:dojo> +cat /=home=/cat/hello/gate/hook  :: XX make this work
    ::
    ::::  /hook/gate/hello/cat
      ::
    /?  314
    ::
    ::::
      !:
    |=  [* [[txt=@tas ~] ~]]
    :-  %noun
    (crip (weld "hello, " (flop (trip txt))))

What did you just do?

One, you used the Hoon REPL to print the constant `"hello,
world"`, which is a fancy way to write the Nock noun

    [104 101 108 108 111 44 32 119 111 114 108 100 0]

Two, you called the Hoon `add` function to see that two plus two
is four.  Math seems to work the same on the off-world colonies.

Three, you ran the `:dojo` application `+hello` with the argument
`%world`, which is just a fancy way to write the atom
`431.316.168.567` (or, for non-Germans, `431,316,168,567`).  You
might recognize it better as `0x64.6c72.6f77` - the ASCII
characters in LSB first order.

(Is Urbit German?  Sadly, no.  But all our noun print formats are
URL-safe, which dot is and comma isn't.)

And you (4) used the `:dojo` application `+cat` to print the Hoon file

    /=home=/cat/hello/gate/hook

which, supposing your current date is

    ~2015.5.9..18.03.32..ba77

(ie, May 9, 2015 at 18:03:32 GMT/LS25 plus 0xba77/47735 seconds), is
equivalent to the global path

    /~waclux-tomwyc/home/~2015.5.9..18.03.32..ba77/cat/hello/gate/hook

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

First off, Arvo has a simple task manager interface, allowing you see the
running commands, along with being able to spawn or kill programs.

Hitting `^V` will bring you to this interface, and running `-prog`
will kill that program while `+prog` will spawn a new one.
If you ever end up with a broken program that is spouting errors
and wish to restart it, do it from this interface. Even our shell
and REPL `:dojo` is just another program in this list!

This is the base view from Arvo as well. If you kill all your running
programs, it will drop you into this interface.

Next, we'll show you how we can quit out of an infinite loop with `^C`:

    ~waclux-tomwyc:dojo> |-($)

When you hit return at the end of this line, Arvo will appear to
hang.  Do not be alarmed!  This is not a bug - it means that
we've started running our infinite loop before printing the next
console prompt.  Simply hit `^C`, and you'll see

    recover: dig: intr
    intr
    [various stack traces]
    ~waclux-tomwyc:dojo> |-($)

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

    ~waclux-tomwyc:dojo> :?begin

    ~waclux-tomwyc:dojo: your ship: ~

Then hit `^D` and you'll kill the current task, the command prompt `:dojo`.

We don't always want to kill the prompting task.  We often want
to switch between tasks, or between tasks and the command line.
Sort of like switching between windows, except in a command line.
We do this with `^X`.

Let's try an example: First, make sure you
have two apps running, like `:dojo` and `:talk`. Then, try:

    ~waclux-tomwyc:dojo> :?begin

    ~waclux-tomwyc:dojo: your ship: ~

But hit `^X` instead of `^D`.  You'll get switched to the next app in line, in this case `:talk`.  Use
it:

    ~waclux-tomwyc:dojo> :?begin

    ~waclux-tomwyc:talk() hello, world!

Hit `^X` again:

    ~waclux-tomwyc:dojo> :?begin

    ~waclux-tomwyc:dojo: your ship: ~

And finally, hit `^D` to kill the task.

Lastly, Arvo is a single-level store.  Since it's not the '70s
anymore and disk is cheap, everything you do is saved for ever.
(In fact, it's saved in two ways - as a memory image and an event
log - so you, or the government if they haz your filez, can
repeat every computation you've ever performed.)

If the current prompt is the task manager, `^D` on an empty
line will log out - as in Unix:

    ~waclux-tomwyc:dojo, talk#
    cyarvin:~/urbit; pwd
    /home/cyarvin/urbit
    cyarvin:~/urbit; echo "hello, world"
    hello, world
    cyarvin:~/urbit;

Then you can restart and be right back where you were - just
run `bin/urbit` without `-c`:

    cyarvin:~/urbit; bin/urbit mypier
    vere: urbit home is mypier
    vere: hostname is cyarvin
    loom: mapped 2048MB
    protected loom
    live: loaded: MB/172.933.120
    boot: installed 190 jets
    cv_start: time: ~2015.5.9..19.03.45..a758
    raft: single-instance mode
    raft:      -> lead
    sist: booting
    rest: checkpoint to event 23.630
    rest: old 0vt.3iqg5, new 0v1j.moa0t
    loaded passcode from mypier/.urb/code.~lacsep-bonnyr
    
    ---------------- playback complete----------------
    ames: on localhost, UDP 46404.
    http: live (insecure) on 8080
    http: live ("secure") on 8443
    term: live on 10023
    ~waclux-tomwyc:dojo, talk# 

Use your arrow keys and you'll see your history is still there.
Arvo is indestructible and can be shut down however you like
without losing data.  Also, starting a new task while an old
one is still running will kill the old one safely.

But don't try to operate the same ship on two Unix hosts at the
same time.  This will confuse everyone, including yourself.

####System administration
Sometimes we make changes to Hoon or Arvo (we never make changes
to Nock) and you need to update your ship.

Luckily, Arvo has some special sauce that allows the same carrier
you initially pulled your files from to push kernel update over-the-air.
You may notice this happening automatically from time to time through mysterious messages
such as:

    ['merge succeeded' {}]

These updates will be applied without having to restart Arvo or any of its
`%vanes`. All of your state, including running tasks, will be unchanged.

Sometimes the interpreter, called `vere` gets updated. In your urbit directory, back in Unixland, run:

    git pull origin master

every so often to get the latest Urbit source code. You'll need to run:

    make clean; make

before executing `bin/urbit mypier` again.


###6. Talk

Okay, fine.  You're a long way from being an Arvo ninja.  But -
you're ready for the two most important uses of Urbit right now.
One, coding.  Two, chatting.

To start chatting, simply press `^X` to switch to the `:talk` app
(If you accidently killed it, no worries: start a new one by typing
`+talk` from the `^V` menu)

    ~waclux-tomwyc:talk()

and type `;join ~doznec/urbit-meta` to join our main chat room.

Most of us are hanging out on `:talk` regularly. We can answer any questions you might have and help you get oriented in this new environment.

