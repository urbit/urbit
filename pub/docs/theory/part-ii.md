---
layout: video
sort: 2
title: demo.2015.b
---

# demo.2015.b

<iframe src="https://player.vimeo.com/video/131715710?color=fff&amp;title=0&amp;byline=0&amp;portrait=0" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>

## DVD commentary

<div class='short'>

#### CY
Hi, DVD viewers!  Once again, it's Curtis Yarvin and Galen Wolfe-Pauly with the second half of "Russian Nock."

#### GWP 
Would you like to play a game of global thermonuclear war?

#### CY
Not today!  But I'm always up for tictactoe.

In this section of our epic major motion picture, we build a simple Urbit appliance: tictactoe.  The point of the exercise is just that it's easy to build distributed social applications in Hoon and Arvo, our language and OS.  At least, it's easy once you know Hoon.

#### GWP
Hoon is actually much easier to learn than it looks.  But why, Curtis, is it easy to build social distributed apps in Urbit?

#### CY
Because Urbit solves all the hard problems for you.  Identity, of course.  But also idempotence, transactionality, protocol definition and validation, publish/subscribe...

#### GWP
Urbit certainly sounds like something that would work for idempotence.

#### CY
It does!  Urbit.  It's what plants crave.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/01.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/01.png)

##### 00.56

#### CY
So we cleared the screen, which means we got to annihilate the universe.

#### GWP
We shot this second chapter in a new test universe.

#### CY
That's why we become neighbors again.  We just made these urbits.

#### GWP
I hated the text on screen at the end of the last take.  Very marketing.

#### CY
But it does include the instructions for what you just did.

First, you synced my `%demo` desk - desk is Urbitese for "branch" - into your `%demo` desk.  That both copies over the current `%demo` state, and subscribes to any future changes.

Second, you started the `%taco` app from your `%demo` desk.

#### GWP
But it didn't work.

#### CY
Neither of us has a `%demo` desk.  And I haven't written `%taco` yet.  But (spoiler) it will work.

Here's why "it doesn't matter what order you do things in" (for certain values of "order" and "thing").  In a distributed system, state changes whenever possible should be knowledge events.  If you learn A and B, it matters not whether you learn A before B or B before A: what you know is AB.  Any data model that works this way is inherently a CRDT.  If you know what that is.

#### GWP
I don't.

#### CY
A conflict-free replicated data type.  It's what plants crave.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/02.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/02.png)

##### 01.01

#### CY
In `%clay`, Urbit's revision-control system, your working tree is actually a desk (again, Urbitese for "branch").

From Urbit you can mount synced Unix directories that work like Dropbox.  Urbit monitors your directory tree with `inotify()`, catches any changes, and commits them as Urbit events.  So you don't need some new Urbit editor to develop for Urbit.  You can keep using the best editor in the world, which is obviously vim.

#### GWP
Or Emacs.  We also have an Emacs mode for Hoon.  Is that what it's called?  A mode?

#### CY
Is it?  I wouldn't know.  Anyway, you definitely want syntax highlighting in Hoon, though I eschew it to keep my hand strong.

#### GWP
Thank you for that, Curtis.

#### CY
Anyway, to make changes in `%demo`, the general practice is to make a `%demo-work` desk whose history is an unstructured series of edits.  Your merges from `%demo-work` to `%demo` are the equivalent of git commits.

(Note that we don't explicitly mount Urbit folders to Unix directories here.  Not that we don't need to, just that we shot this with an older version of Urbit that automounted.)

#### GWP
What's the equivalent of a commit message?

#### CY
As part of your merge change, prepend a line to a commit log on the target.  Why should the log be a built-in feature of the filesystem?  That's so git.

#### GWP
Does the `|merge` command actually do that?

#### CY
No, but it should!

Speaking of commands, we did a `|merge` here and a `|sync` before.  Again, `|sync` sets up a flow, `|merge` is a one-time copy.  So future updates of my `%demo` won't flow into `%demo-work`, but they will flow into *your* `%demo`.  Because I merged and you synced.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/03.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/03.png)

##### 01.11

#### CY
It looks like I'm quitting Urbit and going into an editor.  I'm actually just switching windows.  I could quit Urbit, of course -- it's a database, after all.  But I don't have to.

#### GWP
I think that's just bad direction.

#### CY
It's actually bad editing.  Anyway, here we build the basic state model of a simple tictactoe app.  This is a structure file, so it goes in `/~tasfyn-partyv/demo-work=/sur/taco/hoon` (in Urbit) and `demo-work/sur/taco.hoon` (in Unix).

I honestly think anyone could stare at this file for a couple of minutes and realize what it's doing.  The state of a tictactoe game is: the board, and whose turn it is.  The turn `who` is a boolean.  The board we store in two bitmaps, `box` for X and `boo` for O.  Hoon is very good at bit fiddling and its arrays are just lists, so our boards are just atoms (unsigned integers).

#### GWP
I still think `,[x=@ y=@]` is a really ugly syntax.

#### CY
I think you're right.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/04.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/04.png)

##### 01.23

#### CY
Then we type in `/===/lib/taco/hoon`. This is the "business logic" of tictactoe.  I feel it compares well to the same problem solved in other languages.

I won't go into super deep detail about this code, but you see a couple of cores `++bo` (for board bitmaps) and `++go` (for the `++play` structure we just defined).  If Hoon had C++ classes, these would be classes.  You can see the getters and setters getting and setting.  There's also a nice nested core within `++go`, `++at`.  Hard to find anything quite like a nested core in normal Earth languages.

I actually typed in these files, by the way.  By hand.  Isn't that lame?  I think there are a couple of errors.  Of course, to not break the take, I switched to a different window and copied them in by hand.

#### GWP
Then I created the illusion of superhuman flowing code.  In the editing room.  Have I told you how many edits that took?

#### CY
At least once.  Anyway, I want to emphasize something else, which is that although (as someone who reads Hoon) this is perfectly readable Hoon, it's not quite what real code looks like.  The formatting in particular has been pretty seriously golfed.  This looks like good Hoon to me, but it's insanely overgroomed like a French poodle.  You'd never do this much formatting work if it wasn't a demo.

#### GWP
Honestly, I think normal Hoon looks better.

#### CY
Honestly, it's all about the line count.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/05.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/05.png)

##### 01.41

#### CY
Then it takes us 82 lines of code to create the actual application core.  This seems like a lot.  Actually, there's a lot of code here that other multi-console apps could duplicate, so maybe there should be a library.

This version of `%taco` is a multiplayer game, of course, but kind of a trivial one.  It doesn't have its own network protocol.  Imagine if you were writing a Unix tictactoe server, and it just worked by running the telnet protocol and letting clients telnet into it.

The Urbit equivalent of the telnet protocol is the `%sole` system, which the console driver (the `%hood` appliance) uses to talk to command-line appliances foreign and domestic.  `%sole-action` messages get poked forward; `%sole-effect` messages come back in a subscription.

`%sole` is actually quite a fancy command-line model.  It treats the input line as multi-writer shared state and manages edits with OT.  So the app can reject invalid input as it's being typed, do tab completion, etc.

#### GWP
Overtime?

#### CY
Operational transformation.  It has electrolytes.  Anyway, what the code in this file is doing: parsing the command line (Hoon has pretty good combinator parsers), applying updates, detecting changes, updating subscribers.

Tictactoe is a two-player game, of course, but any number of people could log into `%taco`.  One player is the server; everyone else can play the other side.

#### GWP
But are the parsers monadic?  Does Hoon have monads?

#### CY
Shh!  We try not to use that word.  If it helps you to think in category theory, great.  Category theory is a theory of everything, including Hoon.  For most people it's easier to just learn Hoon.

#### GWP
Sometimes I wonder if it's just that you're not smart enough to learn category theory, Curtis.

#### CY
(in Maryland accent) I'm just a country boy but I'm sure I could figure it out real good if I had the need.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/06.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/06.png)

##### 02.03

#### CY
So what happened here is that when I merged my `%demo-work` desk into my `%demo` desk, your `%demo` desk was synced to it and got a subscription update.  Then, your urbit was already configured to be running the `%taco` appliance from your `%demo` desk.  So when it noticed that the source code existed, your urbit started the app with its default state.

#### GWP
Why am I not responding, still trying?

#### CY
That's the old network layer.  There's still a bunch of 2013 code in there.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/07.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/07.png)

##### 02.14

#### CY
Here I telnet into your `%taco` appliance.  Or more like ssh, because it's secure.

#### GWP
Secure?  Really?

#### CY
Well, it's encrypted.  That means it's probably secure.  It's certainly *designed* to be secure.

#### GWP
I'm sure the NSA is very worried.

#### 02.43

#### CY
Now this is a total party trick.  Instead of editing through `%demo-work` like a responsible developer, I edit `%demo` directly.  When I save the file, without any other command, the effect cascades all the way through and hot-reloads your `%taco` on your urbit, changing the 'X' icon to 'Z' and back again.

Moreover, if you notice, I'm not even editing the `%taco` app core.  I'm editing a library it depends on.  Because dependencies.

#### GWP
Would it be a good idea to do this in production?  Also, what happens if you make a change that doesn't compile?

#### CY
No.  If only because it leaves a gnarly edit history.  Also -- we'll see that in a little bit.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/08.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/08.png)

##### 03.08

#### GWP
Marketing copy in a demo really isn't appropriate.

#### CY
I know.  It is kind of true, though.


[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/09.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/09.png)

##### 03.11

#### CY 
Because I made those edits directly in `%demo`, I need to merge back into `%demo-work`.  Or...

#### GWP
Can't the filesystem handle a trivial three-way merge like that?

#### CY
I think so!

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/10.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/10.png)

##### 03.19

#### CY
For the new version our game state gets more sophisticated.  (I don't know why I said "protocol.")  Now it knows the players and the audience.  

Of course we need to be able to upgrade without losing our state.  So we'll have to write an adapter.  Which is pretty easy, because we just added a couple fields.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/11.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/11.png)

##### 03.28

#### CY
Now we need to be able to print the new information for the prompt.  Some of this code is getting pretty golfy -- I really can't endorse the style of `++vew`.

Also, you probably missed it above, but we added a board-to-JSON translator in `++bo`.  We can turn a Hoon bitfield into a JSON array in one line of code.  It's not even a golfy line.

#### GWP
`a/(turn (gulf 0 9) |=(@ b/(bit +<)))`.  Now that's not printing the JSON, just making the noun that we'll print.  

#### CY
I assert that anyone who can learn JS can learn Hoon.  On the other hand, I don't know JS.

#### GWP
Just don't use the word "monkey" in the same sentence as "front-end," and we'll be fine.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/12.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/12.png)

##### 03.35

#### CY
This is an actual protocol structure - a "mark."  It's like a MIME type, except it's actually a type.  

`%taco-move` is just a coordinate `point`, a pair of integers (defined in `sur/taco.hoon`).

#### GWP
It's actually really nice to have your network messages auto-validated and stuff.

#### CY
Also auto-translated -- as you see, there's an arm for turning `%json` into `%taco-move`.

#### GWP
That's convenient for making moves from a browser client, obviously.

#### CY
Not that the app knows it's dealing with a browser.  It thinks the browser is another urbit.  The HTTP server translates automagically.

#### GWP
Because the mark has that translator arm.  Good times!

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/13.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/13.png)

##### 03.43

#### CY
Another mark: moves go forward, updates go backward.  So we have to translate `%taco-update` (the whole game state) into `%json`.

#### GWP
Again we're just building a JSON-shaped noun.  The web server, `%eyre`, will actually print as text.

#### CY
Because that's a great thing to send over the network.  Text.

#### GWP
We should eventually have a socket driver that doesn't do this.  Right now it's all long-poll / Comet.

#### CY
Please don't say "socket" when you mean "websocket."  It reminds me of how old I am.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/14.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/14.png)

##### 03.51

#### CY
Is this a special template language?  No, it's actually Hoon.  It has custom syntax for building XML-shaped nouns.  So we don't need a template language.

#### GWP
You're so old, you still believe in XHTML.

#### CY
HTML5 was a terrible blow.  But at least I'm not so old, I believe in SGML.

#### GWP
Obviously this "HTML" file is just a shell for a JS app.  As you see, it loads jQuery from a CDN and a bunch of resources from its own urbit.  Including my own `urb.js`.

#### CY
Front-end frameworks are important.  We're all quadrumana at heart.

#### GWP
I'll ignore that remark.  In any case, an `urb.js` client application POSTs commands to the server and gets back a stream of state diffs (Comet, basically).  I used jQuery here but this design works very well with React.

#### CY
It's also exactly the way an urbit client works, so the server doesn't have to know it's talking over JSON to a browser.  Basically the web app doesn't even know it's a web app.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/15.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/15.png)

##### 04.02

#### GWP
Sometimes a client is just a client.

#### CY
I don't think there's any need to code-golf our JS.  Any front-end --

#### GWP
Any front-end *engineer* will have no problem programming for `urb.js`.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/16.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/16.png)

##### 04.07

#### CY
If I edit this CSS, will the browser get a change event and reload it?

#### GWP
If the JS page is written right, I think so.  We normally listen to the page source and its dependencies and refresh on any change.  More long-polling.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/17.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/17.png)

##### 04.25

#### CY
We really screwed up the shoot here.  We were going to do a retake.

I did two things wrong: I merged in the wrong direction (nothing the system can't handle).  And I upgraded every `%taco` file *except the app itself*.

#### GWP
But then we thought: why not show how resilient the system is?  Also it was getting pretty late.

#### CY
So this is what happens when you get an update and it doesn't compile.  It's what should happen.  You see an error message and nothing else happens.  Your app is fine.

#### GWP
As easy as that!

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/18.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/18.png)

##### 04.34

#### CY
This sequence moves a little fast.  But as you see, we're not changing much about the application file proper when we give it its own network protocol and make it a web app.

After all, translating generic console effects into application state changes is a fairly interesting problem.  From our own protocol, no parsing is needed.  Likewise, the web app is just pushing raw commands and subscribing to state changes.

Now, this version of tictactoe is still really a client-server system, not peer-to-peer.  Asymmetric, not symmetric.  But the complexity of distributed state coupling isn't much different.

#### GWP
Now, you sound like you work for Oracle.  Or maybe the Defense Department.  Whatever, does the game actually work?

#### CY
It works for the take we recorded!

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/19.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/19.png)
 
##### 04.40 

#### CY
I think I had to kick it with an event to get it to change the prompt after the reload.  That's supposed to be automatic.

Note that the reason *my* prompt changes is that the new app file, on my urbit, has propagated to your urbit and reloaded the app, which then has streamed the new prompts to all subscribing clients, including both my urbit and yours.

Of course, our state adapter worked and our game hasn't been interrupted.

#### GWP
Is this schema evolution?  Is it typed?  Is it ACID?  How long can application state live this way?

#### CY
Yes, yes, yes, and as long as it wants.  Generally you do want to export core data to the filesystem so that other apps can see it.  The filesystem is not just revision-controlled but also typed, so this isn't too awful.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/20.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/20.png)

##### 04.57 

#### GWP
So you're playing against me on my own urbit.  You're talking to my `:taco` appliance from your urbit (which also is upstream of my installation), through the console protocol.  I'm logged into the same appliance as a web client, authenticated as myself.

#### CY
Pretty much.  Also, this is a really nice-looking tictatoe board and it's a shame our game is so short.  We even grey out the board when it's not your turn.

Note that the client UI even has a presence widget.  This isn't just tictactoe.  It's *social* tictactoe.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/21.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/21.png)

##### 05.19

#### CY
Did you have to just win like that?

#### GWP
I think I panicked.  Does it have to clear the board that fast?  Shouldn't it show my move?

#### CY
It's true.  This isn't the world's most polished tictactoe app.  On the other hand, we do have an operating system to finish. 

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/22.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/ii/22.png)

##### 05.22

#### GWP
We should have credits here.

#### CY
We should!  But since we don't, I'd like to thank Joan Torres for this awesome soundtrack, which he put together on like two days' notice.  In regular life Joan is actually a world-class jazz bassist.  He also writes Urbit jets in C.  Not sure how that's possible, but there you go.  

Unfortunately we borrowed the audio for part I (it's Ulrich Schnauss, _Knuddelmaus_), but we'll fix that as soon as possible!

</div>
