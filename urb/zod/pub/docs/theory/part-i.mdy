---
layout: video
sort: 1
title: demo.2015.a
---

# demo.2015.a

<iframe src="https://player.vimeo.com/video/131710210?color=fff&amp;title=0&amp;byline=0&amp;portrait=0" frameborder="0" webkitallowfullscreen="" mozallowfullscreen="" allowfullscreen=""></iframe>

## DVD commentary

<div class='short'>

#### CY
Hi!  This is Curtis Yarvin, writer and director.  I also played `~tasfyn-partyv`.  I'm here with Galen Wolfe-Pauly - dramaturge, producer, editor, `~firput-miplex`.  Hi, Galen!

#### GWP
Hi!  We definitely didn't do this alone. Philip Monk (`~wictuc-folrex`), Anton Dyudin (`~sondel-forsut`) and Henry Ault all helped a lot. Not to mention everyone who has committed to the repo...

#### CY
This is the urbit demo known officially as ~2015.6.26.  Or informally, as "Russian Nock."

In this commentary track, we'll step you through the film line by line and show you the awesome that is urbit.  And of course we'll exchange snarky remarks about the shoot.

Galen, why "Russian Nock?"

#### GWP
(laughs).  Well, nock is urbit's VM, of course.  But it's from a movie, "Russian Ark," which was shot in one take.

#### CY
Nontrivial for a 13-minute demo of alpha software.

#### GWP
We actually gave up.  We put in a screen clear.

#### CY
One.  At the act break.

Also, truth in advertising!  urbit is alpha and still pretty slow.  A lot of the footage is sped up.  But almost nothing is actually *faked*.

And we never used the reload button.  Everything that looks reactive is.

#### GWP
It looks like the input lines are pasted in, but we actually typed them.  Then I took the footage and

#### CY
And edited the keystrokes together.  One cut per line.  Yes, this workflow was my brilliant idea, thank you.

#### GWP
Next time, actual tools!

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/01.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/01.png)

##### 00.03

#### CY
I'm in my urbit, `~tasfyn-partyv`.  This is of course a VM running on my laptop.

I have an urbit ticket for another planet, `~firput-miplex`.  I want to send it to my friend Aldo van Eyck.

#### GWP
Actually, he's a '50s Dutch architect from Team 10.  Maybe we'll start getting email for him!

#### CY
From my `:dojo` agent, basically the urbit shell, I'm going to run the `|invite` command, which tells `~tasfyn-partyv` to email Aldo the secret he needs to create `~firput-miplex`.

Via an HTTP API to a mail delivery service, of course.  But - the invite is actually the one fake thing in this video.

#### GWP
The Mailgun API connector wasn't quite together yet.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/02.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/02.png)

##### 00.07

#### CY
Via `:talk`, I get a success message from `|invite` - the 55th message `~tasfyn-partyv` got in its lifetime.

#### GWP
Obviously this isn't the real `~tasfyn-partyv`, we're on

#### CY
A test network.  Because we're still alpha.

Then I typed `^X` to switch from `:dojo` to `:talk`.

#### GWP
These two applications are sharing your command line.

#### CY
You multiplex the command line with ^X.  It's like switching windows.  But everyone's output

#### GWP
Gets mixed together and scrolls up.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/03.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/03.png)

##### 00.14

#### CY
In `:talk`, I set my target base to `~firput-miplex`.  

Then send you a `@` message, like irc /me.

#### GWP
Bear in mind, `~firput-miplex` doesn't *exist* yet.

#### CY
That's right.  Suppose you sent someone email, but to an account they haven't created, on a server whose domain isn't registered.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/04.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/04.png)

##### 00.17

#### CY
Obviously I stole this great error message from NFS.

#### GWP
Obviously.

#### CY
In urbit your name, or *base*, is your network address.

We're sending `~firput-miplex` packets but not hearing back.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/05-a.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/05-a.png)

##### 00.22

#### GWP
Aldo gets the email with his urbit ticket.  He can't resist. 

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/05.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/05.png)

##### 00.37

#### GWP
He builds urbit on his macbook and runs it.

#### CY
He probably should get that secret out of his bash history.

#### GWP
That's what Sergey Aleynikov said to himself!

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/06.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/06.png)

##### 00.43

#### CY
Ignore the boot messages behind the curtain.  But look -

#### GWP
I got your message.  Definitely not how email works.

#### CY
It's the way urbit networking works.  The transport layer has no concept of connectivity state.   it just waits.

#### GWP
Nobody who watches our film notices this.  Nobody.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/07.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/07.png)

##### 00.45

#### CY
"Is your neighbor" means you've exchanged keys, basically.

#### GWP
Why do we see it twice?

#### CY
I have no idea.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/08.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/08.png)

##### 01.26

#### GWP
I go to your `tasfyn-partyv.urbit.org` url.  But there's nothing there.  Error page.  What?

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/09.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/09.png)

##### 01.48

#### CY
I use a shell terminal to create the markdown file, edit it in vim, save it, and -

#### GWP
And my browser immediately loads it as a web page.

#### CY
Your error page is long-polling my server, which tracks dependencies for every page, good or bad.

When I saved the markdown file, my urbit was watching the unix directory and committed my save as a change.  That change event goes to the filesystem, `%clay`.

`%clay` applies it and sees that the build system, `%ford`, is watching this subtree.  `%ford` gets the notification and reports a dependency change to the web server `%eyre`, which returns 200 on the long-poll.

Your browser then requests the new version of this url.  `%eyre` forwards the request to `%ford`, which figures out that it can build an appropriate response by converting the markdown file to an HTML tree, then injecting the reload script, then printing the HTML.

#### GWP
This isn't even a dev tool.  It works in production.

#### CY
Not that it scales!  But it works fine behind an nginx cache.

#### GWP
If we edit this commentary transcript right now, it'll change on the reader's screen.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/10.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/10.png)

##### 02.14

#### CY
Here I go and edit the same file again, and we see your refresh script do its work.  Ho hum, boring already.

#### GWP
Imagine living in a world where everyone assumed it should just work this way - why wouldn't the browser update?  Why would you not track dependencies on the page build?

#### CY
Of course it's not just the page content we're tracking.  If I changed the markdown parser, you'd reload again.  It's everything local and time-versioned used in the build.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/11.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/11.png)

##### 02.34

#### CY
Here I send you the magic long-form url that lets you specify version numbers directly.  So you can see the old version of the poem.

#### CY
There are two schools of web server url handling.  There's the open-minded school that let you parse a path your own way, and the closed-minded school that hardcodes its own semantics into the url.  urbit's -

#### GWP
urbit's is the closed-minded school.

#### CY
Exactly.  Your url goes mostly undigested to the `%ford` build system.  If these aren't the urls you want the user to see, wrap it in an nginx rewrite rule.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/12.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/12.png)

##### 02.49

#### CY
Now the same thing but one directory up.

#### GWP
Or `++arch` as we say in urbit.

#### CY
Imagine a unix inode, except if every inode was a directory plus an optional file.  But also, if unix was git.

#### GWP
Or `%clay` as we say in urbit.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/13.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/13.png)

##### 02.53

#### GWP
And the arch autoupdates too.  Okay, we get it!

#### CY
What's really going on here is that the whole subtree of urls under `/home/tree` is being rendered by one `tree.hoon` file, which defines a function that maps the url suffix to an HTML document.

This `tree` function is pretty simple - it just uses the url suffix, `/pub/verse/bunting`, as a path in the filesystem `%clay`.

Since there's no file on that arch, we just show the list of children.  Just like Apache `public_html`.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/14.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/14.png)

##### 03.15

#### CY
Here we're in the root of the urbit doc tree.

#### GWP
Don't look too hard at the fine print!

#### CY
Knuth was wrong about the root of all evil.  It's actually premature documentation.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/15.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/15.png)

##### 03.36

#### GWP
Definitely don't throw away your Wordpress just yet.

#### CY
Galen wrote and designed this tree renderer.  It's actually not bad for the amount of work he put in.

#### GWP
Well, me and Anton mostly.  It's a good start.

#### CY
We should have demoed the navigation controls at the top, which let you move up or to the next sibling.  There's no excuse for tree-structured content without uniform tree navigation affordances.

#### GWP
Jakob Nielsen is shaking his head somewhere.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/16.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/16.png)

##### 03.47

#### CY
Note that you're now surfing `firput-miplex.urbit.org`.  That's your own urbit, of course.  

You're using an application, not just a functional renderer.  So you need to log in with an identity.  The default is you.  And your password is that secret you got in the email - your ticket.

#### GWP
We're not demoing single sign-on here, but we do it. 

#### CY
If I logged into `~firput-miplex` as `~tasfyn-partyv`, it would redirect me to `tasfyn-partyv.urbit.org` with a challenge.  If I was already logged into my own urbit, logging in to yours would be automatic.

We set a cleartext `*.urbit.org` cookie with your urbit base, so within `urbit.org` login is generally automatic.

#### GWP
Obviously this login system is application-independent.  It's a part of urbit, not a part of `:talk.`

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/17.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/17.png)

##### 03.49

#### CY
Isn't that pretty?

#### GWP
Ugh.  Alpha is the name of the game around here.  It's a start.

#### CY
Galen went to art school or something.  He has high standards.

Notice of course that we see the same message backlog in the web ui and the console.  Obviously it's the same app.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/17-a.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/17-a.png)

##### 04.15

#### GWP
Nice little recovery there.  Of course, in "Russian Ark"

#### CY
They'd have reshot the whole movie.  They had standards.

Anyway, `:talk` is really our flagship app.  It weighs in at almost 2000 lines of code counting the protocols.  It's a genuine distributed system and it's being used right now.

To make a long story short, `:talk` is basically Usenet slumming as a chat server.  Well, it could use a few more bells and whistles.  But the bones of NNTP are there.  A chat line is just one type of message...

#### GWP
Next on our agenda, animated cat gifs.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/18.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/18.png)

##### 04.20

#### CY
We actually do support more than four urbits in a chatroom.

#### AD
If there's no clock skew.

#### GWP
The clock skew bug!  That had to happen when we were filming.

#### CY
NTP works, except when it doesn't.  Fortunately Philip was up to the challenge.

#### PM
The bug was in my code.  On the other hand, you put it there.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/19.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/19.png)

##### 04.34

#### GWP
This whole bitcoin sequence is a rendering disaster.  On console and browser alike.

#### CY
Also it makes no sense.  Who types in their bitcoin address?  More thinking, comrade, will have to go into this exercise.

#### GWP
Who else has a language with syntax for a bitcoin address?

#### CY
We need a syntax for bitcoin amounts, too.  That argument is a string.  Lame.

#### GWP
At least the example isn't faked.  Well, unless you count using the coinbase sandbox as "fake."

#### CY
Arguably an exchange API is a silly way to send a payment.  We should run a bitcoin core daemon with an urbit gateway.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/20.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/20.png)

##### 04.55

#### CY
Yes, we actually tweeted to the real twitters here.

#### GWP
Maybe there should be a twitter sandbox.

#### CY
I know a lot of people who could use a twitter sandbox!

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/21.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/21.png)

##### 05.14

#### CY
Here I start lecturing you a bunch.  Anyway, this argument is a little cryptic.  Of course an evil or compromised host can look at any of your data on the service, barring some kind of breakthrough in efficient homomorphic encryption.  And this is true whether you're a VM or a database row.  The Linode hack, for instance, was an attack on cloud VMs.

But it still makes a lot more sense to keep valuable secrets in a cloud-hosted VM than a cloud-hosted application database - because there are all sorts of ways that database can be queried.  Nobody queries a VM.

When the line between hosting provider and hosted computer is drawn at the edge of the VM, it's a very precise line.  It would be outrageous if amazon was peeking into AWS VMs, stealing their data and using it to sell more ads.  So logically you're not safer, but in practice you are.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/22.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/22.png)

##### 05.33

#### CY
Of course it looks better if you unlimit coredumpsize.  So you actually, uh, dump core.  Like the script says.

#### GWP
Curtis went to unix school or something.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/23.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/23.png)

##### 05.41

#### CY
Too many printfs!  But you see that there's only one level of storage in urbit.  At least logically, all data persists.

#### GWP
My iPhone pretends to work this way.  But I'm not sure it does.

#### CY
Practically, the way we create this abstraction is the way all databases work: a checkpoint and a log.  Logging is easy - there are all kinds of great cloud tools for it.

If you look at the printfs, you have a checkpoint at event 9616.  but the log goes up to 9624.  You load this checkpoint, then rerun the last 8 events.

The only rule is that you can't execute the actions caused by an event until you've logged the event.  For example, if an incoming packet causes an outgoing packet, you can't send the outgoing packet until you've logged the incoming.

#### GWP
So every event is a transaction.  Watch out, Oracle!

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/24.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/24.png)

##### 05.52

#### GWP
I'm not sure we can get away with this "NoDB" meme.  Didn't you just say urbit *is* a database?

#### CY
A foolish consistency is the hobgoblin of little minds.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/25.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/25.png)

##### 06.41

#### CY
A lot of this demo is sped up.  But that part is *really* sped up.  Anyway, in case it's not obvious what you did,

#### GWP
I deleted my checkpoint, than restarted from just the log.

#### CY
And arrived at exactly the same bit-for-bit state.

#### GWP
You know, cosmic rays could cause big problems with urbit.

#### CY
I'm aware of that.  It's a real issue with any kind of repeatable computing.  You depend on perfect execution.  Certainly, ECC memory is a must.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/26.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/26.png)

##### 06.54

#### GWP
And... here we see a massive demo fail.

#### CY
Oh, hardly.  First, the nock spec was somehow not in `%clay`.  So we had to add it back in from another window.  Second, you said `+cat` in `:talk`, so you `:talk`ed the cat command rather than `:dojo`ing it.

And third, your browser hung up and reconnected.  Just typical printf noise from alpha code.

#### GWP
I wonder if anyone actually noticed that.

[![](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/27.png)](https://storage.googleapis.com/urbit-extra/preview-1/screenshots/i/27.png)

##### 06.56

#### GWP
And... there's nock.  How did you come up with that, anyway, Curtis?

#### CY
Gosh, I barely even remember.  I started with something much bigger and kept pulling things out.  I guess.

#### GWP
Sort of the Kevin Herbert strategy?

#### CY
(laughs.)  Kids, definitely don't try this at home.

#### GWP
Thanks for watching our film!  Or the first half, anyway...

</div>
