---
next: true
sort: 6
title: Web Apps
---

We've done purely functional web pages as hook files, and we've
done command line apps, so now let's interact with an app over
the web.  The fundamental concepts are no different than from the
command line -- you send "poke" messages to apps and subscribe to
data streams.  The only difference is that you do it with
javascript.

Let's create a small web app that counts the number of times you
poke it.  There'll be a button to poke the app, and line of text
below it saying how many times the app has been poked.  We can
update this by subscribing to that information from the app.

Let's first write the app, `/ape/click.hoon`:

```
/?    314
!:
|%
++  move  ,[bone %diff %mark *]
--
!:
|_  [hid=bowl clicks=@]
++  poke-click
  |=  click=%click
  ~&  [%poked +(clicks)]
  :_  +>.$(clicks +(clicks))
  %+  turn  (prey /the-path hid)
  |=([o=bone *] [o %diff %clicks +(clicks)])
++  peer-the-path
  |=  pax=path
  [[[ost.hid %diff %clicks clicks] ~] +>.$]
--
```

There's nothing really new here, except that we use a couple of
new marks, `click` and `clicks`.  When we get poked with a
`click`, we increment the variable `clicks` in our state and tell
all our subcribers the new value.  When someone subscribes to us
on the path `/the-path`, we immediately give them the current
number of clicks.

Let's take a look at the new marks.  Here's `/mar/click.hoon`:

```
|_  click=%click
++  grab
  |%
  ++  noun  |=(* %click)
  ++  json
    |=  jon=^json
    ?>  =('click' (need (so:jo jon)))
    %click
  --
--
```

The mark `click` has hoon type `%click`, which means the only
valid value is `%click`.  We can convert from `noun` by just
producing `%click`.

We also convert from json by parsing a json string with `so:jo`,
asserting the parsing succeeded with `need`, and asserting the
result was 'click' with `?>`, which asserts that its first child
is true.

> Note the argument to `++json`is `jon=^json`.  Why `^json`?
> `++json` shadows the type definition, so if we want to refer to
> the type, we have to prepend a `^`.  This extends to multiple
> levels:  `^^^foo` means "not the innermost `foo`, nor the next
> innermost, nor the next innermost, but the next innermost".

> `++jo` in `zuse` is a useful library for parsing complex json
> into hoon structures.  Our case is actually simple enough that
> the `?>` line could have been `?>  =([%s 'click'] jon)`.

We can test this mark from the command line:

```
~fintud-macrep:dojo> &click %click
%click
~fintud-macrep:dojo> &click &json [%s 'click']
%click
~fintud-macrep:dojo> &click &json &mime [/text/json (taco '"click"')]
%click
~fintud-macrep:dojo> &click &json &mime [/text/json (taco '"clickety"')]
/~fintud-macrep/home/0/mar/click:<[7 5].[8 11]>
ford: casting %json to %click
ford: cast %click
```

And `/mar/clicks.hoon`:

```
|_  clicks=@
++  grow
  |%
  ++  json
    (joba %clicks (jone clicks))
  --
--
```

`clicks` is just an atom.  We convert to json by creating an
object with a single field "clicks" with our value.

```
~fintud-macrep:dojo> &json &clicks 6
[%o p={[p='clicks' q=[%n p=~.^]]}]
~fintud-macrep:dojo> &mime &json &clicks 6
[[%text %json ~] p=12 q='{"clicks":6}']
```

Now that we know the marks involved, take another look at the app
above.  Everything should be pretty straightforward.  Let's poke
the app from the command line.

```
~fintud-macrep:dojo> |start %click
>=
~fintud-macrep:dojo> :click &click %click
[%poked 1]
>=
~fintud-macrep:dojo> :click &click %click
[%poked 2]
>=
```

**Exercise**:

- Modify `:sink` from the subcriptions chapter to listen to
  `:click` and print out the subscription updates on the command
  line.

That's all that's needed for the back end.  The front end is just
some "sail" html and javascript.  Here's `/pub/click/hymn.hook`:

```
;html
  ;head
    ;script(type "text/javascript", src "//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.1/jquery.min.js");
    ;script(type "text/javascript", src "/~~/~/at/home/lib/urb.js");
    ;title: Clickety!
  ==
  ;body
    ;div#cont
      ;input#go(type "button", value "Poke!");
      ;div#err(class "disabled");
      ;div#clicks;
    ==
    ;script(type "text/javascript", src "click/main.js");
  ==
==
```

You should recognize the sail syntax from an earlier chapter.
Aside from jquery, we also include `urb.js`, which is a framework
for interacting with urbit.

We have a button labeled "Poke!" and a div with id `clicks` where
we'll put the number of clicks.  We also include a small
javascript file where the client-side application logic can be
found.  It's in `/pub/click/main.js`:

```
$(function() {
  var clicks, $go, $clicks, $err

  $go     = $('#go')
  $clicks = $('#clicks')
  $err    = $('#err')
  
  window.urb.appl = "click"

  $go.on("click",
    function() {
      window.urb.send(
        "click", {mark: "click"}
      ,function(err,res) {
        if(err)
          return $err.text("There was an error. Sorry!")
        if(res.data !== undefined && 
           res.data.ok !== undefined && 
           res.data.ok !== true)
          $err.text(res.data.res)
        else
          console.log("poke succeeded");
      })
  })

  window.urb.bind('/the-path',
    function(err,dat) {
      clicks = dat.data.clicks
      $clicks.text(clicks)
    }
  )
})
```

We first set the default application to "click".  A page can, of
course, talk to multiple apps by specifying on each message which
app to talk to.

We set up two event handlers.  When we click the button, we run
`window.urb.send`, which sends a poke to an app.  The arguments
are data, parameters, and callback.  The data is the specific
data to send.  The parameters are all optional, but here's a list
of the available ones:

- `ship`:  target urbit.  Defaults to `window.urb.ship`, which
  defaults to the urbit which served the page.
- `appl`:  target app.  Defaults to `window.urb.appl`, which
  defaults to null.
- `mark`:  mark of the data.  Defaults to `window.urb.send.mark`,
  which defaults to "json".
- `wire`:  wire of the poke.  Defaults to `/`.

In our case, we specify only that the mark is "click".

The callback funciton is called when we receive an
acknowledgment.  If there was an error, we put it in the `err`
div that we defined above.  Otherwise, we printf to the console a
message saying the poke succeeded.  A common pattern is to gray
out a button when you click it and only reenable it when you get
the positive acknowledgment.

We also call `window.urb.bind`, which subscribes to a data stream
on an app.  It also takes three arguments: path, parameters, and
callback.  The path is the path on the app to subscribe to.  The
parameters are all optional (inded, we omit them entirely here),
but are similar to those for `send`:

- `ship`:  target urbit.  Defaults to `window.urb.ship`, which
  defaults to the urbit which served the page.
- `appl`:  target app.  Defaults to `window.urb.appl`, which
  defaults to null.
- `mark`:  mark of expected data.  Data of other marks is
  converted to this mark on the server before coming to the web
  front end.  Defaults to `window.urb.bind.mark`, which defaults
  to "json".
- `wire`:  wire of the subscription.  Defaults to the given path.

The callback function here just updates the data in the `clicks`
div.  A truly robust app would intelligently handle errors, of
course.

This app can be tested from the web by going to
`/~~/home/pub/click` on your server.  Usually, this is
`ship-name.urbit.org/~~/home/pub/click`.

Note that the app doesn't have anything web-specific in itself.
As far as it knows, it's just receiving pokes and subscriptions.
The javascript is fairly pure as well, sending and receiving json
everywhere.  The marks are the translation layer, and they're the
only things that need to know how the hoon types map to json.

**Exercise**:

- Open the app in multiple tabs, click the button, and verify
  that all the tabs stay in sink.  Poke it manually from the
  command line and verify the tabs are updated as well.
