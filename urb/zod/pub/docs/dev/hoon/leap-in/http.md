---
next: false
sort: 8
title: HTTP Requests and Timers
---

There's a variety of arvo services we haven't touched on yet.
Let's figure out how to do HTTP requests and set timers.

Let's build an uptime-monitoring system.  It'll ping a web
service periodically and print out an error when the response
code isn't 2xx.  Here's `ape/up.hoon`:

```
/?    314
|%
++  move  ,[bone card]
++  card
  $%  [%them wire (unit hiss)]
      [%wait wire @da]
  ==
--
|_  [hid=bowl on=_| in-progress=_| target=@t]
++  poke-cord
  |=  url=@t
  ^-  [(list move) _+>.$]
  ?:  =('off' url)
    [~ +>.$(on |)]
  ?:  =('on' url)
    :_  +>.$(on &, in-progress &)
    ?:  |(on in-progress)
      ~
    [ost.hid %them /request ~ (need (epur target)) %get ~ ~]~
  [~ +>.$(target url)]
++  thou-request
  |=  [wir=wire code=@ud headers=mess body=(unit octs)]
  ^-  [(list move) _+>.$]
  ?:  &((gte code 200) (lth code 300))
    ~&  [%all-is-well code]
    :_  +>.$
    ?:  on
      [ost.hid %wait /timer (add ~s10 now.hid)]~
    ~
  ~&  [%we-have-a-problem code]
  ~&  [%headers headers]
  ~&  [%body body]
  :_  +>.$
  ?:  on
    [ost.hid %wait /timer (add ~s10 now.hid)]~
  ~
++  wake-timer
  |=  [wir=wire ~]
  ?:  on
    :_  +>.$
    [ost.hid %them /request ~ (need (epur target)) %get ~ ~]~
  [~ +>.$(in-progress |)]
--
```

There's some fancy stuff going on here.  We'll go through line by
line, though.

Firstly, there's two kinds of cards we're sending to arvo.  A
`%them` card makes an HTTP requrest out of a unit `hiss`.

> Recall that `++unit` means "maybe".  Formally, "`(unit a)` is
> either null or a pair of null and a value in `a`".

A `hiss` (all of the following terms are defined in `zuse`) is a
pair of a `purl` and a `moth`.  A `purl` is a parsed url
structure, which can be created with `++epur`, which is a
function that takes a url as text and parses it into a `(unit
purl)`.  The result is null iff the url is malformed.

A `moth` is a triple of a `meth`, a `math`, and `(unit octs)`.
`meth` is the HTTP method, in this case `%get`.  `math` is a map
of HTTP headers.  The `(unit octs)` is a possible octet stream
representing the body.  If it's null, then no body is sent (as in
this case).

> An octect stream is a pair of the lenght in bytes of the data
> plus the data itself.  Recall that `++taco` takes text and
> turns it into an octet strem.

When you send this request, you can expect a `%thou` with the
response, which we handle later on in `++thou-request`.

For `%wait`, you just pass a `@da` (absolute date), and arvo will
produce a `%wake` when the time comes.

> A timer is guaranteed to not be triggered before the given
> time, but it's impossible to guarantee the timer will be
> triggered at exactly the requested time.

Let's take a look at our state:

```
|_  [hid=bowl on=_| in-progress=_| target=@t]
```

We have three pieces of app-specific state.  `target` is the url
we're monitoring.  `on` and `in-progress` are booleans
representing, respectively, whether or not we're supposed to keep
monitoring and whether or not we're in the middle of a request.

The type of booleans is usually written as `?`, which is a union
of `&` (true) and `|` false.  This type defaults to `&` (true).  In our
case, we want both of these to default to `|` (false).  Because
of that, we use `_|` as the type.  `_value` means "take the type
of `value`, but make the default value be `value`".  Thus, our
type is still booleans, just like `?`, but the default value is
`|` (false).

> This is the same `_` used in `_+>.$`, which means "the same
> type as the value "+>.$".  In other words, the same type as our
> current context and state.

Let's take a look at `++poke-cord`.  When we're poked with a
cord, we first check whether it's `'off'`.  If so, we set `on` to
false.

If not, we check whether it's `on`.  If so, we set `on` and
`in-progress` to true.  If it was already either on or in
progress, then we don't take any other immediate action.  If it
was both off and not in progress, then we send an HTTP request.
This request follows the pattern in `++hiss` well.  `(need (epur
target))`  is the parsed url, `%get` is the HTTP method, `~`
means no extra headers, and another `~` means no body.

> Note the `~` at the end of the move.  This is a convenient
> shortcut for creating a list of a single element.  It's part of
> a small family of such shortcuts.  `~[a b c]` is `[a b c ~]`,
> `[a b c]~` is `[[a b c] ~]` and `\`[a b c]` is `[~ a b c]`.
> These may be mixed and matched to create various convoluted
> structures and emojis.

If the argument is neither 'off' nor 'on', then we assume it's an
actual url, so we save it in `target`.

When the HTTP response comes back, we handle it with
`++thou-request`, which, besides the wire the request was sent
on, takes the status code, the response headers, and the response
body.

We check whether the status code is between 200 and 300.  If so,
all is well, and we print a message saying so.  We start the
timer for ten seconds after the present time.

If we got a bad status code, then we print out the entire
response and start the timer again.

After ten seconds, arvo will give us a `%wake` event, which will
be handled in `++wake-timer`.  If we're still supposed to keep
monitoring, we send the same HTTP request as before.  Otherwise,
we set `in-progress` to false.

Let's try it out:

```
~fintud-macrep:dojo> |start %up
>=
~fintud-macrep:dojo> :up &cord 'http://www.google.com'
>=
[%all-is-well 200]
[%all-is-well 200]
~fintud-macrep:dojo> :up &cord 'http://example.com'
>=
[%all-is-well 200]
~fintud-macrep:dojo> :up &cord 'http://google.com'
>=
[%we-have-a-problem 301]
[ %headers
  ~[
    [p='X-Frame-Options' q='SAMEORIGIN']
    [p='X-XSS-Protection' q='1; mode=block']
    [p='Content-Length' q='219']
    [p='Server' q='gws']
    [p='Cache-Control' q='public, max-age=2592000']
    [p='Expires' q='Sun, 10 Jan 2016 23:45:05 GMT']
    [p='Date' q='Fri, 11 Dec 2015 23:45:05 GMT']
    [p='Content-Type' q='text/html; charset=UTF-8']
    [p='Location' q='http://www.google.com/']
  ]
]
[ %body
  [ ~
    [ p=219
        q
      \/'<HTML><HEAD><meta http-equiv="content-type" content="text/html;charset=utf-8">\0a<TITLE>301 Moved</TI\/
        TLE></HEAD><BODY>\0a<H1>301 Moved</H1>\0aThe document has moved\0a<A HREF="http://www.google.com/">her
        e</A>.\0d\0a</BODY></HTML>\0d\0a'
      \/                                                                                                      \/
    ]
  ]
]
~fintud-macrep:dojo> :up &cord 'off'
```
