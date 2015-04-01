Eyre: Reference
===============

todo

Eyre: Commentary
================

Let us follow the loading of a simple cli app, as it bounces from
browser to server to browser and back.

## Initial request[#init]

An http request for `http://sampel-sipnym.urbit.org/cli` will be [redirected](dns)
to the `%eyre` on ~sampel-sipnym, and come in as a `%this` kiss.

From arvo, requests enter `++call`, which after some type reification are passed
along to `++apex:ye`. In the case of a `%this` kiss, its components are parsed
and handed off to `++handle`. `++apex:handle` will `++process` the request to a
`pest` or a `++done` core, and in the former case `++resolve` the pest into an
outgoing card.

XX it also seems to affect the current ship, test that serving ship name is consistently correct

The pest is produced by `++process`, which will first further `++parse` the
request, and if this does not make the response immediately obvious,
`++process-parsed` the resulting `perk`.

`++parse` produces the `perk`, by attempting to interpret the `pork`(url path)
[`++as-magic-filename`](#mage), `++as-beam`, and `++as-aux-request`. In this
case, `/cli` is parsed by the second case as a `%beam` query to `/=cli=`: a path
which starts with a valid ship name is expected to be a full clay(well, ford)
path, and one starting with a term implies the current serving ship and a case
of `0`, the current revision.

XX spur: when the desks are merged, `/cli` shall point to `/=main=/pub/cli`

The parsed `perk` generates a `%for` pest, `mark`ed as its extension(here
defaulting to `%urb`) and `wire`d with `~` to return unaltered to the client. It
goes on to `++resolve` by being passed to `++ford-get-beam`, which translates
the perk it into a `%f %exec %boil` note, adding an fcgi path-segment containing
query string and auth information.

`%ford`s translation of `/=cli=/hymn/hook` to a self-refreshing `%urb` html page
[deserves its own commentary](../ford/commentary), but we resume in `%eyre`
when the `%made` sign arrives in `++take`, and soon after `++axon:ye`. There the
`wire`, or rather the `whir` it has been verified to be, determines that the
response should be served immediately. However, as the mark is not `%mime`,
another trip to `%ford` is required to encode it, on the same wire; afterwards,
the value of the `%mime` cage is verified to be of the correct type, and finally
delivered back up the requesting duct as a succesful `%thou` HTTP response.

XX `%cast %mime` used to be in ford-get-beam, is there a reason it was removed?

## Back into the breach, or: auxilary requests

Now, it was mentioned that this result is self-refreshing: the `%urb`
translation door injects a `;script@"/~/on/{deps}.js"` into every page, `deps`
is a ford-readable hash of the set of resources that page construction depended
on.

This triggers another `%this` request. Its handling is identical to that of
`/cli` up until `++parse`, where it is seen not `++as-beam` but
`++as-aux-request`(auxillary requests starting with `/~/` or `/~~/`).
`/on/[hash]` is a long-`%poll`, which `++process-parsed`, for a `.js` mark,
answers with a direct `%fin %js`. Its contents are the static `++poll:js`, which
initiates the long-polling loop, run against an injected `urb.js` of 
`{poll: [hash]}`.

A `%fin` `perk` is `resolve`d by `resolve-fin`, which serves `%js` as a
`text/javascript` success `%this`.

When `poll.js` is recieved by the client, it opens an `XMLHttpRequest` for
`/~/on/{window.urb.poll}.json`, bringing us back to `%poll:process`.

In the case of a non-`%js` `/~/on/`, `%poll:process-parsed` turns into a
`++new-dependency`, which stores the listening duct, and `pass-note`s a `%wasp`
with the deps-hash back to `%ford` whence it came. While this occured, the page
has loaded.

Some indeterminate amount of time afterwards, with dropped `/~/on/{...}.json`s
being retried upon expiring and also being stored, a `%news` sign arrives in
`++axon`, and the hash in question is retrieved from the wire, and the listening
long-polls retrieved by the hash. Each receives a 205 "Reload parent view" HTTP
response, which `poll.js` dutifully executes, and a fixed typo of markdown is
rendered.

## Authentication.

Now, while this accurately reflects the presentation of e.g. a markdown file,
`/cli` is an application front-end, and one that permits only owner access. Its
second script is `@"/~~/~/at/main/lib/urb.js"`, semantically equivalent to
`/~/as/own/~/at/main/lib/urb.js`, and handled as follows.

In `++as-aux-request`, `%as %own` becomes `%auth %get our` perk, which
`++process` passes to `++process-parsed` passes to `++process-auth`. There, a
`yac` "ya" core is built `++for-client`: a `++cookie-prefix`, which is just the
serving ship name, is used to get a `++session-from-cookies`, here nil as the
client has no cookie set. In lieu of a cookie, a `++new-ya` is constructed, with
a random token `hole` and a `++new-cyst` which fills out `cyst` session state
from request data.

Returning to `++process-auth`, `%get` checks if the yac is authenticated with
the requested credentials(anonymous requests are always granted), which for the
fresh new `cyst` is not the case (more on success [later](#auth-ok)). Unless
authentiacting as a [foreign ship](#xeno), the only thing left is to
`++show-login-page`, which detects that the requested resource is not `%html`,
and produces a `%red` pest. For `%js`, `%red`irections `++resolve` to
`auth-redir:js`, a line of javascript which prepends `/~~` to the url path.

The owner-authenticated main page request similarly ends in `++show-login-page`,
which for the empty session `%fin`ishes with 401("unauthorized")
`++login-page:xml`.

The login page shows a simple prompt, and requests `/~/at/auth.js` to handle the
submission. And so we are, once again, attempting to divine if what we're doing
makes sense `++as-aux-request`.

To understand `/~/at`, there will first be a brief diversion to `~/auth.json`.
`auth.json`, perk `[%auth %json]`, in `++process-auth` serves `++stat-json:ya`,
containing such information as the serving ship, which identities are associated
with this session, and `oryx`, a CSRF token. The latter must be present on all
stateful requests, in this case needed to execute a log in.

`/~/at` is an alternate interface, which injects `auth.json` data into the
requested file. `/~/at/auth.js`, then, is a request for the built-in `auth:js`
(parsed to and processed from an `[%auth %js ~]` perk), with session data added
as `window.urb`. And indeed, ``[`%js /~/at/auth]`` is parsed to
``[%auth at [`%js /auth]``, which in `++process-auth` is re-`process`ed to
`[%fin %js {static script}]`, which is `++resolve-fin` after an `++add-json` of 
the relevant data. (immediate resolution is necessary to preserve any `..ya`
state change effected by `++auth-json:ya`.

It is at this point that there is first occasion for user input, namely the password.

The `auth:js` script sends a `PUT` request, also to `/~/auth.json`, which is
parsed to a `[%auth %try {password}]` perk, and upon success produces an updated
`auth.json` which reflects the changed `user`. Upon recieving this, the page is
refreshed to retry the original request.

## Authentication: success. [#auth-ok]

Upon refresh, `/~~/cli` brings us for the third time to `%get:process-auth`, but
this time the cookie is set, and the `yac` fetched contains the serving ship as
authenticated. The `++handle` sample is updated to reflect the requesting ship,
and the `process` continues for the rest of the pork, once again serving the
ford page.

The `/~/on/[deps].json` poll starts anew, and `/~~/~/at/main/lib/urb.js` we now
know to serve the window.urb necessary to make requests, and the sandard library
which extends it with a number of wrappers to them and other useful functions.

One of those functions is `urb.bind`, which is used to subscribe to application data.
Userspace javascript sets `urb.appl` to `/tic`, and binds XXX

## A path not taken: magic filenames [#mage]

The `/robots.txt` and `/favicon.(ico|png)` files are static, and served
immediately when caught by a `++parse`. 

XX index.html?

## A path not taken: foreign auth [#xeno]

While this example details a login `/~/as/own`, it is possible to be
authenticated as any ship on the network. A request for such seen in `%get:
process-auth` is passed to `++foreign-auth:ya`, which sends an `%ames /lon`
message to the ship in question. The foreign ship stores the inquiry a responds
with a `/hat`, containing the redirection host; this host is used to send the
client to a `/~/am` url on the foreign client, which acts as a normal login page
but later sends the client back. XX expand, basically the status quo is you're
logged in and `/~/as/foo` is ignored, just setting your `urb.user` XX


## A path not taken: deauthentication

`/~/away`, perk `[%away ~]`, produces a static `++logout-page:xml`, which also
uses `/~/at/auth.js`, to send a `DELETE /~/auth.json`, perk `[%auth %del]`. This
wipes the session from memory.

## Appendix A: DNS [#dns]

The `*.urbit.org` domain can be used to access destroyers and cruisers. In the
common case oh hosted ships, this is done by dynamic DNS directly to the hosting
instance. We do not speak of the uncommon case. When ports are blocked and
infrastructure crumbles around you, only imported martian networking can be
trusted: the `%get` and `%got` [gram]()s are used to proxy [`%this` requests]() and
[`%thou` responses]() respectively.
