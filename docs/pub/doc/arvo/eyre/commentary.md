Eyre: Reference
===============

todo

Eyre: Commentary
================

Let us follow the loading of a simple tic tac toe app, as it bounces from
browser to server to browser and back.

## Initial request

An http request for `http://sampel-sipnym.urbit.org/tic` will be [redirected](dns)
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
case, `/tic` is parsed by the second case as a `%beam` query to `/=tic=`: a path
which starts with a valid ship name is expected to be a full clay(well, ford)
path, and one starting with a term implies the current serving ship and a case
of `0`, the current revision.

XX spur: when the desks are merged, `/tic` shall point to `/=main=/pub/tic`

The parsed `perk` generates a `%for` pest, `mark`ed as its extension(here
defaulting to `%urb`) and `wire`d with `~` to return unaltered to the client. It
goes on to `++resolve` by being passed to `++ford-get-beam`, which translates
the perk it into a `%f %exec %boil` note, adding an fcgi path-segment containing
query string and auth information.

`%ford`s translation of `/=tic=/hymn/hook` to a self-refreshing `%urb` html page
[deserves its own commentary](../../ford/commentary), but we resume in `%eyre`
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
`/tic` up until `++parse`, where it is seen not `++as-beam` but
`++as-aux-request`(auxillary requests starting with `/~/` or `/~~/`).
`/on/[hash]` is a long-`%poll`, which `++process-parsed`, for a `.js` mark,
answers with a direct `%fin %js`. Its contents are the static `++poll:js`, which
initiates the long-polling loop, run against an injected `urb.js` of 
`{poll: [hash]}`.

A `%fin` `perk` is `resolve`d by `resolve-fin`, which serves `%js` as a
`text/javascript` success `%this`.

When `poll.js` is recieved by the client, it opens an `XMLHttpRequest` for
`/~/on/{window.urb.poll}.json`, bringing us back to `%poll:++process`.

In the case of a non-`%js` `/~/on/`, `++process-parsed`

## Appendix A: DNS [#dns]

The `*.urbit.org` domain can be used to access destroyers and cruisers. In the
common case oh hosted ships, this is done by dynamic DNS directly to the hosting
instance. We do not speak of the uncommon case. When ports are blocked and
infrastructure crumbles around you, only imported martian networking can be
trusted: the `%get` and `%got` [gram]()s are used to proxy [`%this` requests]() and
[`%thou` responses]() respectively.

## Appendix B: magic filenames [#mage]

The `/robots.txt` and `/favicon.(ico|png)` files are static, and served immediately when caught by a `++parse`. 

XX index.html?
