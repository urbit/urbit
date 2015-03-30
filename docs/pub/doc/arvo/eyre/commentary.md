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
and handed off to `++handle`.

## Apendix A: DNS[#dns]

The `*.urbit.org` domain can be used to access destroyers and cruisers. In the
common case oh hosted ships, this is done by dynamic DNS directly to the hosting
instance. We do not speak of the uncommon case. When ports are blocked and
infrastructure crumbles around you, only imported martian networking can be
trusted: the `%get` and `%got` [gram]()s are used to proxy [`%this` requests]() and
[`%thou` responses]() respectively.
