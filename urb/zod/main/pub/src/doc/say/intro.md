#Urbit

We got tired of system software from the 1970s. So we wrote our own. From scratch.

##Nock, a minimal virtual machine

[Nock](https://github.com/urbit/urbit/blob/master/urb/zod/spec/nock/5.txt) is a
homoiconic combinator algebra, not much fancier than SKI combinators. The spec
fits on a T-shirt and gzips to 340 bytes. We never extend Nock or call out to Unix from it.

##Hoon, a typed functional language

Hoon is a strict, typed, functional language that compiles itself to Nock.
As a functional systems language, Hoon is especially good at metaprogramming,
self-virtualization, hotpatching; marshalling and validating untyped data;
decoding and encoding binary message formats. Hoon is designed for event
programming, so there is no concurrency model.

##Arvo, a functional operating system

Arvo is an event-driven server OS built on the same event library as node.js
(libuv). Unlike node.js, Arvo is written in Hoon, isolated from Unix and a
persistent single-level store. Arvo is modular. Present modules provide a
network messaging protocol, a REPL and task manager, a revision-controlled
filesystem, a text console, and an HTTP server. 




