<div class="short">

`%time`
=======

Our simple timer.

It allows vanes and applications to set and timer events, which are
managed in a simple priority queue. `%time` produces effects to start
the unix timer, and when the requested `%time` passes, unix sends wake
events to `%time`, which time routes back to original sender. We don't
guarantee that a timer event will happen at exactly the `%time` it was
set for, or even that it'll be particularly close. A timer event is a
request to not be woken until after the given time.

`%eyre` uses `%time` for timing out sessions, and `%clay` uses `%time`
for keeping track of time-specified file requests. `%ames` should
probably use `%time` to keep track of things like network timeouts and
retry timing, but it currently uses its own alarm system.

</div>
