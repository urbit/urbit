# New IPC Protocol

Stubbed out:

- [x] Handle replacement events (stubbed out now b/c interface can't
      handle unparsed nouns)
- [x] Handle IPC errors by killing serf process.
- [x] Handle `peek` and `pack` in `swimming` flow.
- [x] Documentation for `Urbit.Vere.Serf.IPC`.
- [x] Unstub slog/stder/dead callbacks on serf config.
- [x] Remove GoodParse hack in newRunCompute.
- [x] Bring back tank printing.
- [x] Handle serf stderr message correctly.
- [x] Bring back `logEvent`.
- [x] Snapshots should block until that event is commited to disk.
- [x] Hook up error callbacks to IO Drivers.
- [x] Do something useful with error callbacks from IO Drivers.

Bugs:

- [x] In non-daemon mode, serf slogs/stderr output that happens *before*
      the terminal connects should still go to stderr.
- [x] Serf stderr should also be send (along with slogs) to all connected
      terminals.
- [x] `king new` should reject pier directories that already exist.
- [x] In non-daemon-mode, ^D doesn't bring down Urbit properly.
- [x] Spinner updated multiple times with the same event, and this causes
      logging of events to contain duplicates.

King-Haskell specific features:

- [x] Re-implement `collectFX` flow in Serf/Pier.
- [x] Hook up `collectFX` to CLI.
- [ ] Get `collect-all-fx` flow working again.

Performance:

- [x] Batching during replay.
- [x] Batching during normal operation.

Optimization:

- [x] IO Driver Event Prioritization

Polish:

- [x] Cleanup batching flow.
- [x] Think through how to shutdown the serf on exception.
- [x] King should shutdown promptly on ^C. Always takes 2s in practice.
- [x] Bring back progress bars.
- [x] Make sure replay progress bars go to stderr.
- [x] Logging for new IPC flow.
- [x] Logging for boot sequence.
- [x] Take snapshots on clean shutdown.

# Misc Bugs

- [ ] `king run --collect-fx` flag does nothing. Remove or implement.
- [x] Handle ^C in connected terminals. It should interrupt current
      event (send SIGINT to serf, which will cause the current event to
      fail promptly).
- [x] The terminal driver seems to have a race condition when spinner
      changed too quickly.


# Take Advantage of New IPC Features

- [ ] Hook up `scry` to drivers.
  - Any immediate applications of this?

- [ ] Allow scrys to go into the %work batching flow for better latency.

- Handle event errors in other cases:
  - [ ] Ames packet failures should print (but not too often).
  - [ ] Incoming Http requests should produce 500 responses.
  - [ ] Terminal event errors should be printed in connected terminals.
  - [ ] Http client responses should be retried.


# Further IO Driver Startup Flow Betterment

Implement Pier-wide process start events

- [x] Handle %vega and exit effects.
- [x] Handle %trim effect
- [x] Inject entropy event on pier start: ``[//arvo [%wack ENT]]`
- [ ] Verbose flag: `-v` injects `[%verb ~]`

- CLI event injection: `-I file-path`. The `file-path` is a jammed noun
  representing an event: `[wire card]`.
  - [x] Just parse it as an `Ev` for now.
  - [ ] Make the serf IPC code not care about the shape of events and effects.
  - [ ] Support invalid events throughout the system (use `Lenient`?)

# Polish

- [x] Goot logging output in non-verbose mode.
- [x] Command-Line flag to re-enable verbose output.


# Cleanup

- [x] ShutdownSTM action that's passed to the terminal driver should
      live in `KingEnv` and should be available to all drivers.
- [ ] Break most logic from `Main.hs` out into modules.
- [ ] Simplify `Main.hs` flows.
- [ ] Cleanup Terminal Driver code.
- [x] Spin off `racquire` into it's own package.
- [x] Spin off `urbit-noun-core` and `urbit-noun` packages.
- [x] Spin off `urbit-eventlog-lmdb` into it's own package.
- [ ] Spin off `Urbit.Vere.Serf` into it's own package
  - Make it care less about the shape of events and effects.
- [ ] Spin off per-pier logic into it's own package.
  - Probably `urbit-pier`
