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
- [ ] Snapshots should block until that event is commited to disk.

King-Haskell specific features:

- [x] Re-implement `collectFX` flow.
- [ ] Test new `collectFX` flow

Performance:

- [x] Batching during replay.
- [x] Batching during normal operation.

Polish:

- [x] Cleanup batching flow.
- [x] Think through how to shutdown the serf on exception.
- [ ] Logging for new IPC flow.
- [ ] Logging for boot sequence.
- [ ] Bring back progress bars.
- [ ] Hook up error callbacks to IO Drivers.

Unrelated bugs:

- [ ] Handle ^C in connected terminals.
- [ ] terminal driver seems to have a race condition when spinner changed
      too quickly.
- [ ] King should shutdown promptly on ^C. Always takes 2s in practice.
