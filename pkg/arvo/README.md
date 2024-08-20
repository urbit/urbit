# Arvo

A clean-slate operating system.

## Usage

To run Arvo, you'll need [Urbit](https://github.com/urbit/urbit/). To install
Urbit and run Arvo please follow the instructions in the [getting started
docs](https://urbit.org/docs/getting-started/). You'll be on the live network
in a few minutes.

If you're doing development on Arvo, keep reading.

## Documentation

Find Arvo's documentation [on urbit.org](https://urbit.org/docs/learn/arvo/).

## Development

To boot a fake ship from your development files, run `urbit` with the following arguments:

```
urbit -F zod -A /path/to/arvo -c fakezod
```

Mount Arvo's filesystem allows you to update its contents through Unix. To do so, run `|mount` in dojo. It is most common to `|mount /=base=`.

To create a custom pill (bootstrapping object) from the files loaded into the base desk, run `.my/pill +solid`. Your pill will appear in `/path/to/fakezod/.urb/put/my.pill`.

To boot a fake ship with a custom pill, use the `-B` flag:

```
urbit -F zod -A /path/to/arvo -B /path/to.pill -c fakezod
```

To run all tests in `/tests`, run `-test %/tests` in dojo. To run only the tests in `/tests/some/path`, use `-test %/tests/some/path`.

## Maintainers

Most parts of Arvo have dedicated maintainers.

* `/sys/hoon`: @pilfer-pandex (~pilfer-pandex)
* `/sys/zuse`: @pilfer-pandex (~pilfer-pandex)
* `/sys/arvo`: @joemfb (~master-morzod)
* `/sys/vane/ames`: @belisarius222 (~rovnys-ricfer) & @philipcmonk (~wicdev-wisryt)
* `/sys/vane/behn`: @belisarius222 (~rovnys-ricfer)
* `/sys/vane/clay`: @philipcmonk (~wicdev-wisryt) & @belisarius222 (~rovnys-ricfer)
* `/sys/vane/dill`: @fang- (~palfun-foslup)
* `/sys/vane/eyre`: @fang- (~palfun-foslup)
* `/sys/vane/gall`: @philipcmonk (~wicdev-wisryt)
* `/sys/vane/jael`: @fang- (~palfun-foslup) & @philipcmonk (~wicdev-wisryt)
* `/app/acme`: @joemfb (~master-morzod)
* `/app/dns`: @joemfb (~master-morzod)
* `/app/aqua`: @philipcmonk (~wicdev-wisryt)
* `/app/hood`: @belisarius222 (~rovnys-ricfer)
* `/lib/hood/drum`: @fang- (~palfun-foslup)
* `/lib/hood/kiln`: @philipcmonk (~wicdev-wisryt)

## Contributing

Contributions of any form are more than welcome!  Please take a look at our
[contributing guidelines][cont] for details on our git practices, coding
styles, how we manage issues, and so on.

You might also be interested in:

- joining the [urbit-dev][list] mailing list.
- [applying to Hoon School][mail], a course we run to teach the Hoon
  programming language and Urbit application development.

[list]: https://groups.google.com/a/urbit.org/forum/#!forum/dev
[mail]: mailto:support@urbit.org
[cont]: https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md
