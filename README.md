# Arvo

A clean-slate operating system.

## Usage

To run Arvo, you'll need [Urbit](https://github.com/urbit/urbit/). To install Urbit and run Arvo please follow the instructions in the [getting started docs](https://urbit.org/docs/getting-started/). You'll be on the live network in a few minutes.

If you're doing development on Arvo, keep reading.

## Documentation

Find Arvo's documentation [on urbit.org](https://urbit.org/docs/learn/arvo/).

## Development

To boot a fake ship from your development files, run `urbit` with the following arguments:

```
urbit -F zod -A /path/to/arvo -c fakezod
```

Mount Arvo's filesystem allows you to update its contents through Unix. To do so, run `|mount` in dojo. It is most common to `|mount /=home=`.

To create a custom pill (bootstrapping object) from the files loaded into the home desk, run `.my/pill +solid`. Your pill will appear in `/path/to/fakezod/.urb/put/my.pill`.

To boot a fake ship with a custom pill, use the `-B` flag:

```
urbit -F zod -A /path/to/arvo -B /path/to.pill -c fakezod
```

To run all tests in `/tests`, run `+test` in dojo. `+test /some/path` would only run all tests in `/tests/some/path`.

## Maintainers

Most parts of Arvo have dedicated maintainers.

* `/sys/hoon`: @pilfer-pandex (~pilfer-pandex)
* `/sys/zuse`: @pilfer-pandex (~pilfer-pandex)
* `/sys/arvo`: @jtobin (~nidsut-tomdun)
* `/sys/vane/ames`: @belisarius222 (~rovnys-ricfer) & @joemfb (~master-morzod)
* `/sys/vane/behn`: @belisarius222 (~rovnys-ricfer)
* `/sys/vane/clay`: @philipcmonk (~wicdev-wisryt)
* `/sys/vane/dill`: @bernardodelaplaz (~rigdyn-sondur)
* `/sys/vane/eyre`: @eglaysher (~littel-ponnys)
* `/sys/vane/ford`: @belisarius222 (~rovnys-ricfer) & @eglaysher (~littel-ponnys)
* `/sys/vane/gall`: @jtobin (~nidsut-tomdun)
* `/sys/vane/jael`: @fang- (~palfun-foslup) & @joemfb (~master-morzod)
* `/app/acme`: @joemfb (~master-morzod)
* `/app/dns`: @joemfb (~master-morzod)
* `/app/hall`: @fang- (~palfun-foslup)
* `/app/talk`: @fang- (~palfun-foslup)
* `/app/aqua`: @philipcmonk (~wicdev-wisryt)
* `/lib/test`: @eglaysher (~littel-ponnys)

## Contributing

Contributions of any form are more than welcome! If something doesn't seem right, and there is no issue about it yet, feel free to open one.

If you're looking to make code contributions, there are a few things you can do:

- Join the [urbit-dev](https://groups.google.com/a/urbit.org/forum/#!forum/dev) mailing list
- [Sign up for Hoon School](mailto:support@urbit.org?subject=Hoon School), a course we run to teach the Hoon programming language and Urbit application development
- Check out [good contributor issues](https://github.com/urbit/arvo/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+contributor+issue%22)
- Reach out to [support@urbit.org](mailto:support@urbit.org) to say hi and ask any questions you might have
