# Developer Environment Setup

The quickest way to get set up to play with Shrubbery and Sky is to [boot a comet](https://docs.urbit.org/manual/getting-started/self-hosted/cli) and run `|install ~met %neo`. You could also [boot a moon](https://docs.urbit.org/manual/os/basics#moons) for this.

When you boot your ship you should [use the `--loom 33` flag](https://docs.urbit.org/manual/running/vere#--loom-size), which allocates 8GB of memory on your machine for your ship. During Shrubbery development you could end up nuking and restarting the %neo agent many times, which will quickly use up the 2GB of memory ships are allocated by default. If/when your ship runs out of memory, just boot a new one and copy your work into that.

If you want to work from the GitHub repo…
- Clone [urbit/shrub](https://github.com/urbit/shrub) to your machine. (The `develop` branch is the latest stable build.)
- [Boot a fakeship](https://docs.urbit.org/courses/environment#creating-a-fake-ship)
- `> |new-desk %neo` on the fakeship
- `> |mount %neo`
- `$ rsync -avL path/to/pkg/shrub/* path/to/fakeship/neo`
- `> |commit %neo`
- `> |install our %neo`

This will start the %neo agent which runs the Shrubbery prototype. Once that's finished booting, which will take a few minutes, go to `<localhost:port>/neo/sky` to access Sky.

When you want to move changes from your local repo to the fakeship, repeat the `rsync -avL` command above. The %neo agent can do incremental builds and will make changes to your `/con` and `/imp` files as you'd expect, but if you make a change to a `/pro` file you'll have to `|nuke %neo` and `|revive %neo`.
