# Landscape

Landscape provides the primary launching interface for Tlon's suite of userspace applications. This directory contains the front-end web application to power said interface.

Landscape is built primarily using [React], [Typescript], and [Tailwind CSS]. [Vite] ensures that all code and assets are loaded appropriately, bundles the application for distribution and provides a functional dev environment.

## Getting Started

To get started using Landscape first you need to run, `npm i && npm run bootstrap` at the top level of the greater urbit repo. This will install your npm dependencies and correctly link the current implementation of the packages at `pkg/npm/*` to your dependencies.

If you intend to edit those packages will developing on Landscape, you should also have `npm run watch-libs` running to build and re-link them after every change.

Once that's done, you can then run `npm run mock` if you'd like to get started immediately. This will use hard-coded mock data to power the interface so you can work on the interface without being connected to a ship.

To develop against a working ship, you first need to add a `.env.local` file to the root of this directory. This file will not be committed. Adding `VITE_SHIP_URL={URL}` where **{URL}** is the URL of the ship you would like to point to, will allow you to run `npm run dev`. This will proxy all requests to the ship except for those powering the interface, allowing you to see live data.

Regardless of what you run to develop, Vite will hot-reload code changes as you work so you don't have to constantly refresh.

## Deploying

To deploy, run `npm run build` which will bundle all the code and assets into the `dist/` folder. This can then be made into a glob by doing the following:

1. Create or launch an urbit using the -F flag
2. On that urbit, if you don't already have a desk to run from, run `|merge %work our %base` to create a new desk and mount it with `|mount %work`.
3. Now the `%work` desk is accessible through the host OS's filesystem as a directory of that urbit's pier ie `~/zod/work`.
4. From the directory of grid you can run `rsync -avL --delete dist/ ~/zod/work/grid` where `~/zod` is your fake urbit's pier.
5. Once completed you can then run `|commit %work` on your urbit and you should see your files logged back out from the dojo.
6. Now run `=dir /=garden` to switch to the garden desk directory
7. You can now run `-make-glob %work /grid` which will take the grid folder where you just added files and create a glob which can be thought of as a sort of bundle. It will be output to `~/zod/.urb/put`.
8. If you navigate to `~/zod/.urb/put` you should see a file that looks like this `glob-0v5.fdf99.nph65.qecq3.ncpjn.q13mb.glob`. The characters between `glob-` and `.glob` are a hash of the glob's contents.
9. If you're working at Tlon, you can upload this to our Google storage using `gsutil cp glob-*.* gs://bootstrap.urbit.org`. Otherwise any publicly available HTTP endpoint that can serve files should be sufficient for distributing the glob.
10. Once you've uploaded the glob, you should then update the corresponding entry in the docket file that represents Landscape which currently resides at `pkg/garden/desk.docket-0`. Both the full URL and the hash should be updated to match the glob we just created, on the line that looks like this:

```hoon
    glob-http+['https://bootstrap.urbit.org/glob-0v5.fdf99.nph65.qecq3.ncpjn.q13mb.glob' 0v5.fdf99.nph65.qecq3.ncpjn.q13mb]
```

11. This can now be safely committed and deployed.

[react]: https://reactjs.org/
[typescript]: https://www.typescriptlang.org/
[tailwind css]: https://tailwindcss.com/
[vite]: https://vitejs.dev/
