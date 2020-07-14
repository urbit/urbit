Get started building a simple application for Landscape on your [Urbit](http://urbit.org) ship with a few commands.

This tool is experimental and primarily used internally to develop front-end applications. While Tlon does not officially support this tool, you can always get general programming help for Urbit in the `~dopzod/urbit-help` chat.

## Installation

This repository is available as a template; to immediately generate your application's repository you can click [here](https://github.com/urbit/create-landscape-app/generate). Clone the generated repository, `npm install` and then run `npm start` to get started (you can also directly clone this repository, if you wish!).

In order to run your application on your ship, you will need Urbit v.0.10.0 or higher. On your Urbit ship, if you haven't already, mount your pier to Unix with `|mount %`.

## Using

Once you're up and running, your tile lives in `tile/tile.js`, which uses [React](https://reactjs.org) to render itself -- you'll want a basic foothold with it first. When you make changes, the `urbit` directory will update with the compiled application and, if you're running `npm run serve`, it will automatically copy itself to your Urbit ship when you save your changes (more information on that below).

### `npm start`

This runs the wizard. Give it an application name and the location of your Urbit ship's desk and it will customise the files so your new application will run on your ship.

### `npm run build`

This builds your application and copies it into your Urbit ship's desk. In your Urbit (v.0.8.0 or higher) `|commit %home` (or `%your-desk-name`) to synchronise your changes.

If this is the first time you're running your application on your Urbit ship, don't forget to `|start %yourapp`.

### `npm run serve`

Builds the application and copies it into your Urbit ship's desk, watching for changes. In your Urbit (v.0.8.0 or higher) `|commit %home` (or `%your-desk-name`) to synchronise your changes.

## FAQ

### What is a "tile" vs. "full" app?

When you run `npm run start`, the wizard will ask you to specify which you want:

- **tile**: A tile that exists on the Landscape launch screen. A pre-existing example is the "Weather" tile or the "Clock" tile in Landscape.
- **full**: A tile that links to a full-screen application: this means that you will work in both `tile.js` (for the tile interface) and `root.js` (and beyond) in the `src` folder.

No matter which option you specify, the wizard will customise the Hoon boilerplate for you and provide a basic example accordingly.

### How can I ensure my app fits Landscape design?

Landscape makes use of the [Indigo](https://urbit.github.io/indigo-react/) CSS framework. The template tile and full application both make use of it as an example for you to get going fast.

### What if I want to communicate with my ship / provide more functionality besides a front-end?

By default, your app will provide an example of passing state from ship to front-end with the `peer-[yourappname]tile` arm in the app's .hoon file -- in this case, just sending your ship's name as a data prop. The code is well-commented if you don't want to pass state, or if you want to know how to pass almost anything else from your ship to the Landscape interface.

In order to do anything substantial, of course, you'll want to know [Hoon](https://urbit.org/docs/tutorials/hoon/). If this is intimidating, don't panic: `create-landscape-app` is a fantastic way to start learning by leveraging your strengths. This repository is intended to be a boilerplate for rapid front-end development; it's also a gradual, incremental introduction to Hoon for web developers by allowing for rapid prototyping and experimentation with the Landscape interface.

Happy hacking!
