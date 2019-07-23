# Home

Create a `.urbitrc` file in this directory like so:

```
module.exports = {
    URBIT_PIERS: [
      "/path/to/fakezod/home"
    ]
};
```

You'll need `npm` installed (we recommend using [NVM](https://github.com/creationix/nvm) with node version 10.13.0)

Then:

```
npm install
npm install -g gulp-cli
gulp watch
```

Whenever you change some Home source code, this will recompile the code and
copy the updated version into your fakezod pier. Visit localhost:80 to launch Home (or whichever port is printed out to your terminal upon booting your ship).

