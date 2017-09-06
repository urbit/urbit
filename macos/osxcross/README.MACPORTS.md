## OSXCROSS-MACPORTS ##

`osxcross-macports` is a small "packet manager" for 16.000+ binary MacPorts packages.

Packages are installed to `target/macports/pkgs`.

### DEPENDENCIES: ###

`bash`, `wget` and `openssl`

Also ensure that you are using the 10.6 SDK (or later).

### INSTALLATION: ###

Run OSXCross' `./build.sh`, then you should have `osxcross-macports` in PATH.

**Setting up osxcross-macports:**

MacPorts doesn't support 10.5 anymore, so we need to change OSXCross'  
default target to 10.6 (better 10.7, or later).

To achive this, add the following to your bashrc (or similar):

`export MACOSX_DEPLOYMENT_TARGET=10.7`

Then run `osxcross-macports <cmd>`.

### THINGS YOU SHOULD KNOW: ###

**shortcuts:**

`osxcross-mp`, `omp`

\--

**pkg-config:**

OSXCross' `pkg-config` (`<arch>-apple-darwinXX-pkg-config`)  
is automatically aware of MacPorts packages.  

If you want `pkg-config` to be unaware of MacPorts packages  
(for whatever reason), do the following:

`export OSXCROSS_PKG_CONFIG_NO_MP_INC=1`

\--

**automatic compiler includes:**

You can set up automatic compiler includes (`-I / -L / -F`) by doing the
following:

`export OSXCROSS_MP_INC=1`

\--

**verbose messages:**

Can be enabled by adding '-v' or '--verbose'.

\--

**upgrading packages:**

Run `osxcross-macports upgrade`.

This will simply re-install the latest  version of all your installed packages.

**listing all available packages:**

`osxcross-macports search $`

### COMMANDS: ###

osxcross-macports [...]

  * install &lt;pkg1&gt; [&lt;pkg2&gt; [...]]
     * Install <package name> and its deps.

  * search &lt;pkg&gt;
     * Prints a list of matching package names.

  * update-cache
     * Updates the search index cache.

  * clear-cache
     * Clears the download and search cache.

  * remove-dylibs
     * Removes all \*.dylib (useful for static linking).

  * upgrade
     * Reinstalls the latest version of every package.

  * select-mirror
     * Switch to a different mirror.

Useful flags:

  * '-v', '--verbose':
     * Print verbose messages.

  * '-v=2', '--verbose=2':
     * Print more verbose messages.

  * '-s', '--static':
     * Install static libraries only.

  * '-c', '--cflags' &lt;lib&gt;:
     * Shows cflags for &lt;lib&gt; (same as pkg-config).

  * '-l', '--ldflags' &lt;lib&gt;:
     * Shows ldflags for &lt;lib&gt; (same as pkg-config).

Uninstalling is not supported (and probably never will be).

However, you can remove packages by hand. A simpler (and cleaner) way would  
be to remove the whole macports directory (target/macports) and to reinstall  
all other packages again.

### USAGE EXAMPLES: ###

library installation:

    $ osxcross-macports install libsdl2
    searching package libsdl2 ...
    downloading libsdl2-2.0.3_0.darwin_11.x86_64.tbz2 ...
    installing libsdl2 ...
    installed libsdl2

print library flags (osxcross-macports):

    $ osxcross-macports --cflags sdl2
    -D_THREAD_SAFE -I/data/development/osxcross/target/bin/../macports/pkgs/opt/local/include/SDL2 

    $ osxcross-macports --ldflags sdl2
    -L/data/development/osxcross/target/bin/../macports/pkgs/opt/local/lib -lSDL2

print library flags (pkg-config):

    $ x86_64-apple-darwinXX-pkg-config --cflags sdl2
    -D_THREAD_SAFE -I/data/development/osxcross/target/bin/../macports/pkgs/opt/local/include/SDL2 

    $ x86_64-apple-darwinXX-pkg-config --libs sdl2
    -L/data/development/osxcross/target/bin/../macports/pkgs/opt/local/lib -lSDL2

automatic includes:

    OSXCROSS_MP_INC=1 o64-clang file.c -lSDL2

    OSXCROSS_MP_INC=1 make [...]
