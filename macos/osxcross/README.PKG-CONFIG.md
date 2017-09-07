OSXCross' pkg-config ignores `PKG_CONFIG_*` environment variables by default and maps to `OSXCROSS_PKG_*` to `PKG_*` internally to prevent including wrong header files (`/usr/include`, etc.).

You can restore the default behavior by setting `OSXCROSS_PKG_CONFIG_USE_NATIVE_VARIABLES` (env) to 1.

Usage examples:

    $ x86_64-apple-darwin15-pkg-config
    osxcross: warning: x86_64-apple-darwin15-pkg-config is a no-op - please see README.PKG-CONFIG.md for more
    ## Explanation: No MacPorts packages installed and no OSXCROSS_PKG_* variables set = no-op. ##

    $ x86_64-apple-darwin15-pkg-config --libs sdl2
    -L[...]/target/bin/../macports/pkgs/opt/local/lib -lSDL2

    $ OSXCROSS_PKG_CONFIG_NO_MP_INC=1 x86_64-apple-darwin15-pkg-config --libs sdl2
    osxcross: warning: x86_64-apple-darwin15-pkg-config is a no-op - please see README.PKG-CONFIG.md for more

    $ OSXCROSS_PKG_CONFIG_LIBDIR=<path> x86_64-apple-darwin15-pkg-config
    ## Explanation: OSXCROSS_PKG_CONFIG_LIBDIR gets mapped to PKG_CONFIG_LIBDIR internally. ##

    $ OSXCROSS_PKG_CONFIG_USE_NATIVE_VARIABLES=1 PKG_CONFIG_LIBDIR=<path> x86_64-apple-darwin15-pkg-config
    ## Explanation: Restores the default behavior. PKG_CONFIG_LIBDIR is used and OSXCROSS_PKG_* variables will be ignored. ##
