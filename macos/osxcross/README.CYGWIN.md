## STEP BY STEP INSTALLATION GUIDE: ##

#### Step 1: ####

Get [Cygwin](https://cygwin.com):

32-Bit: https://cygwin.com/setup-x86.exe  
64-Bit: https://cygwin.com/setup-x86_64.exe

Note: *You can compile for 64-bit in a 32-bit environment.*

Click on setup-*.exe and install the following packages:

`git`, `make`, `clang`, `gcc-core`, `gcc-objc`, `gcc-g++`,  
`patch`, `wget`, `libuuid-devel`, `openssl` and `pkg-config`

For LTO support you will also need:

`libllvm-devel`, `libllvm`, `libedit` and `libffi`

The Cygwin package of libLLVM lacks the LTO library, but you can create that dll
by issuing the following command:

`./tools/gen_cyglto_dll.sh`

#### Step 2: ####

Open the Cygwin shell:

![](images/cygwin/osxcross/2.png)

#### Step 3: ####

Clone the OSXCross repository:

![](images/cygwin/osxcross/3.png)

#### Step 4: ####

Get the Mac OS X SDK and move it into  
`C:\Cygwin*\home\<User>\osxcross\tarballs`.

Please see [README](README.md) for more.

#### Step 5: ####

Build OSXCross:

![](images/cygwin/osxcross/5_1.png)

![](images/cygwin/osxcross/5_2.png)

#### Step 6: ####

Add the printed target/bin path to your `~/.bashrc`:

![](images/cygwin/osxcross/6.png)

#### Step 7: ####

Restart the Cygwin shell:

![](images/cygwin/osxcross/7.png)

#### Step 8: ####

Verify target/bin is in PATH:

![](images/cygwin/osxcross/8.png)

All done!

## OPTIONAL: ##

### Compiler-RT ###

Please see [README.COMPILER-RT](README.COMPILER-RT.md) for more.

#### Step 1: ####

Build Compiler-RT:

![](images/cygwin/compiler-rt/1.png)

#### Step 2: ####

Execute the printed commands:

![](images/cygwin/compiler-rt/2.png)

![](images/cygwin/compiler-rt/3.png)

#### Step 3: ####

Verify Compiler-RT works:

![](images/cygwin/compiler-rt/4.png)

## EXAMPLE USAGE: ###

### osxcross-macports ###

Please see [README.MACPORTS](README.MACPORTS.md) for more.

#### Step 1: ####

Change the deployment target to >= 10.7:

![](images/cygwin/macports/1.png)

#### Step 2: ####

Restart the Cygwin shell:

![](images/cygwin/macports/2.png)

#### Step 3: ####

Let's install libgeoip:

![](images/cygwin/macports/3.png)

#### Step 4: ####

Write a simple test program:

![](images/cygwin/macports/4.png)

#### Step 5: ####

Compile the test program:

![](images/cygwin/macports/5.png)

... or the xcrun way:

![](images/cygwin/macports/6.png)
