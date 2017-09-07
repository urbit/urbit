/***********************************************************************
 *  OSXCross Compiler Wrapper                                          *
 *  Copyright (C) 2014-2016 by Thomas Poechtrager                      *
 *  t.poechtrager@gmail.com                                            *
 *                                                                     *
 *  This program is free software; you can redistribute it and/or      *
 *  modify it under the terms of the GNU General Public License        *
 *  as published by the Free Software Foundation; either version 2     *
 *  of the License, or (at your option) any later version.             *
 *                                                                     *
 *  This program is distributed in the hope that it will be useful,    *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 *  GNU General Public License for more details.                       *
 *                                                                     *
 *  You should have received a copy of the GNU General Public License  *
 *  along with this program; if not, write to the Free Software        *
 *  Foundation, Inc.,                                                  *
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.      *
 ***********************************************************************/

#include "proginc.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>

using namespace tools;

namespace program {

int dsymutil(int argc, char **argv, Target &target) {
  (void)argc;

  //
  // LLVM 3.6 and LLVM 3.7 come with too early in-development versions of
  // llvm-dsymutil, which we *must* ignore here.
  //
  // 1. Lookup the [osxcross-]llvm-dsymutil binary.
  //    If no binary is found, then silently return 0/EXIT_SUCCESS.
  // 2. Run <llvm-dsymutil -version>.
  // 3. Check whether we are using llvm-dsymutil 3.8 (or later),
  //    otherwise silently return 0/EXIT_SUCCESS.
  // 4. Apply compatibility workarounds.
  // 5. Run [osxcross-]llvm-dsymutil.
  //

  std::string dsymutil;
  char LLVMDsymutilVersionOutput[1024];
  const char *LLVMDsymutilVersionStr;
  LLVMVersion LLVMDsymutilVersion;

  const std::string &ParentProcessName = getParentProcessName();

  if (!debug && ParentProcessName.find("clang") == std::string::npos &&
      ParentProcessName != "collect2" && ParentProcessName != "unknown")
    debug = 1;

  if (char *p = getenv("OSXCROSS_LLVM_DSYMUTIL")) {
    dsymutil = p;
    debug = 1;
  } else {
    if (!realPath("osxcross-llvm-dsymutil", dsymutil) &&
        !realPath("llvm-dsymutil", dsymutil)) {
      if (debug)
        dbg << "dsymutil: cannot find [osxcross-]llvm-dsymutil in PATH"
            << dbg.endl();
      return 0;
    }
  }

  std::string command = dsymutil + " -version";

  if (runcommand(command.c_str(), LLVMDsymutilVersionOutput,
                 sizeof(LLVMDsymutilVersionOutput)) == RUNCOMMAND_ERROR) {
    if (debug)
      dbg << "dsymutil: executing \"" << command << "\" failed"
          << dbg.endl();
    return 0;
  }

  LLVMDsymutilVersionStr = strstr(LLVMDsymutilVersionOutput, "LLVM version ");

  if (!LLVMDsymutilVersionStr) {
    if (debug)
      dbg << "dsymutil: unable to parse llvm-dsymutil version"
          << dbg.endl();
    return 0;
  }

  LLVMDsymutilVersionStr += constexprStrLen("LLVM version ");

  LLVMDsymutilVersion = parseLLVMVersion(LLVMDsymutilVersionStr);

  constexpr LLVMVersion RequiredLLVMDsymutilVersion(3, 8);

  if (LLVMDsymutilVersion < RequiredLLVMDsymutilVersion) {
    if (debug)
      dbg << "ignoring dsymutil invocation: '"
          << dsymutil << "' is too old ("
          << LLVMDsymutilVersion.Str() << " < "
          << RequiredLLVMDsymutilVersion.Str() << ")"
          << dbg.endl();
    return 0;
  }

  std::stringstream lipo;
  std::string triple;

  lipo << target.execpath << PATHDIV
       << target.getDefaultTriple(triple) << "-lipo";

  if (endsWith(dsymutil, "osxcross-llvm-dsymutil")) {
    // This is a patched llvm-dsymutil, just need to set LIPO here.
    setenv("LIPO", lipo.str().c_str(), 1);
  } else {
    // This is an unpatched llvm-dsymutil, need to use stupid workarounds here.

    // There is a bug in the vanilla llvm-dsymutil sources which would cause it
    // to crash when operating on gcc object files.
    // Fix: https://github.com/tpoechtrager/llvm-dsymutil/commit/5e0fea25.patch

    auto fixHint = []() {
      info << "you can build a patched llvm-dsymutil via "
           << "./build_llvm_dsymutil.sh"
           << info.endl();
    };

    if (ParentProcessName == "collect2" &&
        !getenv("OSXCROSS_FORCE_GCC_DSYMUTIL_INVOCATION")) {
      if (!getenv("OSXCROSS_NO_GCC_DSYMUTIL_WARNING")) {
        warn << "dsymutil is a no-op when being invoked via gcc; "
             << "you would get a crash otherwise"
             << warn.endl();
        fixHint();
      }
      return 0;
    }

    //
    // A glorious workaround to make the vanilla llvm-dsymutil find lipo.
    //
    // 1. Create a temporary directory.
    // 2. Store a lipo symlink there.
    // 3. Append <tmpdir> to PATH.
    // 4. Fork the process and wait until the child process exited.
    // 5. Remove the temporary directory and return the llvm-dsymutil
    //    exit code.
    //

    char tmpdir[] = "/tmp/XXXXXX";
    std::string lipolink;
    pid_t pid;

    if (mkdtemp(tmpdir)) {
      lipolink = tmpdir;
      lipolink += "/lipo";

      auto removeTemporaryDirectory = [&]() {
        if ((!lipolink.empty() && unlink(lipolink.c_str())) || rmdir(tmpdir)) {
          warn << "dsymutil: cannot remove temporary directory '"
               << tmpdir << "'" << warn.endl();
        }
      };

      if (!symlink(lipo.str().c_str(), lipolink.c_str())) {
        concatEnvVariable("PATH", tmpdir);

        if ((pid = fork()) == -1) {
          err << "dsymutil: fork() failed" << err.endl();
          removeTemporaryDirectory();
          return 2;
        } else if (pid > 0) {
          int status;

          if (waitpid(pid, &status, 0) == -1) {
            err << "dsymutil: waitpid() failed" << err.endl();
            removeTemporaryDirectory();
            return 2;
          }

          removeTemporaryDirectory();

          if (WIFSIGNALED(status)) {
            int signal = WTERMSIG(status);

            err << "dsymutil: signal: " << strsignal(signal) << err.endl();

            if (signal == SIGSEGV) {
              info << "the vanilla llvm-dsymutil is known to crash "
                   << "when operating on gcc object files" << info.endl();
              fixHint();
            }

            return 2;
          }

          if (WIFEXITED(status))
            return WEXITSTATUS(status);

          abort();
        }
      } else {
        lipolink.clear();
        removeTemporaryDirectory();
      }
    }
  }

  if (execvp(dsymutil.c_str(), argv))
    err << "cannot execute '" << dsymutil << "'" << err.endl();

  return 1;
}

} // namespace program
