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

/*
 * Debug messages can be enabled by setting 'OCDEBUG' (ENV) to >= 1.
 */

#include "compat.h"

#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <climits>
#include <cassert>
#include <unistd.h>
#include <sys/wait.h>

#include "tools.h"
#include "target.h"
#include "progs.h"

using namespace tools;
using namespace target;

int debug = 0;

namespace {

void warnExtension(const char *extension) {
  static bool noextwarnings = !!getenv("OSXCROSS_NO_EXTENSION_WARNINGS");
  if (noextwarnings)
    return;
  warn << extension << " is an osxcross extension" << warn.endl();
  warninfo << "you can silence this warning via "
           << "'OSXCROSS_NO_EXTENSION_WARNINGS=1' (env)" << warninfo.endl();
}

__attribute__((unused))
void warnDeprecated(const char *flag, const char *replacement = nullptr) {
  if (replacement)
    warn << flag << " is deprecated; "
         << "please use " << replacement << " instead"
         << warn.endl();
  else
    warn << flag << " is deprecated and will be "
         << "removed soon" << warn.endl();
}

//
// Command Line Options
//

namespace commandopts {

typedef bool (*optFun)(Target &target, const char *opt, const char *val,
                      char **);

bool versionmin(Target &target, const char *, const char *val, char **) {
  target.OSNum = parseOSVersion(val);

  if (target.OSNum != val)
    warn << "'-mmacosx-version-min=' (" << target.OSNum.Str()
         << " != " << val << ")" << warn.endl();

  return true;
}

bool arch(Target &target, const char *opt, const char *val, char **) {
  Arch arch;

  if (!strcmp(opt, "-arch")) {
    arch = parseArch(val);

    if (arch == Arch::unknown)
      warn << "'-arch': unknown architecture '" << val << "'"
           << warn.endl();

    const char *name = getArchName(arch);

    if (strcmp(val, name))
      warn << "'-arch': '" << val << "' != '" << name << "'" << warn.endl();
  } else {
    if (!strcmp(opt, "-m16") || !strcmp(opt, "-mx32")) {
      err << "'" << opt << "' is not supported" << err.endl();
      return false;
    } else if (!strcmp(opt, "-m32")) {
      arch = Arch::i386;
    } else if (!strcmp(opt, "-m64")) {
      arch = Arch::x86_64;
    } else {
      __builtin_unreachable();
    }
  }

  if (target.isClang())
    target.addArch(arch);
  else
    target.arch = arch;

  return true;
}

bool stdlib(Target &target, const char *, const char *val, char **) {
  if (target.isGCC())
    warnExtension("'-stdlib='");

  size_t i = 0;

  for (auto stdlibname : StdLibNames) {
    if (!strcmp(val, stdlibname)) {
      target.stdlib = static_cast<StdLib>(i);
      break;
    }
    ++i;
  }

  if (i == (sizeof(StdLibNames) / sizeof(StdLibNames[0]))) {
    err << "value of '-stdlib=' must be ";

    for (size_t j = 0; j < i; ++j) {
      err << "'" << StdLibNames[j] << "'";

      if (j == i - 2)
        err << " or ";
      else if (j < i - 2)
        err << ", ";
    }

    err << err.endl();
    return false;
  }

  return true;
}

bool language(Target &target, const char *, const char *val, char **) {
  target.language = val;
  return true;
}

bool usegcclibstdcxx(Target &target, const char *, const char *, char **) {
  target.stdlib = StdLib::libstdcxx;
  target.usegcclibs = true;
  return true;
}

bool compilerpath(Target &target, const char *, const char *path, char **) {
  target.compilerpath = path;
  return true;
}

bool intrinsicpath(Target &target, const char *, const char *path, char **) {
  target.intrinsicpath = path;
  return true;
}

bool runprog(Target &target, const char *, const char *progname, char **cargs) {
  auto *prog = program::getprog(progname);

  if (!prog)
    exit(EXIT_FAILURE);

  std::vector<char *> args;
  args.push_back(const_cast<char *>(progname));

  while (*cargs)
    args.push_back(*cargs++);
  args.push_back(nullptr);

  (*prog)(args.size() - 1, args.data(), target);
}

bool liblto(Target &target, const char *opt, const char *, char **) {
  target.wliblto = !strcmp(opt, "-Wliblto");
  return true;
}

bool checkincludepath(Target &, const char *opt, const char *path, char **) {
#ifndef __APPLE__
  constexpr const char *DangerousIncludePaths[] = { "/usr/include",
                                                    "/usr/local/include" };

  static bool noinccheck = !!getenv("OSXCROSS_NO_INCLUDE_PATH_WARNINGS");

  if (noinccheck)
    return true;

  char buf[PATH_MAX + 1];
  const char *rpath = realpath(path, buf);

  if (!rpath)
    rpath = path;

  for (const char *dpath : DangerousIncludePaths) {
    if (!strncmp(rpath, dpath, strlen(dpath))) {
      warn << "possibly dangerous include path specified: '" << opt << " "
           << path << "'";

      if (strcmp(path, rpath))
        warn << " (" << rpath << ")";

      warn << warn.endl();

      warninfo << "you can silence this warning via "
               << "'OSXCROSS_NO_INCLUDE_PATH_WARNINGS=1' (env)"
               << warninfo.endl();
    }
  }
#else
  (void)opt;
  (void)path;
#endif

  return true;
}

constexpr struct Opt {
  const char *name;
  const size_t namelen;
  const optFun fun;
  const bool valrequired;
  const bool pusharg;
  const char *valseparator;
  const size_t valseparatorlen;
  constexpr Opt(const char *name, optFun fun, const bool valrequired = false,
                const bool pusharg = false, const char *valseparator = nullptr)
      :  name(name), namelen(constexprStrLen(name)), fun(fun),
         valrequired(valrequired), pusharg(pusharg),
         valseparator(valseparator),
         valseparatorlen(valseparator ? constexprStrLen(valseparator) : 0) {}
} opts[] = {
  {"-mmacosx-version-min", versionmin, true, false, "="},
  {"-stdlib", stdlib, true, false, "="},
  {"-arch", arch, true},
  {"-m16", arch},
  {"-m32", arch},
  {"-mx32", arch},
  {"-m64", arch},
  {"-x", language, true, true},
  {"-foc-use-gcc-libstdc++", usegcclibstdcxx},
  {"-foc-run-prog", runprog, true, false, "="}, // for internal use only
  {"-Wliblto", liblto, false, true},
  {"-Wno-liblto", liblto, false, true},
  {"-isystem", checkincludepath, true, true},
  {"-icxx-isystem", checkincludepath, true, true},
  {"-cxx-isystem", checkincludepath, true, true},
  {"-I", checkincludepath, true, true},

  // sets a custom path for the compiler
  {"-foc-compiler-path", compilerpath, true, false, "="},

  // specifies an additional directory to search when looking for clang's
  // intrinsic paths
  {"-foc-intrinsic-path", intrinsicpath, true, false, "="}
};

bool parse(int argc, char **argv, Target &target) {
  target.args.reserve(argc);

  if (char *p = getenv("MACOSX_DEPLOYMENT_TARGET")) {
    target.OSNum = parseOSVersion(p);
    unsetenv("MACOSX_DEPLOYMENT_TARGET");
  }

  for (int i = 1; i < argc; ++i) {
    char *arg = argv[i];

    if (*arg != '-') {
      target.args.push_back(arg);
      continue;
    }

    bool pusharg = true;
    int j = i;

    for (const Opt &opt : opts) {
      if (strncmp(arg, opt.name, opt.namelen))
        continue;

      pusharg = opt.pusharg;

      const char *val = nullptr;

      if (opt.valrequired) {
        val = arg + opt.namelen;

        if (opt.valseparator &&
            strncmp(val, opt.valseparator, opt.valseparatorlen)) {
          err << "expected '" << opt.name << opt.valseparator << "<val>' "
              << "instead of '" << arg << "'" << err.endl();
          return false;
        } else {
          val += opt.valseparatorlen;
        }

        if (!opt.valseparator && !*val && i < argc - 1)
          val = argv[++i];

        if (!*val) {
          err << "missing argument for '" << opt.name << "'" << err.endl();
          return false;
        }
      }

      if (opt.fun && !opt.fun(target, opt.name, val, &argv[i + 1]))
        return false;

      break;
    }

    if (pusharg) {
      for (; j <= i; ++j)
        target.args.push_back(argv[j]);
    }
  }

  return true;
}

} // namespace commandopts

void detectCXXLib(Target &target) {
  if (target.compilername.size() <= 7)
    return;

  StdLib prevstdlib = target.stdlib;

  if (endsWith(target.compilername, "-stdc++")) {
    target.stdlib = StdLib::libstdcxx;
    target.compilername.resize(target.compilername.size() - 7);
  } else if (endsWith(target.compilername, "-gstdc++")) {
    target.stdlib = StdLib::libstdcxx;
    target.usegcclibs = true;
    target.compilername.resize(target.compilername.size() - 8);
  } else if (endsWith(target.compilername, "-libc++")) {
    target.stdlib = StdLib::libcxx;
    target.compilername.resize(target.compilername.size() - 7);
  }

  if (prevstdlib != StdLib::unset && prevstdlib != target.stdlib)
    warn << "ignoring '-stdlib=" << getStdLibString(prevstdlib) << "'"
         << warn.endl();
}

//
// detectTarget():
//  detect target and setup invocation command
//

bool detectTarget(int argc, char **argv, Target &target) {
  const char *cmd = argv[0];
  const char *p = strrchr(cmd, '/');
  size_t len;
  size_t i = 0;

  if (p)
    cmd = &p[1];

  if (auto *prog = program::getprog(cmd))
    (*prog)(argc, argv, target);

  // -> x86_64 <- -apple-darwin13
  p = strchr(cmd, '-');
  len = (p ? p : cmd) - cmd;

  for (auto arch : ArchNames) {
    ++i;

    if (!strncmp(cmd, arch, len)) {
      target.arch = static_cast<Arch>(i - 1);
      cmd += len;

      if (*cmd++ != '-')
        return false;

      if (strncmp(cmd, "apple-", 6))
        return false;

      cmd += 6;

      if (strncmp(cmd, "darwin", 6))
        return false;

      if (!(p = strchr(cmd, '-')))
        return false;

      target.target = std::string(cmd, p - cmd);
      target.compiler = getCompilerIdentifier(&p[1]);
      target.compilername = &p[1];

      if (target.compilername == "cc") {
        target.compiler = getDefaultCompilerIdentifier();
        target.compilername = getDefaultCompilerName();
      } else if (target.compilername == "c++") {
        target.compiler = getDefaultCXXCompilerIdentifier();
        target.compilername = getDefaultCXXCompilerName();
      } else if (auto *prog = program::getprog(target.compilername)) {
        (*prog)(argc, argv, target);
      }

      if (target.target != getDefaultTarget())
        warn << "this wrapper was built for target "
             << "'" << getDefaultTarget() << "'" << warn.endl();

      if (!commandopts::parse(argc, argv, target))
        return false;

      detectCXXLib(target);
      return target.setup();
    }
  }

  if (!strncmp(cmd, "o32", 3))
    target.arch = Arch::i386;
  else if (!strncmp(cmd, "o64h", 4))
    target.arch = Arch::x86_64h;
  else if (!strncmp(cmd, "o64", 3))
    target.arch = Arch::x86_64;
  else
    return false;

  if (const char *p = strchr(cmd, '-')) {
    const char *compilername = &cmd[p - cmd + 1];
    target.compiler = getCompilerIdentifier(compilername);
    target.compilername = compilername;
  }

  if (!commandopts::parse(argc, argv, target))
    return false;

  detectCXXLib(target);
  return target.setup();
}

} // unnamed namespace

int main(int argc, char **argv)
{
  char bbuf[sizeof(benchmark)];
  auto b = new (bbuf) benchmark;
  Target target;
  char **cargs = nullptr;
  int rc = -1;

  if (char *p = getenv("OCDEBUG"))
    debug = atoi(p);

  if (!detectTarget(argc, argv, target)) {
    err << "while detecting target" << err.endl();
    return 1;
  }

  if (debug) {
    b->halt();

    if (debug >= 2) {
      dbg << "detected target triple: " << target.getTriple() << dbg.endl();
      dbg << "detected compiler: " << target.compilername << dbg.endl();

      dbg << "detected stdlib: " << getStdLibString(target.stdlib)
          << dbg.endl();

      b->resume();
    }
  }

#ifdef __DragonFly__
  // Escape DragonFlyBSD's weird PFS paths.
  std::string escapedexecpath;
  escapePath(target.execpath, escapedexecpath);
  concatEnvVariable("COMPILER_PATH", escapedexecpath);
#else
  concatEnvVariable("COMPILER_PATH", target.execpath);
#endif

  auto printCommand = [&]() {
    std::string in;
    std::string out;

    for (int i = 0; i < argc; ++i) {
      in += argv[i];
      in += " ";
    }

    out += target.compilerpath;

    if (target.compilerpath != target.fargs[0]) {
      out += " (";
      out += target.fargs[0];
      out += ") ";
    } else {
      out += " ";
    }

    for (size_t i = 1; i < target.fargs.size(); ++i) {
      out += target.fargs[i];
      out += " ";
    }

    for (auto &arg : target.args) {
      out += arg;
      out += " ";
    }

    dbg << "--> " << in << dbg.endl();

    dbg << "<-- " << out << dbg.endl();
  };

  if (rc == -1) {
    cargs = new char *[target.fargs.size() + target.args.size() + 1];
    size_t i = 0;

    for (auto &arg : target.fargs)
      cargs[i++] = const_cast<char *>(arg.c_str());

    for (auto &arg : target.args)
      cargs[i++] = const_cast<char *>(arg.c_str());

    cargs[i] = nullptr;
  }

  if (debug) {
    time_type diff = b->getDiff();

    if (rc == -1)
      printCommand();

    dbg << "=== time spent in wrapper: " << diff / 1000000.0 << " ms"
        << dbg.endl();
  }

  if (rc == -1 && execvp(target.compilerpath.c_str(), cargs)) {
    err << "invoking compiler failed" << err.endl();

    if (!debug)
      printCommand();

    return 1;
  }

  return rc;
}
