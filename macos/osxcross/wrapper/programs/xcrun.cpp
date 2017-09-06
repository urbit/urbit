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

using namespace tools;
using namespace target;

namespace program {
namespace {

bool showCommand = false;

bool isXcodeTool(const char *tool) {
  constexpr const char *XcodeTools[] = {
    "ar", "as", "c89", "c99", "c11", "c++", "cc", "checksyms", "clang",
    "dsymutil", "codesign_allocate", "dyldinfo", "gcc", "g++", "gcov",
    "gprof", "indr", "install_name_tool", "ld", "libtool", "lipo",
    "machocheck", "nm", "nmedit", "ObjectDump", "objdump", "otool",
    "pagestuff", "pkg-config", "ranlib", "redo_prebinding",
    "seg_addr_table", "seg_hack", "size", "strings", "strip", "sw_vers",
    "unwinddump", "xcodebuild", "xcrun", "bitcode_strip"
  };

  for (const char *xctool : XcodeTools)
    if (!strncmp(tool, xctool, strlen(xctool)))
      return true;

  return false;
}

bool getToolPath(Target &target, std::string &toolpath, const char *tool) {
  const char *filename = getFileName(tool);
  bool isxcodetool = isXcodeTool(filename);

  toolpath.clear();

  if (tool[0] == PATHDIV) {
    if (isxcodetool)
      tool = filename; // map /usr/bin/clang (etc.) to <triple>-clang
    else
      toolpath = tool;
  }

  if (toolpath.empty()) {
    toolpath = target.execpath;
    toolpath += PATHDIV;
    toolpath += getArchName(target.arch);
    toolpath += "-";
    toolpath += getDefaultVendor();
    toolpath += "-";
    toolpath += getDefaultTarget();
    toolpath += "-";
    toolpath += tool;
  }

  if (!fileExists(toolpath.c_str())) {
    // Fall back to system executables so 'xcrun git status' etc. works.
    if (!isxcodetool && realPath(tool, toolpath))
      return true;

    err << "xcrun: cannot find '" << tool << "' executable" << err.endl();
    return false;
  }

  return true;
}

int help(Target&, char**) {
  std::cerr << "https://developer.apple.com/library/mac/documentation/Darwin/"
               "Reference/ManPages/man1/xcrun.1.html" << std::endl;
  return 0;
}

int version(Target&, char**) {
  std::cout << "xcrun version: 0." << std::endl;
  return 0;
}

int sdk(Target&, char **argv) {
  const char *SDK = argv[0];

  if (!strcmp(SDK, "macosx") || !strcmp(SDK, "macosx.internal"))
    return 0;

  // Ignore '/'.
  if (SDK[0] == PATHDIV && !SDK[1])
    return 0;

  std::string SDKPath = SDK;

  while (SDKPath.size() && SDKPath[SDKPath.size() - 1] == PATHDIV)
    SDKPath.erase(SDKPath.size() - 1, 1);

  const char *SDKName = getFileName(SDKPath);

  if (!strncasecmp(SDKName, "MacOSX", 6)) {
    if (!dirExists(SDK)) {
      err << "xcrun: '-sdk': directory '" << SDK << "' does not exist"
          << err.endl();
      return 1;
    }

    setenv("OSXCROSS_SDKROOT", SDK, 1);

    return 0;
  }

  // Ignore empty argument.
  if (SDK[0]) {
    err << "xcrun: '-sdk': expected Mac OS X SDK" << err.endl();
    return 1;
  }

  return 0;
}

int log(Target&, char**) {
  showCommand = true;
  return 0;
}

int find(Target &target, char **argv) {
  if (argv[1])
    return 1;
  std::string toolpath;
  if (!getToolPath(target, toolpath, argv[0]))
    return 1;
  std::cout << toolpath << std::endl;
  return 0;
}

int run(Target &target, char **argv) {
  std::string toolpath;
  std::string command;

  if (!getToolPath(target, toolpath, argv[0]))
    exit(1);

  std::vector<char *> args;
  args.push_back(const_cast<char *>(toolpath.c_str()));

  for (char **arg = &argv[1]; *arg; ++arg)
    args.push_back(*arg);

  args.push_back(nullptr);

  if (showCommand) {
    for (size_t i = 0; i < args.size() - 1; ++i) {
      std::cout << args[i];
      if (i != args.size() - 2)
        std::cout << " ";
    }
    std::cout << std::endl;
  }

  execvp(args[0], args.data());
  err << "xcrun: cannot execute '" << args[0] << "'" << err.endl();
  exit(1);

  // Silence -Wreturn-type warnings in case exit() is not marked as
  // "no-return" for whatever reason.
  __builtin_unreachable();
}

int showSDKPath(Target &target, char**) {
  std::string SDKPath;
  if (!target.getSDKPath(SDKPath))
    return 1;
  std::cout << SDKPath << std::endl;
  return 0;
}

int showSDKVersion(Target &target, char**) {
  std::cout << target.getSDKOSNum().shortStr() << std::endl;
  return 0;
}

} // anonymous namespace

int xcrun(int argc, char **argv, Target &target) {
  if (getenv("xcrun_log"))
    showCommand = true;

  constexpr const char *ENVVARS[] = {
    "DEVELOPER_DIR", "TOOLCHAINS", "xcrun_verbose"
  };

  for (const char *evar : ENVVARS) {
    if (getenv(evar)) {
      warn << "xcrun: ignoring environment variable "
           << "'" << evar << "'" << warn.endl();
    }
  }

  if (char *SDK = getenv("SDKROOT")) {
    unsetenv("OSXCROSS_SDKROOT");
    char *argv[1] = { SDK };
    sdk(target, argv);
  }

  auto dummy = [](Target&, char**) { return 0; };

  ArgParser<int (*)(Target&, char**), 19> argParser = {{
    {"h", help},
    {"help", help},
    {"version", version},
    {"v", dummy},
    {"verbose", dummy},
    {"k", dummy},
    {"kill-cache", dummy},
    {"n", dummy},
    {"no-cache", dummy},
    {"sdk", sdk, 1},
    {"toolchain", dummy, 1},
    {"l", log },
    {"log", log},
    {"f", find, 1},
    {"find", find, 1},
    {"r", run, 1},
    {"run", run, 1},
    {"show-sdk-path", showSDKPath},
    {"show-sdk-version", showSDKVersion}
  }};

  int retVal = 1;

  for (int i = 1; i < argc; ++i) {
    auto b = argParser.parseArg(argc, argv, i);

    if (!b) {
      if (argv[i][0] == '-') {
        err << "xcrun: unknown argument: '" << argv[i] << "'" << err.endl();
        retVal = 2;
        break;
      }

      run(target, &argv[i]);
    }

    retVal = b->fun(target, &argv[i + 1]);

    if (retVal != 0)
      break;

    i += b->numArgs;
  }

  return retVal;
}

} // namespace program
