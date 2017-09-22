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

namespace commandopts {

bool parse(int argc, char **argv, Target &target)
{
  target.args.reserve(argc);
  for (int i = 1; i < argc; ++i)
  {
    target.args.push_back(argv[i]);
  }
  return true;
}

} // namespace commandopts

bool detectTarget(int argc, char **argv, Target &target) {
  const char *cmd = argv[0];
  const char *p = strrchr(cmd, '/');
  size_t len;
  size_t i = 0;

  // TODO: get rid of this enum, just use a WRAPPER_ARCH string or something
  target.arch = Arch::x86_64;

  if (p)
    cmd = &p[1];

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
      target.compilername = &p[1];
      if (target.compilername == "g++") { target.compilername = "clang++"; }
      if (target.compilername == "gcc") { target.compilername = "clang"; }
      target.compiler = getCompilerIdentifier(target.compilername.c_str());

      if (target.compilername == "cc") {
        target.compiler = getDefaultCompilerIdentifier();
        target.compilername = getDefaultCompilerName();
      } else if (target.compilername == "c++") {
        target.compiler = getDefaultCXXCompilerIdentifier();
        target.compilername = getDefaultCXXCompilerName();
      }

      if (target.target != getDefaultTarget())
        warn << "this wrapper was built for target "
             << "'" << getDefaultTarget() << "'" << warn.endl();

      if (!commandopts::parse(argc, argv, target))
        return false;

      return target.setup();
    }
  }

  if (const char *p = strchr(cmd, '-')) {
    const char *compilername = &cmd[p - cmd + 1];
    target.compiler = getCompilerIdentifier(compilername);
    target.compilername = compilername;
  }

  if (!commandopts::parse(argc, argv, target))
    return false;

  return target.setup();
}

} // unnamed namespace

static int compileForTarget(Target & target)
{
  char ** exec_args = new char *[target.fargs.size() + target.args.size() + 1];

  size_t i = 0;

  for (std::string & arg : target.fargs)
  {
    exec_args[i++] = (char *)arg.c_str();
  }

  for (std::string & arg : target.args)
  {
    exec_args[i++] = (char *)arg.c_str();
  }

  exec_args[i] = nullptr;

  execvp(target.compilerpath.c_str(), exec_args);

  int result = errno;
  std::cerr << "execvp failed: " << target.compilerpath << ": "
            << strerror(result) << std::endl;
  return 1;
}

int c_compiler_main(int argc, char ** argv)
{
  Target target;
  bool success = detectTarget(argc, argv, target);

  if (!success)
  {
    err << "while detecting target" << err.endl();
    return 1;
  }

  return compileForTarget(target);
}

int cxx_compiler_main(int argc, char ** argv)
{
  Target target;
  bool success = detectTarget(argc, argv, target);

  if (!success)
  {
    err << "while detecting target" << err.endl();
    return 1;
  }

  return compileForTarget(target);
}

int wrapper_main(int argc, char ** argv)
{
  std::cout <<
    "host: " WRAPPER_HOST "\n"
    "path: " WRAPPER_PATH "\n"
    "sdk_path: " WRAPPER_SDK_PATH "\n"
    "sdk_version: " WRAPPER_SDK_VERSION "\n";
  return 0;
}

struct {
  const char * name;
  int (*main_func)(int argc, char ** argv);
} prgms[] = {
  { WRAPPER_HOST "-gcc", c_compiler_main },
  { WRAPPER_HOST "-cc", c_compiler_main },
  { WRAPPER_HOST "-clang", c_compiler_main },
  { WRAPPER_HOST "-g++", cxx_compiler_main },
  { WRAPPER_HOST "-c++", cxx_compiler_main },
  { WRAPPER_HOST "-clang++", cxx_compiler_main },
  { WRAPPER_HOST "-wrapper", wrapper_main },
  { nullptr, nullptr },
};

const char * get_program_name(const char * path)
{
  const char * p = strrchr(path, '/');
  if (p) { path = p + 1; }
  return path;
}

int main(int argc, char ** argv)
{
  // We only want this wrapper and the compiler it invokes to access a certain
  // set of tools that are determined at build time.  Ignore whatever is on the
  // user's path and use the path specified by our Nix expression instead.
  int result = setenv("PATH", WRAPPER_PATH, 1);
  if (result)
  {
    std::cerr << "wrapper failed to set PATH" << std::endl;
    return 1;
  }

  std::string program_name = get_program_name(argv[0]);

  for (auto * p = prgms; p->name; p++)
  {
    if (program_name == p->name)
    {
      return p->main_func(argc, argv);
    }
  }

  std::cerr << "compiler wrapper invoked with unknown program name: " << argv[0] << std::endl;
  return 1;
}
