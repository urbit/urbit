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

static int do_exec(Target & target)
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

int compiler_main(int argc, char ** argv, const char *compiler_name)
{
  Target target;
  // TODO: get rid of this enum, just use a WRAPPER_ARCH string or something
  target.arch = Arch::x86_64;
  target.compilername = compiler_name;
  target.compilerexecname = compiler_name;
  target.compilerpath = compiler_name;
  target.target = OSXCROSS_TARGET;  // e.g. "darwin15"

  target.args.reserve(argc);
  for (int i = 1; i < argc; ++i)
  {
    target.args.push_back(argv[i]);
  }

  target.fargs.push_back(target.compilerexecname);

  target.fargs.push_back("-target");
  target.fargs.push_back(WRAPPER_HOST);

  target.fargs.push_back("-mmacosx-version-min=" WRAPPER_OS_VERSION_MIN);

  target.fargs.push_back("--sysroot");
  target.fargs.push_back(WRAPPER_SDK_PATH);

  if (target.compilername == "clang++")
  {
    target.fargs.push_back("-stdlib=libc++");
    target.fargs.push_back("-cxx-isystem");
    target.fargs.push_back(WRAPPER_SDK_PATH "/usr/include/c++/v1");
  }

  return do_exec(target);
}

int c_compiler_main(int argc, char ** argv)
{
  return compiler_main(argc, argv, "clang");
}

int cxx_compiler_main(int argc, char ** argv)
{
  return compiler_main(argc, argv, "clang++");
}

int wrapper_main(int argc, char ** argv)
{
  std::cout <<
    "host: " WRAPPER_HOST "\n"
    "path: " WRAPPER_PATH "\n"
    "sdk_path: " WRAPPER_SDK_PATH "\n";
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
