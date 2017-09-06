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
#include <map>

using namespace tools;
using namespace target;

namespace program {
namespace osxcross {

int env(int argc, char **argv) {
  char epath[PATH_MAX + 1];
  char *oldpath = getenv("PATH");

  assert(oldpath);

  if (!getExecutablePath(epath, sizeof(epath)))
    exit(EXIT_FAILURE);

  if (argc <= 1) {
    const std::string &pname = getParentProcessName();

    if (pname == "csh" || pname == "tcsh") {
      std::cerr << std::endl << "you are invoking this program from a C shell, "
                << std::endl << "please use " << std::endl << std::endl
                << "setenv PATH `" << epath << "/osxcross-env -v=PATH`"
                << std::endl << std::endl << "instead." << std::endl
                << std::endl;
    }
  }

  std::vector<std::string> path;
  std::map<std::string, std::string> vars;

  splitPath(oldpath, path);

  if (!hasPath(path, epath))
    path.push_back(epath);

  vars["PATH"] = joinPath(path);

  auto printVariable = [&](const std::string & var)->bool {
    auto it = vars.find(var);
    if (it == vars.end()) {
      std::cerr << "unknown variable '" << var << "'" << std::endl;
      return false;
    }
    std::cout << it->second << std::endl;
    return true;
  };

  if (argc <= 1) {
    std::cout << std::endl;
    for (auto &v : vars) {
      std::cout << "export " << v.first << "=";
      if (!printVariable(v.first))
        return 1;
      std::cout << std::endl;
    }
  } else {
    if (strncmp(argv[1], "-v=", 3))
      return 1;

    const char *var = argv[1] + 3;
    return static_cast<int>(printVariable(var));
  }

  return 0;
}

} // namespace osxcross
} // namespace program
