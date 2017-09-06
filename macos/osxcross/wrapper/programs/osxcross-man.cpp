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
namespace osxcross {

int man(int argc, char **argv, Target &target) {
  std::string SDKPath;

  if (!target.getSDKPath(SDKPath))
    return 1;

  std::vector<std::string> manpaths;

  manpaths.push_back(SDKPath + "/usr/share/man");
  manpaths.push_back(std::string(target.execpath) + "/../share/man");

  unsetenv("MANPATH");

  for (const std::string &manpath : manpaths)
    if (dirExists(manpath))
      concatEnvVariable("MANPATH", manpath);

  if (!getenv("MANPATH")) {
    err << "cannot find man pages!" << err.endl();
    return 1;
  }

  std::vector<char *> args;

  // All the const violation here doesn't matter,
  // the arguments won't be modified

  args.push_back(const_cast<char *>("man"));

  for (int i = 1; i < argc; ++i) {
    char *arg = argv[i];

    // Rewrite gcc to <triple>-gcc for compatibility
    // with other man pages

    constexpr const char *GCCManPages[] = { "gcc", "g++", "cpp", "gcov" };

    for (const char *mp : GCCManPages) {
      if (!strcmp(mp, arg)) {
        std::string *str = new std::string; // Intentionally "leaked"
        target.getDefaultTriple(*str);
        str->append("-");
        str->append(arg);
        arg = const_cast<char *>(str->c_str());
        break;
      }
    }

    args.push_back(arg);
  }

  args.push_back(nullptr);

  execvp(args[0], args.data());
  err << "cannot execute '" << args[0] << "'" << err.endl();
  return 1;
}

} // namespace osxcross
} // namespace program
