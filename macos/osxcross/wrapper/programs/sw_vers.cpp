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

int sw_vers(int argc, char **argv, Target &target) {

  auto genFakeBuildVer = [](std::string & build)->std::string & {
    std::stringstream tmp;

#if __has_builtin(__builtin_readcyclecounter)
    srand(static_cast<unsigned int>(__builtin_readcyclecounter()));
#else
    srand(static_cast<unsigned int>(getNanoSeconds()));
#endif

    for (int i = 0; i < 5; ++i)
      tmp << std::hex << (rand() % 16 + 1);

    build = tmp.str();
    build.resize(5);

    return build;
  };

  auto getProductVer = [&]()->OSVersion {
    char *p = getenv("OSXCROSS_SW_VERS_OSX_VERSION");
    OSVersion OSNum;

    if (!p)
      p = getenv("MACOSX_DEPLOYMENT_TARGET");

    if (p)
      OSNum = parseOSVersion(p);
    else
      OSNum = getDefaultMinTarget();

    if (!OSNum.Num())
      OSNum = target.getSDKOSNum();

    return OSNum;
  };

  if (argc == 2) {
    std::stringstream str;

    if (!strcmp(argv[1], "-productName")) {
      str << "Mac OS X";
    } else if (!strcmp(argv[1], "-productVersion")) {
      str << getProductVer().shortStr();
    } else if (!strcmp(argv[1], "-buildVersion")) {
      std::string build;
      str << genFakeBuildVer(build);
    } else {
      return 1;
    }

    std::cout << str.str() << std::endl;
  } else if (argc == 1) {
    std::string build;

    std::cout << "ProductName:    Mac OS X" << std::endl;
    std::cout << "ProductVersion: " << getProductVer().shortStr() << std::endl;
    std::cout << "BuildVersion:   " << genFakeBuildVer(build) << std::endl;
  }

  return 0;
}

} // namespace program
