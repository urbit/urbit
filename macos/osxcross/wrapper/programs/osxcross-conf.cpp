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

int conf(Target &target) {
  std::string SDKPath;
  OSVersion OSXVersionMin = getDefaultMinTarget();
  const char *ltopath = getLibLTOPath();
  const char *builddir = getBuildDir();

  if (!target.getSDKPath(SDKPath))
    return 1;

  if (!OSXVersionMin.Num())
    OSXVersionMin = target.getSDKOSNum();

  if (!ltopath)
    ltopath = "";

  std::cout << "export OSXCROSS_VERSION=" << getOSXCrossVersion()
            << std::endl;
  std::cout << "export OSXCROSS_OSX_VERSION_MIN=" << OSXVersionMin.shortStr()
            << std::endl;
  std::cout << "export OSXCROSS_TARGET=" << getDefaultTarget()
            << std::endl;
  std::cout << "export OSXCROSS_SDK_VERSION=" << target.getSDKOSNum().shortStr()
            << std::endl;
  std::cout << "export OSXCROSS_SDK=" << SDKPath
            << std::endl;
  std::cout << "export OSXCROSS_TARBALL_DIR=" << target.execpath
            << "/../../tarballs"
            << std::endl;
  std::cout << "export OSXCROSS_PATCH_DIR=" << target.execpath
            << "/../../patches"
            << std::endl;
  std::cout << "export OSXCROSS_TARGET_DIR=" << target.execpath << "/.."
            << std::endl;

  std::cout << "export OSXCROSS_BUILD_DIR=";

  if (builddir[0])
    std::cout << builddir;
  else
    std::cout << target.execpath << "/../../build";

  std::cout << std::endl;

  std::cout << "export OSXCROSS_CCTOOLS_PATH=" << target.execpath
            << std::endl;
  std::cout << "export OSXCROSS_LIBLTO_PATH=" << ltopath
            << std::endl;
  std::cout << "export OSXCROSS_LINKER_VERSION=" << getLinkerVersion()
            << std::endl;

  return 0;
}

} // namespace osxcross
} // namespace program
