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

#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <map>
#include <algorithm>
#include <cstring>
#include <strings.h>
#include <cstdlib>
#include <climits>
#include <cassert>
#include <sys/stat.h>
#include <unistd.h>

#include "tools.h"
#include "target.h"

Target::Target()
{
}

bool Target::isCXX() {
  return compiler == Compiler::CLANGXX || compiler == Compiler::GXX;
}

bool Target::isGCH() {
  if (!language)
    return false;

  return !strcmp(language, "c-header") ||
         !strcmp(language, "c++-header") ||
         !strcmp(language, "objective-c-header") ||
         !strcmp(language, "objective-c++-header");
}

bool Target::setup() {
  std::string SDKPath = WRAPPER_SDK_PATH;
  OSVersion OSNum = parseOSVersion(WRAPPER_OS_VERSION_MIN);
  std::string triple = WRAPPER_HOST;

  fargs.push_back(compilerexecname);

  fargs.push_back("-target");
  fargs.push_back(triple);

  fargs.push_back("--sysroot");
  fargs.push_back(SDKPath);

  if (isCXX())
  {
    fargs.push_back("-stdlib=libc++");
    fargs.push_back("-cxx-isystem");
    fargs.push_back(SDKPath + "/usr/include/c++/v1");
  }

  fargs.push_back(std::string("-mmacosx-version-min=") + OSNum.Str());

  fargs.push_back("-arch");
  fargs.push_back(WRAPPER_ARCH);

  return true;
}

