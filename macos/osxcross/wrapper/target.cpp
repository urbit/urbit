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
    : vendor(getDefaultVendor()),
      arch(Arch::x86_64), target(getDefaultTarget()), stdlib(StdLib::unset),
      wliblto(-1), language() {
  if (!getExecutablePath(execpath, sizeof(execpath)))
    abort();
}

OSVersion Target::getSDKOSNum() const {
  return parseOSVersion(WRAPPER_SDK_VERSION);
}

void Target::addArch(const Arch arch) {
  auto &v = targetarch;
  for (size_t i = 0; i < v.size(); ++i) {
    if (v[i] == arch) {
      v.erase(v.begin() + i);
      addArch(arch);
      return;
    }
  }
  v.push_back(arch);
}

bool Target::haveArch(const Arch arch) {
  for (auto a : targetarch) {
    if (arch == a)
      return true;
  }
  return false;
}

bool Target::isCXX() {
  return (compiler == Compiler::CLANGXX || compiler == Compiler::GXX);
}

bool Target::isGCH() {
  if (!language)
    return false;

  return !strcmp(language, "c-header") ||
         !strcmp(language, "c++-header") ||
         !strcmp(language, "objective-c-header") ||
         !strcmp(language, "objective-c++-header");
}

const std::string &Target::getDefaultTriple(std::string &triple) const {
  triple = getArchName(Arch::x86_64);
  triple += "-";
  triple += getDefaultVendor();
  triple += "-";
  triple += getDefaultTarget();
  return triple;
}

void Target::setCompilerPath() {
  if (!compilerpath.empty()) {
    compilerpath += "/";
    compilerpath += compilername;
  } else {
    if (!realPath(compilername.c_str(), compilerpath, ignoreCCACHE))
      compilerpath = compilername;

    compilerexecname += compilername;
  }
}

bool Target::setup() {
  std::string SDKPath = WRAPPER_SDK_PATH;
  OSVersion SDKOSNum = getSDKOSNum();

  if (targetarch.empty())
    targetarch.push_back(arch);

  std::string triple = getArchName(arch);
  triple += "-";
  triple += vendor;
  triple += "-";
  triple += target;

  std::string otriple = getArchName(Arch::x86_64);
  otriple += "-";
  otriple += vendor;
  otriple += "-";
  otriple += target;

  setCompilerPath();

  if (!OSNum.Num()) {
    if (haveArch(Arch::x86_64h)) {
      OSNum = OSVersion(10, 8); // Default to 10.8 for x86_64h
      if (SDKOSNum < OSNum) {
        err << "'" << getArchName(arch) << "' requires Mac OS X SDK "
            << OSNum.shortStr() << " (or later)" << err.endl();
        return false;
      }
    } else if (stdlib == StdLib::libcxx) {
      OSNum = OSVersion(10, 7); // Default to 10.7 for libc++
    } else {
      OSNum = getDefaultMinTarget();
    }
  }

  if (haveArch(Arch::x86_64h) && OSNum < OSVersion(10, 8)) {
    // -mmacosx-version-min= < 10.8 in combination with '-arch x86_64h'
    // may cause linker errors.

    // Erroring here is really annoying, better risk linking errors instead
    // of enforcing '-mmacosx-version-min= >= 10.8'.

    if (!getenv("OSXCROSS_NO_X86_64H_DEPLOYMENT_TARGET_WARNING"))
      warn << "'-mmacosx-version-min=' should be '>= 10.8' for architecture "
           << "'" << getArchName(Arch::x86_64h) << "'" << warn.endl();
  }

  if (stdlib == StdLib::unset) {
    stdlib = StdLib::libcxx;
  }
  if (OSNum > SDKOSNum) {
    err << "targeted OS X version must be <= " << SDKOSNum.Str() << " (SDK)"
        << err.endl();
    return false;
  } else if (OSNum < OSVersion(10, 4)) {
    err << "targeted OS X version must be >= 10.4" << err.endl();
    return false;
  }

  std::string CXXHeaderPath = SDKPath;
  string_vector AdditionalCXXHeaderPaths;

  auto addCXXPath = [&](const std::string &path) {
    std::string tmp;
    tmp = CXXHeaderPath;
    tmp += "/";
    tmp += path;
    AdditionalCXXHeaderPaths.push_back(tmp);
  };

  switch (stdlib) {
  case StdLib::libcxx: {
    CXXHeaderPath += "/usr/include/c++/v1";
    if (!dirExists(CXXHeaderPath)) {
      err << "cannot find " << getStdLibString(stdlib) << " headers"
          << err.endl();
      return false;
    }
    break;
  }
  case StdLib::libstdcxx: {
    // Use SDK libs

    CXXHeaderPath += "/usr/include/c++/4.2.1";

    std::string tmp = getArchName(arch);
    tmp += "-apple-";
    tmp += target;
    addCXXPath(tmp);

    addCXXPath("backward");

    if (!dirExists(CXXHeaderPath)) {
      err << "cannot find " << getStdLibString(stdlib) << " headers"
          << err.endl();
      return false;
    }

    break;
  }
  case StdLib::unset:
    abort();
  }

  fargs.push_back(compilerexecname);

  {
    fargs.push_back("-target");
    fargs.push_back(triple);

    std::string tmp = "-mlinker-version=";
    tmp += getLinkerVersion();

    fargs.push_back(tmp);
    tmp.clear();

    fargs.push_back("--sysroot");
    fargs.push_back(SDKPath);

    if (isCXX()) {
      tmp = "-stdlib=";
      tmp += getStdLibString(stdlib);
      fargs.push_back(tmp);
    }
  }

  auto addCXXHeaderPath = [&](const std::string &path) {
    fargs.push_back("-cxx-isystem");
    fargs.push_back(path);
  };

  addCXXHeaderPath(CXXHeaderPath);

  for (auto &path : AdditionalCXXHeaderPaths)
    addCXXHeaderPath(path);

  if (OSNum.Num()) {
    std::string tmp;
    tmp = "-mmacosx-version-min=";
    tmp += OSNum.Str();
    fargs.push_back(tmp);
  }

  for (auto arch : targetarch) {
    switch (arch) {
    case Arch::i386:
    case Arch::i486:
    case Arch::i586:
    case Arch::i686:
    case Arch::x86_64:
    case Arch::x86_64h:
      fargs.push_back("-arch");
      fargs.push_back(getArchName(arch));
      break;
    default:
      err << "unsupported architecture: '" << getArchName(arch) << "'"
          << err.endl();
      return false;
    }
  }

  //
  // Silence:
  // warning: libLTO.dylib relative to clang installed dir not found;
  //          using 'ld' default search path instead
  //
  // '-flto' will of course work nevertheless, it's just a buggy
  // cross-compilation warning.
  //
  if (wliblto == -1)
  {
    fargs.push_back("-Wno-liblto");
  }

  return true;
}

