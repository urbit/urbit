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

constexpr const char *getDefaultVendor() { return "apple"; }
constexpr const char *getDefaultTarget() { return OSXCROSS_TARGET; }

constexpr const char *getOSXCrossVersion() {
#ifdef OSXCROSS_VERSION
  return OSXCROSS_VERSION[0] ? OSXCROSS_VERSION : "unknown";
#else
  return "unknown";
#endif
}

inline const char *getSDKSearchDir() {
  const char *SDKSearchDir = getenv("OSXCROSS_SDK_SEARCH_DIR");

#ifdef OSXCROSS_SDK_SEARCH_DIR
  if (!SDKSearchDir)
    SDKSearchDir = OSXCROSS_SDK_SEARCH_DIR;
#endif

  return SDKSearchDir ? SDKSearchDir : "";
}

struct Target {
  Target();

  void overrideDefaultSDKPath(const char *SDKSearchDir);

  void addArch(const Arch arch);
  bool haveArch(const Arch arch);

  bool isCXX();
  bool isGCH();

  bool setup();

  const char *vendor;
  Arch arch;
  std::string target;
  StdLib stdlib;
  Compiler compiler;
  std::string compilerpath;
  std::string compilername;
  std::string compilerexecname;
  string_vector fargs;
  string_vector args;
  const char *language;
  char execpath[PATH_MAX + 1];
  std::string intrinsicpath;
};
