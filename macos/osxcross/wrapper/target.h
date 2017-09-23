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

constexpr const char *getLinkerVersion() { return OSXCROSS_LINKER_VERSION; }
constexpr const char *getBuildDir() { return OSXCROSS_BUILD_DIR; }

constexpr const char *getLibLTOPath() {
#ifdef OSXCROSS_LIBLTO_PATH
  return OSXCROSS_LIBLTO_PATH;
#else
  return nullptr;
#endif
}

constexpr const char *getOSXCrossVersion() {
#ifdef OSXCROSS_VERSION
  return OSXCROSS_VERSION[0] ? OSXCROSS_VERSION : "unknown";
#else
  return "unknown";
#endif
}

#ifdef OSXCROSS_OSX_VERSION_MIN
inline OSVersion getDefaultMinTarget() {
  if (!strcmp(OSXCROSS_OSX_VERSION_MIN, "default"))
    return OSVersion();

  return parseOSVersion(OSXCROSS_OSX_VERSION_MIN);
}
#else
constexpr OSVersion getDefaultMinTarget() { return OSVersion(); }
#endif

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

  OSVersion getSDKOSNum() const;
  void overrideDefaultSDKPath(const char *SDKSearchDir);

  void addArch(const Arch arch);
  bool haveArch(const Arch arch);

  bool isCXX();
  bool isGCH();

  const std::string &getDefaultTriple(std::string &triple) const;

  bool findClangIntrinsicHeaders(std::string &path);

  bool setup();

  const char *vendor;
  Arch arch;
  std::vector<Arch> targetarch;
  std::string target;
  OSVersion OSNum;
  StdLib stdlib;
  ClangVersion clangversion;
  GCCVersion gccversion;
  int wliblto;
  Compiler compiler;
  std::string compilerpath;     // /usr/bin/clang | [...]/target/bin/*-gcc
  std::string compilername;     // clang | gcc
  std::string compilerexecname; // clang | *-apple-darwin-gcc
  string_vector fargs;
  string_vector args;
  const char *language;
  char execpath[PATH_MAX + 1];
  std::string intrinsicpath;
};
