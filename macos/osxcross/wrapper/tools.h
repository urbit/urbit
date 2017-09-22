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

struct stat;

typedef std::vector<std::string> string_vector;

static inline void clear(std::stringstream &sstr) {
  sstr.clear();
  sstr.str(std::string());
}

static inline bool endsWith(std::string const &str, std::string const &end) {
  if (end.size() > str.size())
    return false;
  return std::equal(end.rbegin(), end.rend(), str.rbegin());
}

size_t constexpr constexprStrLen(const char *str) {
  return *str ? 1 + constexprStrLen(str + 1) : 0;
}

// http://stackoverflow.com/a/17469726

enum ColorCode {
  FG_DEFAULT = 39,
  FG_BLACK = 30,
  FG_RED = 31,
  FG_GREEN = 32,
  FG_YELLOW = 33,
  FG_BLUE = 34,
  FG_MAGENTA = 35,
  FG_CYAN = 36,
  FG_LIGHT_GRAY = 37,
  FG_DARK_GRAY = 90,
  FG_LIGHT_RED = 91,
  FG_LIGHT_GREEN = 92,
  FG_LIGHT_YELLOW = 93,
  FG_LIGHT_BLUE = 94,
  FG_LIGHT_MAGENTA = 95,
  FG_LIGHT_CYAN = 96,
  FG_WHITE = 97,
  BG_RED = 41,
  BG_GREEN = 42,
  BG_BLUE = 44,
  BG_DEFAULT = 49
};

class Color {
  ColorCode cc;
public:
  Color(ColorCode cc) : cc(cc) {}
  friend std::ostream &
  operator<<(std::ostream &os, const Color &color) {
    return os;
  }
};

//
// Error message helper
//

static class Message {
private:
  const char *msg;
  Color color;
  std::ostream &os;
  bool printprefix;
public:
  static constexpr char endl() { return '\n'; }
  bool isendl(char c) { return c == '\n'; }
  template<typename T>
  bool isendl(T&&) { return false; }
  template<typename T>
  Message &operator<<(T &&v) {
    if (printprefix) {
      os << Color(FG_DARK_GRAY) << "osxcross: " << color << msg << ": "
         << Color(FG_DEFAULT);
      printprefix = false;
    }
    if (isendl(v)) {
      printprefix = true;
      os << std::endl;
    } else {
      os << v;
    }
    return *this;
  }
  Message(const char *msg, Color color = FG_RED, std::ostream &os = std::cerr)
      : msg(msg), color(color), os(os), printprefix(true) {}
} warn("warning"), err("error"), dbg("debug", FG_LIGHT_MAGENTA),
  info("info", FG_LIGHT_MAGENTA), warninfo("info", FG_LIGHT_MAGENTA);

//
// Executable path
//

char *getExecutablePath(char *buf, size_t len);
const std::string &getParentProcessName();
std::string &fixPathDiv(std::string &path);

//
// Environment
//

void concatEnvVariable(const char *var, const std::string &val);
std::string &escapePath(const std::string &path, std::string &escapedpath);
void splitPath(const char *path, std::vector<std::string> &result);
std::string joinPath(const std::vector<std::string> &path);
bool hasPath(const std::vector<std::string> &path, const char *find);

//
// Files and directories
//

constexpr char PATHDIV = '/';

std::string *getFileContent(const std::string &file, std::string &content);
bool writeFileContent(const std::string &file, const std::string &content);

bool fileExists(const std::string &dir);
bool dirExists(const std::string &dir);
typedef bool (*listfilescallback)(const char *file);
bool isDirectory(const char *file, const char *prefix);
bool listFiles(const char *dir, std::vector<std::string> *files,
               listfilescallback cmp);

typedef bool (*realpathcmp)(const char *file, const struct stat &st);
bool isExecutable(const char *f, const struct stat &);
bool ignoreCCACHE(const char *f, const struct stat &);
bool realPath(const char *file, std::string &result,
              realpathcmp cmp1 = nullptr, realpathcmp cmp2 = nullptr);
bool getPathOfCommand(const char *command, std::string &result,
                      realpathcmp cmp = nullptr);

void stripFileName(std::string &path);

const char *getFileName(const char *file);
const char *getFileExtension(const char *file);

inline const char *getFileName(const std::string &file) {
  return getFileName(file.c_str());
}

inline const char *getFileExtension(const std::string &file) {
  return getFileExtension(file.c_str());
}

//
// Argument Parsing
//

template <typename T, size_t size = 0> struct ArgParser {
  const struct Bind {
    const char *name;
    T fun;
    int numArgs;
  } binds[size];

  const Bind *parseArg(int argc, char **argv, const int numArg = 1) {
    const char *arg = argv[numArg];

    if (*arg != '-')
      return nullptr;

    while (*arg && *arg == '-')
      ++arg;

    for (size_t i = 0; i < size; ++i) {
      const Bind &bind = binds[i];

      if (!strcmp(arg, bind.name)) {
        if (argc - numArg <= bind.numArgs) {
          err << "too few arguments for '-" << bind.name << "'" << err.endl();
          return nullptr;
        }

        return &bind;
      }
    }

    return nullptr;
  }
};

//
// Shell Commands
//

constexpr size_t RUNCOMMAND_ERROR = -1;

size_t runcommand(const char *command, char *buf, size_t len);

//
// Time
//

typedef unsigned long long time_type;
time_type getNanoSeconds();

//
// OSVersion
//

#undef major
#undef minor
#undef patch

struct OSVersion {
  constexpr OSVersion(int major, int minor, int patch = 0)
      : major(major), minor(minor), patch(patch) {}
  constexpr OSVersion() : major(), minor(), patch() {}

  constexpr int Num() const {
    return major * 10000 + minor * 100 + patch;
  };

  constexpr bool operator>(const OSVersion &OSNum) const {
    return Num() > OSNum.Num();
  }

  constexpr bool operator>=(const OSVersion &OSNum) const {
    return Num() >= OSNum.Num();
  }

  constexpr bool operator<(const OSVersion &OSNum) const {
    return Num() < OSNum.Num();
  }

  constexpr bool operator<=(const OSVersion &OSNum) const {
    return Num() <= OSNum.Num();
  }

  constexpr bool operator==(const OSVersion &OSNum) const {
    return Num() == OSNum.Num();
  }

  constexpr bool operator!=(const OSVersion &OSNum) const {
    return Num() != OSNum.Num();
  }

  bool operator!=(const char *val) const {
    size_t c = 0;
    const char *p = val;

    while (*p) {
      if (*p++ == '.')
        ++c;
    }

    switch (c) {
    case 1:
      return shortStr() != val;
    case 2:
      return Str() != val;
    default:
      return true;
    }
  }

  std::string Str() const {
    std::stringstream tmp;
    tmp << major << "." << minor << "." << patch;
    return tmp.str();
  }

  std::string shortStr() const {
    std::stringstream tmp;
    tmp << major << "." << minor;
    return tmp.str();
  }

  int major;
  int minor;
  int patch;
};

static_assert(OSVersion(10, 6) != OSVersion(10, 5), "");

OSVersion parseOSVersion(const char *OSVer);

typedef OSVersion GCCVersion;
static const auto &parseGCCVersion = parseOSVersion;

typedef OSVersion ClangVersion;
static const auto &parseClangVersion = parseOSVersion;

typedef OSVersion LLVMVersion;
static const auto &parseLLVMVersion = parseOSVersion;

//
// Compiler Identifier
//

#undef Compiler
#undef CLANG
#undef CLANGXX
#undef GCC
#undef GXX

enum Compiler : int {
  CLANG,
  CLANGXX,
  GCC,
  GXX,
  UNKNOWN // Upper-case to avoid clash with "enum Arch"
};

inline Compiler getCompilerIdentifier(const char *compilername) {
  if (!strncmp(compilername, "clang++", 7))
    return Compiler::CLANGXX;
  if (!strncmp(compilername, "clang", 5))
    return Compiler::CLANG;
  else if (!strncmp(compilername, "g++", 3))
    return Compiler::GXX;
  else if (!strncmp(compilername, "gcc", 3))
    return Compiler::GCC;
  return Compiler::UNKNOWN;
}

//
// Arch
//

enum Arch {
  armv4t,
  armv5,
  armv6,
  armv7,
  armv7f,
  armv7k,
  armv7s,
  armv6m,
  armv7m,
  armv7em,
  armv8,
  arm64,
  arm64v8,
  i386,
  i486,
  i586,
  i686,
  x86_64,
  x86_64h, // Haswell
  ppc,
  ppc64,
  unknown
};

constexpr const char *ArchNames[] = {
  "armv4t", "armv5",  "armv6",   "armv7",  "armv7f",  "armv7k",  "armv7s",
  "amrv6m", "armv7m", "armv7em", "armv8",  "arm64",   "arm64v8", "i386",
  "i486",   "i586",   "i686",    "x86_64", "x86_64h", "ppc",     "ppc64",
  "unknown"
};

constexpr const char *getArchName(Arch arch) { return ArchNames[arch]; }

inline Arch parseArch(const char *arch) {
  size_t i = 0;
  for (auto archname : ArchNames) {
    if (!strcmp(arch, archname)) {
      return static_cast<Arch>(i);
    }
    ++i;
  }
  return Arch::unknown;
}

//
// Standard Library
//

enum StdLib {
  unset,
  libcxx,
  libstdcxx
};

constexpr const char *StdLibNames[] = { "default", "libc++", "libstdc++" };

constexpr const char *getStdLibString(StdLib stdlib) {
  return StdLibNames[stdlib];
}

