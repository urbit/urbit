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
#include <istream>
#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <climits>
#include <cassert>
#include <sys/time.h>
#include <sys/stat.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#include <CoreServices/CoreServices.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <libproc.h>
#endif

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
#include <sys/sysctl.h>
#include <sys/types.h>
#include <sys/user.h>
#endif

#ifdef __FreeBSD__
#include <libutil.h>
#endif

#include "tools.h"

bool isTerminal() {
  static bool first = false;
  static bool val;

  if (!first) {
    val = !!isatty(fileno(stderr));
    first = true;
  }

  return val;
}

char *getExecutablePath(char *buf, size_t len) {
  char *p;
#ifdef __APPLE__
  unsigned int l = len;
  if (_NSGetExecutablePath(buf, &l) != 0)
    return nullptr;
#elif defined(__FreeBSD__) || defined(__DragonFly__)
  int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
  size_t l = len;
  if (sysctl(mib, 4, buf, &l, nullptr, 0) != 0)
    return nullptr;
#elif defined(__OpenBSD__)
  int mib[4] = {CTL_KERN, KERN_PROC_ARGS, getpid(), KERN_PROC_ARGV};
  char **argv;
  size_t l;
  const char *comm;
  int ok = 0;
  if (sysctl(mib, 4, NULL, &l, NULL, 0) < 0)
    abort();
  argv = new char *[l];
  if (sysctl(mib, 4, argv, &l, NULL, 0) < 0)
    abort();
  comm = argv[0];
  if (*comm == '/' || *comm == '.') {
    char *rpath;
    if ((rpath = realpath(comm, NULL))) {
      strlcpy(buf, rpath, len);
      free(rpath);
      ok = 1;
    }
  } else {
    char *sp;
    char *xpath = strdup(getenv("PATH"));
    char *path = strtok_r(xpath, ":", &sp);
    struct stat st;
    if (!xpath)
      abort();
    while (path) {
      snprintf(buf, len, "%s/%s", path, comm);
      if (!stat(buf, &st) && (st.st_mode & S_IXUSR)) {
        ok = 1;
        break;
      }
      path = strtok_r(NULL, ":", &sp);
    }
    free(xpath);
  }
  if (ok)
    l = strlen(buf);
  else
    l = 0;
  delete[] argv;
#else
  ssize_t l = readlink("/proc/self/exe", buf, len - 1);
  assert(l > 0 && "/proc not mounted?");
  if (l > 0) buf[l] = '\0';
#endif
  if (l <= 0)
    return nullptr;
  buf[len - 1] = '\0';
  p = strrchr(buf, PATHDIV);
  if (p)
    *p = '\0';
  return buf;
}

const std::string &getParentProcessName() {
  static std::string name;
  auto getName = [](const char * path)->const char * {
    if (const char *p = strrchr(path, '/')) {
      return p + 1;
    }
    return path;
  };
  auto ppid = getppid();
#ifdef __APPLE__
  char path[PROC_PIDPATHINFO_MAXSIZE];
  if (proc_pidpath(ppid, path, sizeof(path))) {
    name = getName(path);
    return name;
  }
#elif defined(__FreeBSD__)
  struct kinfo_proc *proc = kinfo_getproc(ppid);
  if (proc) {
    name = getName(proc->ki_comm);
    free(proc);
    return name;
  }
#else
  std::stringstream file;
  file << "/proc/" << ppid << "/comm";
  if (getFileContent(file.str(), name)) {
    if (!name.empty() && name.rbegin()[0] == '\n') {
      name.resize(name.size() - 1);
    }
    return name;
  } else {
    clear(file);
    file << "/proc/" << ppid << "/exe";
    char buf[PATH_MAX + 1];
    if (readlink(file.str().c_str(), buf, sizeof(buf)) > 0) {
      buf[PATH_MAX] = '\0';
      name = getName(buf);
      return name;
    }
  }
#endif
  name = "unknown";
  return name;
}

//
// Environment
//

void concatEnvVariable(const char *var, const std::string &val) {
  std::string nval = val;
  if (char *oldval = getenv(var)) {
    nval += ":";
    nval += oldval;
  }
  setenv(var, nval.c_str(), 1);
}

std::string &escapePath(const std::string &path, std::string &escapedpath) {
  for (const char *p = path.c_str(); *p; ++p) {
    switch (*p) {
    case '"':
    case '\'':
    case '\\':
    case '$':
    case '(':
    case ')':
    case ' ':
    case ';':
    case ':':
      escapedpath += '\\';
    }
    escapedpath += *p;
  }
  return escapedpath;
}

void splitPath(const char *path, std::vector<std::string> &result) {
  char *sp;
  char *xpath = strdup(path);
  char *p = strtok_r(xpath, ":", &sp);
  if (!xpath)
    abort();
  while (p) {
    result.push_back(p);
    p = strtok_r(NULL, ":", &sp);
  }
  free(xpath);
}

std::string joinPath(const std::vector<std::string> &path) {
  std::string tmp;
  std::string escaped;
  for (size_t i = 0; i < path.size(); ++i) {
    escaped.clear();
    tmp += escapePath(path[i], escaped);
    if (i != path.size() - 1)
      tmp += ":";
  }
  return tmp;
}

bool hasPath(const std::vector<std::string> &path, const char *find) {
  for (const std::string &p : path)
    if (p == find)
      return true;
  return false;
}

//
// Files and Directories
//

std::string *getFileContent(const std::string &file, std::string &content) {
  std::ifstream f(file.c_str());

  if (!f.is_open())
    return nullptr;

  f.seekg(0, std::ios::end);
  auto len = f.tellg();
  f.seekg(0, std::ios::beg);

  if (len != static_cast<decltype(len)>(-1))
    content.reserve(static_cast<size_t>(f.tellg()));

  content.assign(std::istreambuf_iterator<char>(f),
                 std::istreambuf_iterator<char>());

  return &content;
}

bool writeFileContent(const std::string &file, const std::string &content) {
  std::ofstream f(file.c_str());

  if (!f.is_open())
    return false;

  f << content;
  return f.good();
}

bool fileExists(const std::string &file) {
  struct stat st;
  return !stat(file.c_str(), &st);
}

bool dirExists(const std::string &dir) {
  struct stat st;
  return !stat(dir.c_str(), &st) && S_ISDIR(st.st_mode);
}

typedef bool (*listfilescallback)(const char *file);

bool isDirectory(const char *file, const char *prefix) {
  struct stat st;
  if (prefix) {
    std::string tmp = prefix;
    tmp += "/";
    tmp += file;
    return !stat(tmp.c_str(), &st) && S_ISDIR(st.st_mode);
  } else {
    return !stat(file, &st) && S_ISDIR(st.st_mode);
  }
}

bool listFiles(const char *dir, std::vector<std::string> *files,
               listfilescallback cmp) {
  DIR *d = opendir(dir);
  dirent *de;

  if (!d)
    return false;

  while ((de = readdir(d))) {
    if ((!cmp || cmp(de->d_name)) && files) {
      files->push_back(de->d_name);
    }
  }

  closedir(d);
  return true;
}

typedef bool (*realpathcmp)(const char *file, const struct stat &st);

bool isExecutable(const char *f, const struct stat &) {
  return !access(f, F_OK | X_OK);
}

bool ignoreCCACHE(const char *f, const struct stat &) {
  const char *name = getFileName(f);
  return name && strstr(name, "ccache") != name;
}

bool realPath(const char *file, std::string &result,
              realpathcmp cmp1, realpathcmp cmp2) {
  char *PATH = getenv("PATH");
  const char *p = PATH ? PATH : "";
  struct stat st;

  result.clear();

  do {
    if (*p == ':')
      ++p;

    while (*p && *p != ':')
      result += *p++;

    result += "/";
    result += file;

    if (!stat(result.c_str(), &st)) {
      char buf[PATH_MAX + 1];

      if (realpath(result.c_str(), buf)) {
        result.assign(buf);
      } else {
        ssize_t len;
        char path[PATH_MAX];
        size_t pathlen;
        size_t n = 0;

        pathlen = result.find_last_of(PATHDIV);

        if (pathlen == std::string::npos)
          pathlen = result.length();
        else
          ++pathlen; // PATHDIV

        memcpy(path, result.c_str(), pathlen); // not null terminated

        while ((len = readlink(result.c_str(), buf, PATH_MAX)) != -1) {
          if (buf[0] != PATHDIV) {
            result.assign(path, pathlen);
            result.append(buf, len);
          } else {
            result.assign(buf, len);
            pathlen = strrchr(buf, PATHDIV) - buf + 1; // + 1: PATHDIV
            memcpy(path, buf, pathlen);
          }
          if (++n >= 1000) {
            err << result << ": too many levels of symbolic links"
                << err.endl();
            result.clear();
            break;
          }
        }
      }

      if ((!cmp1 || cmp1(result.c_str(), st)) &&
          (!cmp2 || cmp2(result.c_str(), st)))
        break;
    }

    result.clear();
  } while (*p);

  return !result.empty();
}

void stripFileName(std::string &path) {
  size_t lastpathdiv = path.find_last_of(PATHDIV);
  if (lastpathdiv != 0 && lastpathdiv != std::string::npos)
    path.resize(lastpathdiv);
}

const char *getFileName(const char *file) {
  const char *p = strrchr(file, PATHDIV);

  if (!p)
    p = file;
  else
    ++p;

  return p;
}

const char *getFileExtension(const char *file) {
  const char *p = strrchr(file, '.');

  if (!p)
    p = "";

  return p;
}

//
// Shell Commands
//

size_t runcommand(const char *command, char *buf, size_t len) {
#define RETURN(v)                                                              \
  do {                                                                         \
    if (p)                                                                     \
      pclose(p);                                                               \
    return (v);                                                                \
  } while (0)

  if (!len)
    return RUNCOMMAND_ERROR;

  FILE *p;
  size_t outputlen;

  if (!(p = popen(command, "r")))
    RETURN(RUNCOMMAND_ERROR);

  if (!(outputlen = fread(buf, sizeof(char), len - 1, p)))
    RETURN(RUNCOMMAND_ERROR);

  buf[outputlen] = '\0';

  RETURN(outputlen);
#undef RETURN
}

//
// Time
//

time_type getNanoSeconds() {
#ifdef __APPLE__
  union {
    AbsoluteTime at;
    time_type ull;
  } tmp;
  tmp.ull = mach_absolute_time();
  Nanoseconds ns = AbsoluteToNanoseconds(tmp.at);
  tmp.ull = UnsignedWideToUInt64(ns);
  return tmp.ull;
#elif defined(__linux__)
  struct timespec tp;
  if (clock_gettime(CLOCK_MONOTONIC, &tp) == 0)
    return static_cast<time_type>((tp.tv_sec * 1000000000LL) + tp.tv_nsec);
#endif
  struct timeval tv;
  if (gettimeofday(&tv, nullptr) == 0)
    return static_cast<time_type>((tv.tv_sec * 1000000000LL) +
                                  (tv.tv_usec * 1000));
  abort();
}

OSVersion parseOSVersion(const char *OSVer) {
  const char *p = OSVer;
  OSVersion OSNum;

  OSNum.major = atoi(p);

  while (*p && *p++ != '.')
    ;
  if (!*p)
    return OSNum;

  OSNum.minor = atoi(p);

  while (*p && *p++ != '.')
    ;
  if (!*p)
    return OSNum;

  OSNum.patch = atoi(p);
  return OSNum;
}
