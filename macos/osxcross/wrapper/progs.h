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

namespace target {
struct Target;
}

namespace program {

using target::Target;

class prog {
public:
  typedef int (*f1)();
  typedef int (*f2)(int, char **);
  typedef int (*f3)(int, char **, Target &);
  typedef int (*f4)(Target &);

  constexpr prog(const char *name, f1 fun)
      : name(name), fun1(fun), fun2(), fun3(), fun4(), type(1) {}

  constexpr prog(const char *name, f2 fun)
      : name(name), fun1(), fun2(fun), fun3(), fun4(), type(2) {}

  constexpr prog(const char *name, f3 fun)
      : name(name), fun1(), fun2(), fun3(fun), fun4(), type(3) {}

  constexpr prog(const char *name, f4 fun)
      : name(name), fun1(), fun2(), fun3(), fun4(fun), type(4) {}

  __attribute__((noreturn))
  void operator()(int argc, char **argv, Target &target) const {
    switch (type) {
    case 1:
      exit(fun1());
    case 2:
      exit(fun2(argc, argv));
    case 3:
      exit(fun3(argc, argv, target));
    case 4:
      exit(fun4(target));
    }
    __builtin_unreachable();
  }

  bool operator==(const char *name) const { return !strcmp(name, this->name); }

  template<class T>
  bool operator==(const T &name) const { return name == this->name; }

  const char *name;

private:
  f1 fun1;
  f2 fun2;
  f3 fun3;
  f4 fun4;
  int type;
};

int sw_vers(int argc, char **argv, target::Target &target);
int xcrun(int argc, char **argv, Target &target);
int dsymutil(int argc, char **argv, target::Target &target);

namespace osxcross {
int version();
int env(int argc, char **argv);
int conf(Target &target);
int cmp(int argc, char **argv);
int man(int argc, char **argv, Target &target);
int pkg_config(int argc, char **argv, Target &target);
}

}
