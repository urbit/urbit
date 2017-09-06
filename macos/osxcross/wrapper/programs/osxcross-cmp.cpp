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

namespace program {
namespace osxcross {

int cmp(int argc, char **argv) {
  if (argc < 4)
    return 1;

  OSVersion a = parseOSVersion(argv[1]);
  OSVersion b = parseOSVersion(argv[3]);

  const char *op = argv[2];
  bool res;

  if (!strcmp(op, ">"))
    res = a > b;
  else if (!strcmp(op, "<"))
    res = a < b;
  else if (!strcmp(op, ">="))
    res = a >= b;
  else if (!strcmp(op, "<="))
    res = a <= b;
  else if (!strcmp(op, "=="))
    res = a == b;
  else if (!strcmp(op, "!="))
    res = a != b;
  else
    return 1;

  std::cout << (res ? "1" : "0");
  return 0;
}

} // namespace osxcross
} // namespace program
