/* j/3/po.c
**
*/
#include "all.h"

u3_noun
u3_po_find_prefix(c3_y one, c3_y two, c3_y three) {
  switch (one) {
    case 'b': switch (two) {
      case 'a': switch (three)  {
        case 'c': return u3nc(0, 238);
        case 'l': return u3nc(0, 107);
        case 'n': return u3nc(0, 92);
        case 'r': return u3nc(0, 183);
        case 't': return u3nc(0, 172);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'c': return u3nc(0, 56);
        case 'd': return u3nc(0, 106);
        case 'l': return u3nc(0, 144);
        case 'n': return u3nc(0, 2);
        case 's': return u3nc(0, 60);
        case 't': return u3nc(0, 182);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'l': return u3nc(0, 45);
        case 'n': return u3nc(0, 244);
        case 'r': return u3nc(0, 188);
        case 's': return u3nc(0, 171);
        case 't': return u3nc(0, 98);
        default: return 0;
      }
      default: return 0;
    }
    case 'd': switch (two) {
      case 'a': switch (three)  {
        case 'b': return u3nc(0, 181);
        case 'c': return u3nc(0, 117);
        case 'l': return u3nc(0, 37);
        case 'n': return u3nc(0, 234);
        case 'p': return u3nc(0, 66);
        case 'r': return u3nc(0, 23);
        case 's': return u3nc(0, 61);
        case 't': return u3nc(0, 215);
        case 'v': return u3nc(0, 105);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'b': return u3nc(0, 179);
        case 'f': return u3nc(0, 57);
        case 'g': return u3nc(0, 193);
        case 'l': return u3nc(0, 49);
        case 'n': return u3nc(0, 217);
        case 'r': return u3nc(0, 11);
        case 's': return u3nc(0, 129);
        case 'v': return u3nc(0, 116);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'c': return u3nc(0, 146);
        case 'l': return u3nc(0, 102);
        case 'n': return u3nc(0, 233);
        case 'p': return u3nc(0, 18);
        case 'r': return u3nc(0, 24);
        case 's': return u3nc(0, 187);
        case 't': return u3nc(0, 47);
        case 'v': return u3nc(0, 236);
        case 'z': return u3nc(0, 0);
        default: return 0;
      }
      default: return 0;
    }
    case 'f': switch (two) {
      case 'a': switch (three)  {
        case 'b': return u3nc(0, 120);
        case 'd': return u3nc(0, 206);
        case 'l': return u3nc(0, 152);
        case 'm': return u3nc(0, 214);
        case 'n': return u3nc(0, 158);
        case 's': return u3nc(0, 195);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'd': return u3nc(0, 8);
        case 'g': return u3nc(0, 138);
        case 'l': return u3nc(0, 194);
        case 'n': return u3nc(0, 90);
        case 'p': return u3nc(0, 255);
        case 'r': return u3nc(0, 169);
        case 't': return u3nc(0, 226);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'd': return u3nc(0, 247);
        case 'g': return u3nc(0, 20);
        case 'l': return u3nc(0, 27);
        case 'n': return u3nc(0, 91);
        case 'p': return u3nc(0, 213);
        case 'r': return u3nc(0, 50);
        case 's': return u3nc(0, 46);
        case 't': return u3nc(0, 221);
        default: return 0;
      }
      default: return 0;
    }
    case 'h': switch (two) {
      case 'a': switch (three)  {
        case 'b': return u3nc(0, 209);
        case 'c': return u3nc(0, 174);
        case 'd': return u3nc(0, 145);
        case 'l': return u3nc(0, 203);
        case 'n': return u3nc(0, 41);
        case 'p': return u3nc(0, 156);
        case 'r': return u3nc(0, 198);
        case 's': return u3nc(0, 170);
        case 't': return u3nc(0, 218);
        case 'v': return u3nc(0, 176);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'd': return u3nc(0, 7);
        case 'l': return u3nc(0, 190);
        case 'n': return u3nc(0, 200);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'b': return u3nc(0, 197);
        case 'c': return u3nc(0, 223);
        case 'd': return u3nc(0, 26);
        case 'l': return u3nc(0, 32);
        case 'p': return u3nc(0, 22);
        case 's': return u3nc(0, 180);
        default: return 0;
      }
      default: return 0;
    }
    case 'l': switch (two) {
      case 'a': switch (three)  {
        case 'b': return u3nc(0, 161);
        case 'c': return u3nc(0, 34);
        case 'd': return u3nc(0, 235);
        case 'g': return u3nc(0, 205);
        case 'n': return u3nc(0, 232);
        case 'p': return u3nc(0, 240);
        case 'r': return u3nc(0, 225);
        case 's': return u3nc(0, 128);
        case 't': return u3nc(0, 134);
        case 'v': return u3nc(0, 252);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'b': return u3nc(0, 39);
        case 'd': return u3nc(0, 21);
        case 'g': return u3nc(0, 111);
        case 'n': return u3nc(0, 178);
        case 's': return u3nc(0, 9);
        case 't': return u3nc(0, 5);
        case 'v': return u3nc(0, 36);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'c': return u3nc(0, 69);
        case 'd': return u3nc(0, 186);
        case 'm': return u3nc(0, 166);
        case 'n': return u3nc(0, 135);
        case 'p': return u3nc(0, 63);
        case 'r': return u3nc(0, 25);
        case 's': return u3nc(0, 48);
        default: return 0;
      }
      default: return 0;
    }
    case 'm': switch (two) {
      case 'a': switch (three)  {
        case 'c': return u3nc(0, 191);
        case 'g': return u3nc(0, 103);
        case 'l': return u3nc(0, 110);
        case 'p': return u3nc(0, 130);
        case 'r': return u3nc(0, 1);
        case 's': return u3nc(0, 202);
        case 't': return u3nc(0, 253);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'c': return u3nc(0, 157);
        case 'd': return u3nc(0, 62);
        case 'g': return u3nc(0, 199);
        case 'l': return u3nc(0, 212);
        case 'n': return u3nc(0, 79);
        case 'p': return u3nc(0, 254);
        case 'r': return u3nc(0, 31);
        case 's': return u3nc(0, 126);
        case 't': return u3nc(0, 196);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'c': return u3nc(0, 148);
        case 'd': return u3nc(0, 19);
        case 'g': return u3nc(0, 162);
        case 'l': return u3nc(0, 67);
        case 'n': return u3nc(0, 122);
        case 'p': return u3nc(0, 208);
        case 'r': return u3nc(0, 93);
        case 's': return u3nc(0, 231);
        case 't': return u3nc(0, 82);
        default: return 0;
      }
      default: return 0;
    }
    case 'n': switch (two) {
      case 'a': switch (three)  {
        case 'c': return u3nc(0, 219);
        case 'l': return u3nc(0, 230);
        case 'm': return u3nc(0, 243);
        case 'p': return u3nc(0, 87);
        case 'r': return u3nc(0, 65);
        case 't': return u3nc(0, 77);
        case 'v': return u3nc(0, 137);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'b': return u3nc(0, 140);
        case 'd': return u3nc(0, 72);
        case 'l': return u3nc(0, 210);
        case 'm': return u3nc(0, 224);
        case 's': return u3nc(0, 124);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'c': return u3nc(0, 250);
        case 'd': return u3nc(0, 136);
        case 'l': return u3nc(0, 216);
        case 'm': return u3nc(0, 139);
        case 'p': return u3nc(0, 88);
        case 'r': return u3nc(0, 97);
        case 's': return u3nc(0, 211);
        case 'v': return u3nc(0, 70);
        default: return 0;
      }
      default: return 0;
    }
    case 'p': switch (two) {
      case 'a': switch (three)  {
        case 'c': return u3nc(0, 149);
        case 'd': return u3nc(0, 114);
        case 'g': return u3nc(0, 141);
        case 'l': return u3nc(0, 127);
        case 'n': return u3nc(0, 78);
        case 'r': return u3nc(0, 185);
        case 's': return u3nc(0, 33);
        case 't': return u3nc(0, 159);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'c': return u3nc(0, 104);
        case 'd': return u3nc(0, 43);
        case 'l': return u3nc(0, 51);
        case 'n': return u3nc(0, 165);
        case 't': return u3nc(0, 242);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'c': return u3nc(0, 173);
        case 'd': return u3nc(0, 81);
        case 'l': return u3nc(0, 239);
        case 'n': return u3nc(0, 248);
        case 's': return u3nc(0, 86);
        default: return 0;
      }
      default: return 0;
    }
    case 'r': switch (two) {
      case 'a': switch (three)  {
        case 'b': return u3nc(0, 131);
        case 'c': return u3nc(0, 184);
        case 'd': return u3nc(0, 201);
        case 'g': return u3nc(0, 204);
        case 'l': return u3nc(0, 143);
        case 'm': return u3nc(0, 52);
        case 'n': return u3nc(0, 123);
        case 'p': return u3nc(0, 228);
        case 'v': return u3nc(0, 150);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'b': return u3nc(0, 222);
        case 'c': return u3nc(0, 167);
        case 'd': return u3nc(0, 147);
        case 'g': return u3nc(0, 16);
        case 'l': return u3nc(0, 64);
        case 'n': return u3nc(0, 28);
        case 'p': return u3nc(0, 151);
        case 's': return u3nc(0, 220);
        case 't': return u3nc(0, 80);
        case 'v': return u3nc(0, 237);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'c': return u3nc(0, 58);
        case 'l': return u3nc(0, 133);
        case 'n': return u3nc(0, 96);
        case 'p': return u3nc(0, 75);
        case 's': return u3nc(0, 245);
        case 'v': return u3nc(0, 35);
        default: return 0;
      }
      default: return 0;
    }
    case 's': switch (two) {
      case 'a': switch (three)  {
        case 'b': return u3nc(0, 13);
        case 'l': return u3nc(0, 115);
        case 'm': return u3nc(0, 4);
        case 'n': return u3nc(0, 68);
        case 'p': return u3nc(0, 177);
        case 'r': return u3nc(0, 229);
        case 't': return u3nc(0, 38);
        case 'v': return u3nc(0, 85);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'b': return u3nc(0, 15);
        case 'c': return u3nc(0, 74);
        case 'd': return u3nc(0, 119);
        case 'g': return u3nc(0, 6);
        case 'l': return u3nc(0, 30);
        case 'm': return u3nc(0, 163);
        case 'p': return u3nc(0, 95);
        case 't': return u3nc(0, 71);
        case 'v': return u3nc(0, 112);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'c': return u3nc(0, 100);
        case 'g': return u3nc(0, 10);
        case 'l': return u3nc(0, 17);
        case 'm': return u3nc(0, 89);
        case 'n': return u3nc(0, 164);
        case 'p': return u3nc(0, 142);
        case 'r': return u3nc(0, 251);
        case 'v': return u3nc(0, 249);
        default: return 0;
      }
      default: return 0;
    }
    case 't': switch (two) {
      case 'a': switch (three)  {
        case 'b': return u3nc(0, 40);
        case 'c': return u3nc(0, 160);
        case 'd': return u3nc(0, 55);
        case 'g': return u3nc(0, 113);
        case 'l': return u3nc(0, 241);
        case 'm': return u3nc(0, 83);
        case 'n': return u3nc(0, 118);
        case 'p': return u3nc(0, 168);
        case 'r': return u3nc(0, 121);
        case 's': return u3nc(0, 109);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'c': return u3nc(0, 42);
        case 'd': return u3nc(0, 175);
        case 'l': return u3nc(0, 154);
        case 'm': return u3nc(0, 108);
        case 'n': return u3nc(0, 155);
        case 'p': return u3nc(0, 73);
        case 'r': return u3nc(0, 53);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'b': return u3nc(0, 132);
        case 'c': return u3nc(0, 189);
        case 'd': return u3nc(0, 153);
        case 'g': return u3nc(0, 29);
        case 'l': return u3nc(0, 84);
        case 'm': return u3nc(0, 192);
        case 'n': return u3nc(0, 246);
        case 'p': return u3nc(0, 207);
        case 'r': return u3nc(0, 44);
        default: return 0;
      }
      default: return 0;
    }
    case 'w': switch (two) {
      case 'a': switch (three)  {
        case 'c': return u3nc(0, 12);
        case 'l': return u3nc(0, 227);
        case 'n': return u3nc(0, 3);
        case 't': return u3nc(0, 101);
        default: return 0;
      }
      case 'i': switch (three)  {
        case 'c': return u3nc(0, 99);
        case 'd': return u3nc(0, 59);
        case 'n': return u3nc(0, 54);
        case 's': return u3nc(0, 14);
        case 't': return u3nc(0, 76);
        default: return 0;
      }
      case 'o': switch (three)  {
        case 'l': return u3nc(0, 125);
        case 'r': return u3nc(0, 94);
        default: return 0;
      }
      default: return 0;
    }
    default: return 0;
  }
}

void
u3_po_to_prefix(u3_noun id, c3_y* a, c3_y* b, c3_y* c)
{
  switch (id) {
    case 0: *a = 'd'; *b = 'o'; *c = 'z'; break;
    case 1: *a = 'm'; *b = 'a'; *c = 'r'; break;
    case 2: *a = 'b'; *b = 'i'; *c = 'n'; break;
    case 3: *a = 'w'; *b = 'a'; *c = 'n'; break;
    case 4: *a = 's'; *b = 'a'; *c = 'm'; break;
    case 5: *a = 'l'; *b = 'i'; *c = 't'; break;
    case 6: *a = 's'; *b = 'i'; *c = 'g'; break;
    case 7: *a = 'h'; *b = 'i'; *c = 'd'; break;
    case 8: *a = 'f'; *b = 'i'; *c = 'd'; break;
    case 9: *a = 'l'; *b = 'i'; *c = 's'; break;
    case 10: *a = 's'; *b = 'o'; *c = 'g'; break;
    case 11: *a = 'd'; *b = 'i'; *c = 'r'; break;
    case 12: *a = 'w'; *b = 'a'; *c = 'c'; break;
    case 13: *a = 's'; *b = 'a'; *c = 'b'; break;
    case 14: *a = 'w'; *b = 'i'; *c = 's'; break;
    case 15: *a = 's'; *b = 'i'; *c = 'b'; break;
    case 16: *a = 'r'; *b = 'i'; *c = 'g'; break;
    case 17: *a = 's'; *b = 'o'; *c = 'l'; break;
    case 18: *a = 'd'; *b = 'o'; *c = 'p'; break;
    case 19: *a = 'm'; *b = 'o'; *c = 'd'; break;
    case 20: *a = 'f'; *b = 'o'; *c = 'g'; break;
    case 21: *a = 'l'; *b = 'i'; *c = 'd'; break;
    case 22: *a = 'h'; *b = 'o'; *c = 'p'; break;
    case 23: *a = 'd'; *b = 'a'; *c = 'r'; break;
    case 24: *a = 'd'; *b = 'o'; *c = 'r'; break;
    case 25: *a = 'l'; *b = 'o'; *c = 'r'; break;
    case 26: *a = 'h'; *b = 'o'; *c = 'd'; break;
    case 27: *a = 'f'; *b = 'o'; *c = 'l'; break;
    case 28: *a = 'r'; *b = 'i'; *c = 'n'; break;
    case 29: *a = 't'; *b = 'o'; *c = 'g'; break;
    case 30: *a = 's'; *b = 'i'; *c = 'l'; break;
    case 31: *a = 'm'; *b = 'i'; *c = 'r'; break;
    case 32: *a = 'h'; *b = 'o'; *c = 'l'; break;
    case 33: *a = 'p'; *b = 'a'; *c = 's'; break;
    case 34: *a = 'l'; *b = 'a'; *c = 'c'; break;
    case 35: *a = 'r'; *b = 'o'; *c = 'v'; break;
    case 36: *a = 'l'; *b = 'i'; *c = 'v'; break;
    case 37: *a = 'd'; *b = 'a'; *c = 'l'; break;
    case 38: *a = 's'; *b = 'a'; *c = 't'; break;
    case 39: *a = 'l'; *b = 'i'; *c = 'b'; break;
    case 40: *a = 't'; *b = 'a'; *c = 'b'; break;
    case 41: *a = 'h'; *b = 'a'; *c = 'n'; break;
    case 42: *a = 't'; *b = 'i'; *c = 'c'; break;
    case 43: *a = 'p'; *b = 'i'; *c = 'd'; break;
    case 44: *a = 't'; *b = 'o'; *c = 'r'; break;
    case 45: *a = 'b'; *b = 'o'; *c = 'l'; break;
    case 46: *a = 'f'; *b = 'o'; *c = 's'; break;
    case 47: *a = 'd'; *b = 'o'; *c = 't'; break;
    case 48: *a = 'l'; *b = 'o'; *c = 's'; break;
    case 49: *a = 'd'; *b = 'i'; *c = 'l'; break;
    case 50: *a = 'f'; *b = 'o'; *c = 'r'; break;
    case 51: *a = 'p'; *b = 'i'; *c = 'l'; break;
    case 52: *a = 'r'; *b = 'a'; *c = 'm'; break;
    case 53: *a = 't'; *b = 'i'; *c = 'r'; break;
    case 54: *a = 'w'; *b = 'i'; *c = 'n'; break;
    case 55: *a = 't'; *b = 'a'; *c = 'd'; break;
    case 56: *a = 'b'; *b = 'i'; *c = 'c'; break;
    case 57: *a = 'd'; *b = 'i'; *c = 'f'; break;
    case 58: *a = 'r'; *b = 'o'; *c = 'c'; break;
    case 59: *a = 'w'; *b = 'i'; *c = 'd'; break;
    case 60: *a = 'b'; *b = 'i'; *c = 's'; break;
    case 61: *a = 'd'; *b = 'a'; *c = 's'; break;
    case 62: *a = 'm'; *b = 'i'; *c = 'd'; break;
    case 63: *a = 'l'; *b = 'o'; *c = 'p'; break;
    case 64: *a = 'r'; *b = 'i'; *c = 'l'; break;
    case 65: *a = 'n'; *b = 'a'; *c = 'r'; break;
    case 66: *a = 'd'; *b = 'a'; *c = 'p'; break;
    case 67: *a = 'm'; *b = 'o'; *c = 'l'; break;
    case 68: *a = 's'; *b = 'a'; *c = 'n'; break;
    case 69: *a = 'l'; *b = 'o'; *c = 'c'; break;
    case 70: *a = 'n'; *b = 'o'; *c = 'v'; break;
    case 71: *a = 's'; *b = 'i'; *c = 't'; break;
    case 72: *a = 'n'; *b = 'i'; *c = 'd'; break;
    case 73: *a = 't'; *b = 'i'; *c = 'p'; break;
    case 74: *a = 's'; *b = 'i'; *c = 'c'; break;
    case 75: *a = 'r'; *b = 'o'; *c = 'p'; break;
    case 76: *a = 'w'; *b = 'i'; *c = 't'; break;
    case 77: *a = 'n'; *b = 'a'; *c = 't'; break;
    case 78: *a = 'p'; *b = 'a'; *c = 'n'; break;
    case 79: *a = 'm'; *b = 'i'; *c = 'n'; break;
    case 80: *a = 'r'; *b = 'i'; *c = 't'; break;
    case 81: *a = 'p'; *b = 'o'; *c = 'd'; break;
    case 82: *a = 'm'; *b = 'o'; *c = 't'; break;
    case 83: *a = 't'; *b = 'a'; *c = 'm'; break;
    case 84: *a = 't'; *b = 'o'; *c = 'l'; break;
    case 85: *a = 's'; *b = 'a'; *c = 'v'; break;
    case 86: *a = 'p'; *b = 'o'; *c = 's'; break;
    case 87: *a = 'n'; *b = 'a'; *c = 'p'; break;
    case 88: *a = 'n'; *b = 'o'; *c = 'p'; break;
    case 89: *a = 's'; *b = 'o'; *c = 'm'; break;
    case 90: *a = 'f'; *b = 'i'; *c = 'n'; break;
    case 91: *a = 'f'; *b = 'o'; *c = 'n'; break;
    case 92: *a = 'b'; *b = 'a'; *c = 'n'; break;
    case 93: *a = 'm'; *b = 'o'; *c = 'r'; break;
    case 94: *a = 'w'; *b = 'o'; *c = 'r'; break;
    case 95: *a = 's'; *b = 'i'; *c = 'p'; break;
    case 96: *a = 'r'; *b = 'o'; *c = 'n'; break;
    case 97: *a = 'n'; *b = 'o'; *c = 'r'; break;
    case 98: *a = 'b'; *b = 'o'; *c = 't'; break;
    case 99: *a = 'w'; *b = 'i'; *c = 'c'; break;
    case 100: *a = 's'; *b = 'o'; *c = 'c'; break;
    case 101: *a = 'w'; *b = 'a'; *c = 't'; break;
    case 102: *a = 'd'; *b = 'o'; *c = 'l'; break;
    case 103: *a = 'm'; *b = 'a'; *c = 'g'; break;
    case 104: *a = 'p'; *b = 'i'; *c = 'c'; break;
    case 105: *a = 'd'; *b = 'a'; *c = 'v'; break;
    case 106: *a = 'b'; *b = 'i'; *c = 'd'; break;
    case 107: *a = 'b'; *b = 'a'; *c = 'l'; break;
    case 108: *a = 't'; *b = 'i'; *c = 'm'; break;
    case 109: *a = 't'; *b = 'a'; *c = 's'; break;
    case 110: *a = 'm'; *b = 'a'; *c = 'l'; break;
    case 111: *a = 'l'; *b = 'i'; *c = 'g'; break;
    case 112: *a = 's'; *b = 'i'; *c = 'v'; break;
    case 113: *a = 't'; *b = 'a'; *c = 'g'; break;
    case 114: *a = 'p'; *b = 'a'; *c = 'd'; break;
    case 115: *a = 's'; *b = 'a'; *c = 'l'; break;
    case 116: *a = 'd'; *b = 'i'; *c = 'v'; break;
    case 117: *a = 'd'; *b = 'a'; *c = 'c'; break;
    case 118: *a = 't'; *b = 'a'; *c = 'n'; break;
    case 119: *a = 's'; *b = 'i'; *c = 'd'; break;
    case 120: *a = 'f'; *b = 'a'; *c = 'b'; break;
    case 121: *a = 't'; *b = 'a'; *c = 'r'; break;
    case 122: *a = 'm'; *b = 'o'; *c = 'n'; break;
    case 123: *a = 'r'; *b = 'a'; *c = 'n'; break;
    case 124: *a = 'n'; *b = 'i'; *c = 's'; break;
    case 125: *a = 'w'; *b = 'o'; *c = 'l'; break;
    case 126: *a = 'm'; *b = 'i'; *c = 's'; break;
    case 127: *a = 'p'; *b = 'a'; *c = 'l'; break;
    case 128: *a = 'l'; *b = 'a'; *c = 's'; break;
    case 129: *a = 'd'; *b = 'i'; *c = 's'; break;
    case 130: *a = 'm'; *b = 'a'; *c = 'p'; break;
    case 131: *a = 'r'; *b = 'a'; *c = 'b'; break;
    case 132: *a = 't'; *b = 'o'; *c = 'b'; break;
    case 133: *a = 'r'; *b = 'o'; *c = 'l'; break;
    case 134: *a = 'l'; *b = 'a'; *c = 't'; break;
    case 135: *a = 'l'; *b = 'o'; *c = 'n'; break;
    case 136: *a = 'n'; *b = 'o'; *c = 'd'; break;
    case 137: *a = 'n'; *b = 'a'; *c = 'v'; break;
    case 138: *a = 'f'; *b = 'i'; *c = 'g'; break;
    case 139: *a = 'n'; *b = 'o'; *c = 'm'; break;
    case 140: *a = 'n'; *b = 'i'; *c = 'b'; break;
    case 141: *a = 'p'; *b = 'a'; *c = 'g'; break;
    case 142: *a = 's'; *b = 'o'; *c = 'p'; break;
    case 143: *a = 'r'; *b = 'a'; *c = 'l'; break;
    case 144: *a = 'b'; *b = 'i'; *c = 'l'; break;
    case 145: *a = 'h'; *b = 'a'; *c = 'd'; break;
    case 146: *a = 'd'; *b = 'o'; *c = 'c'; break;
    case 147: *a = 'r'; *b = 'i'; *c = 'd'; break;
    case 148: *a = 'm'; *b = 'o'; *c = 'c'; break;
    case 149: *a = 'p'; *b = 'a'; *c = 'c'; break;
    case 150: *a = 'r'; *b = 'a'; *c = 'v'; break;
    case 151: *a = 'r'; *b = 'i'; *c = 'p'; break;
    case 152: *a = 'f'; *b = 'a'; *c = 'l'; break;
    case 153: *a = 't'; *b = 'o'; *c = 'd'; break;
    case 154: *a = 't'; *b = 'i'; *c = 'l'; break;
    case 155: *a = 't'; *b = 'i'; *c = 'n'; break;
    case 156: *a = 'h'; *b = 'a'; *c = 'p'; break;
    case 157: *a = 'm'; *b = 'i'; *c = 'c'; break;
    case 158: *a = 'f'; *b = 'a'; *c = 'n'; break;
    case 159: *a = 'p'; *b = 'a'; *c = 't'; break;
    case 160: *a = 't'; *b = 'a'; *c = 'c'; break;
    case 161: *a = 'l'; *b = 'a'; *c = 'b'; break;
    case 162: *a = 'm'; *b = 'o'; *c = 'g'; break;
    case 163: *a = 's'; *b = 'i'; *c = 'm'; break;
    case 164: *a = 's'; *b = 'o'; *c = 'n'; break;
    case 165: *a = 'p'; *b = 'i'; *c = 'n'; break;
    case 166: *a = 'l'; *b = 'o'; *c = 'm'; break;
    case 167: *a = 'r'; *b = 'i'; *c = 'c'; break;
    case 168: *a = 't'; *b = 'a'; *c = 'p'; break;
    case 169: *a = 'f'; *b = 'i'; *c = 'r'; break;
    case 170: *a = 'h'; *b = 'a'; *c = 's'; break;
    case 171: *a = 'b'; *b = 'o'; *c = 's'; break;
    case 172: *a = 'b'; *b = 'a'; *c = 't'; break;
    case 173: *a = 'p'; *b = 'o'; *c = 'c'; break;
    case 174: *a = 'h'; *b = 'a'; *c = 'c'; break;
    case 175: *a = 't'; *b = 'i'; *c = 'd'; break;
    case 176: *a = 'h'; *b = 'a'; *c = 'v'; break;
    case 177: *a = 's'; *b = 'a'; *c = 'p'; break;
    case 178: *a = 'l'; *b = 'i'; *c = 'n'; break;
    case 179: *a = 'd'; *b = 'i'; *c = 'b'; break;
    case 180: *a = 'h'; *b = 'o'; *c = 's'; break;
    case 181: *a = 'd'; *b = 'a'; *c = 'b'; break;
    case 182: *a = 'b'; *b = 'i'; *c = 't'; break;
    case 183: *a = 'b'; *b = 'a'; *c = 'r'; break;
    case 184: *a = 'r'; *b = 'a'; *c = 'c'; break;
    case 185: *a = 'p'; *b = 'a'; *c = 'r'; break;
    case 186: *a = 'l'; *b = 'o'; *c = 'd'; break;
    case 187: *a = 'd'; *b = 'o'; *c = 's'; break;
    case 188: *a = 'b'; *b = 'o'; *c = 'r'; break;
    case 189: *a = 't'; *b = 'o'; *c = 'c'; break;
    case 190: *a = 'h'; *b = 'i'; *c = 'l'; break;
    case 191: *a = 'm'; *b = 'a'; *c = 'c'; break;
    case 192: *a = 't'; *b = 'o'; *c = 'm'; break;
    case 193: *a = 'd'; *b = 'i'; *c = 'g'; break;
    case 194: *a = 'f'; *b = 'i'; *c = 'l'; break;
    case 195: *a = 'f'; *b = 'a'; *c = 's'; break;
    case 196: *a = 'm'; *b = 'i'; *c = 't'; break;
    case 197: *a = 'h'; *b = 'o'; *c = 'b'; break;
    case 198: *a = 'h'; *b = 'a'; *c = 'r'; break;
    case 199: *a = 'm'; *b = 'i'; *c = 'g'; break;
    case 200: *a = 'h'; *b = 'i'; *c = 'n'; break;
    case 201: *a = 'r'; *b = 'a'; *c = 'd'; break;
    case 202: *a = 'm'; *b = 'a'; *c = 's'; break;
    case 203: *a = 'h'; *b = 'a'; *c = 'l'; break;
    case 204: *a = 'r'; *b = 'a'; *c = 'g'; break;
    case 205: *a = 'l'; *b = 'a'; *c = 'g'; break;
    case 206: *a = 'f'; *b = 'a'; *c = 'd'; break;
    case 207: *a = 't'; *b = 'o'; *c = 'p'; break;
    case 208: *a = 'm'; *b = 'o'; *c = 'p'; break;
    case 209: *a = 'h'; *b = 'a'; *c = 'b'; break;
    case 210: *a = 'n'; *b = 'i'; *c = 'l'; break;
    case 211: *a = 'n'; *b = 'o'; *c = 's'; break;
    case 212: *a = 'm'; *b = 'i'; *c = 'l'; break;
    case 213: *a = 'f'; *b = 'o'; *c = 'p'; break;
    case 214: *a = 'f'; *b = 'a'; *c = 'm'; break;
    case 215: *a = 'd'; *b = 'a'; *c = 't'; break;
    case 216: *a = 'n'; *b = 'o'; *c = 'l'; break;
    case 217: *a = 'd'; *b = 'i'; *c = 'n'; break;
    case 218: *a = 'h'; *b = 'a'; *c = 't'; break;
    case 219: *a = 'n'; *b = 'a'; *c = 'c'; break;
    case 220: *a = 'r'; *b = 'i'; *c = 's'; break;
    case 221: *a = 'f'; *b = 'o'; *c = 't'; break;
    case 222: *a = 'r'; *b = 'i'; *c = 'b'; break;
    case 223: *a = 'h'; *b = 'o'; *c = 'c'; break;
    case 224: *a = 'n'; *b = 'i'; *c = 'm'; break;
    case 225: *a = 'l'; *b = 'a'; *c = 'r'; break;
    case 226: *a = 'f'; *b = 'i'; *c = 't'; break;
    case 227: *a = 'w'; *b = 'a'; *c = 'l'; break;
    case 228: *a = 'r'; *b = 'a'; *c = 'p'; break;
    case 229: *a = 's'; *b = 'a'; *c = 'r'; break;
    case 230: *a = 'n'; *b = 'a'; *c = 'l'; break;
    case 231: *a = 'm'; *b = 'o'; *c = 's'; break;
    case 232: *a = 'l'; *b = 'a'; *c = 'n'; break;
    case 233: *a = 'd'; *b = 'o'; *c = 'n'; break;
    case 234: *a = 'd'; *b = 'a'; *c = 'n'; break;
    case 235: *a = 'l'; *b = 'a'; *c = 'd'; break;
    case 236: *a = 'd'; *b = 'o'; *c = 'v'; break;
    case 237: *a = 'r'; *b = 'i'; *c = 'v'; break;
    case 238: *a = 'b'; *b = 'a'; *c = 'c'; break;
    case 239: *a = 'p'; *b = 'o'; *c = 'l'; break;
    case 240: *a = 'l'; *b = 'a'; *c = 'p'; break;
    case 241: *a = 't'; *b = 'a'; *c = 'l'; break;
    case 242: *a = 'p'; *b = 'i'; *c = 't'; break;
    case 243: *a = 'n'; *b = 'a'; *c = 'm'; break;
    case 244: *a = 'b'; *b = 'o'; *c = 'n'; break;
    case 245: *a = 'r'; *b = 'o'; *c = 's'; break;
    case 246: *a = 't'; *b = 'o'; *c = 'n'; break;
    case 247: *a = 'f'; *b = 'o'; *c = 'd'; break;
    case 248: *a = 'p'; *b = 'o'; *c = 'n'; break;
    case 249: *a = 's'; *b = 'o'; *c = 'v'; break;
    case 250: *a = 'n'; *b = 'o'; *c = 'c'; break;
    case 251: *a = 's'; *b = 'o'; *c = 'r'; break;
    case 252: *a = 'l'; *b = 'a'; *c = 'v'; break;
    case 253: *a = 'm'; *b = 'a'; *c = 't'; break;
    case 254: *a = 'm'; *b = 'i'; *c = 'p'; break;
    case 255: *a = 'f'; *b = 'i'; *c = 'p'; break;
    default: u3m_bail(c3__exit);
  }
}

u3_noun
u3_po_find_suffix(c3_y one, c3_y two, c3_y three) {
  switch (one) {
    case 'b': switch (two) {
      case 'e': switch (three)  {
        case 'c': return u3nc(0, 238);
        case 'l': return u3nc(0, 107);
        case 'n': return u3nc(0, 92);
        case 'p': return u3nc(0, 183);
        case 'r': return u3nc(0, 172);
        case 's': return u3nc(0, 56);
        case 't': return u3nc(0, 106);
        case 'x': return u3nc(0, 144);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'd': return u3nc(0, 2);
        case 'r': return u3nc(0, 60);
        case 's': return u3nc(0, 182);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'l': return u3nc(0, 176);
        case 'n': return u3nc(0, 45);
        case 'r': return u3nc(0, 244);
        case 't': return u3nc(0, 188);
        default: return 0;
      }
      default: return 0;
    }
    case 'd': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 171);
        case 'c': return u3nc(0, 98);
        case 'f': return u3nc(0, 181);
        case 'g': return u3nc(0, 117);
        case 'l': return u3nc(0, 37);
        case 'm': return u3nc(0, 234);
        case 'n': return u3nc(0, 66);
        case 'p': return u3nc(0, 23);
        case 'r': return u3nc(0, 61);
        case 's': return u3nc(0, 215);
        case 't': return u3nc(0, 105);
        case 'v': return u3nc(0, 179);
        case 'x': return u3nc(0, 57);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'c': return u3nc(0, 193);
        case 'l': return u3nc(0, 49);
        case 'n': return u3nc(0, 217);
        case 'r': return u3nc(0, 11);
        case 's': return u3nc(0, 129);
        case 't': return u3nc(0, 116);
        case 'x': return u3nc(0, 146);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'l': return u3nc(0, 102);
        case 'n': return u3nc(0, 233);
        case 'r': return u3nc(0, 18);
        case 's': return u3nc(0, 24);
        case 't': return u3nc(0, 187);
        default: return 0;
      }
      default: return 0;
    }
    case 'f': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 47);
        case 'd': return u3nc(0, 236);
        case 'l': return u3nc(0, 120);
        case 'n': return u3nc(0, 206);
        case 'p': return u3nc(0, 152);
        case 'r': return u3nc(0, 158);
        case 's': return u3nc(0, 255);
        case 't': return u3nc(0, 214);
        case 'x': return u3nc(0, 195);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'l': return u3nc(0, 8);
        case 'n': return u3nc(0, 138);
        case 'r': return u3nc(0, 194);
        case 's': return u3nc(0, 90);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'l': return u3nc(0, 169);
        case 'n': return u3nc(0, 226);
        case 'r': return u3nc(0, 247);
        default: return 0;
      }
      default: return 0;
    }
    case 'h': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 20);
        case 'c': return u3nc(0, 27);
        case 'p': return u3nc(0, 91);
        case 's': return u3nc(0, 213);
        case 't': return u3nc(0, 50);
        case 'x': return u3nc(0, 46);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'l': return u3nc(0, 221);
        case 's': return u3nc(0, 209);
        case 't': return u3nc(0, 174);
        default: return 0;
      }
      default: return 0;
    }
    case 'l': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 145);
        case 'c': return u3nc(0, 203);
        case 'd': return u3nc(0, 41);
        case 'g': return u3nc(0, 156);
        case 'n': return u3nc(0, 198);
        case 'p': return u3nc(0, 170);
        case 'r': return u3nc(0, 218);
        case 't': return u3nc(0, 7);
        case 'v': return u3nc(0, 190);
        case 'x': return u3nc(0, 200);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'c': return u3nc(0, 197);
        case 'd': return u3nc(0, 223);
        case 'g': return u3nc(0, 26);
        case 'n': return u3nc(0, 32);
        case 'p': return u3nc(0, 22);
        case 'r': return u3nc(0, 180);
        case 's': return u3nc(0, 161);
        case 't': return u3nc(0, 34);
        case 'x': return u3nc(0, 235);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'd': return u3nc(0, 205);
        case 'n': return u3nc(0, 232);
        case 'r': return u3nc(0, 240);
        case 's': return u3nc(0, 225);
        case 't': return u3nc(0, 128);
        case 'x': return u3nc(0, 134);
        default: return 0;
      }
      default: return 0;
    }
    case 'm': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 114);
        case 'c': return u3nc(0, 141);
        case 'd': return u3nc(0, 127);
        case 'g': return u3nc(0, 78);
        case 'l': return u3nc(0, 185);
        case 'p': return u3nc(0, 33);
        case 'r': return u3nc(0, 159);
        case 's': return u3nc(0, 104);
        case 't': return u3nc(0, 43);
        case 'v': return u3nc(0, 51);
        case 'x': return u3nc(0, 165);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'd': return u3nc(0, 242);
        case 'g': return u3nc(0, 173);
        case 'l': return u3nc(0, 81);
        case 'n': return u3nc(0, 239);
        case 'r': return u3nc(0, 248);
        case 's': return u3nc(0, 93);
        case 't': return u3nc(0, 86);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'l': return u3nc(0, 191);
        case 'n': return u3nc(0, 103);
        case 'r': return u3nc(0, 110);
        default: return 0;
      }
      default: return 0;
    }
    case 'n': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 130);
        case 'c': return u3nc(0, 1);
        case 'd': return u3nc(0, 202);
        case 'l': return u3nc(0, 253);
        case 'm': return u3nc(0, 157);
        case 'p': return u3nc(0, 62);
        case 'r': return u3nc(0, 199);
        case 's': return u3nc(0, 212);
        case 't': return u3nc(0, 79);
        case 'v': return u3nc(0, 254);
        case 'x': return u3nc(0, 31);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'b': return u3nc(0, 126);
        case 'l': return u3nc(0, 196);
        case 'm': return u3nc(0, 148);
        case 'p': return u3nc(0, 19);
        case 's': return u3nc(0, 162);
        case 't': return u3nc(0, 67);
        case 'x': return u3nc(0, 122);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'd': return u3nc(0, 208);
        case 'l': return u3nc(0, 231);
        case 'm': return u3nc(0, 82);
        case 'r': return u3nc(0, 219);
        case 's': return u3nc(0, 230);
        case 't': return u3nc(0, 243);
        case 'x': return u3nc(0, 87);
        default: return 0;
      }
      default: return 0;
    }
    case 'p': switch (two) {
      case 'e': switch (three)  {
        case 'c': return u3nc(0, 252);
        case 'd': return u3nc(0, 39);
        case 'g': return u3nc(0, 21);
        case 'l': return u3nc(0, 111);
        case 'm': return u3nc(0, 178);
        case 'n': return u3nc(0, 9);
        case 'r': return u3nc(0, 5);
        case 's': return u3nc(0, 36);
        case 't': return u3nc(0, 69);
        case 'x': return u3nc(0, 186);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'b': return u3nc(0, 166);
        case 'n': return u3nc(0, 135);
        case 'r': return u3nc(0, 63);
        case 't': return u3nc(0, 25);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'l': return u3nc(0, 48);
        case 'x': return u3nc(0, 149);
        default: return 0;
      }
      default: return 0;
    }
    case 'r': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 65);
        case 'c': return u3nc(0, 77);
        case 'd': return u3nc(0, 137);
        case 'f': return u3nc(0, 140);
        case 'g': return u3nc(0, 72);
        case 'l': return u3nc(0, 210);
        case 'm': return u3nc(0, 224);
        case 'n': return u3nc(0, 124);
        case 'p': return u3nc(0, 250);
        case 's': return u3nc(0, 136);
        case 't': return u3nc(0, 216);
        case 'v': return u3nc(0, 139);
        case 'x': return u3nc(0, 88);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'c': return u3nc(0, 97);
        case 'd': return u3nc(0, 211);
        case 'l': return u3nc(0, 70);
        case 'm': return u3nc(0, 131);
        case 'n': return u3nc(0, 184);
        case 'p': return u3nc(0, 201);
        case 's': return u3nc(0, 143);
        case 't': return u3nc(0, 52);
        case 'x': return u3nc(0, 123);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'c': return u3nc(0, 228);
        case 'd': return u3nc(0, 204);
        case 'g': return u3nc(0, 150);
        case 'l': return u3nc(0, 222);
        case 'm': return u3nc(0, 167);
        case 'n': return u3nc(0, 147);
        case 'p': return u3nc(0, 16);
        case 's': return u3nc(0, 64);
        case 't': return u3nc(0, 28);
        case 'x': return u3nc(0, 151);
        default: return 0;
      }
      default: return 0;
    }
    case 's': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 220);
        case 'c': return u3nc(0, 80);
        case 'd': return u3nc(0, 237);
        case 'f': return u3nc(0, 58);
        case 'g': return u3nc(0, 133);
        case 'l': return u3nc(0, 96);
        case 'm': return u3nc(0, 75);
        case 'n': return u3nc(0, 245);
        case 'p': return u3nc(0, 35);
        case 'r': return u3nc(0, 13);
        case 't': return u3nc(0, 115);
        case 'v': return u3nc(0, 4);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'b': return u3nc(0, 68);
        case 'd': return u3nc(0, 177);
        case 'g': return u3nc(0, 229);
        case 'l': return u3nc(0, 38);
        case 'm': return u3nc(0, 85);
        case 'n': return u3nc(0, 15);
        case 'p': return u3nc(0, 74);
        case 'r': return u3nc(0, 119);
        case 't': return u3nc(0, 6);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'd': return u3nc(0, 30);
        case 'l': return u3nc(0, 163);
        case 'm': return u3nc(0, 95);
        case 'n': return u3nc(0, 71);
        case 'p': return u3nc(0, 112);
        case 'r': return u3nc(0, 100);
        case 't': return u3nc(0, 10);
        case 'x': return u3nc(0, 17);
        default: return 0;
      }
      default: return 0;
    }
    case 't': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 89);
        case 'c': return u3nc(0, 164);
        case 'd': return u3nc(0, 142);
        case 'g': return u3nc(0, 251);
        case 'l': return u3nc(0, 249);
        case 'm': return u3nc(0, 40);
        case 'n': return u3nc(0, 160);
        case 'p': return u3nc(0, 55);
        case 'r': return u3nc(0, 113);
        case 's': return u3nc(0, 241);
        case 'v': return u3nc(0, 83);
        case 'x': return u3nc(0, 118);
        default: return 0;
      }
      case 'u': switch (three)  {
        case 'c': return u3nc(0, 168);
        case 'd': return u3nc(0, 121);
        case 'g': return u3nc(0, 109);
        case 'l': return u3nc(0, 42);
        case 'n': return u3nc(0, 175);
        case 's': return u3nc(0, 154);
        case 'x': return u3nc(0, 108);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'c': return u3nc(0, 155);
        case 'd': return u3nc(0, 73);
        case 'l': return u3nc(0, 53);
        case 'n': return u3nc(0, 132);
        case 'p': return u3nc(0, 189);
        case 'r': return u3nc(0, 153);
        case 'v': return u3nc(0, 29);
        default: return 0;
      }
      default: return 0;
    }
    case 'w': switch (two) {
      case 'e': switch (three)  {
        case 'b': return u3nc(0, 84);
        case 'd': return u3nc(0, 192);
        case 'g': return u3nc(0, 246);
        case 'l': return u3nc(0, 207);
        case 'n': return u3nc(0, 44);
        case 'p': return u3nc(0, 12);
        case 'r': return u3nc(0, 227);
        case 's': return u3nc(0, 3);
        case 't': return u3nc(0, 101);
        case 'x': return u3nc(0, 99);
        default: return 0;
      }
      case 'y': switch (three)  {
        case 'c': return u3nc(0, 59);
        case 'd': return u3nc(0, 54);
        case 'l': return u3nc(0, 14);
        case 'n': return u3nc(0, 76);
        case 't': return u3nc(0, 125);
        case 'x': return u3nc(0, 94);
        default: return 0;
      }
      default: return 0;
    }
    case 'z': switch (two) {
      case 'o': switch (three)  {
        case 'd': return u3nc(0, 0);
        default: return 0;
      }
      default: return 0;
    }
    default: return 0;
  }
}

void
u3_po_to_suffix(u3_noun id, c3_y* a, c3_y* b, c3_y* c)
{
  switch (id) {
    case 0: *a = 'z'; *b = 'o'; *c = 'd'; break;
    case 1: *a = 'n'; *b = 'e'; *c = 'c'; break;
    case 2: *a = 'b'; *b = 'u'; *c = 'd'; break;
    case 3: *a = 'w'; *b = 'e'; *c = 's'; break;
    case 4: *a = 's'; *b = 'e'; *c = 'v'; break;
    case 5: *a = 'p'; *b = 'e'; *c = 'r'; break;
    case 6: *a = 's'; *b = 'u'; *c = 't'; break;
    case 7: *a = 'l'; *b = 'e'; *c = 't'; break;
    case 8: *a = 'f'; *b = 'u'; *c = 'l'; break;
    case 9: *a = 'p'; *b = 'e'; *c = 'n'; break;
    case 10: *a = 's'; *b = 'y'; *c = 't'; break;
    case 11: *a = 'd'; *b = 'u'; *c = 'r'; break;
    case 12: *a = 'w'; *b = 'e'; *c = 'p'; break;
    case 13: *a = 's'; *b = 'e'; *c = 'r'; break;
    case 14: *a = 'w'; *b = 'y'; *c = 'l'; break;
    case 15: *a = 's'; *b = 'u'; *c = 'n'; break;
    case 16: *a = 'r'; *b = 'y'; *c = 'p'; break;
    case 17: *a = 's'; *b = 'y'; *c = 'x'; break;
    case 18: *a = 'd'; *b = 'y'; *c = 'r'; break;
    case 19: *a = 'n'; *b = 'u'; *c = 'p'; break;
    case 20: *a = 'h'; *b = 'e'; *c = 'b'; break;
    case 21: *a = 'p'; *b = 'e'; *c = 'g'; break;
    case 22: *a = 'l'; *b = 'u'; *c = 'p'; break;
    case 23: *a = 'd'; *b = 'e'; *c = 'p'; break;
    case 24: *a = 'd'; *b = 'y'; *c = 's'; break;
    case 25: *a = 'p'; *b = 'u'; *c = 't'; break;
    case 26: *a = 'l'; *b = 'u'; *c = 'g'; break;
    case 27: *a = 'h'; *b = 'e'; *c = 'c'; break;
    case 28: *a = 'r'; *b = 'y'; *c = 't'; break;
    case 29: *a = 't'; *b = 'y'; *c = 'v'; break;
    case 30: *a = 's'; *b = 'y'; *c = 'd'; break;
    case 31: *a = 'n'; *b = 'e'; *c = 'x'; break;
    case 32: *a = 'l'; *b = 'u'; *c = 'n'; break;
    case 33: *a = 'm'; *b = 'e'; *c = 'p'; break;
    case 34: *a = 'l'; *b = 'u'; *c = 't'; break;
    case 35: *a = 's'; *b = 'e'; *c = 'p'; break;
    case 36: *a = 'p'; *b = 'e'; *c = 's'; break;
    case 37: *a = 'd'; *b = 'e'; *c = 'l'; break;
    case 38: *a = 's'; *b = 'u'; *c = 'l'; break;
    case 39: *a = 'p'; *b = 'e'; *c = 'd'; break;
    case 40: *a = 't'; *b = 'e'; *c = 'm'; break;
    case 41: *a = 'l'; *b = 'e'; *c = 'd'; break;
    case 42: *a = 't'; *b = 'u'; *c = 'l'; break;
    case 43: *a = 'm'; *b = 'e'; *c = 't'; break;
    case 44: *a = 'w'; *b = 'e'; *c = 'n'; break;
    case 45: *a = 'b'; *b = 'y'; *c = 'n'; break;
    case 46: *a = 'h'; *b = 'e'; *c = 'x'; break;
    case 47: *a = 'f'; *b = 'e'; *c = 'b'; break;
    case 48: *a = 'p'; *b = 'y'; *c = 'l'; break;
    case 49: *a = 'd'; *b = 'u'; *c = 'l'; break;
    case 50: *a = 'h'; *b = 'e'; *c = 't'; break;
    case 51: *a = 'm'; *b = 'e'; *c = 'v'; break;
    case 52: *a = 'r'; *b = 'u'; *c = 't'; break;
    case 53: *a = 't'; *b = 'y'; *c = 'l'; break;
    case 54: *a = 'w'; *b = 'y'; *c = 'd'; break;
    case 55: *a = 't'; *b = 'e'; *c = 'p'; break;
    case 56: *a = 'b'; *b = 'e'; *c = 's'; break;
    case 57: *a = 'd'; *b = 'e'; *c = 'x'; break;
    case 58: *a = 's'; *b = 'e'; *c = 'f'; break;
    case 59: *a = 'w'; *b = 'y'; *c = 'c'; break;
    case 60: *a = 'b'; *b = 'u'; *c = 'r'; break;
    case 61: *a = 'd'; *b = 'e'; *c = 'r'; break;
    case 62: *a = 'n'; *b = 'e'; *c = 'p'; break;
    case 63: *a = 'p'; *b = 'u'; *c = 'r'; break;
    case 64: *a = 'r'; *b = 'y'; *c = 's'; break;
    case 65: *a = 'r'; *b = 'e'; *c = 'b'; break;
    case 66: *a = 'd'; *b = 'e'; *c = 'n'; break;
    case 67: *a = 'n'; *b = 'u'; *c = 't'; break;
    case 68: *a = 's'; *b = 'u'; *c = 'b'; break;
    case 69: *a = 'p'; *b = 'e'; *c = 't'; break;
    case 70: *a = 'r'; *b = 'u'; *c = 'l'; break;
    case 71: *a = 's'; *b = 'y'; *c = 'n'; break;
    case 72: *a = 'r'; *b = 'e'; *c = 'g'; break;
    case 73: *a = 't'; *b = 'y'; *c = 'd'; break;
    case 74: *a = 's'; *b = 'u'; *c = 'p'; break;
    case 75: *a = 's'; *b = 'e'; *c = 'm'; break;
    case 76: *a = 'w'; *b = 'y'; *c = 'n'; break;
    case 77: *a = 'r'; *b = 'e'; *c = 'c'; break;
    case 78: *a = 'm'; *b = 'e'; *c = 'g'; break;
    case 79: *a = 'n'; *b = 'e'; *c = 't'; break;
    case 80: *a = 's'; *b = 'e'; *c = 'c'; break;
    case 81: *a = 'm'; *b = 'u'; *c = 'l'; break;
    case 82: *a = 'n'; *b = 'y'; *c = 'm'; break;
    case 83: *a = 't'; *b = 'e'; *c = 'v'; break;
    case 84: *a = 'w'; *b = 'e'; *c = 'b'; break;
    case 85: *a = 's'; *b = 'u'; *c = 'm'; break;
    case 86: *a = 'm'; *b = 'u'; *c = 't'; break;
    case 87: *a = 'n'; *b = 'y'; *c = 'x'; break;
    case 88: *a = 'r'; *b = 'e'; *c = 'x'; break;
    case 89: *a = 't'; *b = 'e'; *c = 'b'; break;
    case 90: *a = 'f'; *b = 'u'; *c = 's'; break;
    case 91: *a = 'h'; *b = 'e'; *c = 'p'; break;
    case 92: *a = 'b'; *b = 'e'; *c = 'n'; break;
    case 93: *a = 'm'; *b = 'u'; *c = 's'; break;
    case 94: *a = 'w'; *b = 'y'; *c = 'x'; break;
    case 95: *a = 's'; *b = 'y'; *c = 'm'; break;
    case 96: *a = 's'; *b = 'e'; *c = 'l'; break;
    case 97: *a = 'r'; *b = 'u'; *c = 'c'; break;
    case 98: *a = 'd'; *b = 'e'; *c = 'c'; break;
    case 99: *a = 'w'; *b = 'e'; *c = 'x'; break;
    case 100: *a = 's'; *b = 'y'; *c = 'r'; break;
    case 101: *a = 'w'; *b = 'e'; *c = 't'; break;
    case 102: *a = 'd'; *b = 'y'; *c = 'l'; break;
    case 103: *a = 'm'; *b = 'y'; *c = 'n'; break;
    case 104: *a = 'm'; *b = 'e'; *c = 's'; break;
    case 105: *a = 'd'; *b = 'e'; *c = 't'; break;
    case 106: *a = 'b'; *b = 'e'; *c = 't'; break;
    case 107: *a = 'b'; *b = 'e'; *c = 'l'; break;
    case 108: *a = 't'; *b = 'u'; *c = 'x'; break;
    case 109: *a = 't'; *b = 'u'; *c = 'g'; break;
    case 110: *a = 'm'; *b = 'y'; *c = 'r'; break;
    case 111: *a = 'p'; *b = 'e'; *c = 'l'; break;
    case 112: *a = 's'; *b = 'y'; *c = 'p'; break;
    case 113: *a = 't'; *b = 'e'; *c = 'r'; break;
    case 114: *a = 'm'; *b = 'e'; *c = 'b'; break;
    case 115: *a = 's'; *b = 'e'; *c = 't'; break;
    case 116: *a = 'd'; *b = 'u'; *c = 't'; break;
    case 117: *a = 'd'; *b = 'e'; *c = 'g'; break;
    case 118: *a = 't'; *b = 'e'; *c = 'x'; break;
    case 119: *a = 's'; *b = 'u'; *c = 'r'; break;
    case 120: *a = 'f'; *b = 'e'; *c = 'l'; break;
    case 121: *a = 't'; *b = 'u'; *c = 'd'; break;
    case 122: *a = 'n'; *b = 'u'; *c = 'x'; break;
    case 123: *a = 'r'; *b = 'u'; *c = 'x'; break;
    case 124: *a = 'r'; *b = 'e'; *c = 'n'; break;
    case 125: *a = 'w'; *b = 'y'; *c = 't'; break;
    case 126: *a = 'n'; *b = 'u'; *c = 'b'; break;
    case 127: *a = 'm'; *b = 'e'; *c = 'd'; break;
    case 128: *a = 'l'; *b = 'y'; *c = 't'; break;
    case 129: *a = 'd'; *b = 'u'; *c = 's'; break;
    case 130: *a = 'n'; *b = 'e'; *c = 'b'; break;
    case 131: *a = 'r'; *b = 'u'; *c = 'm'; break;
    case 132: *a = 't'; *b = 'y'; *c = 'n'; break;
    case 133: *a = 's'; *b = 'e'; *c = 'g'; break;
    case 134: *a = 'l'; *b = 'y'; *c = 'x'; break;
    case 135: *a = 'p'; *b = 'u'; *c = 'n'; break;
    case 136: *a = 'r'; *b = 'e'; *c = 's'; break;
    case 137: *a = 'r'; *b = 'e'; *c = 'd'; break;
    case 138: *a = 'f'; *b = 'u'; *c = 'n'; break;
    case 139: *a = 'r'; *b = 'e'; *c = 'v'; break;
    case 140: *a = 'r'; *b = 'e'; *c = 'f'; break;
    case 141: *a = 'm'; *b = 'e'; *c = 'c'; break;
    case 142: *a = 't'; *b = 'e'; *c = 'd'; break;
    case 143: *a = 'r'; *b = 'u'; *c = 's'; break;
    case 144: *a = 'b'; *b = 'e'; *c = 'x'; break;
    case 145: *a = 'l'; *b = 'e'; *c = 'b'; break;
    case 146: *a = 'd'; *b = 'u'; *c = 'x'; break;
    case 147: *a = 'r'; *b = 'y'; *c = 'n'; break;
    case 148: *a = 'n'; *b = 'u'; *c = 'm'; break;
    case 149: *a = 'p'; *b = 'y'; *c = 'x'; break;
    case 150: *a = 'r'; *b = 'y'; *c = 'g'; break;
    case 151: *a = 'r'; *b = 'y'; *c = 'x'; break;
    case 152: *a = 'f'; *b = 'e'; *c = 'p'; break;
    case 153: *a = 't'; *b = 'y'; *c = 'r'; break;
    case 154: *a = 't'; *b = 'u'; *c = 's'; break;
    case 155: *a = 't'; *b = 'y'; *c = 'c'; break;
    case 156: *a = 'l'; *b = 'e'; *c = 'g'; break;
    case 157: *a = 'n'; *b = 'e'; *c = 'm'; break;
    case 158: *a = 'f'; *b = 'e'; *c = 'r'; break;
    case 159: *a = 'm'; *b = 'e'; *c = 'r'; break;
    case 160: *a = 't'; *b = 'e'; *c = 'n'; break;
    case 161: *a = 'l'; *b = 'u'; *c = 's'; break;
    case 162: *a = 'n'; *b = 'u'; *c = 's'; break;
    case 163: *a = 's'; *b = 'y'; *c = 'l'; break;
    case 164: *a = 't'; *b = 'e'; *c = 'c'; break;
    case 165: *a = 'm'; *b = 'e'; *c = 'x'; break;
    case 166: *a = 'p'; *b = 'u'; *c = 'b'; break;
    case 167: *a = 'r'; *b = 'y'; *c = 'm'; break;
    case 168: *a = 't'; *b = 'u'; *c = 'c'; break;
    case 169: *a = 'f'; *b = 'y'; *c = 'l'; break;
    case 170: *a = 'l'; *b = 'e'; *c = 'p'; break;
    case 171: *a = 'd'; *b = 'e'; *c = 'b'; break;
    case 172: *a = 'b'; *b = 'e'; *c = 'r'; break;
    case 173: *a = 'm'; *b = 'u'; *c = 'g'; break;
    case 174: *a = 'h'; *b = 'u'; *c = 't'; break;
    case 175: *a = 't'; *b = 'u'; *c = 'n'; break;
    case 176: *a = 'b'; *b = 'y'; *c = 'l'; break;
    case 177: *a = 's'; *b = 'u'; *c = 'd'; break;
    case 178: *a = 'p'; *b = 'e'; *c = 'm'; break;
    case 179: *a = 'd'; *b = 'e'; *c = 'v'; break;
    case 180: *a = 'l'; *b = 'u'; *c = 'r'; break;
    case 181: *a = 'd'; *b = 'e'; *c = 'f'; break;
    case 182: *a = 'b'; *b = 'u'; *c = 's'; break;
    case 183: *a = 'b'; *b = 'e'; *c = 'p'; break;
    case 184: *a = 'r'; *b = 'u'; *c = 'n'; break;
    case 185: *a = 'm'; *b = 'e'; *c = 'l'; break;
    case 186: *a = 'p'; *b = 'e'; *c = 'x'; break;
    case 187: *a = 'd'; *b = 'y'; *c = 't'; break;
    case 188: *a = 'b'; *b = 'y'; *c = 't'; break;
    case 189: *a = 't'; *b = 'y'; *c = 'p'; break;
    case 190: *a = 'l'; *b = 'e'; *c = 'v'; break;
    case 191: *a = 'm'; *b = 'y'; *c = 'l'; break;
    case 192: *a = 'w'; *b = 'e'; *c = 'd'; break;
    case 193: *a = 'd'; *b = 'u'; *c = 'c'; break;
    case 194: *a = 'f'; *b = 'u'; *c = 'r'; break;
    case 195: *a = 'f'; *b = 'e'; *c = 'x'; break;
    case 196: *a = 'n'; *b = 'u'; *c = 'l'; break;
    case 197: *a = 'l'; *b = 'u'; *c = 'c'; break;
    case 198: *a = 'l'; *b = 'e'; *c = 'n'; break;
    case 199: *a = 'n'; *b = 'e'; *c = 'r'; break;
    case 200: *a = 'l'; *b = 'e'; *c = 'x'; break;
    case 201: *a = 'r'; *b = 'u'; *c = 'p'; break;
    case 202: *a = 'n'; *b = 'e'; *c = 'd'; break;
    case 203: *a = 'l'; *b = 'e'; *c = 'c'; break;
    case 204: *a = 'r'; *b = 'y'; *c = 'd'; break;
    case 205: *a = 'l'; *b = 'y'; *c = 'd'; break;
    case 206: *a = 'f'; *b = 'e'; *c = 'n'; break;
    case 207: *a = 'w'; *b = 'e'; *c = 'l'; break;
    case 208: *a = 'n'; *b = 'y'; *c = 'd'; break;
    case 209: *a = 'h'; *b = 'u'; *c = 's'; break;
    case 210: *a = 'r'; *b = 'e'; *c = 'l'; break;
    case 211: *a = 'r'; *b = 'u'; *c = 'd'; break;
    case 212: *a = 'n'; *b = 'e'; *c = 's'; break;
    case 213: *a = 'h'; *b = 'e'; *c = 's'; break;
    case 214: *a = 'f'; *b = 'e'; *c = 't'; break;
    case 215: *a = 'd'; *b = 'e'; *c = 's'; break;
    case 216: *a = 'r'; *b = 'e'; *c = 't'; break;
    case 217: *a = 'd'; *b = 'u'; *c = 'n'; break;
    case 218: *a = 'l'; *b = 'e'; *c = 'r'; break;
    case 219: *a = 'n'; *b = 'y'; *c = 'r'; break;
    case 220: *a = 's'; *b = 'e'; *c = 'b'; break;
    case 221: *a = 'h'; *b = 'u'; *c = 'l'; break;
    case 222: *a = 'r'; *b = 'y'; *c = 'l'; break;
    case 223: *a = 'l'; *b = 'u'; *c = 'd'; break;
    case 224: *a = 'r'; *b = 'e'; *c = 'm'; break;
    case 225: *a = 'l'; *b = 'y'; *c = 's'; break;
    case 226: *a = 'f'; *b = 'y'; *c = 'n'; break;
    case 227: *a = 'w'; *b = 'e'; *c = 'r'; break;
    case 228: *a = 'r'; *b = 'y'; *c = 'c'; break;
    case 229: *a = 's'; *b = 'u'; *c = 'g'; break;
    case 230: *a = 'n'; *b = 'y'; *c = 's'; break;
    case 231: *a = 'n'; *b = 'y'; *c = 'l'; break;
    case 232: *a = 'l'; *b = 'y'; *c = 'n'; break;
    case 233: *a = 'd'; *b = 'y'; *c = 'n'; break;
    case 234: *a = 'd'; *b = 'e'; *c = 'm'; break;
    case 235: *a = 'l'; *b = 'u'; *c = 'x'; break;
    case 236: *a = 'f'; *b = 'e'; *c = 'd'; break;
    case 237: *a = 's'; *b = 'e'; *c = 'd'; break;
    case 238: *a = 'b'; *b = 'e'; *c = 'c'; break;
    case 239: *a = 'm'; *b = 'u'; *c = 'n'; break;
    case 240: *a = 'l'; *b = 'y'; *c = 'r'; break;
    case 241: *a = 't'; *b = 'e'; *c = 's'; break;
    case 242: *a = 'm'; *b = 'u'; *c = 'd'; break;
    case 243: *a = 'n'; *b = 'y'; *c = 't'; break;
    case 244: *a = 'b'; *b = 'y'; *c = 'r'; break;
    case 245: *a = 's'; *b = 'e'; *c = 'n'; break;
    case 246: *a = 'w'; *b = 'e'; *c = 'g'; break;
    case 247: *a = 'f'; *b = 'y'; *c = 'r'; break;
    case 248: *a = 'm'; *b = 'u'; *c = 'r'; break;
    case 249: *a = 't'; *b = 'e'; *c = 'l'; break;
    case 250: *a = 'r'; *b = 'e'; *c = 'p'; break;
    case 251: *a = 't'; *b = 'e'; *c = 'g'; break;
    case 252: *a = 'p'; *b = 'e'; *c = 'c'; break;
    case 253: *a = 'n'; *b = 'e'; *c = 'l'; break;
    case 254: *a = 'n'; *b = 'e'; *c = 'v'; break;
    case 255: *a = 'f'; *b = 'e'; *c = 's'; break;
    default: u3m_bail(c3__exit);
  }
}

u3_noun
u3qc_po_ins(u3_noun a)
{
  c3_y byt_y[3];
  u3r_bytes(0, 3, byt_y, a);

  return u3_po_find_prefix(byt_y[0], byt_y[1], byt_y[2]);
}

u3_noun
u3wcp_ins(u3_noun cor)
{
  u3_noun a;
  u3x_mean(cor, u3x_sam, &a, 0);

  if ( c3n == u3ud(a) ) {
    return u3m_bail(c3__fail);
  }

  return u3qc_po_ins(a);
}

u3_noun
u3qc_po_ind(u3_noun a)
{
  c3_y byt_y[3];
  u3r_bytes(0, 3, byt_y, a);

  return u3_po_find_suffix(byt_y[0], byt_y[1], byt_y[2]);
}

u3_noun
u3wcp_ind(u3_noun cor)
{
  u3_noun a;
  u3x_mean(cor, u3x_sam, &a, 0);

  if ( c3n == u3ud(a) ) {
    return u3m_bail(c3__fail);
  }

  return u3qc_po_ind(a);
}

u3_noun
u3wcp_tos(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
       (c3n == u3ud(a)) ||
       (a >= 256) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    c3_y byt_y[3];
    u3_po_to_prefix(a, &byt_y[0], &byt_y[1], &byt_y[2]);
    return (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
  }
}

u3_noun
u3wcp_tod(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
       (c3n == u3ud(a)) ||
       (a >= 256) )
  {
    return u3m_bail(c3__exit);
  } else {
    c3_y byt_y[3];
    u3_po_to_suffix(a, &byt_y[0], &byt_y[1], &byt_y[2]);
    return (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
  }
}
