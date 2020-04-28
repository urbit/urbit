/* j/3/po.c
**
*/
#include "all.h"

u3_noun
po_find_prefix(char one, char two, char three) {
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

u3_noun
po_find_suffix(char one, char two, char three) {
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


  //  good old linear search
  //
  static u3_noun
  _po_find(u3_noun buf,
           u3_noun a)
  {
    if ( !_(u3a_is_cat(a)) ) {
      return u3_nul;
    }
    else {
      c3_w i_w;
      c3_w a_w = a;

      for ( i_w = 0; i_w < 256; i_w++ ) {
        c3_y byt_y[3];
        c3_w but_w;

        u3r_bytes((i_w * 3), 3, byt_y, buf);
        but_w = (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));

        if ( but_w == a_w ) {
          return u3nc(u3_nul, i_w);
        }
      }
      return u3_nul;
    }
  }

u3_noun
u3qc_po_ins(u3_noun a)
{
  c3_y byt_y[3];
  u3r_bytes(0, 3, byt_y, a);

  return po_find_prefix(byt_y[0], byt_y[1], byt_y[2]);
}

u3_noun
u3wcp_ins(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
       (c3n == u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qc_po_ins(a);
  }
}

u3_noun
u3qc_po_ind(u3_noun a)
{
  c3_y byt_y[3];
  u3r_bytes(0, 3, byt_y, a);

  return po_find_suffix(byt_y[0], byt_y[1], byt_y[2]);
}

u3_noun
u3wcp_ind(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
       (c3n == u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qc_po_ind(a);
  }
}

  u3_noun
  u3wcp_tos(u3_noun cor)
  {
    u3_noun x, a, buf;

    if ( (c3n == u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
         (c3n == u3du(x)) ||
         (c3n == u3ud(buf = u3h(x))) ||
         (c3n == u3ud(a)) ||
         (a >= 256) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      c3_y byt_y[3];

      u3r_bytes((a * 3), 3, byt_y, buf);
      return (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
    }
  }
  u3_noun
  u3wcp_tod(u3_noun cor)
  {
    u3_noun x, a, buf;

    if ( (c3n == u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
         (c3n == u3du(x)) ||
         (c3n == u3ud(buf = u3t(x))) ||
         (c3n == u3ud(a)) ||
         (a >= 256) )
    {
      return u3m_bail(c3__exit);
    } else {
      c3_y byt_y[3];

      u3r_bytes((a * 3), 3, byt_y, buf);
      return (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
    }
  }
