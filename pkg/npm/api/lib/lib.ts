import _ from "lodash";
import f from "lodash/fp";
import bigInt, { BigInteger } from "big-integer";

import { Resource } from "../groups/types";
import { Post, GraphNode } from "../graph/types";

const DA_UNIX_EPOCH = bigInt("170141184475152167957503069145530368000"); // `@ud` ~1970.1.1

const DA_SECOND = bigInt("18446744073709551616"); // `@ud` ~s1

/**
 * Given a bigint representing an urbit date, returns a unix timestamp.
 *
 * @param   {BigInteger}  da  The urbit date
 *
 * @return  {number}          The unix timestamp
 */
export function daToUnix(da: BigInteger): number {
  // ported from +time:enjs:format in hoon.hoon
  const offset = DA_SECOND.divide(bigInt(2000));
  const epochAdjusted = offset.add(da.subtract(DA_UNIX_EPOCH));

  return Math.round(
    epochAdjusted.multiply(bigInt(1000)).divide(DA_SECOND).toJSNumber()
  );
}

/**
 * Given a unix timestamp, returns a bigint representing an urbit date
 *
 * @param   {number}      unix  The unix timestamp
 *
 * @return  {BigInteger}        The urbit date
 */
export function unixToDa(unix: number): BigInteger {
  const timeSinceEpoch =  bigInt(unix).multiply(DA_SECOND).divide(bigInt(1000));
  return DA_UNIX_EPOCH.add(timeSinceEpoch);
}


export function makePatDa(patda: string): BigInteger {
  return bigInt(udToDec(patda));
}

export function udToDec(ud: string): string {
  return ud.replace(/\./g, "");
}

export function decToUd(str: string): string {
  return _.trimStart(
    f.flow(
      f.split(""),
      f.reverse,
      f.chunk(3),
      f.map(f.flow(f.reverse, f.join(""))),
      f.reverse,
      f.join(".")
    )(str),
    "0."
  );
}

export function resourceAsPath(resource: Resource): string {
  const { name, ship } = resource;
  return `/ship/~${ship}/${name}`;
}

export function uuid(): string {
  let str = "0v";
  str += Math.ceil(Math.random() * 8) + ".";
  for (let i = 0; i < 5; i++) {
    let _str = Math.ceil(Math.random() * 10000000).toString(32);
    _str = ("00000" + _str).substr(-5, 5);
    str += _str + ".";
  }

  return str.slice(0, -1);
}

/*
  Goes from:
    ~2018.7.17..23.15.09..5be5    // urbit @da
  To:
    (javascript Date object)
*/
export function daToDate(st: string): Date {
  const dub = function (n: string) {
    return parseInt(n) < 10 ? "0" + parseInt(n) : n.toString();
  };
  const da = st.split("..");
  const bigEnd = da[0].split(".");
  const lilEnd = da[1].split(".");
  const ds = `${bigEnd[0].slice(1)}-${dub(bigEnd[1])}-${dub(bigEnd[2])}T${dub(
    lilEnd[0]
  )}:${dub(lilEnd[1])}:${dub(lilEnd[2])}Z`;
  return new Date(ds);
}

/*
  Goes from:
    (javascript Date object)
  To:
    ~2018.7.17..23.15.09..5be5    // urbit @da
*/

export function dateToDa(d: Date, mil: boolean = false): string {
  const fil = function (n: number) {
    return n >= 10 ? n : "0" + n;
  };
  return (
    `~${d.getUTCFullYear()}.` +
    `${d.getUTCMonth() + 1}.` +
    `${fil(d.getUTCDate())}..` +
    `${fil(d.getUTCHours())}.` +
    `${fil(d.getUTCMinutes())}.` +
    `${fil(d.getUTCSeconds())}` +
    `${mil ? "..0000" : ""}`
  );
}

export function deSig(ship: string): string | null {
  if (!ship) {
    return null;
  }
  return ship.replace("~", "");
}

// trim patps to match dojo, chat-cli
export function cite(ship: string) {
  let patp = ship,
    shortened = '';
  if (patp === null || patp === '') {
    return null;
  }
  if (patp.startsWith('~')) {
    patp = patp.substr(1);
  }
  // comet
  if (patp.length === 56) {
    shortened = '~' + patp.slice(0, 6) + '_' + patp.slice(50, 56);
    return shortened;
  }
  // moon
  if (patp.length === 27) {
    shortened = '~' + patp.slice(14, 20) + '^' + patp.slice(21, 27);
    return shortened;
  }
  return `~${patp}`;
}


export function uxToHex(ux: string) {
  if (ux.length > 2 && ux.substr(0, 2) === '0x') {
    const value = ux.substr(2).replace('.', '').padStart(6, '0');
    return value;
  }

  const value = ux.replace('.', '').padStart(6, '0');
  return value;
}

export const hexToUx = (hex: string): string => {
  const ux = f.flow(
    f.chunk(4),
    f.map(x => _.dropWhile(x, (y: unknown) => y === 0).join('')),
    f.join('.')
  )(hex.split(''));
  return `0x${ux}`;
};


// encode the string into @ta-safe format, using logic from +wood.
// for example, 'some Chars!' becomes '~.some.~43.hars~21.'
//
export function stringToTa(str: string): string {
  let out = "";
  for (let i = 0; i < str.length; i++) {
    const char = str[i];
    let add = "";
    switch (char) {
      case " ":
        add = ".";
        break;
      case ".":
        add = "~.";
        break;
      case "~":
        add = "~~";
        break;
      default:
        const charCode = str.charCodeAt(i);
        if (
          (charCode >= 97 && charCode <= 122) || // a-z
          (charCode >= 48 && charCode <= 57) || // 0-9
          char === "-"
        ) {
          add = char;
        } else {
          // TODO behavior for unicode doesn't match +wood's,
          //     but we can probably get away with that for now.
          add = "~" + charCode.toString(16) + ".";
        }
    }
    out = out + add;
  }
  return "~." + out;
}


/**
 * Formats a numbers as a `@ud` inserting dot where needed
 */
export function numToUd(num: number): string {
  return f.flow(
    f.split(''),
    f.reverse,
    f.chunk(3),
    f.reverse,
    f.map(s => s.join('')),
    f.join('.')
  )(num.toString())
}

export const buntPost = (): Post => ({
  author: '',
  contents: [],
  hash: null,
  index: '',
  signatures: [],
  'time-sent': 0
});

export function makeNodeMap(posts: Post[]): Record<string, GraphNode> {
  const nodes: Record<string, GraphNode> = {};
  posts.forEach((p: Post) => {
    nodes[String(p.index)] = { children: null, post: p };
  });
  return nodes;
}
