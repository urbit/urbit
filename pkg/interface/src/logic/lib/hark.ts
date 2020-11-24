import bigInt, { BigInteger } from "big-integer";
import f from "lodash/fp";
import { Unreads } from "~/types";

export function getLastSeen(
  unreads: Unreads,
  path: string,
  index: string
): BigInteger | undefined {
  const lastSeenIdx = unreads.graph?.[path]?.[index]?.unreads;
  if (!(typeof lastSeenIdx === "string")) {
    return bigInt.zero;
  }
  return f.flow(f.split("/"), f.last, (x) => (!!x ? bigInt(x) : undefined))(
    lastSeenIdx
  );
}
