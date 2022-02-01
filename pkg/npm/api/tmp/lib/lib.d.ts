import { BigInteger } from "big-integer";
import { Resource } from "../groups/types";
import { Post, GraphNode } from "../graph/types";
/**
 * Given a bigint representing an urbit date, returns a unix timestamp.
 *
 * @param   {BigInteger}  da  The urbit date
 *
 * @return  {number}          The unix timestamp
 */
export declare function daToUnix(da: BigInteger): number;
/**
 * Given a unix timestamp, returns a bigint representing an urbit date
 *
 * @param   {number}      unix  The unix timestamp
 *
 * @return  {BigInteger}        The urbit date
 */
export declare function unixToDa(unix: number): BigInteger;
export declare function makePatDa(patda: string): BigInteger;
export declare function udToDec(ud: string): string;
export declare function decToUd(str: string): string;
export declare function resourceAsPath(resource: Resource): string;
export declare function uuid(): string;
export declare function daToDate(st: string): Date;
export declare function dateToDa(d: Date, mil?: boolean): string;
export declare function deSig(ship: string): string | null;
export declare function cite(ship: string): string;
export declare function uxToHex(ux: string): string;
export declare const hexToUx: (hex: string) => string;
export declare function stringToTa(str: string): string;
export declare const buntPost: () => Post;
export declare function makeNodeMap(posts: Post[]): Record<string, GraphNode>;
