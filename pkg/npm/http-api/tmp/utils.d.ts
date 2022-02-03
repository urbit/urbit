export declare function camelize(str: string): string;
export declare function uncamelize(str: string, separator?: string): string;
/**
 * Returns a hex string of given length.
 *
 * Poached from StackOverflow.
 *
 * @param len Length of hex string to return.
 */
export declare function hexString(len: number): string;
/**
 * Generates a random UID.
 *
 * Copied from https://github.com/urbit/urbit/blob/137e4428f617c13f28ed31e520eff98d251ed3e9/pkg/interface/src/lib/util.js#L3
 */
export declare function uid(): string;
