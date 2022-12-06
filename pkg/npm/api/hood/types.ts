
/**
 * A pending commit, awaiting a future kelvin version
 */
interface Woof {
  aeon: number;
  weft: Weft;
}

interface Rein {
  /**
   * Agents not in manifest that should be running
   */
  add: string[];
  /**
   * Agents in manifest that should not be running
   */
  sub: string[];
}

export interface Rail {
  /**
   * Original publisher of desk, if available
   */
  publisher: string | null;
  /**
   * Ship of foreign vat
   */
  ship: string;
  /**
   * Desk of foreign vat
   */
  desk: string;
  /**
   * Aeon (version number) that we currently have synced
   */
  aeon: number;
  next: Woof[];
  paused: boolean;
}

/**
 * A tracker of a foreign {@link Vat}
 *
 */
export interface Arak {
  rein: Rein;
  rail: Rail | null;
}

/**
 * A component's kelvin version
 */
export interface Weft {
  /**
   *  Name of the component
   *
   *  @remarks
   *  Usually %zuse, %hoon, or %lull
   */
  name: string;
  /**
   * Kelvin version
   *
   */
  kelvin: number;
}

export interface KilnDiffBlock {
  block: {
    desk: string;
    arak: Arak;
    weft: Weft;
    blockers: string[];
  };
}

export interface KilnDiffReset {
  reset: {
    desk: string;
    arak: Arak;
  };
}

export interface KilnDiffMerge {
  merge: {
    desk: string;
    arak: Arak;
  };
}

export interface KilnDiffMergeSunk {
  'merge-sunk': {
    desk: string;
    arak: Arak;
    tang: string;
  };
}

export interface KilnDiffMergeFail {
  'merge-fail': {
    desk: string;
    arak: Arak;
    tang: string;
  };
}

export type KilnDiff =
  | KilnDiffBlock
  | KilnDiffReset
  | KilnDiffMerge
  | KilnDiffMergeSunk
  | KilnDiffMergeFail;

/**
 * Cases for revision
 *
 */
export interface Cass {
  /**
   * Revision number
   */
  ud: number;
  /**
   * Timestamp of revision, as stringifed `@da`
   *
   * @remarks
   * If \@da is outside valid positive unix timestamp, value will be zero
   */
  da: string;
}

/**
 * A local desk installation
 */
export interface Vat {
  /**
   * Desk that this Vat describes
   */
  desk: string;
  /**
   * Hash of the desk, rendered as `@uv`
   *
   * @remarks
   * Equivalent to
   * ```hoon
   * .^(@uv %cz /=desk=)
   * ```
   */
  hash: string;
  /**
   * Current revision
   */
  cass: Cass;
  /**
   * Foreign sync
   */
  arak: Arak;
}

export interface Vats {
  [desk: string]: Vat;
}
/**
 * TODO: crisp one-liner describing a Pike
 */
export interface Pike {
  /**
   * Hash of the desk, rendered as `@uv`
   *
   * @remarks
   * Equivalent to
   * ```hoon
   * .^(@uv %cz /=desk=)
   * ```
   */
  hash: string;
  sync: {
    /**
     * Source desk for this Pike
     */
    desk: string;
    /**
     * Source ship for this Pike
     */
    ship: string;
  } | null;
  /**
   *  {@link Weft}s associated with this Pike
   */
  wefts: Weft[];
  /**
   * how live is this pike?
   * 
   * live - app is running
   * held - app is not running, but is trying to run. this state can be entered
   * in two main ways: 
   *   - when installing an app but it hasn't finished downloading (or it did 
   *     but failed to install for some reason)
   *   - when user forced a kelvin upgrade by suspending desks.
   * dead - app is not running
   */
  zest: "live" | "dead" | "held";
}

export interface Pikes {
  [desk: string]: Pike;
}
