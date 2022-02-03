import { Poke, Scry } from '../lib';
import { Chad } from './types';
export declare function chadIsRunning(chad: Chad): boolean;
export declare const scryCharges: Scry;
export declare const scryDockets: Scry;
export declare const scryTreaties: Scry;
export declare const scryDefaultAlly: Scry;
export declare const scryAllies: Scry;
export declare const scryAllyTreaties: (ship: string) => Scry;
/**
 * Uninstall a desk, and remove docket
 */
export declare function docketUninstall(desk: string): Poke<string>;
export declare function docketInstall(ship: string, desk: string): Poke<any>;
export declare function allyShip(ship: string): Poke<any>;
