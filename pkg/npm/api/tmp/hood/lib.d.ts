import { Poke, Scry } from '../lib';
import { Vats, Vat } from './types';
export declare const getVats: Scry;
/**
 * Install a foreign desk
 */
export declare function kilnInstall(ship: string, desk: string, local?: string): Poke<any>;
/**
 * Uninstall a desk
 */
export declare function kilnUninstall(desk: string): Poke<any>;
export declare function kilnSuspend(desk: string): Poke<any>;
export declare function kilnRevive(desk: string): Poke<any>;
export declare function kilnBump(force?: boolean, except?: string[]): {
    app: string;
    mark: string;
    json: {
        force: boolean;
        except: string[];
    };
};
export declare function kilnPause(desk: string): {
    app: string;
    mark: string;
    json: string;
};
export declare function kilnResume(desk: string): {
    app: string;
    mark: string;
    json: string;
};
export declare const scryLag: Scry;
export declare function getBlockers(vats: Vats): string[];
export declare function getVatPublisher(vat: Vat): string | undefined;
