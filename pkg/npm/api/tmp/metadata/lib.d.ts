import { Path, Poke, PatpNoSig } from '../lib';
import { MdAppName, Association, Metadata, MetadataUpdate, MetadataUpdateAdd, MetadataUpdateRemove, MetadataEditField, MetadataUpdateEdit } from './types';
export declare const METADATA_UPDATE_VERSION = 2;
export declare const metadataAction: <T extends MetadataUpdate>(data: T, version?: number) => Poke<T>;
export declare const add: (ship: PatpNoSig, appName: MdAppName, resource: Path, group: Path, title: string, description: string, dateCreated: string, color: string, moduleName: string) => Poke<MetadataUpdateAdd>;
export { add as metadataAdd };
export declare const remove: (appName: MdAppName, resource: string, group: string) => Poke<MetadataUpdateRemove>;
export { remove as metadataRemove };
export declare const edit: (association: Association, edit: MetadataEditField) => Poke<MetadataUpdateEdit>;
export { edit as metadataEdit };
/**
 * @deprecated use {@link edit} instead
 */
export declare const update: (association: Association, newMetadata: Partial<Metadata>) => Poke<MetadataUpdateAdd>;
export { update as metadataUpdate };
