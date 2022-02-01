import { Patp, Poke, Scry } from '../lib';
import { Contact, ContactUpdateAdd, ContactUpdateEdit, ContactUpdateRemove, ContactEditField, ContactShare, ContactUpdate, ContactUpdateAllowShips, ContactUpdateAllowGroup, ContactUpdateSetPublic } from './types';
export declare const CONTACT_UPDATE_VERSION = 0;
declare const storeAction: <T extends ContactUpdate>(data: T, version?: number) => Poke<T>;
export { storeAction as contactStoreAction };
export declare const addContact: (ship: Patp, contact: Contact) => Poke<ContactUpdateAdd>;
export declare const removeContact: (ship: Patp) => Poke<ContactUpdateRemove>;
export declare const share: (recipient: Patp, version?: number) => Poke<ContactShare>;
export declare const editContact: (ship: Patp, editField: ContactEditField) => Poke<ContactUpdateEdit>;
export declare const allowShips: (ships: Patp[]) => Poke<ContactUpdateAllowShips>;
export declare const allowGroup: (ship: string, name: string) => Poke<ContactUpdateAllowGroup>;
export declare const setPublic: (setPublic: any) => Poke<ContactUpdateSetPublic>;
export declare const retrieve: (ship: string) => {
    app: string;
    mark: string;
    json: {
        add: {
            resource: {
                ship: string;
                name: string;
            };
            ship: string;
        };
    };
};
export declare const fetchIsAllowed: (entity: string, name: string, ship: string, personal: boolean) => Scry;
