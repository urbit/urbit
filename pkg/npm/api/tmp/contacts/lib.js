export const CONTACT_UPDATE_VERSION = 0;
const storeAction = (data, version = CONTACT_UPDATE_VERSION) => ({
    app: 'contact-store',
    mark: `contact-update-${version}`,
    json: data
});
export { storeAction as contactStoreAction };
export const addContact = (ship, contact) => {
    contact['last-updated'] = Date.now();
    return storeAction({
        add: { ship, contact }
    });
};
export const removeContact = (ship) => storeAction({
    remove: { ship }
});
export const share = (recipient, version = CONTACT_UPDATE_VERSION) => ({
    app: 'contact-push-hook',
    mark: 'contact-share',
    json: { share: recipient }
});
export const editContact = (ship, editField) => storeAction({
    edit: {
        ship,
        'edit-field': editField,
        timestamp: Date.now()
    }
});
export const allowShips = (ships) => storeAction({
    allow: {
        ships
    }
});
export const allowGroup = (ship, name) => storeAction({
    allow: {
        group: { ship, name }
    }
});
export const setPublic = (setPublic) => {
    return storeAction({
        'set-public': setPublic
    });
};
export const retrieve = (ship) => {
    const resource = { ship, name: '' };
    return {
        app: 'contact-pull-hook',
        mark: 'pull-hook-action',
        json: {
            add: {
                resource,
                ship
            }
        }
    };
};
export const fetchIsAllowed = (entity, name, ship, personal) => {
    const isPersonal = personal ? 'true' : 'false';
    return {
        app: 'contact-store',
        path: `/is-allowed/${entity}/${name}/${ship}/${isPersonal}`
    };
};
//# sourceMappingURL=lib.js.map