import { uxToHex } from '../lib';
export const METADATA_UPDATE_VERSION = 2;
export const metadataAction = (data, version = METADATA_UPDATE_VERSION) => ({
    app: 'metadata-push-hook',
    mark: `metadata-update-${version}`,
    json: data
});
export const add = (ship, appName, resource, group, title, description, dateCreated, color, moduleName) => metadataAction({
    add: {
        group,
        resource: {
            resource,
            'app-name': appName
        },
        metadata: {
            title,
            description,
            color,
            'date-created': dateCreated,
            creator: `~${ship}`,
            config: { graph: moduleName },
            picture: '',
            hidden: false,
            preview: false,
            vip: ''
        }
    }
});
export { add as metadataAdd };
export const remove = (appName, resource, group) => metadataAction({
    remove: {
        group,
        resource: {
            resource,
            'app-name': appName
        }
    }
});
export { remove as metadataRemove };
export const edit = (association, edit) => metadataAction({
    edit: {
        group: association.group,
        resource: {
            resource: association.resource,
            'app-name': association['app-name']
        },
        edit
    }
});
export { edit as metadataEdit };
/**
 * @deprecated use {@link edit} instead
 */
export const update = (association, newMetadata) => {
    const metadata = { ...association.metadata, ...newMetadata };
    metadata.color = uxToHex(metadata.color);
    return metadataAction({
        add: {
            group: association.group,
            resource: {
                resource: association.resource,
                'app-name': association['app-name']
            },
            metadata
        }
    });
};
export { update as metadataUpdate };
//# sourceMappingURL=lib.js.map