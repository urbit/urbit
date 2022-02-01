import { Path, Patp } from '../lib';
export declare type MdAppName = 'groups' | 'graph';
export declare type MetadataUpdate = MetadataUpdateInitial | MetadataUpdateAdd | MetadataUpdateUpdate | MetadataUpdateRemove | MetadataUpdateEdit;
export interface MetadataUpdateInitial {
    associations: ResourceAssociations;
}
export declare type ResourceAssociations = {
    [p in Path]: Association;
};
export declare type MetadataUpdateAdd = {
    add: AssociationPoke;
};
export declare type MetadataUpdateUpdate = {
    update: AssociationPoke;
};
export interface MetadataUpdateEdit {
    edit: {
        resource: MdResource;
        group: string;
        edit: MetadataEditField;
    };
}
export declare type MetadataEditField = Partial<Omit<Metadata, 'config' | 'creator' | 'date-created'>>;
export declare type MetadataUpdateRemove = {
    remove: {
        resource: MdResource;
        group: string;
    };
};
export interface MdResource {
    resource: string;
    'app-name': MdAppName;
}
export interface MetadataUpdatePreview {
    group: string;
    channels: Associations;
    'channel-count': number;
    members: number;
    metadata: Metadata;
}
export declare type Associations = {
    groups: AppAssociations<GroupConfig>;
    graph: AppAssociations<GraphConfig>;
};
export declare type AppAssociations<C = MetadataConfig> = {
    [p in Path]: Association<C>;
};
export declare type Association<C = MetadataConfig> = MdResource & {
    group: Path;
    metadata: Metadata<C>;
};
export interface AssociationPoke {
    group: Path;
    resource: MdResource;
    metadata: Metadata;
}
export interface Metadata<C = MetadataConfig> {
    color: string;
    creator: Patp;
    'date-created': string;
    description: string;
    title: string;
    config: C;
    hidden: boolean;
    picture: string;
    preview: boolean;
    vip: PermVariation;
}
export declare type MetadataConfig = GroupConfig | GraphConfig;
export interface GraphConfig {
    graph: string;
}
export interface GroupConfig {
    group: undefined | {} | MdResource;
}
export declare type PermVariation = '' | ' ' | 'reader-comments' | 'member-metadata' | 'host-feed' | 'admin-feed';
