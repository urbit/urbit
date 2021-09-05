import { Path, Patp } from '../lib';

export type MdAppName = 'groups' | 'graph';

export type MetadataUpdate =
  MetadataUpdateInitial
| MetadataUpdateAdd
| MetadataUpdateUpdate
| MetadataUpdateRemove
| MetadataUpdateEdit;

export interface MetadataUpdateInitial {
  associations: ResourceAssociations;
}

export type ResourceAssociations = {
  [p in Path]: Association;
}

export type MetadataUpdateAdd = {
  add: AssociationPoke;
}

export type MetadataUpdateUpdate = {
  update: AssociationPoke;
}

export interface MetadataUpdateEdit {
  edit: {
    resource: MdResource;
    group: string;
    edit: MetadataEditField;
  }
}

export type MetadataEditField = Partial<Omit<Metadata, 'config' | 'creator' | 'date-created'>>;

export type MetadataUpdateRemove = {
  remove: {
    resource: MdResource;
    group: string;
  }
}

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

export type Associations = {
  groups: AppAssociations<GroupConfig>
  graph: AppAssociations<GraphConfig>;
}

export type AppAssociations<C = MetadataConfig> = {
  [p in Path]: Association<C>;
}

export type Association<C = MetadataConfig> = MdResource & {
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

export type MetadataConfig = GroupConfig | GraphConfig;

export interface GraphConfig {
  graph: string;
}

export interface GroupConfig {
  group: undefined | {} | MdResource;
}

export type PermVariation = '' | ' ' | 'reader-comments' | 'member-metadata' | 'host-feed' | 'admin-feed';
