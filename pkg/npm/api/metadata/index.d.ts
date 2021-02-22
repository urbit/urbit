import { AppName, Path, Patp } from '..';

export type MetadataUpdate =
  MetadataUpdateInitial
| MetadataUpdateAdd
| MetadataUpdateUpdate
| MetadataUpdateRemove;

interface MetadataUpdateInitial {
  associations: ResourceAssociations;
}

type ResourceAssociations = {
  [p in Path]: Association;
}

type MetadataUpdateAdd = {
  add: AssociationPoke;
}

type MetadataUpdateUpdate = {
  update: AssociationPoke;
}

type MetadataUpdateRemove = {
  remove: MdResource & {
    group: Path;
  }
}

export interface MetadataUpdatePreview {
  group: string;
  channels: Associations;
  "channel-count": number;
  members: number;
  metadata: Metadata;
}

export type Associations = Record<AppName, AppAssociations>;

export type AppAssociations = {
  [p in Path]: Association;
}

interface MdResource {
  resource: Path;
  'app-name': AppName;
}

export type Association = MdResource & {
  group: Path;
  metadata: Metadata;
};

export interface AssociationPoke {
  group: Path;
  resource: MdResource;
  metadata: Metadata;
}

export interface Metadata {
  color: string;
  creator: Patp;
  'date-created': string;
  description: string;
  title: string;
  module: string;
  picture: string;
  preview: boolean;
  vip: PermVariation;
}

export type PermVariation = '' | 'reader-comments' | 'member-metadata';
