import { AppName, Path, Patp } from './noun';


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
  add: Association;
}

type MetadataUpdateUpdate = {
  update: Association;
}

type MetadataUpdateRemove = {
  remove: Resource & {
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

interface Resource {
  resource: Path;
  'app-name': AppName;
}

export type Association = Resource & {
  group: Path;
  metadata: Metadata;
};

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
