import { AppName, Path, Patp } from '..';


export type MetadataUpdate =
  MetadataUpdateInitial
| MetadataUpdateAdd
| MetadataUpdateUpdate
| MetadataUpdateRemove;

export interface MetadataUpdateInitial {
  associations: ResourceAssociations;
}

export type ResourceAssociations = {
  [p in Path]: Association;
}

export type MetadataUpdateAdd = {
  add: Association;
}

export type MetadataUpdateUpdate = {
  update: Association;
}

export type MetadataUpdateRemove = {
  remove: Resource & {
    'group-path': Path;
  }
}

export type Associations = Record<AppName, AppAssociations>;

export type AppAssociations = {
  [p in Path]: Association;
}

export interface Resource {
  'app-path': Path;
  'app-name': AppName;
}

export type Association = Resource & {
  'group-path': Path;
  metadata: Metadata;
};

export interface Metadata {
  color: string;
  creator: Patp;
  'date-created': string;
  description: string;
  title: string;
  module: string;
}
