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
    'group-path': Path;
  }
}

export type Associations = Record<AppName, AppAssociations>;

export type AppAssociations = {
  [p in Path]: Association;
}

interface Resource {
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
