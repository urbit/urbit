import { Resource } from "..";
import { AppName, Path, Patp } from "../lib";

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
  add: AssociationPoke;
}

export type MetadataUpdateUpdate = {
  update: AssociationPoke;
}

export type MetadataUpdateRemove = {
  remove: {
    resource: MdResource;
    group: string;
  }
}

export interface MdResource {
  resource: string;
  app: AppName;
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
  config: MetadataConfig;
  picture: string;
  hidden: boolean;
  preview: boolean;
  vip: PermVariation;
}

type MetadataConfig = GroupConfig | GraphConfig;

interface GroupConfig {
  group: null | {} | Resource;
}
interface GraphConfig {
  graph: string;
}

export type PermVariation = '' | 'reader-comments' | 'member-metadata' | 'host-feed' | 'admin-feed';
