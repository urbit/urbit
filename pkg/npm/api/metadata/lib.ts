import { Path, Poke, uxToHex, PatpNoSig } from '../lib';
import { MdAppName, Association, Metadata, MetadataUpdate, MetadataUpdateAdd, MetadataUpdateRemove, MetadataEditField, MetadataUpdateEdit } from './types';

export const METADATA_UPDATE_VERSION = 2;

export const metadataAction = <T extends MetadataUpdate>(data: T, version: number = METADATA_UPDATE_VERSION): Poke<T> => ({
  app: 'metadata-push-hook',
  mark: `metadata-update-${version}`,
  json: data
});

export const add = (
  ship: PatpNoSig,
  appName: MdAppName,
  resource: Path,
  group: Path,
  title: string,
  description: string,
  dateCreated: string,
  color: string,
  moduleName: string
): Poke<MetadataUpdateAdd> => metadataAction({
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

export const remove = (
  appName: MdAppName,
  resource: string,
  group: string
): Poke<MetadataUpdateRemove> => metadataAction<MetadataUpdateRemove>({
  remove: {
    group,
    resource: {
      resource,
      'app-name': appName
    }
  }
});

export { remove as metadataRemove };

export const edit = (
  association: Association,
  edit: MetadataEditField
): Poke<MetadataUpdateEdit> => metadataAction<MetadataUpdateEdit>({
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
export const update = (
  association: Association,
  newMetadata: Partial<Metadata>
): Poke<MetadataUpdateAdd> => {
  const metadata = { ...association.metadata, ...newMetadata };
    metadata.color = uxToHex(metadata.color);
    return metadataAction<MetadataUpdateAdd>({
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
