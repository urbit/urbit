import { AppName, Path, PatpNoSig, Poke } from "..";
import { Association, Metadata, MetadataUpdateAdd, MetadataUpdateUpdate } from './index.d';

export const action = <T>(data: T): Poke<T> => ({
  app: 'metadata-hook',
  mark: 'metadata-action',
  json: data
});

export const add = (
  appName: AppName,
  resource: string,
  group: string,
  metadata: Metadata,
): Poke<MetadataUpdateAdd> => {
  return action({
    add: {
      group,
      resource: {
        resource,
        'app-name': appName
      },
      metadata
    }
  });
}

export const update = (
  association: Association,
  newMetadata: Partial<Metadata>
): Poke<MetadataUpdateAdd> => {
  const { resource, metadata, group } = association;
  return action({ 
    add: {
      group,
      resource: {
        resource,
        'app-name': association['app-name'],
      },
      metadata: {...metadata, ...newMetadata }
    }
  });
}
