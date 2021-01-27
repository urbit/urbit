import { AppName, Path, PatpNoSig, Poke } from "..";
import { Association, Metadata, MetadataUpdateAdd, MetadataUpdateUpdate } from './index.d';

export const action = <T>(data: T): Poke<T> => ({
  app: 'metadata-hook',
  mark: 'metadata-action',
  json: data
});

export const add = (
  ship: PatpNoSig,
  appName: AppName,
  appPath: Path,
  groupPath: Path,
  title: string,
  description: string,
  dateCreated: string,
  color: string,
  moduleName: string
): Poke<MetadataUpdateAdd> => {
  const creator = `~${ship}`;
  return action({
    add: {
      'group-path': groupPath,
      resource: {
        'app-path': appPath,
        'app-name': appName
      },
      metadata: {
        title,
        description,
        color,
        'date-created': dateCreated,
        creator,
        'module': moduleName
      }
    }
  });
}

export const update = (
  association: Association,
  newMetadata: Partial<Metadata>
): Poke<MetadataUpdateUpdate> => {
  return action({ 
    add: {
      'group-path': association['group-path'], 
      resource: {
        'app-path': association['app-path'],
        'app-name': association['app-name'],
      },
      metadata: {...association.metadata, ...newMetadata }
    }
  });
}