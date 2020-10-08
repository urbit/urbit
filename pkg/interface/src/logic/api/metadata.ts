
import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path, Patp, Association, Metadata } from '~/types';

export default class MetadataApi extends BaseApi<StoreState> {


  metadataAdd(appName: string, appPath: Path, groupPath: Path, title: string, description: string, dateCreated: string, color: string, moduleName: string) {
    const creator = `~${this.ship}`;
    return this.metadataAction({
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

  update(association: Association, newMetadata: Partial<Metadata>) {
    const metadata = {...association.metadata, ...newMetadata };
    return this.metadataAction({ 
      add: {
        'group-path': association['group-path'], 
        resource: {
          'app-path': association['app-path'],
          'app-name': association['app-name'],
        },
        metadata
      }
    });
  }

  private metadataAction(data) {
    return this.action('metadata-hook', 'metadata-action', data);
  }
}
