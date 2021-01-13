
import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path, Patp, Association, Metadata } from '~/types';

export default class MetadataApi extends BaseApi<StoreState> {


  metadataAdd(appName: string, resource: Path, group: Path, title: string, description: string, dateCreated: string, color: string, moduleName: string) {
    const creator = `~${this.ship}`;
    return this.metadataAction({
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
        group: association.group,
        resource: {
          resource: association.resource,
          'app-name': association['app-name']
        },
        metadata
      }
    });
  }

  private metadataAction(data) {
    return this.action('metadata-hook', 'metadata-action', data);
  }
}
