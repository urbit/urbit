
import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path, Patp } from '../types/noun';

export default class MetadataApi extends BaseApi<StoreState> {

  metadataAdd(appName: string, appPath: Path, groupPath: Path, title: string, description: string, dateCreated: string, color: string) {
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
          creator
        }
      }
    });
  }

  private metadataAction(data) {
    return this.action('metadata-hook', 'metadata-action', data);
  }
}
