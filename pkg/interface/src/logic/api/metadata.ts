
import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path, Patp, Association, Metadata, MetadataUpdatePreview } from '@urbit/api';
import { uxToHex } from '../lib/util';

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
          'module': moduleName,
          picture: '',
          preview: false,
          vip: ''
        }
      }
    });
  }

  remove(appName: string, resource: string, group: string) {
    return this.metadataAction({
      remove: {
        group,
        resource: {
          resource,
          'app-name': appName
        }
      }
    });
  }

  update(association: Association, newMetadata: Partial<Metadata>) {
    const metadata = { ...association.metadata, ...newMetadata };
    metadata.color = uxToHex(metadata.color);
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

  preview(group: string) {
    return new Promise<MetadataUpdatePreview>((resolve, reject) => {
      const tempChannel: any = new (window as any).channel();
      let done = false;

      setTimeout(() => {
        if(done) {
          return;
        }
        done = true;
        tempChannel.delete();
        reject(new Error('offline'));
      }, 15000);

      tempChannel.subscribe(window.ship, 'metadata-pull-hook', `/preview${group}`,
        (err) => {
          console.error(err);
          reject(err);
          tempChannel.delete();
        },
        (ev: any) => {
          if ('metadata-hook-update' in ev) {
            done = true;
            tempChannel.delete();
            const upd = ev['metadata-hook-update'].preview as MetadataUpdatePreview;
            resolve(upd);
          } else {
            done = true;
            tempChannel.delete();
            reject(new Error('no-permissions'));
          }
        },
        (quit) => {
          tempChannel.delete();
          if(!done) {
            reject(new Error('offline'));
          }
        },
        (a) => {
          console.log(a);
        }
      );
    });
  }

  private metadataAction(data) {
    return this.action('metadata-push-hook', 'metadata-update', data);
  }
}
