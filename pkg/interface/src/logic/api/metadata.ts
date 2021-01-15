
import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path, Patp, Association, Metadata, MetadataUpdatePreview } from '~/types';

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

  preview(group: string) {
    return new Promise<MetadataUpdatePreview>((resolve, reject) => {
      const tempChannel: any = new (window as any).channel();
      let done = false;
      tempChannel.subscribe(window.ship, "metadata-pull-hook", `/preview${group}`,
        (err) => {
          reject(err);
          tempChannel.delete();
        },
        (ev: any) => {
          done = true;
          tempChannel.delete();
          const upd = ev['metadata-update'].preview as MetadataUpdatePreview;
          resolve(upd);
        },
        (quit) => {
          tempChannel.delete();
          if(!done) {
            reject(quit)
          }
        },
        (a) => {
          console.log(a);
        }
      );
    })
  }



  private metadataAction(data) {
    return this.action('metadata-push-hook', 'metadata-update', data);
  }
}
