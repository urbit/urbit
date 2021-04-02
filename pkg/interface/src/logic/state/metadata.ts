import { MetadataUpdatePreview, Associations } from "@urbit/api";

import { BaseState, createState } from "./base";

export const METADATA_MAX_PREVIEW_WAIT = 150000;

export interface MetadataState extends BaseState<MetadataState> {
  associations: Associations;
  // preview: (group: string) => Promise<MetadataUpdatePreview>;
};

const useMetadataState = createState<MetadataState>('Metadata', {
  associations: { groups: {}, graph: {}, contacts: {}, chat: {}, link: {}, publish: {} },
  // preview: async (group): Promise<MetadataUpdatePreview> => {
  //   return new Promise<MetadataUpdatePreview>((resolve, reject) => {
  //     const api = useApi();
  //     let done = false;
  
  //     setTimeout(() => {
  //       if (done) {
  //         return;
  //       }
  //       done = true;
  //       reject(new Error('offline'));
  //     }, METADATA_MAX_PREVIEW_WAIT);
  
  //     api.subscribe({
  //       app: 'metadata-pull-hook',
  //       path: `/preview${group}`,
  //       // TODO type this message?
  //       event: (message) => {
  //         if ('metadata-hook-update' in message) {
  //           done = true;
  //           const update = message['metadata-hook-update'].preview as MetadataUpdatePreview;
  //           resolve(update);
  //         } else {
  //           done = true;
  //           reject(new Error('no-permissions'));
  //         }
  //         // TODO how to delete this subscription? Perhaps return the susbcription ID as the second parameter of all the handlers
  //       },
  //       err: (error) => {
  //         console.error(error);
  //         reject(error);
  //       },
  //       quit: () => {
  //         if (!done) {
  //           reject(new Error('offline'));
  //         }
  //       }
  //     });
  //   });
  // },
});


export default useMetadataState;