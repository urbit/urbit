import React from "react";
import create, { State }  from 'zustand';
import { persist } from 'zustand/middleware';

import { MetadataUpdatePreview, Associations } from "@urbit/api";

// import useApi from "~/logic/lib/useApi";
import { stateSetter } from "~/logic/lib/util";

export const METADATA_MAX_PREVIEW_WAIT = 150000;

export interface MetadataState extends State {
  associations: Associations;
  // preview: (group: string) => Promise<MetadataUpdatePreview>;
  set: (fn: (state: MetadataState) => void) => void;
};

const useMetadataState = create<MetadataState>(persist((set, get) => ({
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
  set: fn => stateSetter(fn, set),
}), {
  name: 'LandscapeMetadataState'
}));

function withMetadataState<P, S extends keyof MetadataState>(Component: any, stateMemberKeys?: S[]) {
  return React.forwardRef((props: Omit<P, S>, ref) => {
    const metadataState = stateMemberKeys ? useMetadataState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ): useMetadataState();
    return <Component ref={ref} {...metadataState} {...props} />
  });
}

export { useMetadataState as default, withMetadataState };