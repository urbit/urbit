import { Association, Associations, MetadataUpdatePreview } from '@urbit/api';
import _ from 'lodash';
import { useCallback, useEffect, useState } from 'react';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';
import airlock from '~/logic/api';
import history from '~/logic/lib/history';
import { reduce } from '../reducers/metadata-update';
import { getNotificationRedirectFromLink } from '../lib/notificationRedirects';

export const METADATA_MAX_PREVIEW_WAIT = 150000;

export interface MetadataState {
  associations: Associations;
  loaded: boolean;
  getPreview: (group: string) => Promise<MetadataUpdatePreview
  >;
  onLoad: () => void;
  previews: {
    [group: string]: MetadataUpdatePreview
  }
}

// @ts-ignore investigate zustand types
const useMetadataState = createState<MetadataState>(
  'Metadata',
  (set, get) => ({
    loaded: false,
    associations: {
      groups: {},
      graph: {}
    },
    previews: {},
    getPreview: async (group: string): Promise<MetadataUpdatePreview> => {
      const state = get();
      if(group in state.previews) {
        return state.previews[group];
      }
      try {
        const preview = await airlock.subscribeOnce('metadata-pull-hook', `/preview${group}`, 20 * 1000);
        if('metadata-hook-update' in preview) {
          const newState = get();
          newState.set((s) => {
            s.previews[group] = preview['metadata-hook-update'].preview;
          });
          return preview['metadata-hook-update'].preview;
        } else {
          throw 'no-permissions';
        }
      } catch (e) {
        if(e === 'timeout') {
          throw 'offline';
        }
        throw e;
      }
    },
    onLoad: () => {
      handleGridRedirect();
    }
  }),
  ['loaded'],
  [
    (set, get) =>
      createSubscription('metadata-store', '/all', (j) => {
        const d = _.get(j, 'metadata-update', false);
        if (d) {
          reduceStateN(get(), d, reduce);
        }
      })
  ]
);

const { graph, groups } = useMetadataState.getState().associations;

if (Object.keys(graph).length > 0 || Object.keys(groups).length > 0) {
  handleGridRedirect();
}

export function useAssocForGraph(graph: string) {
  return useMetadataState(
    useCallback(s => s.associations.graph[graph] as Association | undefined, [
      graph
    ])
  );
}

export function useAssocForGroup(group: string) {
  return useMetadataState(
    useCallback(
      s => s.associations.groups[group] as Association | undefined,
      [group]
    )
  );
}

const selPreview = (s: MetadataState) => [s.previews, s.getPreview] as const;

export function usePreview(group: string) {
  const [error, setError] = useState(null);
  const [previews, getPreview] = useMetadataState(selPreview);
  useEffect(() => {
    let mounted = true;
    (async () => {
      try {
        await getPreview(group);
      } catch (e) {
        if(mounted) {
          setError(e);
        }
      }
    })();

    return () => {
      mounted = false;
    };
  }, [group]);

  const preview = previews[group];

  return { error, preview };
}

function handleGridRedirect() {
  const query = new URLSearchParams(window.location.search);

  if(query.has('grid-note')) {
    history.push(getNotificationRedirectFromLink(query.get('grid-note')));
  } else if(query.has('grid-link')) {
    const link = decodeURIComponent(query.get('grid-link')!);
    history.push(`/perma${link}`);
  }
}

export function useGraphsForGroup(group: string) {
  const graphs = useMetadataState(s => s.associations.graph);
  return _.pickBy(graphs, (a: Association) => a.group === group);
}

export default useMetadataState;
