import _ from 'lodash';
import { NotificationGraphConfig } from '@urbit/api';
import { Notification } from './hark-types';
import { mockNotification } from './mock-data';
import { useMockData } from './util';
import { BaseState, createState, createSubscription, reduceStateN } from './base';

export interface HarkState {
  notifications: Notification[];
  notificationsGraphConfig: NotificationGraphConfig;
  [ref: string]: unknown;
}

type BaseHarkState = HarkState & BaseState<HarkState>;

function updateState(
  key: string,
  transform: (state: BaseHarkState, data: any) => void
): (json: any, state: BaseHarkState) => BaseHarkState {
  return (json: any, state: BaseHarkState) => {
    if (_.has(json, key)) {
      transform(state, _.get(json, key, undefined));
    }
    return state;
  };
}

export const reduceGraph = [
  updateState('initial', (draft, data) => {
    draft.notificationsGraphConfig = data;
  }),
  updateState('set-mentions', (draft, data) => {
    draft.notificationsGraphConfig.mentions = data;
  })
];

export const useHarkStore = createState<HarkState>(
  'Hark',
  () => ({
    notifications: useMockData ? [mockNotification] : [],
    notificationsGraphConfig: {
      watchOnSelf: false,
      mentions: false,
      watching: []
    }
  }),
  [],
  [
    (set, get) =>
      createSubscription('hark-graph-hook', '/updates', (j) => {
        const graphHookData = _.get(j, 'hark-graph-hook-update', false);
        if (graphHookData) {
          reduceStateN(get(), graphHookData, reduceGraph);
        }
      })
  ]
);
