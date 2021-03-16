import { GroupUpdate } from '@urbit/api/groups';
import { SubscriptionRequestInterface, UrbitInterface } from '@urbit/http-api';
import { resourceAsPath } from '~/logic/lib/util';
import { handleSubscriptionError, handleSubscriptionQuit } from '../lib/subscriptionHandlers';
import { reduceState } from '../state/base';
import useGroupState, { GroupState } from '../state/group';

const initial = (json: any, state: GroupState): GroupState => {
  const data = json.initial;
  if(data) {
    state.pendingJoin = data;
  }
  return state;
};

const progress = (json: any, state: GroupState): GroupState => {
  const data = json.progress;
  if(data) {
    const { progress, resource } = data;
    state.pendingJoin = { ...state.pendingJoin, [resource]: progress };
    if(progress === 'done') {
      setTimeout(() => {
        delete state.pendingJoin[resource];
      }, 10000);
    }
  }
  return state;
};

const GroupViewReducer = (json: any) => {
  const data = json['group-view-update'];
  if (data) {
    useGroupState.getState().set(state => {
      state = reduceState<GroupState, GroupUpdate>(useGroupState, data, [
        progress,
        initial
      ]);
    })
  }
};

export const groupViewSubscription = (channel: UrbitInterface): SubscriptionRequestInterface => {
  const event = GroupViewReducer;
  const err = handleSubscriptionError(channel, groupViewSubscription);
  const quit = handleSubscriptionQuit(channel, groupViewSubscription);
  return {
    app: 'group-view',
    path: '/all',
    event, err, quit
  };
}

export default GroupViewReducer;