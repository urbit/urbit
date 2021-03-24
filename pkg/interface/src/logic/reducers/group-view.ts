import { GroupUpdate } from '@urbit/api/groups';
import { resourceAsPath } from '~/logic/lib/util';
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

export const GroupViewReducer = (json: any) => {
  const data = json['group-view-update'];
  if (data) {
    reduceState<GroupState, GroupUpdate>(useGroupState, data, [
      progress,
      initial
    ]);
  }
};