import { resourceAsPath } from '~/logic/lib/util';

const initial = (json: any, state: any) => {
  const data = json.initial;
  if(data) {
    state.pendingJoin = data;
  }
};

const progress = (json: any, state: any) => {
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
};

export const GroupViewReducer = (json: any, state: any) => {
  const data = json['group-view-update'];
  if(data) {
    progress(data, state);
    initial(data, state);
  }
};
