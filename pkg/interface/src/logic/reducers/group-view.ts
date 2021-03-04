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
    state.pendingJoin[resource].progress = progress;
    if(progress === 'done') {
      setTimeout(() => {
        delete state.pendingJoin[resource];
      }, 10000);
    }
  }
};

const hide = (json: any, state: any) => {
  const data = json.hide;
  if(data) {
    state.pendingJoin[data].hidden = true;
  }

}

export const GroupViewReducer = (json: any, state: any) => {
  const data = json['group-view-update'];
  if(data) {
    progress(data, state);
    initial(data, state);
    hide(data, state);
  }
};
