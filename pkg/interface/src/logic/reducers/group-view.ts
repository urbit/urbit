import { resourceAsPath } from "~/logic/lib/util";


const initial = (json: any, state: any) => {
  const data = json.initial;
  if(data) {
    state.pendingJoin = new Set(data);
  }
}

const progress = (json: any, state: any) => {
  const data = json.progress;
  if(data) {
    const res = resourceAsPath(data.resource);
    const { progress } = data;
    if(progress === 'start') {
      state.pendingJoin.add(res);
    } else if (progress !== 'added') {
      state.pendingJoin.delete(res);
    }
  }
}

export const GroupViewReducer = (json: any, state: any) => {
  const data = json['group-view-update'];
  if(data) {
    progress(data, state);
    initial(data, state);
  }
}
