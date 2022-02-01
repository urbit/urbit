import {
  BigIntOrderedMap,
  HarkPlace,
  Timebox,
  HarkStats,
  harkBinToId,
  makePatDa
} from '@urbit/api';
import _ from 'lodash';
import { compose } from 'lodash/fp';
import { BaseState } from '../state/base';
import { HarkState as State } from '../state/hark';

type HarkState = State & BaseState<State>;

function calculateCount(json: any, state: HarkState) {
  state.notificationsCount = Object.keys(state.unseen).length;
  return state;
}

function groupInitial(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.notificationsGroupConfig = data;
  }
  return state;
}

function graphInitial(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.notificationsGraphConfig = data;
  }
  return state;
}

function graphListen(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'listen', false);
  if (data) {
    state.notificationsGraphConfig.watching = [
      ...state.notificationsGraphConfig.watching,
      data
    ];
  }
  return state;
}

function graphIgnore(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'ignore', false);
  if (data) {
    state.notificationsGraphConfig.watching = state.notificationsGraphConfig.watching.filter(
      ({ graph, index }) => !(graph === data.graph && index === data.index)
    );
  }
  return state;
}

function groupListen(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'listen', false);
  if (data) {
    state.notificationsGroupConfig = [...state.notificationsGroupConfig, data];
  }
  return state;
}

function groupIgnore(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'ignore', false);
  if (data) {
    state.notificationsGroupConfig = state.notificationsGroupConfig.filter(
      n => n !== data
    );
  }
  return state;
}

function graphMentions(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'set-mentions', undefined);
  if (!_.isUndefined(data)) {
    state.notificationsGraphConfig.mentions = data;
  }
  return state;
}

function graphWatchSelf(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'set-watch-on-self', undefined);
  if (!_.isUndefined(data)) {
    state.notificationsGraphConfig.watchOnSelf = data;
  }
  return state;
}

function readAll(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'read-all');
  if(data) {
    clearState(state);
  }
  return state;
}

const emptyStats = () => ({
  each: [],
  count: 0,
  last: 0
});

function updateNotificationStats(state: HarkState, place: HarkPlace, f: (s: HarkStats) => Partial<HarkStats>) {
  if(place.desk !== (window as any).desk) {
    return;
  }
  const old = state.unreads?.[place.path] || emptyStats();
  state.unreads[place.path] = { ...old, ...f(old) };
}

function seenIndex(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'saw-place');
  if(data) {
    const last = data?.time || Date.now();
    updateNotificationStats(state, data.place, s => ({ last }));
  }
  return state;
}

function readEach(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'read-each');
  if (data) {
    const { place, path } = data;
    updateNotificationStats(state, place, s => ({ each: s.each.filter(e => e !== path) }));
  }
  return state;
}

function readSince(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'read-count');
  if(data) {
    updateNotificationStats(state, data, s => ({ count: 0 }));
  }
  return state;
}

function unreadSince(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'unread-count');
  if (data) {
    const { inc, count, place } = data;
    updateNotificationStats(state, place, s => ({ count: inc ? s.count + count  : s.count - count }));
  }
  return state;
}

function unreadEach(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'unread-each');
  if(data) {
    const { place, path } = data;
    updateNotificationStats(state, place, s => ({ each: [...s.each, path] }));
  }
  return state;
}

function allStats(json: any, state: HarkState): HarkState {
  if('all-stats' in json) {
    const data = json['all-stats'];
    data.forEach(({ place, stats }) => {
      if(place.desk !== (window as any).desk) {
        return;
      }
      state.unreads[place.path] = stats;
    });
  }
  return state;
}

function clearState(state: HarkState): HarkState {
  const initialState = {
    notifications: new BigIntOrderedMap<Timebox>(),
    unseen: {},
    seen: {},
    notificationsGroupConfig: [],
    notificationsGraphConfig: {
      watchOnSelf: false,
      mentions: false,
      watching: []
    },
    unreads: {},
    notificationsCount: 0,
    unreadNotes: {}
  };
  Object.assign(state, initialState);

  return state;
}

const dnd = (json: any, state: HarkState): HarkState => {
  const data = _.get(json, 'set-dnd', undefined);
  if (!_.isUndefined(data)) {
    state.doNotDisturb = data;
  }
  return state;
};

function more(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'more', false);
  if (data) {
    _.forEach(data, (d) => {
      reduce(d, state);
    });
  }
  return state;
}

function added(json: any, state: HarkState): HarkState {
  if('added' in json) {
    const { bin } = json.added;
    if(bin.place.desk !== window.desk) {
      return state;
    }
    const binId = harkBinToId(bin);
    state.unseen[binId] = json.added;
  }

  return state;
}

function archived(json: any, state: HarkState): HarkState {
  if('archived' in json) {
    const { lid, notification } = json.archived;
      const seen = 'seen' in lid ? 'seen' : 'unseen';
      const binId = harkBinToId(notification.bin);
      delete state[seen][binId];
      const time = makePatDa(json.archived.time);
      const timebox = state.archive?.get(time) || {};
      timebox[binId] = notification;
      state.archive = state.archive.set(time, timebox);
  }
  return state;
}

function timebox(json: any, state: HarkState): HarkState {
  if('timebox' in json) {
    const { timebox } = json;
    const { lid, notifications } = timebox;
    if('archive' in lid) {
        const time = makePatDa(lid.archive);
        const old = state.archive.get(time) || {};
        notifications.forEach((note: any) => {
          if(note.bin.place.desk !== window.desk) {
            return;
          }
          const binId = harkBinToId(note.bin);
          old[binId] = note;
        });
        state.archive = state.archive.set(time, old);
    } else {
        const seen = 'seen' in lid ? 'seen' : 'unseen';
        notifications.forEach((note: any) => {
          if(note.bin.place.desk !== window.desk) {
            return;
          }
          const binId = harkBinToId(note.bin);
          state[seen][binId] = note;
        });
    }
  }
  return state;
}

function opened(json: any, state: HarkState): HarkState {
  if('opened' in json) {
    const bins = Object.keys(state.unseen);
    bins.forEach((bin) => {
      const old = state.seen[bin];
      const curr = state.unseen[bin];
      curr.body = [...curr.body, ...(old?.body || [])];
      state.seen[bin] = curr;
      delete state.unseen[bin];
    });
  }
  return state;
}

function delPlace(json: any, state: HarkState): HarkState {
  if('del-place' in json) {
    const { path, desk } = json['del-place'];
    const pathId = `${desk}${path}`;
    const wipeBox = (t: Timebox) => {
      Object.keys(t).forEach((bin) => {
        if (bin.startsWith(pathId)) {
          delete t[bin];
        }
      });
    };
    wipeBox(state.unseen);
    wipeBox(state.seen);
    state.archive.keys().forEach((key) => {
      wipeBox(state.archive.get(key)!);
    });
  }
  return state;
}

export function reduce(data, state) {
  const reducers = [
    calculateCount,
    allStats,
    more,
    dnd,
    readEach,
    readSince,
    unreadSince,
    unreadEach,
    seenIndex,
    readAll,
    added,
    timebox,
    archived,
    opened,
    delPlace
  ];
  const reducer = compose(reducers.map(r => (s) => {
    return r(data, s);
  }));
  return reducer(state);
}

export const reduceGraph = [
  graphInitial,
  graphIgnore,
  graphListen,
  graphWatchSelf,
  graphMentions
];

export const reduceGroup = [
  groupInitial,
  groupListen,
  groupIgnore
];

