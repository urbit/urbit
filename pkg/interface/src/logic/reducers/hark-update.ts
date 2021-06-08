import {
  NotificationContents,
  NotifIndex,
  Timebox
} from '@urbit/api';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import _ from 'lodash';
import { compose } from 'lodash/fp';
import { makePatDa } from '~/logic/lib/util';
import { describeNotification, getReferent } from '../lib/hark';
import { BaseState } from '../state/base';
import { HarkState as State } from '../state/hark';

type HarkState = State & BaseState<State>;

function calculateCount(json: any, state: HarkState) {
  state.notificationsCount = Object.keys(state.unreadNotes).length;
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

function removeGraph(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'remove-graph');
  if(data) {
    delete state.unreads.graph[data];
  }
  return state;
}

function seenIndex(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'seen-index');
  if(data) {
    updateNotificationStats(state, data.index, 'last', () => data.time);
  }
  return state;
}

function readEach(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'read-each');
  if (data) {
    updateUnreads(state, data.index, u => u.delete(data.target));
  }
  return state;
}

function readSince(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'read-count');
  if(data) {
    updateUnreadCount(state, data, () => 0);
  }
  return state;
}

function unreadSince(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'unread-count');
  if (data) {
    updateUnreadCount(state, data.index, u => u + 1);
  }
  return state;
}

function unreadEach(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'unread-each');
  if(data) {
    updateUnreads(state, data.index, us => us.add(data.target));
  }
  return state;
}

function unreads(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'unreads');
  if(data) {
    clearState(state);
    data.forEach(({ index, stats }) => {
      const { unreads, notifications, last } = stats;
      updateNotificationStats(state, index, 'last', () => last);
      _.each(notifications, ({ time, index }) => {
        if(!time) {
          addNotificationToUnread(state, index);
        }
      });
      if('count' in unreads) {
        updateUnreadCount(state, index, (u = 0) => u + unreads.count);
      } else {
        updateUnreads(state, index, s => new Set());
        unreads.each.forEach((u: string) => {
          updateUnreads(state, index, s => s.add(u));
        });
      }
    });
  }
  return state;
}

function clearState(state: HarkState): HarkState {
  const initialState = {
    notifications: new BigIntOrderedMap<Timebox>(),
    archivedNotifications: new BigIntOrderedMap<Timebox>(),
    notificationsGroupConfig: [],
    notificationsGraphConfig: {
      watchOnSelf: false,
      mentions: false,
      watching: []
    },
    unreads: {
      graph: {},
      group: {}
    },
    notificationsCount: 0
  };
  Object.assign(state, initialState);

  return state;
}

function updateUnreadCount(state: HarkState, index: NotifIndex, count: (c: number) => number): HarkState {
  if(!('graph' in index)) {
    return state;
  }
  const property = [index.graph.graph, index.graph.index, 'unreads'];
  const curr = _.get(state.unreads.graph, property, 0);
  const newCount = count(curr);
  _.set(state.unreads.graph, property, newCount);
  return state;
}

function updateUnreads(state: HarkState, index: NotifIndex, f: (us: Set<string>) => void): HarkState {
  if(!('graph' in index)) {
    return state;
  }
  const unreads: any = _.get(state.unreads.graph, [index.graph.graph, index.graph.index, 'unreads'], new Set<string>());
  f(unreads);

  _.set(state.unreads.graph, [index.graph.graph, index.graph.index, 'unreads'], unreads);
  return state;
}

function addNotificationToUnread(state: HarkState, index: NotifIndex) {
  if('graph' in index) {
    const path = [index.graph.graph, index.graph.index, 'notifications'];
    const curr = _.get(state.unreads.graph, path, []);
    _.set(state.unreads.graph, path,
      [
        ...curr.filter((c) => {
          return !(notifIdxEqual(c.index, index));
        }),
        { index }
      ]
    );
  } else if ('group' in index) {
    const path = [index.group.group, 'notifications'];
    const curr = _.get(state.unreads.group, path, []);
    _.set(state.unreads.group, path,
      [
        ...curr.filter(c => !notifIdxEqual(c.index, index)),
        { index }
      ]
    );
  }
}
function removeNotificationFromUnread(state: HarkState, index: NotifIndex) {
  if('graph' in index) {
    const path = [index.graph.graph, index.graph.index, 'notifications'];
    const curr = _.get(state.unreads.graph, path, []);
    _.set(state.unreads.graph, path, curr.filter(c => !notifIdxEqual(c.index, index)));
  } else if ('group' in index) {
    const path = [index.group.group, 'notifications'];
    const curr = _.get(state.unreads.group, path, []);
    _.set(state.unreads.group, path, curr.filter(c => !notifIdxEqual(c.index, index)));
  }
}

function updateNotificationStats(state: HarkState, index: NotifIndex, statField: 'unreads' | 'last', f: (x: number) => number, notify = false) {
    if('graph' in index) {
      const curr: any = _.get(state.unreads.graph, [index.graph.graph, index.graph.index, statField], 0);
      _.set(state.unreads.graph, [index.graph.graph, index.graph.index, statField], f(curr));
    } else if('group' in index) {
      const curr: any = _.get(state.unreads.group, [index.group.group, statField], 0);
      _.set(state.unreads.group, [index.group.group, statField], f(curr));
    }
}

function added(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'added', false);
  if (data) {
    const { index, notification } = data;
    const [fresh] = _.partition(state.unreadNotes, ({ index: idx }) => !notifIdxEqual(index, idx));
    state.unreadNotes = [...fresh, { index, notification }];

    if ('Notification' in window && !state.doNotDisturb) {
      const description = describeNotification(data);
      const referent = getReferent(data);
      new Notification(`${description} ${referent}`, {
        tag: 'landscape',
        image: '/img/favicon.png',
        icon: '/img/favicon.png',
        badge: '/img/favicon.png',
        renotify: true
      });
    }
  }
  return state;
}

const dnd = (json: any, state: HarkState): HarkState => {
  const data = _.get(json, 'set-dnd', undefined);
  if (!_.isUndefined(data)) {
    state.doNotDisturb = data;
  }
  return state;
};

const timebox = (json: any, state: HarkState): HarkState => {
  const data = _.get(json, 'timebox', false);
  if (data) {
    if (data.time) {
      const time = makePatDa(data.time);
      state.notifications = state.notifications.set(time, data.notifications);
    } else {
      state.unreadNotes = data.notifications;
      _.each(data.notifications, ({ index }) => {
        addNotificationToUnread(state, index);
      });
    }
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

function notifIdxEqual(a: NotifIndex, b: NotifIndex) {
  if ('graph' in a && 'graph' in b) {
    return (
      a.graph.graph === b.graph.graph &&
      a.graph.group === b.graph.group &&
      a.graph.mark === b.graph.mark &&
      a.graph.description === b.graph.description
    );
  } else if ('group' in a && 'group' in b) {
    return (
      a.group.group === b.group.group &&
      a.group.description === b.group.description
    );
  }
  return false;
}

function mergeNotifs(a: NotificationContents, b: NotificationContents) {
  if ('graph' in a && 'graph' in b) {
    return {
      graph: [...a.graph, ...b.graph]
    };
  } else if ('group' in a && 'group' in b) {
    return {
      group: [...a.group, ...b.group]
    };
  }
  return a;
}

function read(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'note-read', false);
  if (data) {
    const { index } = data;
    const time = makePatDa(data.time);
    const [read, unread] = _.partition(state.unreadNotes,({ index: idx }) => notifIdxEqual(index, idx));
    state.unreadNotes = unread;
    const oldTimebox = state.notifications.get(time) ?? [];
    const [toMerge, rest] = _.partition(oldTimebox, i => notifIdxEqual(index, i.index));
    if(toMerge.length > 0 && read.length > 0) {
      read[0].notification.contents = mergeNotifs(read[0].notification.contents, toMerge[0].notification.contents);
    }
    state.notifications = state.notifications.set(time, [...read, ...rest]);
    removeNotificationFromUnread(state, index);
  }
  return state;
}

function archive(json: any, state: HarkState): HarkState {
  const data = _.get(json, 'archive', false);
  if (data) {
    const { index } = data;
    if(data.time) {
      const time = makePatDa(data.time);
      const timebox = state.notifications.get(time);
      if (!timebox) {
        console.warn('Modifying nonexistent timebox');
        return state;
      }
      const unarchived = _.filter(timebox, idxNotif =>
        !notifIdxEqual(index, idxNotif.index)
      );
      if(unarchived.length === 0) {
        console.log('deleting entire timebox');
        state.notifications = state.notifications.delete(time);
      } else {
        state.notifications = state.notifications.set(time, unarchived);
      }
    } else {
      state.unreadNotes = state.unreadNotes.filter(({ index: idx }) => !notifIdxEqual(idx, index));
      removeNotificationFromUnread(state, index);
    }
  }
  return state;
}

export function reduce(data, state) {
  const reducers = [
    calculateCount,
    read,
    archive,
    timebox,
    more,
    dnd,
    added,
    unreads,
    readEach,
    readSince,
    unreadSince,
    unreadEach,
    seenIndex,
    removeGraph,
    readAll
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

