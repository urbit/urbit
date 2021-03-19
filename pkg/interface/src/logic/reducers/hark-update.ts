import {
  Notifications,
  NotifIndex,
  NotificationGraphConfig,
  GroupNotificationsConfig,
  UnreadStats
} from '@urbit/api';
import { makePatDa } from '~/logic/lib/util';
import _ from 'lodash';
import { StoreState } from '../store/type';
import { BigIntOrderedMap } from '../lib/BigIntOrderedMap';
import bigInt, {BigInteger} from 'big-integer';

type HarkState = Pick<StoreState, 'notifications' | 'notificationsGraphConfig' | 'notificationsGroupConfig' | 'unreads' >;

export const HarkReducer = (json: any, state: HarkState) => {
  const data = _.get(json, 'harkUpdate', false);
  if (data) {
    reduce(data, state);
    calculateCount(state);
  }
  const graphHookData = _.get(json, 'hark-graph-hook-update', false);
  if (graphHookData) {
    graphInitial(graphHookData, state);
    graphIgnore(graphHookData, state);
    graphListen(graphHookData, state);
    graphWatchSelf(graphHookData, state);
    graphMentions(graphHookData, state);
  }
  const groupHookData = _.get(json, 'hark-group-hook-update', false);
  if (groupHookData) {
    groupInitial(groupHookData, state);
    groupListen(groupHookData, state);
    groupIgnore(groupHookData, state);
  }
};

function calculateCount(state: HarkState) {
  let count = 0;
  _.forEach(state.unreads.graph, (graphs) => {
    _.forEach(graphs, graph => {
      count += (graph?.notifications || []).length;
    });
  });
  _.forEach(state.unreads.group, group => {
    count += (group?.notifications || []).length;
  })
  state.notificationsCount = count;
}

function groupInitial(json: any, state: HarkState) {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.notificationsGroupConfig = data;
  }
}

function graphInitial(json: any, state: HarkState) {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.notificationsGraphConfig = data;
  }
}

function graphListen(json: any, state: HarkState) {
  const data = _.get(json, 'listen', false);
  if (data) {
    state.notificationsGraphConfig.watching = [
      ...state.notificationsGraphConfig.watching,
      data
    ];
  }
}

function graphIgnore(json: any, state: HarkState) {
  const data = _.get(json, 'ignore', false);
  if (data) {
    state.notificationsGraphConfig.watching = state.notificationsGraphConfig.watching.filter(
      ({ graph, index }) => !(graph === data.graph && index === data.index)
    );
  }
}

function groupListen(json: any, state: HarkState) {
  const data = _.get(json, 'listen', false);
  if (data) {
    state.notificationsGroupConfig = [...state.notificationsGroupConfig, data];
  }
}

function groupIgnore(json: any, state: HarkState) {
  const data = _.get(json, 'ignore', false);
  if (data) {
    state.notificationsGroupConfig = state.notificationsGroupConfig.filter(
      n => n !== data
    );
  }
}

function graphMentions(json: any, state: HarkState) {
  const data = _.get(json, 'set-mentions', undefined);
  if (!_.isUndefined(data)) {
    state.notificationsGraphConfig.mentions = data;
  }
}

function graphWatchSelf(json: any, state: HarkState) {
  const data = _.get(json, 'set-watch-on-self', undefined);
  if (!_.isUndefined(data)) {
    state.notificationsGraphConfig.watchOnSelf = data;
  }
}

function reduce(data: any, state: HarkState) {
  unread(data, state);
  read(data, state);
  archive(data, state);
  timebox(data, state);
  more(data, state);
  dnd(data, state);
  added(data, state);
  unreads(data, state);
  readEach(data, state);
  readSince(data, state);
  unreadSince(data, state);
  unreadEach(data, state);
  seenIndex(data, state);
  removeGraph(data, state);
  readAll(data, state);
}

function readAll(json: any, state: HarkState) {
  const data = _.get(json, 'read-all');
  if(data) { 
    clearState(state);
  }
}

function removeGraph(json: any, state: HarkState) {
  const data = _.get(json, 'remove-graph');
  if(data) {
    delete state.unreads.graph[data];
  }
}

function seenIndex(json: any, state: HarkState) {
  const data = _.get(json, 'seen-index');
  if(data) {
    updateNotificationStats(state, data.index, 'last', () => data.time);
  }
}

function readEach(json: any, state: HarkState) {
  const data = _.get(json, 'read-each');
  if(data) {
    updateUnreads(state, data.index, u => u.delete(data.target));
  }
}

function readSince(json: any, state: HarkState) {
  const data = _.get(json, 'read-count');
  if(data) {
    updateUnreadCount(state, data, () => 0);
  }
}

function unreadSince(json: any, state: HarkState) {
  const data = _.get(json, 'unread-count');
  if(data) {
    updateUnreadCount(state, data.index, u => u + 1);
  }
}

function unreadEach(json: any, state: HarkState) {
  const data = _.get(json, 'unread-each');
  if(data) {
    updateUnreads(state, data.index, us => us.add(data.target));
  }
}

function unreads(json: any, state: HarkState) {
  const data = _.get(json, 'unreads');
  if(data) {
    clearState(state);
    data.forEach(({ index, stats }) => {
      const { unreads, notifications, last } = stats;
      console.log(notifications);
      _.each(notifications, ({ time, index }) => {
        addNotificationToUnread(state, index, makePatDa(time));
      });
      if('count' in unreads) {
        updateUnreadCount(state, index, (u = 0) => u + unreads.count);
      } else {
        unreads.each.forEach((u: string) => {
          updateUnreads(state, index, s => s.add(u));
        });
      }
    });
  }
}

function clearState(state) {
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

  Object.keys(initialState).forEach((key) => {
    state[key] = initialState[key];
  });
}

function updateUnreadCount(state: HarkState, index: NotifIndex, count: (c: number) => number) {
  if(!('graph' in index)) {
    return;
  }
  const property = [index.graph.graph, index.graph.index, 'unreads'];
  const curr = _.get(state.unreads.graph, property, 0);
  const newCount = count(curr);
  _.set(state.unreads.graph, property, newCount);
}

function updateUnreads(state: HarkState, index: NotifIndex, f: (us: Set<string>) => void) {
  if(!('graph' in index)) {
    return;
  }
  const unreads = _.get(state.unreads.graph, [index.graph.graph, index.graph.index, 'unreads'], new Set<string>());
  const oldSize = unreads.size;
  f(unreads);
  const newSize = unreads.size;
  _.set(state.unreads.graph, [index.graph.graph, index.graph.index, 'unreads'], unreads);
}

function addNotificationToUnread(state: HarkState, index: NotifIndex, time: BigInteger) {
  if('graph' in index) {
    const path = [index.graph.graph, index.graph.index, 'notifications'];
    const curr = _.get(state.unreads.graph, path, []);
    _.set(state.unreads.graph, path,
      [
        ...curr.filter(c => !(c.time.eq(time) && notifIdxEqual(c.index, index))),
        { time, index}
      ]
    );
  } else if ('group' in index) {
    const path = [index.group.group, 'notifications'];
    const curr = _.get(state.unreads.group, path, []);
    _.set(state.unreads.group, path,
      [
        ...curr.filter(c => !(c.time.eq(time) && notifIdxEqual(c.index, index))),
        { time, index}
      ]
    );
  }
}

function removeNotificationFromUnread(state: HarkState, index: NotifIndex, time: BigInteger) {
  if('graph' in index) {
    const path = [index.graph.graph, index.graph.index, 'notifications'];
    const curr = _.get(state.unreads.graph, path, []);
    _.set(state.unreads.graph, path,
      curr.filter(c => !(c.time.eq(time) && notifIdxEqual(c.index, index))),
    );
  } else if ('group' in index) {
    const path = [index.group.group, 'notifications'];
    const curr = _.get(state.unreads.group, path, []);
    _.set(state.unreads.group, path,
      curr.filter(c => !(c.time.eq(time) && notifIdxEqual(c.index, index))),
    );
  }
}

function updateNotificationStats(state: HarkState, index: NotifIndex, statField: 'unreads' | 'last', f: (x: number) => number) {

    if('graph' in index) {
      const curr = _.get(state.unreads.graph, [index.graph.graph, index.graph.index, statField], 0);
      _.set(state.unreads.graph, [index.graph.graph, index.graph.index, statField], f(curr));
    } else if('group' in index) {
      const curr = _.get(state.unreads.group, [index.group.group, statField], 0);
      _.set(state.unreads.group, [index.group.group, statField], f(curr));
    }
}

function added(json: any, state: HarkState) {
  const data = _.get(json, 'added', false);
  if (data) {
    const { index, notification } = data;
    const time = makePatDa(data.time);
    const timebox = state.notifications.get(time) || [];
    addNotificationToUnread(state, index, time);

    const arrIdx = timebox.findIndex(idxNotif =>
      notifIdxEqual(index, idxNotif.index)
    );
    if (arrIdx !== -1) {
      if(timebox[arrIdx]?.notification?.read) {
      }
      timebox[arrIdx] = { index, notification };
      state.notifications.set(time, timebox);
    } else {
      state.notifications.set(time, [...timebox, { index, notification }]);
    }
  }
}

const dnd = (json: any, state: HarkState) => {
  const data = _.get(json, 'set-dnd', undefined);
  if (!_.isUndefined(data)) {
    state.doNotDisturb = data;
  }
};

const timebox = (json: any, state: HarkState) => {
  const data = _.get(json, 'timebox', false);
  if (data) {
    const time = makePatDa(data.time);
    if (!data.archive) {
      state.notifications.set(time, data.notifications);
    }
  }
};

function more(json: any, state: HarkState) {
  const data = _.get(json, 'more', false);
  if (data) {
    _.forEach(data, d => reduce(d, state));
  }
}

function notifIdxEqual(a: NotifIndex, b: NotifIndex) {
  if ('graph' in a && 'graph' in b) {
    return (
      a.graph.graph === b.graph.graph &&
      a.graph.group === b.graph.group &&
      a.graph.module === b.graph.module &&
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

function setRead(
  time: string,
  index: NotifIndex,
  read: boolean,
  state: HarkState
) {
  const patDa = makePatDa(time);
  const timebox = state.notifications.get(patDa);
  if (_.isNull(timebox)) {
    console.warn('Modifying nonexistent timebox');
    return;
  }
  const arrIdx = timebox.findIndex(idxNotif =>
    notifIdxEqual(index, idxNotif.index)
  );
  if (arrIdx === -1) {
    console.warn('Modifying nonexistent index');
    return;
  }
  timebox[arrIdx].notification.read = read;
  state.notifications.set(patDa, timebox);
}

function read(json: any, state: HarkState) {
  const data = _.get(json, 'read-note', false);
  if (data) {
    const { time, index } = data;
    removeNotificationFromUnread(state, index, makePatDa(time));
    setRead(time, index, true, state);
  }
}

function unread(json: any, state: HarkState) {
  const data = _.get(json, 'unread-note', false);
  if (data) {
    const { time, index } = data;
    addNotificationToUnread(state, index, makePatDa(time));
    setRead(time, index, false, state);
  }
}

function archive(json: any, state: HarkState) {
  const data = _.get(json, 'archive', false);
  if (data) {
    const { index } = data;
    removeNotificationFromUnread(state, index, makePatDa(data.time))
    const time = makePatDa(data.time);
    const timebox = state.notifications.get(time);
    if (!timebox) {
      console.warn('Modifying nonexistent timebox');
      return;
    }
    const [archived, unarchived] = _.partition(timebox, idxNotif =>
      notifIdxEqual(index, idxNotif.index)
    );
    if(unarchived.length === 0) {
      console.log('deleting entire timebox');
      state.notifications.delete(time);
    } else {
      state.notifications.set(time, unarchived);
    }
  }
}
