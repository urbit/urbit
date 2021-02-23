import _ from 'lodash';
import { compose } from 'lodash/fp';

import { NotifIndex, Timebox, makePatDa } from '@urbit/api';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import { SubscriptionRequestInterface, UrbitInterface } from '@urbit/http-api';

import useHarkState, { HarkState } from '~/logic/state/hark';

const clearState = (state: HarkState): HarkState => {
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
  return state;
}

const updateUnreadCount = (state: HarkState, index: NotifIndex, count: (c: number) => number): HarkState => {
  if(!('graph' in index)) {
    return state;
  }
  const property = [index.graph.graph, index.graph.index, 'unreads'];
  const curr = _.get(state.unreads.graph, property, 0);
  const newCount = count(curr);
  _.set(state.unreads.graph, property, newCount);
  return state;
}

const updateUnreads = (state: HarkState, index: NotifIndex, f: (us: Set<string>) => void): HarkState => {
  if(!('graph' in index)) {
    return state;
  }
  const unreads = f(_.get(state.unreads.graph, [index.graph.graph, index.graph.index, 'unreads'], new Set<string>()));
  _.set(state.unreads.graph, [index.graph.graph, index.graph.index, 'unreads'], unreads);
  return state;
}

const updateNotificationStats = (state: HarkState, index: NotifIndex, statField: 'notifications' | 'unreads' | 'last', f: (x: number) => number): HarkState => {
  if(statField === 'notifications') {
    state.notificationsCount = f(state.notificationsCount);
  }
  if('graph' in index) {
    const curr = _.get(state.unreads.graph, [index.graph.graph, index.graph.index, statField], 0);
    _.set(state.unreads.graph, [index.graph.graph, index.graph.index, statField], f(curr));
  } else if('group' in index) {
    const curr = _.get(state.unreads.group, [index.group.group, statField], 0);
    _.set(state.unreads.group, [index.group.group, statField], f(curr));
  }
  return state;
}

const notifIdxEqual = (a: NotifIndex, b: NotifIndex): boolean => {
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

const setRead = (
  time: string,
  index: NotifIndex,
  read: boolean,
  state: HarkState
): HarkState => {
  const patDa = makePatDa(time);
  const timebox = state.notifications.get(patDa);
  if (_.isNull(timebox)) {
    console.warn('Modifying nonexistent timebox');
    return state;
  }
  const arrIdx = timebox.findIndex(idxNotif =>
    notifIdxEqual(index, idxNotif.index)
  );
  if (arrIdx === -1) {
    console.warn('Modifying nonexistent index');
    return state;
  }
  timebox[arrIdx].notification.read = read;
  state.notifications.set(patDa, timebox);
  return state;
}

export const readAll = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'read-all');
  if(data) { 
    return clearState(state);
  }
  return state;
}

export const removeGraph = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'remove-graph');
  if(data) {
    delete state.unreads.graph[data];
  }
  return state;
}

export const seenIndex = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'seen-index');
  if(data) {
    return updateNotificationStats(state, data.index, 'last', () => data.time);
  }
  return state;
}

export const readEach = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'read-each');
  if(data) {
    return updateUnreads(state, data.index, u => u.delete(data.target));
  }
  return state;
}

export const readSince = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'read-count');
  if(data) {
    return updateUnreadCount(state, data, () => 0);
  }
  return state;
}

export const unreadSince = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'unread-count');
  if(data) {
    return updateUnreadCount(state, data.index, u => u + 1);
  }
  return state;
}

export const unreadEach = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'unread-each');
  if(data) {
    return updateUnreads(state, data.index, us => us.add(data.target));
  }
  return state;
}

export const unreads = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'unreads');
  if(data) {
    state = clearState(state);
    data.forEach(({ index, stats }) => {
      const { unreads, notifications, last } = stats;
      state = updateNotificationStats(state, index, 'notifications', x => x + notifications);
      state = updateNotificationStats(state, index, 'last', () => last);
      if('count' in unreads) {
        state = updateUnreadCount(state, index, (u = 0) => u + unreads.count);
      } else {
        unreads.each.forEach((u: string) => {
          state = updateUnreads(state, index, s => s.add(u));
        });
      }
    });
  }
  return state;
}

export const added = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'added', false);
  if (data) {
    const { index, notification } = data;
    const time = makePatDa(data.time);
    const timebox = state.notifications.get(time) || [];

    const arrIdx = timebox.findIndex(idxNotif =>
      notifIdxEqual(index, idxNotif.index)
    );
    if (arrIdx !== -1) {
      if(timebox[arrIdx]?.notification?.read) {
        state = updateNotificationStats(state, index, 'notifications', x => x+1);
      }
      timebox[arrIdx] = { index, notification };
      state.notifications.set(time, timebox);
    } else {
      updateNotificationStats(state, index, 'notifications', x => x+1);
      state.notifications.set(time, [...timebox, { index, notification }]);
    }
  }
  return state;
}

export const dnd = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'set-dnd', undefined);
  if (!_.isUndefined(data)) {
    state.doNotDisturb = data;
  }
  return state;
};

export const timebox = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'timebox', false);
  if (data) {
    const time = makePatDa(data.time);
    if (!data.archive) {
      state.notifications.set(time, data.notifications);
    }
  }
  return state;
};

export const more = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'more', false);
  if (data) {
    _.forEach(data, d => harkReducer(d));
  }
  return state;
}

export const read = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'read-note', false);
  if (data) {
    const { time, index } = data;
    state = updateNotificationStats(state, index, 'notifications', x => x-1);
    state = setRead(time, index, true, state);
  }
  return state;
}

export const unread = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'unread-note', false);
  if (data) {
    const { time, index } = data;
    state = updateNotificationStats(state, index, 'notifications', x => x+1);
    state = setRead(time, index, false, state);
  }
  return state;
}

export const archive = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'archive', false);
  if (data) {
    const { index } = data;
    const time = makePatDa(data.time);
    const timebox = state.notifications.get(time);
    if (!timebox) {
      console.warn('Modifying nonexistent timebox');
      return state;
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
    const newlyRead = archived.filter(x => !x.notification.read).length;
    return updateNotificationStats(state, index, 'notifications', x => x - newlyRead);
  }
  return state;
}

// TODO type message as some kind of mega combination of all possible message that can be received
export const harkReducer = (message) => {
  useHarkState.setState(
    compose([
      unread,
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
      readAll,
    ].map(reducer => reducer.bind(reducer, message['harkUpdate']))
    )(useHarkState.getState())
  );
};

export const harkSubscription = (channel: UrbitInterface): SubscriptionRequestInterface => {
  const event = harkReducer;
  const err = (message) => {
    console.error(message);
    channel.subscribe(harkSubscription(channel));
  };
  const quit = (message) => {
    console.error(message);
    channel.subscribe(harkSubscription(channel));
  };
  return {
    app: 'hark-store',
    path: '/updates',
    event, err, quit
  };
};

export const graphInitial = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.notificationsGraphConfig = data;
  }
}

export const graphListen = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'listen', false);
  if (data) {
    state.notificationsGraphConfig.watching = [
      ...state.notificationsGraphConfig.watching,
      data
    ];
  }
  return state;
}

export const graphIgnore = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'ignore', false);
  if (data) {
    state.notificationsGraphConfig.watching = state.notificationsGraphConfig.watching.filter(
      ({ graph, index }) => !(graph === data.graph && index === data.index)
    );
  }
  return state;
}

export const graphMentions = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'set-mentions', undefined);
  if (!_.isUndefined(data)) {
    state.notificationsGraphConfig.mentions = data;
  }
  return state;
}

export const graphWatchSelf = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'set-watch-on-self', undefined);
  if (!_.isUndefined(data)) {
    state.notificationsGraphConfig.watchOnSelf = data;
  }
  return state;
}

export const harkGraphHookReducer = (message) => {
  useHarkState.setState(
    compose([
      graphInitial,
      graphIgnore,
      graphListen,
      graphWatchSelf,
      graphMentions,
    ].map(reducer => reducer.bind(reducer, message['hark-graph-hook-update']))
    )(useHarkState.getState())
  );
};

export const harkGraphHookSubscription = (channel: UrbitInterface): SubscriptionRequestInterface => {
  const event = harkGraphHookReducer;
  const err = (message) => {
    console.error(message);
    channel.subscribe(harkGraphHookSubscription(channel));
  };
  const quit = (message) => {
    console.error(message);
    channel.subscribe(harkGraphHookSubscription(channel));
  };
  return {
    app: 'hark-graph-hook',
    path: '/updates',
    event, err, quit
  };
}

export const groupInitial = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.notificationsGroupConfig = data;
  }
  return state;
}



export const groupListen = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'listen', false);
  if (data) {
    state.notificationsGroupConfig = [...state.notificationsGroupConfig, data];
  }
  return state;
}

export const groupIgnore = (json: HarkUpdate, state: HarkState): HarkState => {
  const data = _.get(json, 'ignore', false);
  if (data) {
    state.notificationsGroupConfig = state.notificationsGroupConfig.filter(
      n => n !== data
    );
  }
  return state;
}

export const harkGroupHookReducer = (message) => {
  useHarkState.setState(
    compose([
      groupInitial,
      groupListen,
      groupIgnore,
    ].map(reducer => reducer.bind(reducer, message['hark-group-hook-update']))
    )(useHarkState.getState())
  );
};

export const harkGroupHookSubscription = (channel: UrbitInterface): SubscriptionRequestInterface => {
  const event = harkGroupHookReducer;
  const err = (message) => {
    console.error(message);
    channel.subscribe(harkGroupHookSubscription(channel));
  };
  const quit = (message) => {
    console.error(message);
    channel.subscribe(harkGroupHookSubscription(channel));
  };
  return {
    app: 'hark-group-hook',
    path: '/updates',
    event, err, quit
  };
}

export default harkSubscription;