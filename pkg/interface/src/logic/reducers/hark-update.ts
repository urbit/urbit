import {
  Notifications,
  NotifIndex,
  NotificationGraphConfig,
  GroupNotificationsConfig,
} from "~/types";
import { makePatDa } from "~/logic/lib/util";
import _ from "lodash";
import { StoreState } from "../store/type";

type HarkState = Pick<StoreState,
  "notificationsChatConfig"
  | "notificationsGroupConfig" 
  | "notificationsGraphConfig" 
  | "notifications"
  | "notificationsCount"
  | "archivedNotifications"
  | "unreads">;

export const HarkReducer = (json: any, state: HarkState) => {
  const data = _.get(json, "harkUpdate", false);
  if (data) {
    reduce(data, state);
  }
  const graphHookData = _.get(json, "hark-graph-hook-update", false);
  if (graphHookData) {
    graphInitial(graphHookData, state);
    graphIgnore(graphHookData, state);
    graphListen(graphHookData, state);
    graphWatchSelf(graphHookData, state);
    graphMentions(graphHookData, state);
  }
  const groupHookData = _.get(json, "hark-group-hook-update", false);
  if (groupHookData) {
    groupInitial(groupHookData, state);
    groupListen(groupHookData, state);
    groupIgnore(groupHookData, state);
  }

  const chatHookData = _.get(json, "hark-chat-hook-update", false);
  if(chatHookData) {

    chatInitial(chatHookData, state);
    chatListen(chatHookData, state);
    chatIgnore(chatHookData, state);

  }
};

function chatInitial(json: any, state: HarkState) {
  const data = _.get(json, "initial", false);
  if (data) {
    state.notificationsChatConfig = data;
  }
}


function chatListen(json: any, state: HarkState) {
  const data = _.get(json, "listen", false);
  if (data) {
    state.notificationsChatConfig = [...state.notificationsChatConfig, data];
  }
}

function chatIgnore(json: any, state: HarkState) {
  const data = _.get(json, "ignore", false);
  if (data) {
    state.notificationsChatConfig = state.notificationsChatConfig.filter(x => x !== data);
  }
}

function groupInitial(json: any, state: HarkState) {
  const data = _.get(json, "initial", false);
  if (data) {
    state.notificationsGroupConfig = data;
  }
}

function graphInitial(json: any, state: HarkState) {
  const data = _.get(json, "initial", false);
  if (data) {
    state.notificationsGraphConfig = data;
  }
}

function graphListen(json: any, state: HarkState) {
  const data = _.get(json, "listen", false);
  if (data) {
    state.notificationsGraphConfig.watching = [
      ...state.notificationsGraphConfig.watching,
      data,
    ];
  }
}

function graphIgnore(json: any, state: HarkState) {
  const data = _.get(json, "ignore", false);
  if (data) {
    state.notificationsGraphConfig.watching = state.notificationsGraphConfig.watching.filter(
      ({ graph, index }) => !(graph === data.graph && index === data.index)
    );
  }
}

function groupListen(json: any, state: HarkState) {
  const data = _.get(json, "listen", false);
  if (data) {
    state.notificationsGroupConfig = [...state.notificationsGroupConfig, data];
  }
}

function groupIgnore(json: any, state: HarkState) {
  const data = _.get(json, "ignore", false);
  if (data) {
    state.notificationsGroupConfig = state.notificationsGroupConfig.filter(
      (n) => n !== data
    );
  }
}

function graphMentions(json: any, state: HarkState) {
  const data = _.get(json, "set-mentions", undefined);
  if (!_.isUndefined(data)) {
    state.notificationsGraphConfig.mentions = data;
  }
}

function graphWatchSelf(json: any, state: HarkState) {
  const data = _.get(json, "set-watch-on-self", undefined);
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
}

function unreads(json: any, state: HarkState) {
  const data = _.get(json, 'unreads');
  if(data) {
    data.forEach(({ index, unread }) => {
      updateUnreads(state, index, x => x + unread);
    });
  }
}

function updateUnreads(state: HarkState, index: NotifIndex, f: (u: number) => number) {
      state.notificationsCount = f(state.notificationsCount);
      if('graph' in index) {
        const curr = state.unreads.graph[index.graph.graph] || 0;
        state.unreads.graph[index.graph.graph] = f(curr);
      } else if('group' in index) {
        const curr = state.unreads.group[index.group.group] || 0;
        state.unreads.group[index.group.group] = f(curr);
      } else if('chat' in index) {
        const curr = state.unreads.chat[index.chat.chat] || 0
        state.unreads.chat[index.chat.chat] = f(curr);
      } 
}

function added(json: any, state: HarkState) {
  const data = _.get(json, "added", false);
  if (data) {
    const { index, notification } = data;
    const time = makePatDa(data.time);
    const timebox = state.notifications.get(time) || [];

    const arrIdx = timebox.findIndex((idxNotif) =>
      notifIdxEqual(index, idxNotif.index)
    );
    if (arrIdx !== -1) {
      if(timebox[arrIdx]?.notification?.read) {
        updateUnreads(state, index, x => x+1);
      }
      timebox[arrIdx] = { index, notification };
      state.notifications.set(time, timebox);
    } else {
      updateUnreads(state, index, x => x+1);
      state.notifications.set(time, [...timebox, { index, notification }]);
    }
  }
}

const dnd = (json: any, state: HarkState) => {
  const data = _.get(json, "set-dnd", undefined);
  if (!_.isUndefined(data)) {
    state.doNotDisturb = data;
  }
};

const timebox = (json: any, state: HarkState) => {
  const data = _.get(json, "timebox", false);
  if (data) {
    const time = makePatDa(data.time);
    if (data.archive) {
      state.archivedNotifications.set(time, data.notifications);
    } else {
      state.notifications.set(time, data.notifications);
    }
  }
};

function more(json: any, state: HarkState) {
  const data = _.get(json, "more", false);
  if (data) {
    _.forEach(data, (d) => reduce(d, state));
  }
}

function notifIdxEqual(a: NotifIndex, b: NotifIndex) {
  if ("graph" in a && "graph" in b) {
    return (
      a.graph.graph === b.graph.graph &&
      a.graph.group === b.graph.group &&
      a.graph.module === b.graph.module &&
      a.graph.description === b.graph.description
    );
  } else if ("group" in a && "group" in b) {
    return (
      a.group.group === b.group.group &&
      a.group.description === b.group.description
    );
  } else if ("chat" in a && "chat" in b) {
    return a.chat.chat === b.chat.chat &&
      a.chat.mention === b.chat.mention;
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
    console.warn("Modifying nonexistent timebox");
    return;
  }
  const arrIdx = timebox.findIndex((idxNotif) =>
    notifIdxEqual(index, idxNotif.index)
  );
  if (arrIdx === -1) {
    console.warn("Modifying nonexistent index");
    return;
  }
  timebox[arrIdx].notification.read = read;
  state.notifications.set(patDa, timebox);
}

function read(json: any, state: HarkState) {
  const data = _.get(json, "read", false);
  if (data) {
    const { time, index } = data;
    updateUnreads(state, index, x => x-1);
    setRead(time, index, true, state);
  }
}

function unread(json: any, state: HarkState) {
  const data = _.get(json, "unread", false);
  if (data) {
    const { time, index } = data;
    updateUnreads(state, index, x => x+1);
    setRead(time, index, false, state);
  }
}

function archive(json: any, state: HarkState) {
  const data = _.get(json, "archive", false);
  if (data) {
    const { index } = data;
    const time = makePatDa(data.time);
    const timebox = state.notifications.get(time);
    if (!timebox) {
      console.warn("Modifying nonexistent timebox");
      return;
    }
    const [archived, unarchived] = _.partition(timebox, (idxNotif) =>
      notifIdxEqual(index, idxNotif.index)
    );
    state.notifications.set(time, unarchived);
    const archiveBox = state.archivedNotifications.get(time) || [];
    const readCount = archived.filter(
      ({ notification }) => !notification.read
    ).length;
    updateUnreads(state, index, x => x - readCount);
    state.archivedNotifications.set(time, [
      ...archiveBox,
      ...archived.map(({ notification, index }) => ({
        notification: { ...notification, read: true },
        index,
      })),
    ]);
  }
}
