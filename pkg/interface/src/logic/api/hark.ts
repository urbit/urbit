import BaseApi from "./base";
import { StoreState } from "../store/type";
import { dateToDa, decToUd } from "../lib/util";
import {NotifIndex, IndexedNotification} from "~/types";
import { BigInteger } from 'big-integer';
import {getParentIndex} from "../lib/notification";

export class HarkApi extends BaseApi<StoreState> {
  private harkAction(action: any): Promise<any> {
    return this.action("hark-store", "hark-action", action);
  }

  private graphHookAction(action: any) {
    return this.action("hark-graph-hook", "hark-graph-hook-action", action);
  }

  private groupHookAction(action: any) {
    return this.action("hark-group-hook", "hark-group-hook-action", action);
  }
  
  private chatHookAction(action: any) {
    return this.action("hark-chat-hook", "hark-chat-hook-action", action);
  }

  private actOnNotification(frond: string, intTime: BigInteger, index: NotifIndex) {
    const time = decToUd(intTime.toString());
    return this.harkAction({
      [frond]: {
        time,
        index
      }
    });
  }

  async setMentions(mentions: boolean) {
    await this.graphHookAction({
      'set-mentions': mentions
    });
    return this.chatHookAction({
      'set-mentions': mentions
    });
  }

  setWatchOnSelf(watchSelf: boolean) {
    return this.graphHookAction({
      'set-watch-on-self': watchSelf
    });
  }

  setDoNotDisturb(dnd: boolean) {
    return this.harkAction({
      'set-dnd': dnd
    });
  }

  archive(time: BigInteger, index: NotifIndex) {
    return this.actOnNotification('archive', time, index);
  }

  read(time: BigInteger, index: NotifIndex) {
    return this.actOnNotification('read', time, index);
  }

  readIndex(index: NotifIndex) {
    return this.harkAction({
      'read-index': index
    });
  }

  unread(time: BigInteger, index: NotifIndex) {
    return this.actOnNotification('unread', time, index);
  }

  seen() {
    return this.harkAction({ seen: null });
  }

  mute(notif: IndexedNotification) {
    if('graph' in notif.index && 'graph' in notif.notification.contents) {
      const { index } = notif;
      const parentIndex = getParentIndex(index.graph, notif.notification.contents.graph)
      if(!parentIndex) {
        return Promise.resolve();
      }
      return this.ignoreGraph(index.graph.graph, parentIndex);
    }
    if('group' in notif.index) {
      const { group } = notif.index.group;
      return this.ignoreGroup(group);
    }
    if('chat' in notif.index) {
      return this.ignoreChat(notif.index.chat.chat);
    }
    return Promise.resolve();
  }

  unmute(notif: IndexedNotification) {
    if('graph' in notif.index && 'graph' in notif.notification.contents) {
      const { index } = notif;
      const parentIndex = getParentIndex(index.graph, notif.notification.contents.graph)
      if(!parentIndex) {
        return Promise.resolve();
      }
      return this.listenGraph(index.graph.graph, parentIndex);
    }
    if('group' in notif.index) {
      return this.listenGroup(notif.index.group.group);
    }
    if('chat' in notif.index) {
      return this.listenChat(notif.index.chat.chat);
    }
    return Promise.resolve();
  }

  ignoreGroup(group: string) {
    return this.groupHookAction({
      ignore: group
    })
  }

  ignoreGraph(graph: string, index: string) {
    return this.graphHookAction({
      ignore: {
        graph,
        index
      }
    })
  }

  ignoreChat(chat: string) {
    return this.chatHookAction({
      ignore: chat
    });
  }


  listenGroup(group: string) {
    return this.groupHookAction({
      listen: group
    })
  }

  listenGraph(graph: string, index: string) {
    return this.graphHookAction({
      listen: {
        graph,
        index
      }
    })
  }

  listenChat(chat: string) {
    return this.chatHookAction({
      listen: chat
    });
  }

  getMore(archive = false) {
    const offset = this.store.state[
      archive ? 'archivedNotifications' : 'notifications'
    ].size;
    const count = 3;
    return this.getSubset(offset,count, archive);
  }

  async getSubset(offset:number, count:number, isArchive: boolean) {
    const where = isArchive ? 'archive' : 'inbox';
    const data = await this.scry("hark-store", `/recent/${where}/${offset}/${count}`);
    this.store.handleEvent({ data });
  }

  async getTimeSubset(start?: Date, end?: Date) {
    const s = start ? dateToDa(start) : "-";
    const e = end ? dateToDa(end) : "-";
    const result = await this.scry("hark-hook", `/recent/${s}/${e}`);
    this.store.handleEvent({
      data: result,
    });
  }
}
