import BaseApi from "./base";
import { StoreState } from "../store/type";
import { dateToDa, decToUd } from "../lib/util";
import {NotifIndex} from "~/types";
import { BigInteger } from 'big-integer';

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

  unread(time: BigInteger, index: NotifIndex) {
    return this.actOnNotification('unread', time, index);
  }

  seen() {
    return this.harkAction({ seen: null });
  }

  mute(index: NotifIndex) {
    if('graph' in index) {
      const { graph } = index.graph;
      return this.ignoreGraph(graph);
    }
    if('group' in index) {
      const { group } = index.group;
      return this.ignoreGroup(group);
    }
    if('chat' in index) {
      return this.ignoreChat(index.chat);
    }
    return Promise.resolve();
  }

  unmute(index: NotifIndex) {
    if('graph' in index) {
      return this.listenGraph(index.graph.graph);
    }
    if('group' in index) {
      return this.listenGroup(index.group.group);
    }
    if('chat' in index) {
      return this.listenChat(index.chat);
    }
    return Promise.resolve();
  }

  ignoreGroup(group: string) {
    return this.groupHookAction({
      ignore: group
    })
  }

  ignoreGraph(graph: string) {
    return this.graphHookAction({
      ignore: graph
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

  listenGraph(graph: string) {
    return this.graphHookAction({
      listen: graph
    })
  }

  listenChat(chat: string) {
    return this.chatHookAction({
      listen: chat
    });
  }

  async getTimeSubset(start?: Date, end?: Date) {
    const s = start ? dateToDa(start) : "-";
    const e = end ? dateToDa(end) : "-";
    const result = await this.scry("hark-hook", `/time-subset/${s}/${e}`);
    this.store.handleEvent({
      data: result,
    });
  }
}
