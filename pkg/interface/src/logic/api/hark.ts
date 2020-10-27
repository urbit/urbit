import BaseApi from "./base";
import { StoreState } from "../store/type";
import { dateToDa, decToUd } from "../lib/util";
import {NotifIndex} from "~/types";

export class HarkApi extends BaseApi<StoreState> {
  private harkAction(action: any): Promise<any> {
    return this.action("hark-store", "hark-action", action);
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

  private graphHookAction(action: any) {
    return this.action("hark-graph-hook", "hark-graph-hook-action", action);
  }

  setMentions(mentions: boolean) {
    return this.graphHookAction({
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

  async getTimeSubset(start?: Date, end?: Date) {
    const s = start ? dateToDa(start) : "-";
    const e = end ? dateToDa(end) : "-";
    const result = await this.scry("hark-hook", `/time-subset/${s}/${e}`);
    this.store.handleEvent({
      data: result,
    });
  }
}
