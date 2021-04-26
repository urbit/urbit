import BaseApi from './base';
import { StoreState } from '../store/type';

export type Bolt =
  | string
  | { aro: 'd' | 'l' | 'r' | 'u' }
  | { bac: null }
  | { del: null }
  | { hit: { r: number, c: number } }
  | { ret: null }

export type Belt =
  | Bolt
  | { mod: { mod: 'ctl' | 'met' | 'hyp', key: Bolt } }
  | { txt: Array<string> };

export type Task =
  | { belt: Belt }
  | { blew: { w: number, h: number } }
  | { flow: { term: string, apps: Array<{ who: string, app: string }> } }
  | { hail: null }
  | { hook: null }

export default class TermApi extends BaseApi<StoreState> {
  public sendBelt(session: string, belt: Belt) {
    if (session === '') {
      //TODO  remove? reduntant, probably minimal perf gains
      return this.action('herm', 'belt', belt);
    } else {
      return this.sendTask(session, { 'belt': belt });
    }
  }

  public sendTask(session: string, task: Task) {
    return this.action('herm', 'herm-task', { 'session': session, ...task });
  }

  public getSessions(): Promise<Array<string>> {
    return this.scry<Array<string>>('herm', '/sessions');
  }
}
