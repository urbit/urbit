import BaseApi from './base';
import { StoreState } from '../store/type';

export type Belt =
  | { aro: 'd' | 'l' | 'r' | 'u' }
  | { bac: null }
  | { ctl: string }
  | { del: null }
  | { hit: { r: number, c: number } }
  | { met: string }
  | { ret: null }
  | { txt: Array<string> };

export default class TermApi extends BaseApi<StoreState> {
  public sendBelt(belt: Belt) {
    return this.action('herm', 'belt', belt);
  }
}
