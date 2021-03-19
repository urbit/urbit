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

export default class TermApi extends BaseApi<StoreState> {
  public sendBelt(belt: Belt) {
    return this.action('herm', 'belt', belt);
  }
}
