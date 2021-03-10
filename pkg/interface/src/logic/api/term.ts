import BaseApi from './base';
import { StoreState } from '../store/type';

type Bork =
  | { aro: 'd' | 'l' | 'r' | 'u' }
  | { bac: null }
  | { del: null }
  | { hit: { r: number, c: number } }
  | { ret: null }

export type Bolt =
  | string
  | Bork

export type Belt =
  | Bork
  | { key: { mod: null | 'ctl' | 'met' | 'hyp', key: Bolt } }
  | { txt: Array<string> };

export default class TermApi extends BaseApi<StoreState> {
  public sendBelt(belt: Belt) {
    return this.action('herm', 'belt', belt);
  }
}
