import BaseApi from './base';
import { StoreState } from '../store/type';

export default class WalletApi extends BaseApi<StoreState> {

  bitcoin(data: any) {
    return this.action('wallet', 'json', data);
  }

  requestAddress(ship: any, network: any) {
    console.log(network);
    return this.action('wallet', 'bitcoin-action', {
      request: {
        ship: ship,
        network: network
      }
    })
  }

  addXPubKey(xpubkey: any) {
    // console.log(config, badges);
    return this.action('wallet', 'bitcoin-action', {
      add: xpubkey
    });
  }
}
