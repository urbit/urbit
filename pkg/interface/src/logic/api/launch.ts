import BaseApi from './base';
import { StoreState } from '../store/type';

export default class LaunchApi extends BaseApi<StoreState> {
  add(name: string, tile = { basic : { title: '', linkedUrl: '', iconUrl: '' } }) {
    return this.launchAction({ add: { name, tile } });
  }

  remove(name: string) {
    return this.launchAction({ remove: name });
  }

  changeFirstTime(firstTime = true) {
    return this.launchAction({ 'change-first-time': firstTime });
  }

  changeIsShown(name: string, isShown = true) {
    return this.launchAction({ 'change-is-shown': { name, isShown } });
  }

  weather(location: string) {
    return this.action('weather', 'json', location);
  }

  private launchAction(data) {
    return this.action('launch', 'launch-action', data);
  }
}

