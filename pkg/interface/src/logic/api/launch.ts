import BaseApi from './base';
import { StoreState } from '../store/type';


export default class LaunchApi extends BaseApi<StoreState> {

  add(name: string, tile = { basic : { title: '', linkedUrl: '', iconUrl: '' }}) {
    return this.launchAction({ add: { name, tile } });
  }

  remove(name: string) {
    return this.launchAction({ remove: name });
  }

  changeOrder(orderedTiles: string[] = []) {
    return this.launchAction({ 'change-order': orderedTiles });
  }

  changeFirstTime(firstTime = true) {
    return this.launchAction({ 'change-first-time': firstTime });
  }

  changeIsShown(name: string, isShown = true) {
    return this.launchAction({ 'change-is-shown': { name, isShown }});
  }

  weather(latlng: any) {
    return this.action('weather', 'json', latlng);
  }

  private launchAction(data) {
    return this.action('launch', 'launch-action', data);
  }

}

