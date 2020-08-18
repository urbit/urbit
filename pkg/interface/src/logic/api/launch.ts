import BaseApi from './base';
import { StoreState } from '../store/type';


export default class LaunchApi extends BaseApi<StoreState> {

  add(name: string, tile = { basic : { title: '', linkedUrl: '', iconUrl: '' }}) {
    this.launchAction({ add: { name, tile } });
  }

  remove(name: string) {
    this.launchAction({ remove: name });
  }

  changeOrder(orderedTiles = []) {
    this.launchAction({ 'change-order': orderedTiles });
  }

  changeFirstTime(firstTime = true) {
    this.launchAction({ 'change-first-time': firstTime });
  }

  changeIsShown(name: string, isShown = true) {
    this.launchAction({ 'change-is-shown': { name, isShown }});
  }

  weather(latlng: any) {
    this.action('weather', 'json', latlng);
  }

  private launchAction(data) {
    this.action('launch', 'launch-action', data);
  }

}

