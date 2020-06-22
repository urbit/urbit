import BaseStore from "../store/base";
import BaseApi from "../api/base";
import { Path } from "../types/noun";

export default class BaseSubscription<S extends object> {
  constructor(public store: BaseStore<S>, public api: BaseApi<S>, public channel: any) {
    this.channel.setOnChannelError(this.onChannelError.bind(this));
  }

  delete() {
    this.channel.delete();
  }

  onChannelError(err) {
    console.error('event source error: ', err);
    setTimeout(() => {
      this.store.clear();
      this.start();
    }, 2000);
  }

  subscribe(path: Path, app: string) {
    return this.api.subscribe(path, 'PUT', this.api.ship, app,
      this.handleEvent.bind(this),
      (err) => {
        console.log(err);
        this.subscribe(path, app);
      },
      () => {
        this.subscribe(path, app);
      });
  }

  unsubscribe(id: number) {
    this.api.unsubscribe(id);
  }

  start() {
    // extend
  }

  handleEvent(diff) {
    // extend
    this.store.handleEvent(diff);
  }
}

