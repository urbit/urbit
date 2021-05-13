import { Path } from '@urbit/api';
import BaseApi from '../api/base';
import BaseStore from '../store/base';

export default class BaseSubscription<S extends object> {
  private errorCount = 0;
  constructor(public store: BaseStore<S>, public api: BaseApi<S>, public channel: any) {
    this.channel.setOnChannelError(this.onChannelError.bind(this));
    this.channel.setOnChannelOpen(this.onChannelOpen.bind(this));
  }

  clearQueue() {
    this.channel.clearQueue();
  }

  delete() {
    this.channel.delete();
  }

  // Exists to allow subclasses to hook
  restart() {
    this.handleEvent({ data: { connection: 'reconnecting' } });
    this.start();
  }

  onChannelOpen(e: any) {
    this.errorCount = 0;
    this.handleEvent({ data: { connection: 'connected' } });
  }

  onChannelError(err) {
    console.error('event source error: ', err);
    this.errorCount++;
    if(this.errorCount >= 5) {
      console.error('bailing out, too many retries');
      this.handleEvent({ data: { connection: 'disconnected' } });
      return;
    }
    this.handleEvent({ data: { connection: 'reconnecting' } });
    setTimeout(() => {
      this.restart();
    }, Math.pow(2,this.errorCount - 1) * 750);
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

