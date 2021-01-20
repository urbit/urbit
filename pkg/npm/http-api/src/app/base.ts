import Urbit from '..';

export interface UrbitAppInterface {
  airlock: Urbit;
  app: string;
}

export default class UrbitApp implements UrbitAppInterface {
  airlock: Urbit;

  get app(): string {
    throw new Error('Access app property on base UrbitApp');
  }

  constructor(airlock: Urbit) {
    this.airlock = airlock;
  }

  /**
   * Getter that barfs if no ship has been passed
   */
  get ship(): string {
    if (!this.airlock.ship) {
      throw new Error('No ship specified');
    }
    return this.airlock.ship;
  }

  /**
   * Helper to allow any app to handle subscriptions.
   * 
   * @param path Path on app to subscribe to
   */
  subscribe(path: string) {
    const ship = this.ship;
    const app = this.app;
    return this.airlock.subscribe(app, path);
  }
  // TODO handle methods that don't exist
}
