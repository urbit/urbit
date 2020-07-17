import _ from "lodash";
import { uuid } from "../lib/util";
import { Patp, Path } from "../types/noun";
import BaseStore from '../store/base';

export default class BaseApi<S extends object = {}> {
  bindPaths: Path[] = [];
  constructor(public ship: Patp, public channel: any, public store: BaseStore<S>) {}

  unsubscribe(id: number) {
    this.channel.unsubscribe(id);

  }

  subscribe(path: Path, method, ship = this.ship, app: string, success, fail, quit) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    return this.channel.subscribe(
      this.ship,
      app,
      path,
      (err) => {
        fail(err);
      },
      (event) => {
        success({
          data: event,
          from: {
            ship,
            path,
          },
        });
      },
      (qui) => {
        quit(qui);
      }
    );
  }

  action(appl: string, mark: string, data: any): Promise<any> {
    return new Promise((resolve, reject) => {
      this.channel.poke(
        (window as any).ship,
        appl,
        mark,
        data,
        (json) => {
          resolve(json);
        },
        (err) => {
          reject(err);
        }
      );
    });
  }

  scry<T>(app: string, path: Path): Promise<T> {
    return fetch(`/~/scry/${app}${path}.json`).then(r => r.json() as Promise<T>);
  }
}
