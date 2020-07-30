import BaseApi from "./base";
import { StoreState } from "../store/type";

export default class LocalApi extends BaseApi<StoreState> {
  getBaseHash() {
    this.scry<string>('file-server', '/clay/base/hash').then(baseHash => {
      this.store.handleEvent({ data: { local: { baseHash } } });
    });
  }

  sidebarToggle() {
    this.store.handleEvent({
      data: {
        local: {
          sidebarToggle: true
        }
      }
    })
  }

  setDark(isDark: boolean) {
    this.store.handleEvent({
      data: {
        local: {
          setDark: isDark
        }
      }
    });
  }

}
