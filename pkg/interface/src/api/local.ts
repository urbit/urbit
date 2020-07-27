import BaseApi from "./base";
import { StoreState } from "../store/type";
import { SelectedGroup } from "../types/local-update";

export default class LocalApi extends BaseApi<StoreState> {
  getBaseHash() {
    this.scry<string>('file-server', '/clay/base/hash').then(baseHash => {
      this.store.handleEvent({ data: { local: { baseHash } } });
    });
  }

  setSelected(selected: SelectedGroup[]) {
    this.store.handleEvent({
      data: {
        local: {
          selected
        }
      }
    })
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
