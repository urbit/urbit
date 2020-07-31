import BaseApi from "./base";
import { StoreState } from "../store/type";
import { SelectedGroup, BackgroundConfig } from "../types/local-update";

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

  setBackground(backgroundConfig: BackgroundConfig) {
    this.store.handleEvent({
      data: {
        local: {
          backgroundConfig
        }
      }
    });
  }

  hideAvatars(hideAvatars: boolean) {
    this.store.handleEvent({
      data: {
        local: {
          hideAvatars
        }
      }
    });
  }

  hideNicknames(hideNicknames: boolean) {
    this.store.handleEvent({
      data: {
        local: {
          hideNicknames
        }
      }
    });
  }

  dehydrate() {
    this.store.dehydrate();
  }

}
