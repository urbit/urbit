import BaseApi from "./base";
import { StoreState } from "../store/type";
import { BackgroundConfig, LocalUpdateRemoteContentPolicy } from "../types/local-update";

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

  setOmnibox() {
    this.store.handleEvent({
      data: {
        local: {
          omniboxShown: true
        },
      },
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

  setRemoteContentPolicy(policy: LocalUpdateRemoteContentPolicy) {
    this.store.handleEvent({
      data: {
        local: {
          remoteContentPolicy: policy
        }
      }
    });
  }

  dehydrate() {
    this.store.dehydrate();
  }

}
