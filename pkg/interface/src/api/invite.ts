import BaseApi from "./base";
import { StoreState } from "../store/type";
import { Serial, Path } from "../types/noun";

export default class InviteApi extends BaseApi<StoreState> {
  accept(app: Path, uid: Serial) {
    return this.inviteAction({
      accept: {
        path: app,
        uid
      }
    });
  }

  decline(app: Path, uid: Serial) {
    return this.inviteAction({
      decline: {
        path: app,
        uid
      }
    });
  }

  private inviteAction(action) {
    return this.action('invite-store', 'json', action);
  }
}
