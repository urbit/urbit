import BaseApi from "./base";
import { StoreState } from "../store/type";
import { Serial, Path } from "~/types/noun";

export default class InviteApi extends BaseApi<StoreState> {
  accept(app: string, uid: Serial) {
    return this.inviteAction({
      accept: {
        term: app,
        uid
      }
    });
  }

  decline(app: string, uid: Serial) {
    return this.inviteAction({
      decline: {
        term: app,
        uid
      }
    });
  }

  private inviteAction(action) {
    return this.action('invite-store', 'invite-action', action);
  }
}
