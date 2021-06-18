import { Serial } from '@urbit/api';
import { StoreState } from '../store/type';
import BaseApi from './base';

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
