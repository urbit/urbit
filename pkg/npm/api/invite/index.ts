import { InviteUpdate, InviteUpdateAccept, InviteUpdateDecline } from "./index.d";
import { Poke, Serial } from "..";

export const action = <T extends InviteUpdate>(data: T): Poke<T> => ({
  app: 'invite-store',
  mark: 'invite-action',
  json: data
});

export const accept = (
  app: string,
  uid: Serial
): Poke<InviteUpdateAccept> => action({
  accept: {
    term: app,
    uid
  }
});

export const decline = (
  app: string,
  uid: Serial
): Poke<InviteUpdateDecline> => action({
  decline: {
    term: app,
    uid
  }
});
