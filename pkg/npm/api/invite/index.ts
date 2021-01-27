import { InviteAction, InviteActionAccept, InviteActionDecline } from "./index.d";
import { Poke, Serial } from "..";

export const action = <T>(data: T): Poke<T> => ({
  app: 'invite-store',
  mark: 'invite-action',
  json: data
});

export const accept = (
  app: string,
  uid: Serial
): Poke<InviteActionAccept> => action({
  accept: {
    term: app,
    uid
  }
});

export const decline = (
  app: string,
  uid: Serial
): Poke<InviteActionDecline> => action({
  decline: {
    term: app,
    uid
  }
});