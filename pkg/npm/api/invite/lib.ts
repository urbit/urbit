import { Poke, Serial } from "..";
import { InviteUpdate, InviteUpdateAccept, InviteUpdateDecline } from "./types";

export const inviteAction = <T extends InviteUpdate>(data: T): Poke<T> => ({
  app: 'invite-store',
  mark: 'invite-action',
  json: data
});

export const accept = (
  app: string,
  uid: Serial
): Poke<InviteUpdateAccept> => inviteAction({
  accept: {
    term: app,
    uid
  }
});

export const decline = (
  app: string,
  uid: Serial
): Poke<InviteUpdateDecline> => inviteAction({
  decline: {
    term: app,
    uid
  }
});
