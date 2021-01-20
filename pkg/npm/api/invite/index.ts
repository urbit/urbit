import { InviteAction, InviteActionAccept, InviteActionDecline } from "./types";
import { Poke, Serial } from "..";

const action = (data: any): Poke<"invite-action", InviteAction> => {
  return {
    app: 'invite-store',
    mark: 'invite-action',
    json: data
  }; 
}

const accept = (app: string, uid: Serial): Poke<"invite-action", InviteActionAccept> => {
  return action({
    accept: {
      term: app,
      uid
    }
  });
}

const decline = (app: string, uid: Serial): Poke<"invite-action", InviteActionDecline> => {
  return action({
    decline: {
      term: app,
      uid
    }
  });
}

export { action, accept, decline };