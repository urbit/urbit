import { Serial, PatpNoSig, Path } from '..';
import { Resource } from "../groups/update.d";

export type InviteUpdate =
  InviteUpdateInitial
| InviteUpdateCreate
| InviteUpdateDelete
| InviteUpdateInvite
| InviteUpdateAccept
| InviteUpdateAccepted
| InviteUpdateDecline;

interface InviteUpdateAccept {
  accept: {
    term: string;
    uid: Serial;
  }
}

interface InviteUpdateInitial {
  initial: Invites;
}

interface InviteUpdateCreate {
  create: {
    term: string;
  };
}

interface InviteUpdateDelete {
  delete: {
    term: string;
  };
}

interface InviteUpdateInvite {
  invite: {
    term: string;
    uid: Serial;
    invite: Invite;
  };
}

interface InviteUpdateAccepted {
  accepted: {
    term: string;
    uid: Serial;
  };
}

interface InviteUpdateDecline {
  decline: {
    term: string;
    uid: Serial;
  };
}

// actual datastructures


export type Invites = {
  [p in Path]: AppInvites;
};

export type AppInvites = {
  [s in Serial]: Invite;
};

export interface Invite {
  app: string;
  recipient: PatpNoSig;
  resource: Resource;
  ship: PatpNoSig;
  text: string;
}
