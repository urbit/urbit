import { Serial, PatpNoSig, Path } from '../lib';
import { Resource } from "../groups";

export type InviteUpdate =
  InviteUpdateInitial
| InviteUpdateCreate
| InviteUpdateDelete
| InviteUpdateInvite
| InviteUpdateAccept
| InviteUpdateAccepted
| InviteUpdateDecline;

export interface InviteUpdateAccept {
  accept: {
    term: string;
    uid: Serial;
  }
}

export interface InviteUpdateInitial {
  initial: Invites;
}

export interface InviteUpdateCreate {
  create: {
    term: string;
  };
}

export interface InviteUpdateDelete {
  delete: {
    term: string;
  };
}

export interface InviteUpdateInvite {
  invite: {
    term: string;
    uid: Serial;
    invite: Invite;
  };
}

export interface InviteUpdateAccepted {
  accepted: {
    term: string;
    uid: Serial;
  };
}

export interface InviteUpdateDecline {
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
