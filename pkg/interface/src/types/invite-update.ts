import { Serial, PatpNoSig, Path } from './noun';
import {Resource} from './group-update';

export type InviteUpdate =
  InviteUpdateInitial
| InviteUpdateCreate
| InviteUpdateDelete
| InviteUpdateInvite
| InviteUpdateAccepted
| InviteUpdateDecline;


interface InviteUpdateInitial {
  initial: Invites;
}

interface InviteUpdateCreate {
  create: {
    path: Path;
  };
}

interface InviteUpdateDelete {
  delete: {
    path: Path;
  };
}

interface InviteUpdateInvite {
  invite: {
    path: Path;
    uid: Serial;
    invite: Invite;
  };
}

interface InviteUpdateAccepted {
  accepted: {
    path: Path;
    uid: Serial;
  };
}

interface InviteUpdateDecline {
  decline: {
    path: Path;
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
