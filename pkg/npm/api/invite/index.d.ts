import { Serial, PatpNoSig, Path } from '..';

export type InviteUpdate =
  InviteUpdateInitial
| InviteUpdateCreate
| InviteUpdateDelete
| InviteUpdateInvite
| InviteUpdateAccepted
| InviteUpdateDecline;

export interface InviteUpdateInitial {
  initial: Invites;
}

export interface InviteUpdateCreate {
  create: {
    path: Path;
  };
}

export interface InviteUpdateDelete {
  delete: {
    path: Path;
  };
}

export interface InviteUpdateInvite {
  invite: {
    path: Path;
    uid: Serial;
    invite: Invite;
  };
}

export interface InviteUpdateAccepted {
  accepted: {
    path: Path;
    uid: Serial;
  };
}

export interface InviteUpdateDecline {
  decline: {
    path: Path;
    uid: Serial;
  };
}

export type InviteAction =
  InviteActionAccept
| InviteActionDecline;

export interface InviteActionAccept {
  accept: {
    term: string,
    uid: Serial
  }
}

export interface InviteActionDecline {
  decline: {
    term: string,
    uid: Serial
  }
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
  path: Path;
  recipient: PatpNoSig;
  ship: PatpNoSig;
  text: string;
}
