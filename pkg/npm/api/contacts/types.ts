import { Path, Patp } from "..";
import { Resource } from "../groups";

export type ContactUpdate =
  | ContactUpdateAdd
  | ContactUpdateRemove
  | ContactUpdateEdit
  | ContactUpdateInitial
  | ContactUpdateAllowGroup
  | ContactUpdateAllowShips
  | ContactUpdateSetPublic;

  export interface ContactUpdateAdd {
  add: {
    ship: Patp;
    contact: Contact;
  };
}

export interface ContactUpdateRemove {
  remove: {
    ship: Patp;
  };
}

export interface ContactUpdateEdit {
  edit: {
    ship: Patp;
    "edit-field": ContactEditField;
    timestamp: number;
  };
}

export interface ContactUpdateAllowShips {
  allow: {
    ships: Patp[];
  }
}

export interface ContactUpdateAllowGroup {
  allow: {
    group: Path;
  }
}

export interface ContactUpdateSetPublic {
  'set-public': boolean;
}

export interface ContactShare {
  share: Patp;
}

export interface ContactUpdateInitial {
  initial: Rolodex;
}

export type Rolodex = {
  [p in Patp]: Contact;
};

export type Contacts = Rolodex;

export interface Contact {
  nickname: string;
  bio: string;
  status: string;
  color: string;
  avatar: string | null;
  cover: string | null;
  groups: Path[];
  'last-updated': number;
}

type ContactKeys = keyof Contact;

export type ContactEditFieldPrim = Exclude<ContactKeys, "groups" | "last-updated">;

export type ContactEditField = Partial<Pick<Contact, ContactEditFieldPrim>> & {
  'add-group'?: Resource;
  'remove-group'?: Resource;
};
