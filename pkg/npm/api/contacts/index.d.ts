import { Path, Patp } from "..";
import {Resource} from "../groups/update.d";

export type ContactUpdate =
  | ContactUpdateAdd
  | ContactUpdateRemove
  | ContactUpdateEdit
  | ContactUpdateInitial

interface ContactUpdateAdd {
  add: {
    ship: Patp;
    contact: Contact;
  };
}

interface ContactUpdateRemove {
  remove: {
    ship: Patp;
  };
}

interface ContactUpdateEdit {
  edit: {
    path: Path;
    ship: Patp;
    "edit-field": ContactEditField;
    timestamp: number;
  };
}

interface ContactUpdateAllowShips {
  allow: {
    ships: Patp[];
  }
}

interface ContactUpdateAllowGroup {
  allow: {
    group: Path;
  }
}

interface ContactUpdateSetPublic {
  'set-public': boolean;
}

export interface ContactShare {
  share: Patp;
}

interface ContactUpdateInitial {
  initial: Rolodex;
}

export type Rolodex = {
  [p in Patp]: Contact;
};

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
