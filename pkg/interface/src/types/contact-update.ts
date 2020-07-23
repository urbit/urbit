import { Path, Patp } from "./noun";

export type ContactUpdate =
  | ContactUpdateCreate
  | ContactUpdateDelete
  | ContactUpdateAdd
  | ContactUpdateRemove
  | ContactUpdateEdit
  | ContactUpdateInitial
  | ContactUpdateContacts;

interface ContactUpdateCreate {
  create: Path;
}

interface ContactUpdateDelete {
  delete: Path;
}

interface ContactUpdateAdd {
  add: {
    path: Path;
    ship: Patp;
    contact: Contact;
  };
}

interface ContactUpdateRemove {
  remove: {
    path: Path;
    ship: Patp;
  };
}

interface ContactUpdateEdit {
  edit: {
    path: Path;
    ship: Patp;
    "edit-field": ContactEdit;
  };
}

interface ContactUpdateInitial {
  initial: Rolodex;
}

interface ContactUpdateContacts {
  contacts: {
    path: Path;
    contacts: Contacts;
  };
}

//

type ContactAvatar = ContactAvatarUrl | ContactAvatarOcts;

export type Rolodex = {
  [p in Path]: Contacts;
};

export type Contacts = {
  [p in Patp]: Contact;
};

interface ContactAvatarUrl {
  url: string;
}

interface ContactAvatarOcts {
  octs: string;
}
export interface Contact {
  nickname: string;
  email: string;
  phone: string;
  website: string;
  notes: string;
  color: string;
  avatar: string | null;
}

export type ContactEdit = {
  [k in keyof Contact]: Contact[k];
};
