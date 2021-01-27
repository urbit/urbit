import { Enc, Path, Patp, Poke } from "..";
import { Contact, ContactEdit, ContactUpdateCreate, ContactUpdateEdit, ContactUpdateRemove } from "./index.d";
import { GroupPolicy, Resource } from "../groups/index.d"

export const viewAction = <T>(data: T): Poke<T> => ({
  app: 'contact-view',
  mark: 'json',
  json: data
});

export const hookAction = <T>(data: T): Poke<T> => ({
  app: 'contact-hook',
  mark: 'contact-action',
  json: data
});

export const create = (
  name: string,
  policy: Enc<GroupPolicy>,
  title: string,
  description: string
): Poke<ContactUpdateCreate> => viewAction({ // TODO which type is correct?
  create: {
    name,
    policy,
    title,
    description
  }
});

export const share = (
  recipient: Patp,
  path: Patp,
  ship: Patp,
  contact: Contact
): Poke<any> => viewAction({ // TODO type
  share: {
    recipient,
    path,
    ship,
    contact
  }
});

export const remove = (
  path: Path,
  ship: Patp
): Poke<ContactUpdateRemove> => viewAction({
  remove: {
    path,
    ship
  }
});

export const edit = (
  path: Path,
  ship: Patp,
  editField: ContactEdit
): Poke<ContactUpdateEdit> => hookAction({
  edit: {
    path,
    ship,
    'edit-field': editField
  }
});

export const invite = (
  resource: Resource,
  ship: Patp,
  text: string = ''
): Poke<any> => viewAction({ // TODO type
  invite: {
    resource,
    ship,
    text
  }
});

export const join = (
  resource: Resource
): Poke<any> => viewAction({ // TODO type
  join: resource
});