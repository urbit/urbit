import { Enc, Path, Patp, Poke } from "..";
import {
  Contact,
  ContactUpdateAdd,
  ContactUpdateEdit,
  ContactUpdateRemove,
  ContactEditField,
  ContactShare,
  ContactUpdate,
} from "./index.d";

export const storeAction = <T extends ContactUpdate>(data: T): Poke<T> => ({
  app: "contact-store",
  mark: "contact-action",
  json: data,
});

export const add = (ship: Patp, contact: Contact): Poke<ContactUpdateAdd> => {
  contact["last-updated"] = Date.now();

  return storeAction({
    add: { ship, contact },
  });
};

export const remove = (ship: Patp): Poke<ContactUpdateRemove> =>
  storeAction({
    remove: { ship },
  });

export const share = (recipient: Patp): Poke<ContactShare> => ({
  app: "contact-push-hook",
  mark: "contact-action",
  json: { share: recipient },
});

export const edit = (
  path: Path,
  ship: Patp,
  editField: ContactEditField
): Poke<ContactUpdateEdit> =>
  storeAction({
    edit: {
      path,
      ship,
      "edit-field": editField,
      timestamp: Date.now(),
    },
  });

