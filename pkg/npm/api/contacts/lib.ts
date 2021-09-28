
import { Path, Patp, Poke, resourceAsPath, Scry } from "../lib";
import {
  Contact,
  ContactUpdateAdd,
  ContactUpdateEdit,
  ContactUpdateRemove,
  ContactEditField,
  ContactShare,
  ContactUpdate,
  ContactUpdateAllowShips,
  ContactUpdateAllowGroup,
  ContactUpdateSetPublic,
} from "./types";

export const CONTACT_UPDATE_VERSION: number = 0;

const storeAction = <T extends ContactUpdate>(data: T, version: number = CONTACT_UPDATE_VERSION): Poke<T> => ({
  app: "contact-store",
  mark: `contact-update-${version}`,
  json: data,
});

export { storeAction as contactStoreAction };

export const addContact = (ship: Patp, contact: Contact): Poke<ContactUpdateAdd> => {
  contact["last-updated"] = Date.now();

  return storeAction({
    add: { ship, contact },
  });
};

export const removeContact = (ship: Patp): Poke<ContactUpdateRemove> =>
  storeAction({
    remove: { ship },
  });

export const share = (recipient: Patp, version: number = CONTACT_UPDATE_VERSION): Poke<ContactShare> => ({
  app: "contact-push-hook",
  mark: "contact-share",
  json: { share: recipient },
});

export const editContact = (
  ship: Patp,
  editField: ContactEditField
): Poke<ContactUpdateEdit> =>
  storeAction({
    edit: {
      ship,
      "edit-field": editField,
      timestamp: Date.now(),
    },
  });

export const allowShips = (
  ships: Patp[]
): Poke<ContactUpdateAllowShips> => storeAction({
  allow: {
    ships
  }
});

export const allowGroup = (
  ship: string,
  name: string
): Poke<ContactUpdateAllowGroup> => storeAction({
  allow: {
    group: resourceAsPath({ ship, name })
  }
});

export const setPublic = (
  setPublic: any
): Poke<ContactUpdateSetPublic> => {
  return storeAction({
    'set-public': setPublic
  });
}

export const retrieve = (
  ship: string
) => {
  const resource = { ship, name: '' };
  return {
    app: 'contact-pull-hook',
    mark: 'pull-hook-action',
    json: {
      add: {
        resource,
        ship
      }
    }
  };
}

export const fetchIsAllowed = (
  entity: string,
  name: string,
  ship: string,
  personal: boolean
): Scry => {
  const isPersonal = personal ? 'true' : 'false';
  return {
    app: 'contact-store',
    path: `/is-allowed/${entity}/${name}/${ship}/${isPersonal}`
  }
};
