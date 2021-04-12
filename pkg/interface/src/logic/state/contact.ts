import { Patp, Rolodex, Scry } from "@urbit/api";

import { BaseState, createState } from "./base";

export interface ContactState extends BaseState<ContactState> {
  contacts: Rolodex;
  isContactPublic: boolean;
  nackedContacts: Set<Patp>;
  // fetchIsAllowed: (entity, name, ship, personal) => Promise<boolean>;
};

const useContactState = createState<ContactState>('Contact', {
  contacts: {},
  nackedContacts: new Set(),
  isContactPublic: false,
  // fetchIsAllowed: async (
  //   entity,
  //   name,
  //   ship,
  //   personal
  // ): Promise<boolean> => {
  //   const isPersonal = personal ? 'true' : 'false';
  //   const api = useApi();
  //   return api.scry({
  //     app: 'contact-store',
  //     path: `/is-allowed/${entity}/${name}/${ship}/${isPersonal}`
  //   });
  // },
}, ['nackedContacts']);

export default useContactState;