import create from 'zustand';
import produce from 'immer';
import { useCallback, useEffect } from 'react';
import { omit } from 'lodash-es';
import api from './api';
import { mockAllies, mockCharges, mockTreaties } from './mock-data';
import {
  Allies,
  Charge,
  Charges,
  ChargeUpdateInitial,
  scryAllies,
  scryAllyTreaties,
  scryCharges,
  Treaty,
  Docket,
  Treaties,
  chadIsRunning,
  AllyUpdateIni,
  TreatyUpdateIni,
  docketInstall,
  ChargeUpdate
} from '@urbit/api/docket';
import _ from 'lodash';
import {kilnRevive, kilnSuspend} from '@urbit/api/hood';

const useMockData = import.meta.env.MODE === 'mock';

interface DocketState {
  charges: Charges;
  treaties: Treaties;
  allies: Allies;
  fetchCharges: () => Promise<void>;
  requestTreaty: (ship: string, desk: string) => Promise<Treaty>;
  fetchAllies: () => Promise<Allies>;
  fetchAllyTreaties: (ally: string) => Promise<Treaties>;
  toggleDocket: (desk: string) => Promise<void>;
  installDocket: (ship: string, desk: string) => Promise<number | void>;
  uninstallDocket: (desk: string) => Promise<number | void>;
}

async function fakeRequest<T>(data: T, time = 300): Promise<T> {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve(data);
    }, time);
  });
}

const useDocketState = create<DocketState>((set, get) => ({
  fetchCharges: async () => {
    const charg = useMockData
      ? await fakeRequest(mockCharges)
      : (await api.scry<ChargeUpdateInitial>(scryCharges)).initial;

    const charges = Object.entries(charg).reduce((obj: Charges, [key, value]) => {
      // eslint-disable-next-line no-param-reassign
      obj[key] = normalizeDocket<Charge>(value);
      return obj;
    }, {});

    set({ charges });
  },
  fetchAllies: async () => {
    const allies = useMockData ? mockAllies : (await api.scry<AllyUpdateIni>(scryAllies)).ini;
    set({ allies });
    return allies;
  },
  fetchAllyTreaties: async (ally: string) => {
    let treaties = useMockData ? mockTreaties : (await api.scry<TreatyUpdateIni>(scryAllyTreaties(ally))).ini;
    treaties = _.mapValues(treaties, normalizeDocket);
    set((s) => ({ treaties: { ...s.treaties, ...treaties } }));
    return treaties;
  },
  requestTreaty: async (ship: string, desk: string) => {
    const { treaties } = get();
    if (useMockData) {
      set({ treaties: await fakeRequest(treaties) });
      return treaties[desk];
    }

    const key = `${ship}/${desk}`;
    if (key in treaties) {
      return treaties[key];
    }

    const result = await api.subscribeOnce('docket', `/treaty/${key}`, 20000);
    const treaty = { ...normalizeDocket(result), ship, desk };
    set((state) => ({
      treaties: { ...state.treaties, [key]: treaty }
    }));
    return treaty;
  },
  installDocket: async (ship: string, desk: string) => {
    const treaty = get().treaties[`${ship}/${desk}`];
    if (!treaty) {
      throw new Error('Bad install');
    }
    if (useMockData) {

      set((state) => addCharge(state, desk, {...treaty, chad: { install: null }}));
      await new Promise<void>((res) => setTimeout(() => res(), 5000));
      set((state) => addCharge(state, desk, {...treaty, chad: { glob: null }}));
    }

    return api.poke(docketInstall(ship, desk));
  },
  uninstallDocket: async (desk: string) => {
    set((state) => delCharge(state, desk));
    if (useMockData) {
      return;
    }
    await api.poke({
      app: 'docket',
      mark: 'docket-uninstall',
      json: desk
    });
  },
  toggleDocket: async (desk: string) => {
    if(useMockData) {
      set(
        produce((draft) => {
          const charge = draft.charges[desk];
          charge.chad = chadIsRunning(charge.chad) ? { suspend: null } : { glob: null };
        })
      );
    }
    const { charges } = get();
    const charge = charges[desk];
    if(!charge) {
      return;
    }
    const suspended = 'suspend' in charge.chad;
    if(suspended) {
      await api.poke(kilnRevive(desk));
    } else {
      await api.poke(kilnSuspend(desk));
    }
  },
  treaties: useMockData ? normalizeDockets(mockTreaties) : {},
  charges: {},
  allies: useMockData ? mockAllies : {},
  set
}));

function normalizeDocket<T extends Docket>(docket: T): T {
  const color = docket?.color?.startsWith('#')
    ? docket.color
    : `#${docket.color.slice(2).replace('.', '')}`.toUpperCase();

  return {
    ...docket,
    color
  };
}

function normalizeDockets<T extends Docket>(dockets: Record<string, T>): Record<string, T> {
  return Object.entries(dockets).reduce((obj: Record<string, T>, [key, value]) => {
    // eslint-disable-next-line no-param-reassign
    obj[key] = normalizeDocket(value);
    return obj;
  }, {});
}

function addCharge(state: DocketState, desk: string, charge: Charge) {
  return { charges: { ...state.charges, [desk]: normalizeDocket(charge) } };
}

function delCharge(state: DocketState, desk: string) {
  return { charges: omit(state.charges, desk) };
}

api.subscribe({
  app: 'docket',
  path: '/charges',
  event: (data: ChargeUpdate) => {
    useDocketState.setState((state) => {

      if ('add-charge' in data) {
        const { desk, charge } = data['add-charge']
        return addCharge(state, desk, charge)
      }

      if ('del-charge' in data) {
        const desk = data['del-charge'];
        return delCharge(state, desk)
      }

      return { charges: state.charges };
    });
  }
});

const selCharges = (s: DocketState) => {
  return s.charges;
};

export function useCharges() {
  return useDocketState(selCharges);
}

export function useCharge(desk: string) {
  return useDocketState(useCallback((state) => state.charges[desk], [desk]));
}

const selRequest = (s: DocketState) => s.requestTreaty;
export function useRequestDocket() {
  return useDocketState(selRequest);
}

const selAllies = (s: DocketState) => s.allies;
export function useAllies() {
  return useDocketState(selAllies);
}

export function useAllyTreaties(ship: string) {
  useEffect(() => {
    useDocketState.getState().fetchAllyTreaties(ship);
  }, [ship]);

  return useDocketState(
    useCallback(
      (s) => {
        const charter = s.allies[ship];
        return _.pick(s.treaties, ...(charter || []));
      },
      [ship]
    )
  );
}

export function useTreaty(host: string, desk: string) {
  return useDocketState(
    useCallback(
      (s) => {
        const ref = `${host}/${desk}`;
        return s.treaties[ref];
      },
      [host, desk]
    )
  );
}

// xx useful for debugging
//window.docket = useDocketState.getState;

export default useDocketState;
