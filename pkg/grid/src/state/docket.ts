import fuzzy from 'fuzzy';
import create from 'zustand';
import produce from 'immer';
import { useCallback } from 'react';
import { omit } from 'lodash-es';
import api from './api';
import { Treaty, Dockets, Docket, Provider, Treaties, Providers } from './docket-types';
import { fakeRequest, mockProviders, mockTreaties } from './mock-data';

const useMockData = import.meta.env.MODE === 'mock';

interface ChargesResponse {
  initial: Dockets;
}

interface DocketState {
  charges: Dockets;
  treaties: Treaties;
  providers: Providers;
  fetchCharges: () => Promise<void>;
  requestTreaty: (ship: string, desk: string) => Promise<Treaty>;
  fetchProviders: (query?: string) => Promise<Provider[]>;
  fetchProviderTreaties: (provider: string) => Promise<Treaty[]>;
  toggleDocket: (desk: string) => Promise<void>;
  installDocket: (ship: string, desk: string) => Promise<number | void>;
  uninstallDocket: (desk: string) => Promise<number | void>;
}

const stableTreatyMap = new Map<string, Treaty[]>();

const useDocketState = create<DocketState>((set, get) => ({
  fetchCharges: async () => {
    const dockets = useMockData
      ? await fakeRequest(mockTreaties)
      : ((await (await fetch('/~/scry/docket/charges.json')).json()) as ChargesResponse).initial;

    const charges = normalizeDockets(dockets);

    set({ charges });
  },
  fetchProviders: async (query?: string) => {
    const providers = Object.values(mockProviders);
    const searchTexts = providers.map((p) => p.shipName + (p.nickname || ''));
    return fakeRequest(fuzzy.filter(query || '', searchTexts).map((el) => providers[el.index]));
  },
  fetchProviderTreaties: async (provider: string) => {
    const { treaties, providers } = get();
    const dev = providers[provider];
    const treatyList = Object.values(treaties);

    if (dev.treaties) {
      return dev.treaties.map((key) => treaties[key]);
    }

    if (!stableTreatyMap.has(provider)) {
      stableTreatyMap.set(
        provider,
        treatyList.filter(() => !!Math.round(Math.random()))
      );
    }

    const providerTreaties = stableTreatyMap.get(provider) || [];

    set(
      produce((draft: DocketState) => {
        if (!draft.providers[provider].treaties) {
          draft.providers[provider].treaties = [];
        }

        providerTreaties.forEach((treaty) => {
          // may need to do this when not mock data
          // const key = `${provider}/${treaty.desk}`;
          // draft.treaties[key] = treaty;
          draft.providers[provider].treaties?.push(treaty.desk);
        });
      })
    );

    return fakeRequest(providerTreaties);
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
    if (useMockData) {
      const treaties = await fakeRequest(mockTreaties);
      const docket = treaties[desk];
      set((state) => addCharge(state, { desk, docket }));
    }

    return api.poke({
      app: 'hood',
      mark: 'kiln-install',
      json: {
        ship,
        desk,
        local: desk
      }
    });
  },
  uninstallDocket: async (desk: string) => {
    if (useMockData) {
      set((state) => delCharge(state, desk));
    }

    return api.poke({
      app: 'docket',
      mark: 'docket-uninstall',
      json: desk
    });
  },
  toggleDocket: async (desk: string) => {
    set(
      produce((draft) => {
        const docket = draft.charges[desk];
        docket.status = docket.status === 'active' ? 'suspended' : 'active';
      })
    );
  },
  treaties: useMockData ? normalizeDockets(mockTreaties) : {},
  charges: {},
  providers: useMockData ? mockProviders : {},
  set
}));

function normalizeDocket<T extends Docket>(docket: T): T {
  return {
    ...docket,
    status: docket.status || 'active',
    color: `#${docket.color.slice(2).replace('.', '')}`.toUpperCase()
  };
}

function normalizeDockets<T extends Docket>(dockets: Record<string, T>): Record<string, T> {
  return Object.entries(dockets).reduce((obj: Record<string, T>, [key, value]) => {
    // eslint-disable-next-line no-param-reassign
    obj[key] = normalizeDocket(value);
    return obj;
  }, {});
}

interface AddDockEvent {
  'add-dock': {
    desk: string;
    docket: Docket;
  };
}

interface DelDockEvent {
  'del-dock': string;
}

type DocketEvent = AddDockEvent | DelDockEvent;

function addCharge(state: DocketState, { desk, docket }: AddDockEvent['add-dock']) {
  return { charges: { ...state.charges, [desk]: normalizeDocket(docket) } };
}

function delCharge(state: DocketState, desk: DelDockEvent['del-dock']) {
  return { charges: omit(state.charges, desk) };
}

api.subscribe({
  app: 'docket',
  path: '/charges',
  event: (data: DocketEvent) => {
    useDocketState.setState((state) => {
      console.log(data);

      if ('add-dock' in data) {
        return addCharge(state, data['add-dock']);
      }

      if ('del-dock' in data) {
        return delCharge(state, data['del-dock']);
      }

      return { charges: state.charges };
    });
  }
});

const selCharges = (s: DocketState) => {
  return omit(s.charges, 'grid');
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

export default useDocketState;
