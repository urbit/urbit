import { Docket, Treaties, Treaty } from '@urbit/api';
import { useCallback } from 'react';
import create from 'zustand';
import airlock from '~/logic/api';

interface DocketState {
  treaties: Treaties;
  requestTreaty: (ship: string, desk: string) => Promise<Treaty>;
}

const useDocketState = create<DocketState>((set, get) => ({
  requestTreaty: async (ship: string, desk: string) => {
    const { treaties } = get();
    const key = `${ship}/${desk}`;
    if (key in treaties) {
      return treaties[key];
    }

    const result = await airlock.subscribeOnce('treaty', `/treaty/${key}`, 20000);
    const treaty = { ...normalizeDocket(result, desk), ship };
    set(state => ({
      treaties: { ...state.treaties, [key]: treaty }
    }));
    return treaty;
  },
  treaties: {},
  set
}));

function normalizeDocket<T extends Docket>(docket: T, desk: string): T {
  const color = docket?.color?.startsWith('#')
    ? docket.color
    : `#${docket.color.slice(2).replace('.', '')}`.toUpperCase();

  return {
    ...docket,
    desk,
    color
  };
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

export default useDocketState;
