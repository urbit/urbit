import { applyPatches, Patch, produceWithPatches, setAutoFreeze, enablePatches } from 'immer';
import { compose } from 'lodash/fp';
import _ from 'lodash';
import create, { GetState, SetState, UseStore } from 'zustand';
import { persist } from 'zustand/middleware';
import Urbit, { SubscriptionRequestInterface } from '@urbit/http-api';

setAutoFreeze(false);
enablePatches();

export const stateSetter = <T extends {}>(
  fn: (state: Readonly<T & BaseState<T>>) => void,
  set: (newState: T & BaseState<T>) => void,
  get: () => T & BaseState<T>
): void => {
  const old = get();
  const [state] = produceWithPatches(old, fn) as readonly [(T & BaseState<T>), any, Patch[]];
  // console.log(patches);
  set(state);
};

export const optStateSetter = <T extends {}>(
  fn: (state: T & BaseState<T>) => void,
  set: (newState: T & BaseState<T>) => void,
  get: () => T & BaseState<T>
): string => {
  const old = get();
  const id = _.uniqueId();
  const [state, ,patches] = produceWithPatches(old, fn) as readonly [(T & BaseState<T>), any, Patch[]];
  set({ ...state, patches: { ...state.patches, [id]: patches } });
  return id;
};

export const reduceState = <
  S extends {},
  U
>(
  state: UseStore<S & BaseState<S>>,
  data: U,
  reducers: ((data: U, state: S & BaseState<S>) => S & BaseState<S>)[]
): void => {
  const reducer = compose(reducers.map(r => sta => r(data, sta)));
  state.getState().set((state) => {
    reducer(state);
  });
};

export const reduceStateN = <
  S extends {},
  U
>(
  state: S & BaseState<S>,
  data: U,
  reducers: ((data: U, state: S & BaseState<S>) => S & BaseState<S>)[]
): void => {
  const reducer = compose(reducers.map(r => sta => r(data, sta)));
  state.set(reducer);
};

export const optReduceState = <S, U>(
  state: UseStore<S & BaseState<S>>,
  data: U,
  reducers: ((data: U, state: S & BaseState<S>) => BaseState<S> & S)[]
): string => {
  const reducer = compose(reducers.map(r => sta => r(data, sta)));
  return state.getState().optSet((state) => {
    reducer(state);
  });
};

export let stateStorageKeys: string[] = [];

export const stateStorageKey = (stateName: string) => {
  stateName = `Landscape${stateName}State`;
  stateStorageKeys = [...new Set([...stateStorageKeys, stateName])];
  return stateName;
};

(window as any).clearStates = () => {
  stateStorageKeys.forEach((key) => {
    localStorage.removeItem(key);
  });
};

export interface BaseState<StateType extends {}> {
  rollback: (id: string) => void;
  patches: {
    [id: string]: Patch[];
  };
  set: (fn: (state: BaseState<StateType>) => void) => void;
  addPatch: (id: string, ...patch: Patch[]) => void;
  removePatch: (id: string) => void;
  optSet: (fn: (state: BaseState<StateType>) => void) => string;
  initialize: (api: Urbit) => void;
}

export function createSubscription(app: string, path: string, e: (data: any) => void): SubscriptionRequestInterface {
  const request = {
    app,
    path,
    event: e,
    err: () => {},
    quit: () => {}
  };
  // TODO: err, quit handling (resubscribe?)
  return request;
}

export const createState = <T extends {}>(
  name: string,
  properties: T | ((set: SetState<T & BaseState<T>>, get: GetState<T & BaseState<T>>) => T),
  blacklist: (keyof BaseState<T> | keyof T)[] = [],
  subscriptions: ((set: SetState<T & BaseState<T>>, get: GetState<T & BaseState<T>>) => SubscriptionRequestInterface)[] = []
): UseStore<T & BaseState<T>> => create<T & BaseState<T>>(persist<T & BaseState<T>>((set, get) => ({
  initialize: (api: Urbit) => {
    subscriptions.forEach(sub => api.subscribe(sub(set, get)));
  },
  // @ts-ignore investigate zustand types
  set: fn => stateSetter(fn, set, get),
  optSet: (fn) => {
    return optStateSetter(fn, set, get);
  },
  patches: {},
  addPatch: (id: string, ...patch: Patch[]) => {
      // @ts-ignore investigate immer types
    set(({ patches }) => ({ patches: { ...patches, [id]: patch } }));
  },
  removePatch: (id: string) => {
      // @ts-ignore investigate immer types
    set(({ patches }) => ({ patches: _.omit(patches, id) }));
  },
  rollback: (id: string) => {
    set((state) => {
        const applying = state.patches[id];
        return { ...applyPatches(state, applying), patches: _.omit(state.patches, id) };
    });
  },
  ...(typeof properties === 'function' ? (properties as any)(set, get) : properties)
}), {
  blacklist,
  name: stateStorageKey(name),
  version: process.env.LANDSCAPE_SHORTHASH as any
}));

export async function doOptimistically<A, S extends {}>(state: UseStore<S & BaseState<S>>, action: A, call: (a: A) => Promise<any>, reduce: ((a: A, fn: S & BaseState<S>) => S & BaseState<S>)[]) {
  let num: string | undefined = undefined;
  try {
    num = optReduceState(state, action, reduce);
    await call(action);
    state.getState().removePatch(num);
  } catch (e) {
    console.error(e);
    if(num) {
      state.getState().rollback(num);
    }
  }
}
