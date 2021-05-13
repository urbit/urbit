import produce, { applyPatches, Patch, produceWithPatches, setAutoFreeze, enablePatches } from 'immer';
import { compose } from 'lodash/fp';
import _ from 'lodash';
import create, { UseStore } from 'zustand';
import { persist } from 'zustand/middleware';

setAutoFreeze(false);
enablePatches();

export const stateSetter = <T extends {}>(
  fn: (state: Readonly<T & BaseState<T>>) => void,
  set: (newState: T & BaseState<T>) => void
): void => {
  set(produce(fn) as any);
};

export const optStateSetter = <T extends {}>(
  fn: (state: T & BaseState<T>) => void,
  set: (newState: T & BaseState<T>) => void,
  get: () => T & BaseState<T>
): string => {
  const old = get();
  const id = _.uniqueId()
  const [state, ,patches] = produceWithPatches(old, fn) as readonly [(T & BaseState<T>), any, Patch[]];
  set({ ...state, patches: { ...state.patches, [id]: patches }});
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

export interface BaseState<StateType> {
  rollback: (id: string) => void;
  patches: {
    [id: string]: Patch[];
  };
  set: (fn: (state: BaseState<StateType>) => void) => void;
  addPatch: (id: string, ...patch: Patch[]) => void;
  removePatch: (id: string) => void;
  optSet: (fn: (state: BaseState<StateType>) => void) => string;
}

export const createState = <T extends {}>(
  name: string,
  properties: T,
  blacklist: (keyof BaseState<T> | keyof T)[] = []
): UseStore<T & BaseState<T>> => create<T & BaseState<T>>(persist<T & BaseState<T>>((set, get) => ({
  set: fn => stateSetter(fn, set),
  optSet: fn => {
    return optStateSetter(fn, set, get);
  },
  patches: {},
  addPatch: (id: string, ...patch: Patch[]) => {
    set(({ patches }) => ({ patches: {...patches, [id]: patch }}));
  },
  removePatch: (id: string) => {
    set(({ patches }) => ({ patches: _.omit(patches, id)}));
  }, 
  rollback: (id: string) => {
    set(state => {
        const applying = state.patches[id]
        return {...applyPatches(state, applying), patches: _.omit(state.patches, id) }
    });
  },
  ...properties
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
    state.getState().removePatch(num)
  } catch (e) {
    console.error(e);
    if(num) {
      state.getState().rollback(num);
    }
  }
}
