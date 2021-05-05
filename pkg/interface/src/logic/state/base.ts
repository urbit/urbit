import produce, { setAutoFreeze } from "immer";
import { compose } from "lodash/fp";
import create, { State, UseStore } from "zustand";
import { persist, devtools } from "zustand/middleware";

setAutoFreeze(false);


export const stateSetter = <StateType>(
  fn: (state: StateType) => void,
  set
): void => {
  set(produce(fn));
};

export const reduceState = <
  StateType extends BaseState<StateType>,
  UpdateType
>(
  state: UseStore<StateType>,
  data: UpdateType,
  reducers: ((data: UpdateType, state: StateType) => StateType)[]
): void => {
  const reducer = compose(reducers.map(r => sta => r(data, sta)));
  state.getState().set(state => {
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
  stateStorageKeys.forEach(key => {
    localStorage.removeItem(key);
  });
}

export interface BaseState<StateType> extends State {
  set: (fn: (state: StateType) => void) => void;
}

export const createState = <T extends {}>(
  name: string,
  properties: T,
  blacklist: string[] = []
): UseStore<T & BaseState<T>> => create(persist((set, get) => ({
  set: fn => stateSetter(fn, set),
  ...properties
}), {
  blacklist,
  name: stateStorageKey(name),
  version: process.env.LANDSCAPE_SHORTHASH as any
}));
