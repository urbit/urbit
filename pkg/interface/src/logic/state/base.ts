import produce from "immer";
import { compose } from "lodash/fp";
import create, { State, UseStore } from "zustand";
import { persist, devtools } from "zustand/middleware";


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

export const createState = <StateType extends BaseState<any>>(
  name: string,
  properties: Omit<StateType, 'set'>,
  blacklist: string[] = []
): UseStore<StateType> => create(persist((set, get) => ({
  // TODO why does this typing break?
  set: fn => stateSetter(fn, set),
  ...properties
}), {
  blacklist,
  name: stateStorageKey(name),
  version: process.env.LANDSCAPE_SHORTHASH
}));
