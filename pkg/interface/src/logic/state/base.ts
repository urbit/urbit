import produce from "immer";
import { compose } from "lodash/fp";
import create, { State, UseStore } from "zustand";
import { persist } from "zustand/middleware";


export const stateSetter = <StateType>(
  fn: (state: StateType) => void,
  set
): void => {
  // fn = (state: StateType) => {
  //   // TODO this is a stub for the store debugging
  //   fn(state);
  // }
  return set(fn);
  // TODO we want to use the below, but it makes everything read-only
  return set(produce(fn));
};

export const reduceState = <
  StateType extends BaseState<StateType>,
  UpdateType
>(
  state: UseStore<StateType>,
  data: UpdateType,
  reducers: ((data: UpdateType, state: StateType) => StateType)[]
): void => {
  const oldState = state.getState();
  const reducer = compose(reducers.map(reducer => reducer.bind(reducer, data)));
  const newState = reducer(oldState);
  state.getState().set(state => state = newState);
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
  version: 1, // TODO version these according to base hash
}));