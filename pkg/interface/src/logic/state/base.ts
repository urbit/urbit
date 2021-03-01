import create, { State, UseStore } from "zustand";
import { persist } from "zustand/middleware";
import { stateSetter, stateStorageKey } from "../lib/util";

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