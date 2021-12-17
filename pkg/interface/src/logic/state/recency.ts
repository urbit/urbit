import create from "zustand";
import _ from "lodash";
import { persist } from "zustand/middleware";
import produce from "immer";
import {
  clearStorageMigration,
  createStorageKey,
  storageVersion,
} from "../lib/util";

interface Recency {
  sorted: string[];
  visits: {
    [id: string]: number[];
  };
}

const places = ["graphs", "ships", "groups"] as const;
type Place = typeof places[number];

interface RecentsState {
  graphs: Recency;
  ships: Recency;
  groups: Recency;
  set: (f: (s: RecentsState) => void) => void;
}

function derive(
  old: RecentsState,
  updated: RecentsState
): Partial<RecentsState> {
  let res = {} as Partial<RecentsState>;
  function resort<T extends Place>(kind: T) {
    const upd = updated[kind];
    if (old[kind].visits === upd.visits) {
      return;
    }
    const now = Date.now() / 1000;
    const sec_in_week = 604_800;
    let sorted = _.take(
      Object.entries(upd.visits)
        .map(([id, dates]) => {
          let weighted = dates.reduce((acc, date) => {
            let diff = now - date;
            let weight = Math.exp(-1 * (diff / sec_in_week));
            return acc + weight;
          }, 0);
          return [id, weighted] as const;
        })
        .sort(([, a], [, b]) => b - a)
        .map(([id]) => id),
      20
    );
    let visits = _.pick(upd.visits, sorted);
    res[kind] = { sorted, visits };
  }
  places.forEach(resort);

  return res;
}

const emptyRecency = (): Recency => ({
  sorted: [],
  visits: {},
});

const useRecentsState = create<RecentsState>(
  persist(
    (set, get) => ({
      set: (f) => {
        const old = get();
        const updated = produce(old, f);
        const derived = derive(old, updated);
        set({ ...updated, ...derived });
      },
      addRecency: (place: Place, id: string) => {
        const state = get();
        const visits = state[place].visits[id] || [];
        const now = Date.now() / 1000;
        state.set(draft => {
          draft[place].visits[id] = [now, ...visits];
        });
      },
      graphs: emptyRecency(),
      ships: emptyRecency(),
      groups: emptyRecency(),
    }),
    {
      name: createStorageKey("recents"),
      version: storageVersion,
      migrate: clearStorageMigration,
    }
  )
);
