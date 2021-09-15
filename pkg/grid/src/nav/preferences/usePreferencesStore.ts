import create from 'zustand';
import { fakeRequest } from '../../state/util';

const useMockData = import.meta.env.MODE === 'mock';

interface PreferencesStore {
  theme: 'light' | 'dark' | 'automatic';
  currentTheme: 'light' | 'dark';
  otasEnabled: boolean;
  otaSource: string;
  doNotDisturb: boolean;
  mentions: boolean;
  setOTASource: (source: string) => Promise<void>;
  toggleOTAs: () => Promise<void>;
  toggleDoNotDisturb: () => Promise<void>;
  toggleMentions: () => Promise<void>;
}

export const usePreferencesStore = create<PreferencesStore>((set) => ({
  theme: 'automatic',
  currentTheme: 'light',
  otasEnabled: true,
  otaSource: useMockData ? '~sabbus' : '',
  doNotDisturb: false,
  mentions: true,
  /**
   * a lot of these are repetitive, we may do better with a map of settings
   * and some generic way to update them through pokes. That way, we could
   * just have toggleSetting(key) and run a similar op for all
   */
  toggleOTAs: async () => {
    if (useMockData) {
      await fakeRequest({});
      set((state) => ({ otasEnabled: !state.otasEnabled }));
    }
  },
  setOTASource: async (source: string) => {
    if (useMockData) {
      await fakeRequest({});
      set({ otaSource: source });
    }
  },
  toggleDoNotDisturb: async () => {
    if (useMockData) {
      await fakeRequest({});
      set((state) => ({ doNotDisturb: !state.doNotDisturb }));
    }
  },
  toggleMentions: async () => {
    if (useMockData) {
      await fakeRequest({});
      set((state) => ({ mentions: !state.mentions }));
    }
  }
}));
