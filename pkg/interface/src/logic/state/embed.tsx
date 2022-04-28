import { useCallback } from 'react';
import create from 'zustand';
import { suspend, Suspender } from '../lib/suspend';
import { jsonFetch } from '~/logic/lib/util';

export interface EmbedState {
  embeds: {
    [url: string]: any;
  };
  getEmbed: (url: string) => Suspender<any>;
  fetch: (url: string) => Promise<any>;
}

const OEMBED_PROVIDER = 'https://noembed.com/embed';

const useEmbedState = create<EmbedState>((set, get) => ({
  embeds: {},
  fetch: async (url: string) => {
    const { embeds } = get();
    if(url in embeds) {
      return embeds[url];
    }
    const search = new URLSearchParams({
      url
    });

    const embed = await jsonFetch(`${OEMBED_PROVIDER}?${search.toString()}`);
    return embed;
  },
  getEmbed: (url: string): Suspender<any> => {
    const { fetch, embeds } = get();
    if(url in embeds) {
      return embeds[url];
    }
    const { embeds: es } = get();
    const embed = suspend(fetch(url), {});
    set({ embeds: { ...es, [url]: embed } });
    return embed;
  }
}));

export function useEmbed(url: string) {
  return useEmbedState(useCallback(s => s.getEmbed(url), [url]));
}

export default useEmbedState;
