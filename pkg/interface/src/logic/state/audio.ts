import { useMemo } from 'react';
import create from 'zustand';
import shallow from 'zustand/shallow';

export interface Track {
  title: string;
  url: string;
}

function formatTime(num: number) {
  const minutes = Math.floor(num / 60);
  const seconds = Math.floor(num % 60);
  return `${minutes}:${seconds.toString().padStart(2, '0')}`;
}

export interface AudioState {
  //  transport
  play: () => void;
  pause: () => void;
  next: () => void;
  prev: () => void;
  playing: boolean;
  //  queueing
  queue: Track[];
  queueTracks: (ts: Track[], from: string) => void;
  clearQueue: () => void;
  cursor?: number;
  updateCursor: (cur: number) => void;
  changeTrack: (t: Track) => void;
  playingFrom?: string;
  //  playback
  domNode?: HTMLAudioElement;
  initDom: (url: string) => HTMLAudioElement;
  duration: number;
  durationInterval: NodeJS.Timeout | null;
  progress: number;
}

const useAudioState = create<AudioState>((set, get) => ({
  playing: false,
  domNode: undefined,
  cursor: undefined,
  playingFrom: undefined,
  play: () => {
    const { initDom, playing, domNode, cursor, queue } = get();
    if (playing) {
      return;
    }
    if( typeof cursor === 'undefined') {
      set({ cursor: 0 });
    }
    set({ playing: true });
    if (domNode) {
      domNode.play();
    } else {
      const track = queue[cursor ?? 0];
      if (track) {
        initDom(track.url);
      } else {
        set({ cursor: queue.length - 1 });
      }
    }
  },
  pause: () => {
    const { playing, domNode } = get();
    if (playing && domNode) {
      domNode.pause();
      set({ playing: false });
    }
  },
  queue: [] as Track[],
  queueTracks: (ts, resource) => {
    const { queue } = get();
    set({ queue: [...queue, ...ts], playingFrom: resource });
  },
  updateCursor: (cur: number) => {
    const { queue, initDom, domNode } = get();
    const newTrack = queue[cur];
    if (newTrack) {
      initDom(newTrack.url);
    } else {
      domNode?.pause?.();
      set({ playing: false });
    }
    set({ cursor: cur });
  },
  changeTrack: (t) => {
    const { cursor, updateCursor, queue } = get();
    const newQueue = [
      ...queue.slice(0, cursor + 1),
      t,
      ...queue.slice(cursor + 1)
    ];
    set({ queue: newQueue });
    updateCursor((cursor ?? -1) + 1);
  },
  clearQueue: () => {
    set({ queue: [], cursor: undefined, playingFrom: undefined });
  },
  next: () => {
    const { cursor, updateCursor } = get();
    updateCursor(cursor + 1);
  },
  prev: () => {
    const { cursor, updateCursor } = get();
    updateCursor(cursor - 1);
  },
  duration: 0,
  progress: 0,
  durationInterval: null,
  initDom: (url: string) => {
    const { domNode, durationInterval } = get();
    durationInterval && clearInterval(durationInterval);
    domNode?.pause?.();
    const audio = new Audio(url);
    audio.preload = 'metadata';
    audio.autoplay = true;

    function updateTime() {
      set({ progress: audio.currentTime });
    }
    function onMetadata() {
      set({ duration: audio.duration });
    }
    function onEnd() {
      const { next } = get();
      next();
    }
    audio.addEventListener('loadedmetadata', onMetadata);
    audio.addEventListener('ended', onEnd);
    const newDurationInterval = setInterval(updateTime, 250);
    set({
      playing: true,
      domNode: audio,
      durationInterval: newDurationInterval
    });

    return audio;
  }
}));

const selQueueAndCursor = (s: AudioState) => [s.cursor, s.queue, s.playingFrom] as const;

export function useFutureQueue() {
  const [cursor, queue, playingFrom] = useAudioState(selQueueAndCursor, shallow);

  return useMemo(
    () => [queue.slice((cursor ?? 0) + 1), playingFrom] as const,
    [cursor, queue]
  );
}

export function useCurrentTrack() {
  const [cursor, queue] = useAudioState(selQueueAndCursor, shallow);
  return useMemo(() => queue[cursor], [queue, cursor]);
}
const selTransport = (s: AudioState) =>
  [s.play, s.pause, s.playing, s.next, s.prev] as const;
export function useTransport() {
  const [play, pause, playing, next, prev] = useAudioState(selTransport);
  return useMemo(() => ({ playing, play, pause, next, prev }), [
    playing,
    play,
    pause,
    next,
    prev
  ]);
}

const selSeek = (s: AudioState) => [s.progress, s.duration] as const;
export function useFormattedSeek() {
  const [progress, duration] = useAudioState(selSeek);
  return useMemo(
    () => ({ progress: formatTime(progress), duration: formatTime(duration) }),
    [progress, duration]
  );
}

export default useAudioState;
