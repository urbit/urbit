import shallow from 'zustand/shallow';
import { useHarkStore } from './hark';

export const useNotifications = () => {
  const [unreads, reads] = useHarkStore((s) => [s.unreads, s.reads], shallow);
  const hasAnyNotifications = unreads.length > 0 || reads.size > 0;

  return {
    unreads,
    reads,
    hasAnyNotifications
  };
};
