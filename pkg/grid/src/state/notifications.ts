import shallow from 'zustand/shallow';
import { useHarkStore } from './hark';

export const useNotifications = () => {
  const [unseen, seen] = useHarkStore((s) => [s.unseen, s.seen], shallow);
  const hasAnyNotifications = Object.keys(seen).length > 0 || Object.keys(unseen).length > 0;

  return {
    unseen,
    seen,
    hasAnyNotifications
  };
};
